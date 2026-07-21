#! /usr/bin/env python
#    Visualization for Bifurcation Manifolds
#    Copyright (C) 1997 Randy Paffenroth and John Maddocks
#
#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU  General Public
#    License as published by the Free Software Foundation; either
#    version 2 of the License, or (at your option) any later version.
#
#    This library is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    Library General Public License for more details.
#
#    You should have received a copy of the GNU Library General Public
#    License along with this library; if not, write to the Free
#    Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
#    MA 02111-1307, USA

import os
import sys
import struct
try:
    from UserDict import UserDict
    from UserList import UserList
except ImportError: #Python 3
    from collections import UserDict, UserList
try:
    file
except NameError: #Python 3
    from io import IOBase as file
from auto import AUTOExceptions
import copy
from auto import parseB
from auto import parseC
from auto import Points
import gzip
from auto import AUTOutil
from auto.AUTOutil import format19_10E3
import types

# Import file I/O utilities from separate module to avoid cyclic imports
from auto.fileS import fileS, PrematureEndofData, IncorrectHeaderLength, NPAR

# The parseS class parses an AUTO fort.8 file
# THESE EXPECT THE FILE TO HAVE VERY SPECIFIC FORMAT!
# it provides 4 methods:
# read and write take as an arguement either and input or output
#    stream (basically any object with has the method "readline"
#    for reading and "write" for writing)
#    
# readFilename and writeFilename take as an arguement a filename
#    in which to read/write the parameters (basically it opens the
#    file and then calles "read" or "write"
#    
# Once the data is read in the class provides a list all the points
# in the fort.8 file.

class parseS(list):
    def __init__(self,filename=None):
        if isinstance(filename, str):
            list.__init__(self)
            self.readFilename(filename)
        else:
            if filename is None:
                list.__init__(self)
            else:
                list.__init__(self,filename)

    def __str__(self):
        rep = ""
        rep = rep + "Number of solutions: " + str(len(self)) + "\n"
        labels = self.getLabels()
        rep = rep + "Labels: "
        for label in labels:
            rep = rep + str(label) + " "
        rep = rep + "\n"
        return rep

    def __call__(self,label=None):
        return self.getLabel(label)

    def load(self,**kw):
        """Load solution with the given AUTO constants.
        Returns a shallow copy with a copied set of updated constants
        """
        from auto.AUTOSolution import AUTOSolution
        irs = kw.get("IRS")
        if irs is None:
            irs = (kw.get("constants") or {}).get("IRS")
        sol = None
        if irs is not None:
            if irs != 0:
                try:    
                    sol = self(irs)
                except KeyError:
                    pass
        elif len(self) > 0:
            sol = self[-1]
        else:
            raise AUTOExceptions.AUTORuntimeError(
                "Cannot start from empty solution list.")
        if sol is None:
            sol = AUTOSolution()
        return sol.load(**kw)

    # This function needs a little explanation
    # It trys to read a new point from the input file, and if
    # it cannot (because the file ends prematurely) is sets the
    # file pointer back to the way it was when it started and returns
    # This will be used to check a file to see if it has a new point
    # and it will ignore a partially created solution.
    # Basically it for an VBM kind of program.
    def tryNextPointRead(self,inputfile):
        from auto.AUTOSolution import AUTOSolution
        current_position = inputfile.tell()
        try:
            self.append(AUTOSolution(inputfile,name=inputfile.name))
        except PrematureEndofData:
            inputfile.seek(current_position)

    def read(self,inputfile=None):
        # We now go through the file and read the solutions.
        from auto.AUTOSolution import AUTOSolution
        if inputfile is None:
            # read everything into memory
            for solution in self:
                solution.read()
            return
        if not isinstance(inputfile, fileS):
            inputfile = fileS(inputfile)
        for i in range(len(inputfile.solutions)):
            solution = AUTOSolution(inputfile,i,inputfile.name)
            self.append(solution)
        if len(self) > 0:
            mbr, mlab = 0, 0
            for d in self:
                if d["BR"] > mbr: mbr = d["BR"]
                if d["LAB"] > mlab: mlab = d["LAB"]
            for d in self:
                d._mbr, d._mlab = mbr, mlab

    def write(self,output,mlab=False):
        for i in range(len(self)):
            x = self[i]
            if i == len(self) - 1:
                # maybe write a header after the last solution so that AUTO can
                # pickup a new branch and solution label number
                x.write(output,mlab)
            else:
                x.write(output)
        output.flush()

    def readFilename(self,filename):
        inputfile = fileS(filename)
        self.read(inputfile)
        inputfile.conditionalclose()
        # else don't close but garbage collect

    def writeFilename(self,filename,append=False,mlab=False):
        # read all solutions because we may overwrite
        self.read()
        if append:
            output = open(filename,"ab")
        else:
            output = open(filename,"wb")
        self.write(output,mlab)
        output.close()

    # Removes solutions with the given labels or type name
    def deleteLabel(self,label=None,keep=0):
        if label is None:
            label = [lab for lab in parseB.all_point_types if lab not in
                     ["No Label", "RG", "UZ"]]
        if isinstance(label, (str, int)):
            label = [label]
        indices = []
        for i in range(len(self)):
            x = self[i]
            if ((not keep and (x["Label"] in label or x["Type name"] in label)
                 or (keep and not x["Label"] in label and 
                              not x["Type name"] in label))):
                indices.append(i)
        indices.reverse()
        for i in indices:
            del self[i]
        if len(self) > 0:
            maxlab = max(self.getLabels())
            for d in self:
                d._mlab = maxlab
            
    # Relabels the first solution with the given label
    def relabel(self,old_label=None,new_label=None):
        if old_label is None and new_label is None:
            i = 1
            new = parseS()
            for d in self:
                news = d.__class__(d)
                news["LAB"] = i
                news._mlab = len(self)
                i = i + 1
                new.append(news)
            return new
        if isinstance(old_label, int):
            old_label = [old_label]
            new_label = [new_label]
        for j in range(len(old_label)):
            for d in self:
                if d["Label"] == old_label[j]:
                    d["Label"] = new_label[j]
        if len(self) > 0:
            maxlab = max(self.getLabels())
            for d in self:
                d._mlab = maxlab

    # Make all labels in the file unique and sequential
    def uniquelyLabel(self):
        i = 1
        for d in self:
            d["Label"] = i
            d._mlab = len(self)
            i = i + 1

    # Given a label, return the correct solution
    def getLabel(self,label):
        if label is None:
            return self
        if isinstance(label, int):
            for d in self:
                if d["Label"] == label:
                    return d
            raise KeyError("Label %s not found"%label)
        if isinstance(label, str) and len(label) > 2 and label[-1].isdigit():
            j = 2
            if not label[2].isdigit():
                j = 3
            number = int(label[j:])
            i = 0
            for d in self:
                if d["Type name"] == label[:j]:
                    i = i + 1
                    if i == number:
                        return d
            raise KeyError("Label %s not found"%label)
        if isinstance(label, types.FunctionType):
            # accept a user-defined boolean function
            f = label
            cnt = getattr(f,"func_code",getattr(f,"__code__",None)).co_argcount
            if cnt == 1:
                # function takes just one parameter
                s = [s for s in self if f(s)]
            elif cnt == 2:
                # function takes two parameters: compare all solutions
                # with each other
                indices = set([])
                for i1, s1 in enumerate(self):
                    if i1 in indices:
                        continue
                    for i2 in range(i1+1,len(self)):
                        if i2 not in indices and f(s1, self[i2]):
                            indices.add(i2)
                s = [self[i] for i in sorted(indices)]
            else:
                raise AUTOExceptions.AUTORuntimeError(
                    "Invalid number of arguments for %s."%f.__name__)
            return self.__class__(s)
        if not AUTOutil.isiterable(label):
            label = [label]
        data = []
        counts = [0]*len(label)
        for d in self:
            ap = None
            if d["Label"] in label or d["Type name"] in label:
                ap = d
            for i in range(len(label)):
                lab = label[i]
                j = 2
                if len(lab) > 2 and not lab[2].isdigit():
                    j = 3
                if (isinstance(lab, str) and len(lab) > j and
                    d["Type name"] == lab[:j]):
                    counts[i] = counts[i] + 1
                    if counts[i] == int(lab[j:]):
                        ap = d
            if ap is not None:
                data.append(ap)
        return self.__class__(data)

    def getIndex(self,index):
        return self[index]

    # Return a list of all the labels in the file.
    def getLabels(self):
        return [x["Label"] for x in self]

def pointtest(a,b):
    keys = ['Type number', 'Type name', 'Parameter NULL vector',
            'Free Parameters', 'Branch number',
            'data', 'NCOL', 'Label', 'ISW', 'NTST',
            'Point number', 'Parameters']

    # make sure the solutions are fully parsed...
    scratch=a['Parameters']
    scratch=b['Parameters']
    for key in keys:
        if key not in a:
            raise AUTOExceptions.AUTORegressionError("No %s label"%(key,))
    if len(a["data"]) != len(b["data"]):
        raise AUTOExceptions.AUTORegressionError("Data sections have different lengths")


def test():
    print("Testing reading from a filename")
    foo = parseS()
    foo.readFilename("test_data/fort.8")    
    if len(foo) != 5:
        raise AUTOExceptions.AUTORegressionError("File length incorrect")
    pointtest(foo.getIndex(0),foo.getIndex(3))

    print("Testing reading from a stream")
    foo = parseS()
    fp = open("test_data/fort.8","rb")
    foo.read(fp)    
    if len(foo) != 5:
        raise AUTOExceptions.AUTORegressionError("File length incorrect")
    pointtest(foo.getIndex(0),foo.getIndex(3))

    
    
    print("parseS passed all tests")

if __name__ == '__main__' :
    test()
