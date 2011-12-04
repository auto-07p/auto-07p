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
import AUTOExceptions
import copy
import parseB
import parseC
import Points
import runAUTO
import gzip
import AUTOutil
import types

# End of data exception definition
class PrematureEndofData(Exception):
    pass

class IncorrectHeaderLength(Exception):
    pass

# This is the number of parameters.  Should be read from auto.h...
# This is not required anymore in AUTO97, since the last entry in
# the header line is this number
NPAR = 20

class fileS(object):
    def __init__(self, filename):
        if isinstance(filename, str):
            inputfile = AUTOutil.openFilename(filename,"rb")
        else:
            inputfile = filename
        self.inputfile = inputfile
        self.name = inputfile.name
        self.solutions = []

        # We now go through the file and read the solutions.
        prev = None
        # for fort.8 we need to read everything into memory; otherwise load the
        # data on demand from disk when we really need it
        # on Windows always load everything because deleting open files is
        # impossible there
        inmemory = (os.path.basename(inputfile.name) == 'fort.8' or
                    sys.platform in ['cygwin', 'win32'])
        while len(inputfile.read(1)) > 0:
            line = inputfile.readline()
            if not line: raise PrematureEndofData
            try:
                header = list(map(int, line.split()))
            except ValueError:
                raise PrematureEndofData
            if len(header) < 10:
                raise PrematureEndofData
            if len(header) == 10:
                # This is the case for AUTO94 and before
                header = header + [NPAR]
            numLinesPerEntry = header[8]
            start_of_data = inputfile.tell()
            if prev is not None and all(
                [header[i] == prevheader[i] for i in [4, 6, 7, 8, 11]]):
                # guess the end from the previous solution
                end += inputfile.tell() - prev
                # See if the guess for the solution end is correct
                inputfile.seek(end)
                data = inputfile.readline().split()
                # This is where we detect the end of the file
                if len(data) == 0:
                    data = inputfile.read(1)
                if len(data) != 0:
                    try:
                        # Check length of line...
                        if len(data) != 12 and len(data) != 16:
                            raise IncorrectHeaderLength
                        # and the fact they are all integers
                        map(int,data)
                        # and the fact that NCOL*NTST+1=NTPL
                        if int(data[9])*int(data[10])+1 != int(data[6]):
                            end = None
                        # If it passes all these tests we say it is a header line
                        # and we can read quickly
                    except:
                        # otherwise the guessed end is not valid
                        end = None
            else:
                end = None
            data = None
            if end is None:
                # We skip the correct number of lines in the entry to
                # determine its end.
                inputfile.seek(start_of_data)
                if inmemory:
                    data = "".encode("ascii").join([inputfile.readline()
                                   for i in range(numLinesPerEntry)])
                else:
                    for i in range(numLinesPerEntry):
                        inputfile.readline()
                end = inputfile.tell()
            elif inmemory:
                inputfile.seek(start_of_data)
                data = inputfile.read(end - start_of_data)
            else:
                inputfile.seek(end)
            if data is None:
                data = (start_of_data, end)
            self.solutions.append({'header': header, 'data': data})
            prev = start_of_data
            prevheader = header

    def readstr(self, i):
        solution = self.solutions[i]
        data = solution['data']
        if not isinstance(data, tuple):
            return data
        start = data[0]
        end = data[1]
        self.inputfile.seek(start)
        solution['data'] = self.inputfile.read(end - start)
        return solution['data']

    def readfloats(self, i, total):
        if not Points.numpyimported:
            Points.importnumpy()       
        N = Points.N
        data = self.readstr(i)
        if hasattr(N, "ndarray") and isinstance(data, N.ndarray):
            return data
        fromstring = Points.fromstring
        if fromstring:
            fdata = []
            if "D".encode("ascii") not in data:
                fdata = fromstring(data, dtype=float, sep=' ')
            if fdata == [] or len(fdata) != total:
                fdata = N.array(map(parseB.AUTOatof,
                                    data.split()), 'd')
            else:
                #make sure the last element is correct
                #(fromstring may not do this correctly for a
                #string like -2.05071-106)
                fdata[-1] = parseB.AUTOatof(
                    data[data.rfind(" ".encode("ascii"))+1:].strip())
        else:
            data = data.split()
            try:
                fdata = N.array(map(float, data), 'd')
            except ValueError:
                fdata = N.array(map(parseB.AUTOatof, data), 'd')
        if total != len(fdata):
            raise PrematureEndofData
        del self.solutions[i]['data']
        self.solutions[i]['data'] = fdata
        return fdata

    def conditionalclose(self):
        # if everything is in memory, close the file
        for s in self.solutions:
            if isinstance(s['data'], tuple):
                return
        self.inputfile.close()

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
        current_position = inputfile.tell()
        try:
            self.append(AUTOSolution(inputfile,name=inputfile.name))
        except PrematureEndofData:
            inputfile.seek(current_position)

    def read(self,inputfile=None):
        # We now go through the file and read the solutions.
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

# an old-style point and point keys within an AUTOSolution
class SLPointKey(UserList):
    def __init__(self, solution=None, index=None, coords=None):
        if coords=="u dot":
            self.solution = solution["udotps"]
        else:
            self.solution = solution
        self.index = index
    def __getattr__(self, attr):
        if attr == 'data':
            return [point[self.index] for point in self.solution.coordarray]
        raise AttributeError(attr)
    def __setitem__(self, i, item):
        self.solution.coordarray[i,self.index] = item
    def __str__(self):
        return str(self.data)
    def append(self, item):
        self.enlarge(1)
        self.solution.coordarray[-1,self.index] = item
        self.data.append(item)
    def extend(self, other):
        self.enlarge(len(other))
        for i in range(len(other)):
            self.solution.coordarray[-len(other)+i,self.index] = other[i]
    def enlarge(self, ext):
        # enlarges the dimension of coordarray and coordnames
        s = self.solution
        if s._dims is None:
            s._dims = [s.dimension]*len(s)
            s0 = s.coordnames[0]
            s.extend([s0[0:s0.find('(')+1]+str(s.dimension+i+1)+')'
                      for i in range(ext)])
        s._dims[self.index] = s.dimension
        if min(s._dims) == max(s._dims):
            s._dims = None

class SLPoint(Points.Point):
    def __init__(self, p, solution=None, index=None):
        Points.Point.__init__(self, p)
        self.index = index
        self.solution = solution

    def __contains__(self, key):
        return key in ["u", "u dot", "t"] or Points.Point.has_key(self,key)

    def has_key(self, key):
        return self.__contains__(key)

    def __getitem__(self, coords):
        if coords == "t":
            return self.solution.indepvararray[self.index]
        if coords in ["u", "u dot"]:
            return SLPointKey(self.solution, self.index, coords)
        return Points.Point.__getitem__(self, coords)

    def __setitem__(self, coords, item):
        if coords == 't':
            self.solution.indepvararray[self.index] = item
        Points.Point.__setitem__(self, coords, item)

    def __str__(self):
        return str({ "t" : self["t"], "u" : self["u"], "u dot" : self["u dot"]})

    __repr__ = __str__

class AUTOParameters(Points.Point):
    def __init__(self, kwd=None, **kw):
        if isinstance(kwd, self.__class__):
            for k,v in kwd.__dict__.items():
                self.__dict__[k] = v
            return
        if kwd is None and kw == {}:
            self.coordnames = []
            self.dimension = 0
            return
        coordnames = kw.get("coordnames",[])[:]
        if "coordarray" in kw:
            coordarray = kw["coordarray"]
            if len(coordarray) < len(coordnames):
                kw["coordarray"] = (list(coordarray) +
                                    (len(coordnames)-len(coordarray)) * [0.0])
            for i in range(len(coordnames),len(coordarray)):
                coordnames.append("PAR(%d)"%(i+1))
            kw["coordtype"] = Points.float64
            kw["coordnames"] = coordnames
        Points.Point.__init__(self,kwd,**kw)

    def __call__(self,index):
        return self.coordarray[index-1]

    def __str__(self):
        rep = []
        for i in range(1,len(self)+1,5):
            j = min(i+4,len(self))
            line = "PAR(%-7s "%("%d:%d):"%(i,j))
            for k in range(i,j+1):
                if self.coordnames[k-1] != "PAR(%d)"%k:
                    for k in range(i,j+1):
                        line += "    %-15s"%self.coordnames[k-1]
                    rep.append(line)
                    line = 12*" "
                    break
            line += "".join(["%19.10E"%self(k) for k in range(i,j+1)])
            rep.append(line)
        return "\n".join(rep)

# The AUTOsolution class parses an AUTO fort.8 file
# THESE EXPECT THE FILE TO HAVE VERY SPECIFIC FORMAT!
# it provides 4 methods:
# read and write take as an argument either and input or output
#    stream (basically any object with has the method "readline"
#    for reading and "write" for writing)
#    
# readFilename and writeFilename take as an argument a filename
#    in which to read/write the parameters (basically it opens the
#    file and then calles "read" or "write"
#    
# Used by itself is only reads in ONE solution from the file
# for example readFilename will only read the first solution
# Commonly it will be used in a container class only using the
# read and write methods and letting the outside class take care
# of opening the file.

class AUTOSolution(UserDict,Points.Pointset):

    data_keys = set(["PT", "BR", "TY number", "TY", "LAB",
                 "ISW", "NTST", "NCOL", "Active ICP", "rldot",
                 "udotps", "NPARI", "NDIM", "IPS", "IPRIV"])

    long_data_keys = {
        "Parameters": "p",
        "parameters": "p",
        "Parameter NULL vector": "rldot",
        "Free Parameters": "Active ICP",
        "Point number": "PT",
        "Branch number": "BR",
        "Type number": "TY number",
        "Type name": "TY",
        "TY name": "TY",
        "Label": "LAB"}

    def __init__(self,input=None,index=None,name=None,t=None,**kw):
        if isinstance(input,self.__class__):
            for k,v in input.__dict__.items():
                self.__dict__[k] = v
            UserDict.__init__(self,input)
        else:
            UserDict.__init__(self)
            self.c = None
            self.__input          = None
            self.__fullyParsed    = False
            self._dims            = None
            self._mbr             = 0
            self._mlab            = 0
            self.name = name
            self.data.update({"BR":1, "PT":1, "TY number":9, "LAB":0,
                              "ISW":1, "NTST": 1, "NCOL": 0,
                              "NPARI":0, "NDIM": 0, "IPS": None, "IPRIV":0})
            self.indepvarname = 't'
            self.coordnames = []
            self.__parnames = []

            if isinstance(input,(file,gzip.GzipFile)):
                input = fileS(input)
            if isinstance(input,fileS):
                self.read(input,index)
            elif input is not None:
                self.__readarray(input,t)
        self.update(**kw)

    def update(self, dct=None, **kw):
        par = None
        if "constants" in kw:
            self.c = kw["constants"]
            del kw["constants"]
            unames = dict(self.c.get("unames") or [])
            self.coordnames = [unames.get(i+1,'U(%d)'%(i+1))
                               for i in range(len(self.coordnames))]
            parlen = len(self.__parnames)
            parnames = dict(self.c.get("parnames") or [])
            if parnames != {}:
                parlen = max(parlen, max(parnames))
            par = dict(self.c.get("PAR") or [])
            for key in par:
                if key not in parnames.values():
                    parlen = max(key, parlen)
            self.__parnames = [parnames.get(i,"PAR(%d)"%i)
                               for i in range(1,parlen+1)]
            if ((self.name is None or os.path.basename(self.name) == 'fort.8')
                and self.c.get("e") is not None):
                self.name = self.c["e"]

        if self.name is None:
            self.name = ''
        if self.__fullyParsed:
            self.makeIxMaps()
            self.PAR = AUTOParameters(coordnames=self.__parnames,
                                      coordarray=Points.array(self.PAR),
                                      name=self.name)
        if dct is not None:
            for k,v in dct.items():
                self.data[k] = v
        for k,v in kw.items():
            self[k] = v

        if par is not None and not self.__nodata():
            self["PAR"] = par
            u = self.c.get("U")
            if u is not None:
                self["U"] = u

    def __nodata(self):
        return self.__input is None and not self.__fullyParsed

    def __getstate__(self):
        # For pickle: read everything
        self.__readAll()
        return Points.Pointset.__getstate__(self)

    def __str__(self):
        self.__readAll()
        keys = list(self.data)
        for key in ["BR","PT","LAB","TY number","ISW","NTST","NCOL","NDIM",
                    "IPS","IPRIV","NPARI"]:
            keys.remove(key)
        keys.sort()
        rep="  BR    PT  TY  LAB ISW NTST NCOL"
        #add corresponding L2-NORM, etc, from fort.7
        if self["IPS"] is not None:
            rep = rep+" NDIM IPS IPRIV"
        for key in keys:
            if key not in self.data_keys:
                rep = rep+"%19s"%key
        rep=rep+ "\n%4d%6d%4s%5d%4d%5d%5d" % (self["BR"], self["PT"],
                                              self["TY"], self["LAB"],
                                              self["ISW"], self["NTST"],
                                              self["NCOL"])
        if self["IPS"] is not None:
            rep = rep+"%5d%4d%6d"%(self["NDIM"],self["IPS"],self["IPRIV"])
        for key in list(keys):
            if key not in self.data_keys:
                rep = rep+"%19.10E"%self[key]
                keys.remove(key)
        if not self.__nodata():
            rep=rep+"\n"+Points.Pointset.__repr__(self)
        for key in keys:
            v = self[key]
            if isinstance(v,Points.Pointset):
                v = repr(v)
            elif type(v) not in [type(1),type(1.0),Points.float64,type("")]:
                v = list(v)
            if type(v) == type([]) and type(v[0]) not in [type(1),type(1.0),
                                                          Points.float64]:
                v = map(str,v)
            rep=rep+"\n"+str(key)+": "+str(v)
        if not self.__nodata():
            rep=rep+"\n"+str(self.PAR)
        return rep

    def __repr__(self):
        result = id(self)
        if result < 0:
            # avoid negative addresses and Python 2.3 warnings
            result += 256 ** struct.calcsize('P')
        return "<_=%s instance at %#010x>"%(self.__class__.__name__,result)

    def __len__(self):
        return Points.Pointset.__len__(self)

    def __setitem__(self,key,value):
        if (type(key) == type("") and not key in self.coordnames and
            key != self.indepvarname and not key in self.__parnames):
            shortkey = self.long_data_keys.get(key,key)
            if shortkey in self.data_keys:
                if shortkey == "TY":
                    value = parseB.reverse_type_translation(value)
                    shortkey = "TY number"
                elif shortkey == "BR":
                    self._mbr = 0
                elif shortkey == "LAB":
                    self._mlab = 0
                self.data[shortkey] = value
                return
            if shortkey == "PAR":
                if type(value) == type({}):
                    value = value.items()
                for k,v in value:
                    if isinstance(k,str):
                        self.PAR[k] = v
                    else:
                        self.PAR[k-1] = v
                return
            if shortkey == "p":
                self.PAR = AUTOParameters(coordnames=self.__parnames,
                                          coordarray=value, name=self.name)
                return
            if shortkey == "U":
                if type(value) == type({}):
                    value = value.items()
                for i,(k,v) in enumerate(value):
                    if isinstance(k,str):
                        value[i] = self.coordnames.index(k)
                if len(self.coordarray[0]) > 1:
                    # reduce solution to one point
                    del self.coordarray
                    del self.indepvararray
                    self.coordarray = Points.N.array([[0.0]]*max(dict(value)))
                    self.indepvararray = Points.N.array([0.0])
                    self.data.update({"NTST": 1, "NCOL": 0})
                    del self.data["Active ICP"]
                    del self.data["rldot"]
                    del self.data["udotps"]
                for k,v in value:
                    self.coordarray[k-1,0] = v
                return
        try:
            Points.Pointset.__setitem__(self,key,value)
        except (TypeError, ValueError, KeyError, IndexError):
            if self.__nodata():
                raise AUTOExceptions.AUTORuntimeError("Unknown option: %s"%key)
            self.PAR[key] = value

    def __getitem__(self,key):
        big_data_keys = ["data","Active ICP","rldot","p","udotps"]
        if (isinstance(key,str) and key not in self.coordnames and
            key != self.indepvarname and key not in self.__parnames):
            shortkey = self.long_data_keys.get(key,key)
            if shortkey in big_data_keys:
                self.__readAll()
            if shortkey in self.data_keys:
                if shortkey == "TY":
                    return parseB.type_translation(
                        self.data["TY number"])["short name"]
                return self.data[shortkey]
            if shortkey == "p":
                return self.PAR
            if shortkey == "data":
                return self
        if isinstance(key,str) and hasattr(self,"b") and key in self.b:
            if not self.__fullyParsed or not key in self.PAR:
                if key not in self.coordnames:
                    return self.b[key]
        self.__readAll()
        if isinstance(key, str):
            try:
                return Points.Pointset.__getitem__(self,key)
            except:
                try:
                    return self.PAR[key]
                except:
                    return self.data[key]
        ret = Points.Pointset.__getitem__(self,key)
        if not isinstance(key, int):
            return ret
        return SLPoint(ret, self, key)

    def __call__(self, p=None, coords=None):
        if p is None:
            return(str(self))
        return Points.Pointset.__call__(self, p, coords)

    def __copy__(self):
        return self.__class__(self)

    def copy(self):
        return self.__copy__()

    def __contains__(self,key):
        return (key in ["data","TY"] or key in self.long_data_keys or
                key in self.__parnames or
                (not self.__fullyParsed and key in self.data_keys) or
                (self.__fullyParsed and key in self.data) or
                (hasattr(self,"b") and key in self.b) or
                Points.Pointset.has_key(self,key))

    def has_key(self, key):
        return self.__contains__(key)

    def get(self, key, failobj=None):
        if key in self:
            return self[key]
        return failobj

    def type(self):
        return parseB.type_translation(self["Type number"])["long name"]

    def load(self,**kw):
        """Load solution with the given AUTO constants.
        Returns a shallow copy with a copied set of updated constants
        """
        constants = kw.get("constants")
        if "constants" in kw:
            del kw["constants"]
        c = parseC.parseC(self.c)
        datakw = {}
        for key in self.data_keys:
            if key in kw and (key not in c or key in ["LAB", "TY"]):
                datakw[key] = kw[key]
                if key not in ["LAB", "TY"]:
                    del kw[key]
        oldirs = c["IRS"]
        c.update(constants, **kw)
        if oldirs is not None and c["IRS"] == 0:
            return AUTOSolution(constants=c, **datakw)
        if self["LAB"] != 0:
            c["IRS"] = self["LAB"]
        return AUTOSolution(self, constants=c, **datakw)

    def run(self,**kw):
        """Run AUTO.

        Run AUTO from the solution with the given AUTO constants.
        Returns a bifurcation diagram of the result.
        """
        return runAUTO.runAUTO(selected_solution=self.load(**kw)).run()

    def readAllFilename(self,filename):
        inputfile = fileS(filename)
        self.readAll(inputfile)
        inputfile.close()

    def readFilename(self,filename):
        inputfile = fileS(filename)
        self.read(inputfile)
        inputfile.conditionalclose()
        # else don't close the input file but garbage collect

    def writeFilename(self,filename,mlab=False):
        output = open(filename,"wb")
        self.write(output,mlab)
        output.flush()
        output.close()

    def writeRawFilename(self,filename):
        output = open(filename,"w")
        self.writeRaw(output)
        output.flush()
        output.close()
        
    def toArray(self):
        return [
            [vector["t"]] + [point for point in vector["u"]]
            for vector in self["data"]]

    def writeRaw(self,output):
        s = "%24.15E"*(self.coordarray.shape[0]+1)
        for i in range(self.coordarray.shape[1]):
            output.write(s%((self.indepvararray[i],)+
                            tuple(self.coordarray[:,i]))+"\n")
            
    def read(self, inputfile=None, index=0):
        if self.__fullyParsed:
            return
        if inputfile is None:
            # read data into memory
            self.__input.readstr(self.__index)
            self.__input.conditionalclose()
            return
        if not isinstance(inputfile, fileS):
            inputfile = fileS(inputfile)
        self.__input = inputfile
        self.__index = index
        self.__readHeader()
    
    def readAll(self, inputfile):
        self.read(inputfile)
        self.__readAll()

    def __readHeader(self):
        header = self.__input.solutions[self.__index]['header']
        self.indepvarname = 't'
        self.__numEntriesPerBlock = header[7]
        ndim = self.__numEntriesPerBlock-1
        if ndim < len(self.coordnames):
            self.coordnames = self.coordnames[:ndim]
        for i in range(len(self.coordnames),
                       self.__numEntriesPerBlock-1):
            self.coordnames.append("U(%d)"%(i+1))

        self.update({"NPARI": 0, "NDIM": 0, "IPS": None, "IPRIV": 0})
        for i, key in enumerate(["BR", "PT", "TY number", "LAB", "",
                                 "ISW", "", "", "", "NTST",
                                 "NCOL", "", "NPARI", "NDIM", "IPS",
                                 "IPRIV"]):
            if key and i < len(header):
                self[key] = header[i]
        self.__numChangingParameters = header[4]
        self.__numSValues = header[6]
        self.__numLinesPerEntry = header[8]
        self.__numFreeParameters = header[11]
        
    def __readAll(self):
        if self.__fullyParsed or self.__nodata():
            return
        if not Points.numpyimported:
            Points.importnumpy()
        fromstring = Points.fromstring
        N = Points.N
        self.__fullyParsed = True
        n = self.__numEntriesPerBlock
        nrows = self.__numSValues
        total = n * nrows + self.__numFreeParameters
        nlinessmall = (((n-1)/7+1) * nrows + (self.__numFreeParameters+6)/7)
        if self["NTST"] != 0 and self.__numLinesPerEntry > nlinessmall:
            total += 2 * self.__numChangingParameters + (n-1) * nrows
        fdata = self.__input.readfloats(self.__index, total)
        ups = N.reshape(fdata[:n * nrows],(nrows,n))
        self.indepvararray = ups[:,0]
        self.coordarray = N.transpose(ups[:,1:])
        j = n * nrows

        # Check if direction info is given
        if self["NTST"] != 0 and self.__numLinesPerEntry > nlinessmall:
            nfpr = self.__numChangingParameters
            self["Active ICP"] = list(map(int,fdata[j:j+nfpr]))
            j = j + nfpr
            self["rldot"] = fdata[j:j+nfpr]
            j = j + nfpr
            n = n - 1
            self["udotps"] = N.transpose(
                N.reshape(fdata[j:j+n * self.__numSValues],(-1,n)))
            udotnames = ["UDOT(%d)"%(i+1) for i in
                         range(self.__numEntriesPerBlock-1)]
            self["udotps"] = Points.Pointset({
                "coordarray": self["udotps"],
                "coordnames": udotnames,
                "name": self.name})
            self["udotps"]._dims = None
            j = j + n * nrows

        self.PAR = fdata[j:j+self.__numFreeParameters]
        Points.Pointset.__init__(self,{
                "indepvararray": self.indepvararray,
                "indepvarname": self.indepvarname,
                "coordarray": self.coordarray,
                "coordnames": self.coordnames,
                "name": self.name})
        self.update()

    def __readarray(self,coordarray,indepvararray=None):
        #init from array
        if not Points.numpyimported:
            Points.importnumpy()        
        N = Points.N
        if not hasattr(coordarray[0],'__len__'):
            # point
            indepvararray = [0.0]
            ncol = 0
            ntst = 1
            coordarray = [[d] for d in coordarray]
            pararray = []
        else:
            # time + solution
            if indepvararray is None:
                indepvararray = coordarray[0]
                coordarray = coordarray[1:]
            ncol = 1
            ntst = len(indepvararray)-1
            t0 = indepvararray[0]
            period = indepvararray[-1] - t0
            if period != 1.0 or t0 != 0.0:
                #scale to [0,1]
                for i in range(len(indepvararray)):
                    indepvararray[i] = (indepvararray[i] - t0)/period
            # set PAR(11) to period
            pararray = 10*[0.0] + [period]
        indepvarname = "t"
        ndim = len(coordarray)
        coordnames = ["U(%d)"%(i+1) for i in range(ndim)]
        Points.Pointset.__init__(self,{"indepvararray": indepvararray,
                                       "indepvarname": indepvarname,
                                       "coordarray": coordarray,
                                       "coordnames": coordnames})
        self.__fullyParsed = True
        self.data.update({"NTST": ntst, "NCOL": ncol, "LAB": 1, "NDIM": ndim})
        self.__numChangingParameters = 1
        self.PAR = pararray

    def __getattr__(self,attr):
        if self.__nodata():
            raise AUTOExceptions.AUTORuntimeError("Solution without data.")
        if not self.__fullyParsed and attr != "__del__":
            self.__readAll()
            return getattr(self,attr)
        raise AttributeError(attr)

    def write(self,output,mlab=False):
        if self.__nodata():
            return
        try:
            "".encode("ascii") + ""
            def write_enc(s):
                #write encoded
                output.write(s)
        except TypeError: #Python 3.0
            def write_enc(s):
                #write encoded
                output.write(s.encode("ascii"))

        if self.__fullyParsed:
            ndim = len(self.coordarray)
            npar = len(self["Parameters"])
            ntpl = len(self)
            nrowpr = (ndim//7+1) * ntpl + (npar+6)//7
            nfpr = self.__numChangingParameters
            if "Active ICP" in self.data:
                nfpr = len(self.get("Active ICP",[0]))
                nrowpr += (nfpr+19)//20 + (nfpr+6)//7 + (ndim+6)//7 * ntpl
        else:
            ndim = self.__numEntriesPerBlock-1
            npar = self.__numFreeParameters
            ntpl = self.__numSValues
            nfpr = self.__numChangingParameters
            nrowpr = self.__numLinesPerEntry

        line = "%6d%6d%6d%6d%6d%6d%8d%6d%8d%5d%5d%5d" % (self["BR"],
                                                         self["PT"],
                                                         self["TY number"],
                                                         self["LAB"],
                                                         nfpr,
                                                         self["ISW"],
                                                         ntpl,
                                                         ndim+1,
                                                         nrowpr,
                                                         self["NTST"],
                                                         self["NCOL"],
                                                         npar
                                                         )
        if self["IPS"] is not None:
            line += "%5d%5d%5d%5d" % (self["NPARI"],self["NDIM"],self["IPS"],
                                      self["IPRIV"])
        write_enc(line+os.linesep)
        # If the file isn't already parsed, we can just copy from the input
        # file into the output file
        if not self.__fullyParsed:
            output.write(self.__input.readstr(self.__index))
        # Otherwise we do a normal write.  NOTE: if the solution isn't already
        # parsed it will get parsed here.
        else:
            slist = []
            for i in range(len(self.indepvararray)):
                slist.append("    "+"%19.10E" % (self.indepvararray[i]))
                for j in range(1,len(self.coordarray)+1):
                    if j%7==0:
                        slist.append(os.linesep+"    ")
                    slist.append("%19.10E" % (self.coordarray[j-1,i]))
                slist.append(os.linesep)
            write_enc("".join(slist))
            if "Active ICP" in self.data:
                # Solution contains derivative information.
                j = 0
                for parameter in self["Active ICP"]:
                    write_enc("%5d" % (parameter))
                    j = j + 1
                    if j%20==0:
                        write_enc(os.linesep)
                if j%20!=0:
                    write_enc(os.linesep)

                line = "    "
                i = 0
                for vi in self["rldot"]:
                    num = "%19.10E" % (vi)
                    if i != 0 and i%7==0:
                        line = line + os.linesep + "    "
                    line = line + num
                    i = i + 1
                write_enc(line+os.linesep)

                # write UDOTPS
                slist = []
                c = self["udotps"].coordarray
                l = len(c)
                for i in range(len(self.indepvararray)):
                    slist.append("    ")
                    for j in range(len(self.coordarray)):
                        if j!=0 and j%7==0:
                            slist.append(os.linesep+"    ")
                        if j<l:
                            slist.append("%19.10E" %c[j,i])
                        else:
                            slist.append("%19.10E" %0)
                    slist.append(os.linesep)
                write_enc("".join(slist))

            line = "    "
            j = 0
            for parameter in self.PAR.toarray():
                num = "%19.10E" % (parameter)
                line = line + num 
                j = j + 1
                if j%7==0:
                    write_enc(line+os.linesep)
                    line = "    "
            if j%7!=0:
                write_enc(line+os.linesep)
        if mlab and (self._mbr > 0 or self._mlab > 0) and not (
            self._mbr == self["BR"] and self._mlab == self["LAB"]):
            # header for empty solution so that AUTO can pickup the maximal
            # label and branch numbers.
            if self["IPS"] is not None:
                write_enc("%6d%6d%6d%6d%6d%6d%8d%6d%8d%5d%5d%5d%5d%5d%5d%5d%s"%
                      ((self._mbr, 0, 0, self._mlab) + 12*(0,) + (os.linesep,)))
            else:
                write_enc("%6d%6d%6d%6d%6d%6d%8d%6d%8d%5d%5d%5d%s"%
                      ((self._mbr, 0, 0, self._mlab) + 8*(0,) + (os.linesep,)))
        output.flush()

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
