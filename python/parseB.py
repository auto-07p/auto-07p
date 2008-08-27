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

import string
import os
import sys
import AUTOExceptions
import types
import parseC
try:
    import matplotlib.numerix
    N = matplotlib.numerix
except ImportError:
    try:
        import numpy
        N = numpy
        N.nonzero = N.flatnonzero
    except ImportError:
        try:
            import Numeric
            N = Numeric
        except ImportError:
            import array
            N = array

type_translation_dict = {
       0: {"long name" : "No Label","short name" : "No Label"},
       1: {"long name" : "Branch point (algebraic problem)","short name" : "BP"},
       2: {"long name" : "Fold (algebraic problem)","short name" : "LP"},
       3: {"long name" : "Hopf bifurcation (algebraic problem)","short name" : "HB"},
       4: {"long name" : "Regular point (every NPR steps)","short name" : "RG"},
      -4: {"long name" : "User requested point","short name" : "UZ"},
       5: {"long name" : "Fold (ODE)","short name" : "LP"},
       6: {"long name" : "Bifurcation point (ODE)","short name" : "BP"},
       7: {"long name" : "Period doubling bifurcation (ODE)","short name" : "PD"},
       8: {"long name" : "Bifurcation to invarient torus (ODE)","short name" : "TR"},
       9: {"long name" : "Normal begin or end","short name" : "EP"},
      -9: {"long name" : "Abnormal termination","short name" : "MX"}}

# A little dictionary to transform types to human readable strings
def type_translation(type):
    if type>=0:
        type=type%10
    else:
        type=-((-type)%10)
    if type in type_translation_dict.keys():
        return type_translation_dict[type]
    else:
        return {"long name" : "Unknown type",
                "short name" : "Unknown type"}
    
# The parseB and AUTOBranch classes parse an AUTO fort.7 file
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
# in the fort.7 file.

# a branch within the parseB class
class AUTOBranch:
    def __init__(self,input=None,screen_lines=0,prevline=None):
        if input is not None:
            self.read(input,screen_lines,prevline)

    def __parse(self,headerlist=None,ncolumns=None,linelen=None,
                datalist=None):
	if not headerlist:
            return
        header = string.join(headerlist,"")
        self.header = header
        if header != "":
            self.constants = self.parseHeader(header)
        line = headerlist[-1]
        if string.find(line, " PT ") != -1:
            columnlen = (linelen - 19) / (ncolumns - 4)
            n = linelen - columnlen * (ncolumns - 4)
            self.coordnames = []
            for i in range(ncolumns-4):
                self.coordnames.append(string.rstrip(line[n:n+columnlen]))
                n = n + columnlen
        if not hasattr(N,"transpose"):
            self.__parsearray(ncolumns,datalist)
            return
        try:
            data = N.array(map(float, datalist),'d')
        except:
            data = N.array(map(AUTOatof, datalist),'d')
        self.BR = int(data[0])
        data.shape = (-1,ncolumns)
        self.coordarray = N.transpose(data[:,4:]).copy()
        self.labels = {}
        for i in N.nonzero(N.fabs(data[:,2])+data[:,3]):
            self.labels[i] = {"LAB":int(data[i,3]),
                              "PT":int(data[i,1]),
                              "TY number":int(data[i,2])}
        points = data[:,1]
        # self.stability gives a list of point numbers where the stability
        # changes: the end point of each part is stored
        self.stability = N.concatenate((N.compress(
                    N.less(points[:-1]*points[1:],0),
                    points[:-1]),points[-1:])).astype('i')

    def __parsearray(self,ncolumns,datalist):
        # for those without numpy...
        try:
            data = map(float, datalist)
        except:
            data = map(AUTOatof, datalist)
        self.BR = int(data[0])
        columns = []
        try:
            for i in range(4,ncolumns):
                columns.append(N.array('d',data[i::ncolumns]))
        except TypeError:
            for i in range(4,ncolumns):
                columns.append(N.array('d',
                                       map(lambda j, d=data: 
                                           d[j], xrange(i,len(data),ncolumns))))
        self.coordarray = columns
        self.labels = {}
        self.stability = []
        prevpt = data[1]
        stab = []
        for j in xrange(0,len(data),ncolumns):
            [pt,ty,lab] = map(int,data[j+1:j+4])
            if lab != 0 or ty != 0:
                self.labels[j/ncolumns] = {"LAB":lab,"TY number":ty,"PT":pt}
            if pt * prevpt < 0:
                self.stability.append(prevpt)
            prevpt = pt
        self.stability.append(pt)

    def __str__(self):
        return self.summary()

    def __getitem__(self,index):
        return self.getIndex(index)

    def __call__(self,label=None):
        if label:
            return self.getLabel(label)
        return self

    def __len__(self):
        return len(self.coordarray[0])

    # Removes solutions with the given labels or type names
    def deleteLabel(self,label=None,keepTY=0,keep=0):
        if label == None:
            label=['BP','LP','HB','PD','TR','EP','MX']
        if type(label) != types.ListType:
            label = [label]
        labels = self.labels
        for k,v in labels.items():
            ty_name = type_translation(v["TY number"])["short name"]
            if ((not keep and (v["LAB"] in label or ty_name in label)) or
               (keep and not v["LAB"] in label and not ty_name in label)):
                v["LAB"] = 0
                if not keepTY:
                    v["TY number"] = 0
                if v["TY number"] == 0:
                    del labels[k]
            
    # Relabels the first solution with the given label
    def relabel(self,old_label,new_label):
        labels = self.labels
        if type(old_label)  == types.IntType:
            for v in labels.values():
                if v["LAB"] == old_label:
                    v["LAB"] = new_label
        else:
            for j in range(len(old_label)):
                for v in labels.values():
                    if v["LAB"] == old_label[j]:
                        v["LAB"] = new_label[j]

    # Make all labels in the file unique and sequential
    def uniquelyLabel(self,label=1):
        keys = self.labels.keys()
        keys.sort()
        for index in keys:
            v = self.labels[index]
            if v["LAB"] != 0:
                v["LAB"] = label
                label = label + 1

    # Given a label, return the correct solution
    def getLabel(self,label):
        if type(label) == types.IntType:
            for k,v in self.labels.items():
                if v["LAB"] == label:
                    return self.getIndex(k)
            return
        if type(label) != types.ListType:
            label = [label]        
        labels = {}
        for k,v in self.labels.items():
            ty_name = type_translation(v["TY number"])["short name"]
            if v["LAB"] in label or ty_name in label:
                labels[k] = v
        if labels == {}:
            return
        new = self.__class__()
        new.BR = self.BR
        new.header = self.header
        new.coordnames = self.coordnames
        new.coordarray = self.coordarray
        new.labels = labels
        new.stability = self.stability
        return new

    # Return a parseB style line item; if given a string, return the
    # relevant column
    def getIndex(self,index):
        if type(index) == type(""):
            index = string.strip(index)
            for i in range(len(self.coordnames)):
                if string.strip(self.coordnames[i]) == index:
                    return self.coordarray[i]
            raise IndexError
        label = None
        if index in self.labels.keys():
            label = self.labels[index]
        if label:
            pt = label["PT"]
            tynumber = label["TY number"]
            lab = label["LAB"]
        else:
            pt = index+1
            for p in self.stability:
                if abs(p) >= pt:
                    if p < 0:
                        pt = -pt
                    break
            tynumber = 0
            lab = 0
        data = []
        for j in range(len(self.coordarray)):
            data.append(self.coordarray[j][index])
        return {"BR": self.BR,
                "PT": pt,
                "TY number": tynumber,
                "TY name": type_translation(tynumber)["short name"],
                "LAB": lab,
                "data": data,
                "section": 0,
                "index": index}

    # Get all the labels from the solution
    def getLabels(self):
        labels = []
        keys = self.labels.keys()
        keys.sort()
        for index in keys:
            x = self.labels[index]
            if x["LAB"] != 0:
                labels.append(x["LAB"])
        return labels

    def writeRawFilename(self,filename):
	output = open(filename,"w")
	self.writeRaw(output)
        output.flush()
	output.close()
        
    def toArray(self):
        array = []
        data = self.coordarray
        for i in range(len(data[0])):
            row = []
            for j in range(len(data)):
                row.append(data[j][i])
            array.append(row)
        return array

    def writeRaw(self,output):
        data = self.coordarray
        for i in range(len(data[0])):
            for j in range(len(data)):
                output.write(str(data[j][i])+" ")
            output.write("\n")
                
    def write(self,output,columnlen=19):
        format = "%"+str(-columnlen)+"s"
        if self.header != "":
            for l in string.split(self.header,"\n"):
                if string.find(l," PT ") == -1 and l != "":
                    output.write(l+"\n")
        if self.coordnames != []:
            output_line = ["   0    PT  TY  LAB "]
            for name in self.coordnames:
                output_line.append(format%name)
            output.write(string.join(output_line,"")+'\n')
        br = self.BR
        data = self.coordarray
        istab = 0
        format = "%"+str(columnlen)+"."+str(columnlen-9)+"E"
        for i in range(len(data[0])):
            if self.labels.has_key(i):
                label = self.labels[i]
                pt = label["PT"]
                tynumber = label["TY number"]
                lab = label["LAB"]
            else:
                pt = i+1
                if self.stability[istab] < 0:
                    pt = -pt
                tynumber = 0
                lab = 0
            if pt == self.stability[istab]:
                istab = istab + 1
            output_line = "%4d%6d%4d%5d"%(br,pt,tynumber,lab)
            for j in range(len(data)):
                output_line = output_line + format%data[j][i]
            output.write(output_line+"\n")

    def writeShort(self):
        self.write(sys.stdout,columnlen=14)

    def summary(self):
        slist = []
        data = self.coordarray
        if self.coordnames != []:
            output_line = ["\n  BR    PT  TY  LAB "]
            for name in self.coordnames:
                output_line.append("%-14s"%name)
            slist.append(string.join(output_line,""))
        keys = self.labels.keys()
        keys.sort()
        for index in keys:
            label = self.labels[index]
            ty_number = label["TY number"]
            if ty_number == 0:
                continue
            ty_name = type_translation(ty_number)["short name"]
            if ty_name=='RG':
                ty_name = '  '
            output_line = "%4d%6d%4s%5d"%(abs(self.BR),abs(label["PT"]),
                                          ty_name,label["LAB"])
            for i in range(len(data)):
                output_line = output_line + "%14.5E"%data[i][index]
            slist.append(output_line)
        return string.join(slist,"\n")

    def writeScreen(self):
        sys.stdout.write(self.summary())

    def writeFilename(self,filename):
	output = open(filename,"w")
	self.write(output)
	output.close()

    def read(self,inputfile,screen_lines=0,prevline=None):
        # We now go through the file and read the branches.
        # read the branch header
        # A section is defined as a part of a fort.7
        # file between "headers", i.e. those parts
        # of the fort.7 file which start with a 0
        # and contain information about the branch
        # FIXME:  I am not sure of this is the correct
        # fix to having multiple sections of a fort.7
        # file with the same branch number.  What it comes
        # dowm to is keeping the fort.7 and fort.8 files
        # in sync.  I.e. I could make sure that
        # this branch numbers are unique here, but then
        # the fort.7 file will not match the fort.8 file.
        # Another way for a section to start is with a point number
        # equal to 1.
        self._lastline = None
        split = string.split
        if hasattr(str,"split"):
            split = str.split
        if prevline:
            line = prevline
        elif hasattr(inputfile,"next"):
            line = inputfile.next()
        else:
            if type(inputfile) != type([]):
                inputfile = inputfile.readlines()
            line = inputfile.pop(0)
        headerlist = []
        columns = split(line)
        if columns[0] == '0':
            headerlist.append(line)
            for line in inputfile:
                columns = split(line)
                if columns[0] != '0':
                    break
                headerlist.append(line)
        if type(inputfile) == type([]):
            del inputfile[:len(headerlist)]
        ncolumns = len(columns)
        linelen = len(line)
        datalist = []
        if columns[0] != '0':
            if screen_lines:
                if columns[2] != 0:
                    datalist = columns
                n = 1
                for line in inputfile:
                    columns = split(line)
                    if columns[0] == '0' or columns[1] == '-1' or columns[1] == '1':
                        break
                    if columns[2] != 0:
                        datalist.extend(columns)
                    n = n + 1
            else:
                datalist = columns
                for line in inputfile:
                    columns = split(line)
                    if columns[0] == '0' or columns[1] == '-1' or columns[1] == '1':
                        break
                    datalist.extend(columns)
            if columns[0] == '0' or columns[1] == '-1' or columns[1] == '1':
                self._lastline = line
        self.__parse(headerlist,ncolumns,linelen,datalist)
        if type(inputfile) == type([]):
            del inputfile[:len(self.coordarray[0])]

    def readFilename(self,filename,screen_lines=0):
	inputfile = open(filename,"r")
	self.read(inputfile,screen_lines)
	inputfile.close()

    def parseHeader(self,header):
        split = string.split
        if hasattr(str,"split"):
            split = str.split
        header = split(header,'\n')
        dict = parseC.parseC()
        i = 0
        words = split(header[0])
        if len(words) < 5:
            return
        for key in ["RL0","RL1","A0","A1"]:
            i = i + 1
            dict[key] = AUTOatof(words[i])
        key = ""
        for line in header[1:]:
            line = string.replace(line,"="," ")
            line = string.replace(line,"s:",":")
            words = split(line)
            if len(words) < 2:
                break
            if words[1] in ["User-specified", "Active"]:
                index = words.index("parameter:") + 1
                if words[1][0] == "U":
                    key = "ICP"
                else:
                    key = "Active ICP"
                dict[key] = map(int,words[index:])
                continue
            i = 1
            while i < len(words):
                key = words[i]
                v = words[i+1]
                i = i+2
                if key[0] in ["E","D"]:
                    v = AUTOatof(v)
                else:
                    try:
                        v = int(v)
                    except:
                        v = 9999
                dict[key] = v
        return dict

class parseB(AUTOBranch):
    def __init__(self,filename=None,screen_lines=0):
        self.branches = []
        if type(filename) == types.StringType:
            self.readFilename(filename,screen_lines)

    def __len__(self):
        l = 0
        for d in self.branches:
            l = l + len(d)
        return l

    # Removes solutions with the given labels or type names
    def deleteLabel(self,label=None,keepTY=0,keep=0):
        for d in self.branches:
            d.deleteLabel(label,keepTY,keep)
            
    # Relabels the first solution with the given label
    def relabel(self,old_label,new_label):
        for d in self.branches:
            d.relabel(old_label,new_label)

    # Make all labels in the file unique and sequential
    def uniquelyLabel(self):
        label = 1
        for d in self.branches:
            d.uniquelyLabel(label)
            for v in d.labels.values():
                if v["TY number"] != 0:
                    label = v["LAB"]
            label = label + 1
            
    # Given a label, return the correct solution
    def getLabel(self,label):
        if type(label) == types.IntType:
            for d in self.branches:
                item = d.getLabel(label)
                if item:
                    return item
            return
        new = self.__class__()
        new.branches = []
        for d in self.branches:
            newbranch = d.getLabel(label)
            if newbranch:
                new.branches.append(newbranch)
        return new

    # Given an index, return the correct solution
    # Return a parseB style line item
    def getIndex(self,index):
        if type(index) == type(""):
            return self.branches[0].getIndex(index)
        section = 0
        i = index
        for d in self.branches:
            l = len(d.coordarray[0])
            if i < l:
                item = d.getIndex(i)
                item["section"] = section
                item["index"] = index
                return item
            i = i - l
            section = section + 1
        raise IndexError

    # Get all the labels from the solution
    def getLabels(self):
        labels = []
        for d in self.branches:
            labels.extend(d.getLabels())
        return labels

    def toArray(self):
        array = []
        for d in self.branches:
            array.extend(d.toArray())
        return array

    def writeRaw(self,output):
        for d in self.branches:
            d.writeRaw(output)
            output.write("\n")
                
    def write(self,output):
        for d in self.branches:
            d.write(output)

    def writeShort(self):
        for d in self.branches:
            d.writeShort()

    def summary(self):
        slist = []
        for branch in self.branches:
            slist.append(branch.__str__())
        return string.join(slist,"\n")+"\n"

    def read(self,inputfile,screen_lines=0):
        # We now go through the file and read the branches.
        prevline = None
        if not hasattr(inputfile,"next"):
            inputfile = inputfile.readlines()
        while 1:
            branch = AUTOBranch(inputfile,screen_lines,prevline)
            prevline = branch._lastline
            self.branches.append(branch)
            if prevline is None:
                break

def AUTOatof(input_string):
    #Sometimes AUTO messes up the output.  I.e. it gives an
    #invalid floating point number of the form x.xxxxxxxE
    #instead of x.xxxxxxxE+xx.  Here we assume the exponent
    #is 0 and make it into a real real number :-)
    try:
        value=float(input_string)
    except (ValueError):
        try:
            if input_string[-1] == "E":
                #  This is the case where you have 0.0000000E
                value=float(string.strip(input_string)[0:-1])
            elif input_string[-4] in ["-","+"]:
                #  This is the case where you have x.xxxxxxxxx-yyy
                #  or x.xxxxxxxxx+yyy (standard Fortran but not C)
                value=float(input_string[:-4]+'E'+input_string[-4:])
            elif input_string[-4] == "D":
                #  This is the case where you have x.xxxxxxxxxD+yy
                #  or x.xxxxxxxxxD-yy (standard Fortran but not C)
                value=float(input_string[:-4]+'E'+input_string[-3:])
            else:
                print "Encountered value I don't understand"
                print input_string
                print "Setting to 0"
                value=0.0
        except:
            print "Encountered value which raises an exception while processing!!!"
            print input_string
            print "Setting to 0"
            value=0.0
            
            
    return value

def pointtest(a,b):
    if not(a.has_key("TY name")):
        raise AUTOExceptions.AUTORegressionError("No TY label")
    if not(a.has_key("TY number")):
        raise AUTOExceptions.AUTORegressionError("No TY label")
    if not(a.has_key("BR")):
        raise AUTOExceptions.AUTORegressionError("No BR label")
    if not(a.has_key("data")):
        raise AUTOExceptions.AUTORegressionError("No data label")
    if not(a.has_key("PT")):
        raise AUTOExceptions.AUTORegressionError("No PT label")
    if not(a.has_key("LAB")):
        raise AUTOExceptions.AUTORegressionError("No LAB label")
    if not(len(a["data"]) == len(b["data"])):
        raise AUTOExceptions.AUTORegressionError("Data sections have different lengths")

def test():
    print "Testing reading from a filename"
    foo = parseB()
    foo.readFilename("test_data/fort.7")    
    if len(foo) != 150:
        raise AUTOExceptions.AUTORegressionError("File length incorrect")
    pointtest(foo.getIndex(0),foo.getIndex(57))

    print "Testing reading from a stream"
    foo = parseB()
    fp = open("test_data/fort.7","r")
    foo.read(fp)
    if len(foo) != 150:
        raise AUTOExceptions.AUTORegressionError("File length incorrect")
    pointtest(foo.getIndex(0),foo.getIndex(57))


    print "Testing label manipulation"
    labels = foo.getLabels()
    foo.relabel(labels[0],57)
    labels = foo.getLabels()
    if labels[0] != 57:
        raise AUTOExceptions.AUTORegressionError("Error in either relabel or getLabel")
    foo.deleteLabel(labels[1])
    new_labels = foo.getLabels()
    if len(labels) != len(new_labels) + 1:
        raise AUTOExceptions.AUTORegressionError("Error in label deletion")
        
    
    print "parseB passed all tests"

if __name__ == '__main__' :
    test()








