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
import UserList

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
    
# The parseB class parses an AUTO fort.7 file
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

class parseB(UserList.UserList):
    def __init__(self,filename=None,screen_lines=0):
        if type(filename) == types.StringType:
            UserList.UserList.__init__(self)
            self.readFilename(filename,screen_lines)
        else:
            UserList.UserList.__init__(self,filename)

    def __str__(self):
        rep=""
        for x in self.data:
            if x["TY number"] != 0:
                rep=rep+"TY:  "+type_translation(x["TY number"])["long name"]+" "
                rep=rep+"LAB: "+str(x["LAB"])+"\n"
                rep=rep+str(x["data"])+"\n"
                
        return rep

    def __call__(self,label):
        return self.getLabel(label)

    # Removes solutions with the given labels or type names
    def deleteLabel(self,label=None,keepTY=0,keep=0):
        if label == None:
            label=['BP','LP','HB','PD','TR','EP','MX']
        if type(label) != types.ListType:
            label = [label]
        for i in range(len(self.data)):
            d = self.data[i]
            if ((not keep and (d["LAB"] in label or d["TY name"] in label)) or
               (keep and not d["LAB"] in label and not d["TY name"] in label)):
                self.data[i]["LAB"] = 0
                if not keepTY:
                    self.data[i]["TY number"]  = 0
                    self.data[i]["TY name"]    = type_translation(0)["short name"]
            
    # Relabels the first solution with the given label
    def relabel(self,old_label,new_label):
        if type(old_label)  == types.IntType:
            for i in range(len(self.data)):
                if self.data[i]["LAB"] == old_label:
                    self.data[i]["LAB"] = new_label
        else:
            for j in range(len(old_label)):
                for i in range(len(self.data)):
                    if self.data[i]["LAB"] == old_label[j]:
                        self.data[i]["LAB"] = new_label[j]

    # Make all labels in the file unique and sequential
    def uniquelyLabel(self):
        label = 1
        for i in range(len(self)):
            if self.data[i]["TY number"] != 0:
                self.data[i]["LAB"] = label
                label = label + 1
            
    # Given a label, return the correct solution
    def getLabel(self,label):
        if type(label) == types.IntType:
            for i in range(len(self)):
                if self.data[i]["LAB"] == label:
                    return self.data[i]
            return
        items = parseB()
        items.data = []
        if type(label) != types.ListType:
            label = [label]
        for i in range(len(self.data)):
            d = self.data[i]                
            if (d["LAB"] != 0 and not d["LAB"] in label and
                not d["TY name"] in label):
                d = d.copy()
                d["LAB"] = 0
                d["TY number"] = 0
                d["TY name"]   = type_translation(0)["short name"]
            items.data.append(d)
        return items

    # Given an index, return the correct solution
    def getIndex(self,index):
        return self.data[index]

    # Get all the labels from the solution
    def getLabels(self):
        labels = []
        for x in self.data:
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
        for vector in self:
            array.append([])
            for point in vector["data"]:
                array[-1].append(point)
        return array

    def writeRaw(self,output):
        first = 1
        for vector in self:
            if vector["PT"] == 1 and first == 0:
                output.write("\n")
            first = 0
            for point in vector["data"]:
                output.write(str(point)+" ")
            output.write("\n")
                
    def write(self,output):
        for line in self.data:
            if line.has_key("header"):
                output.write(line["header"])
            output_line = "%4d%6d%4d%5d"%(line["BR"],line["PT"],
                                          line["TY number"],line["LAB"])
            for data in line["data"]:
                output_line = output_line + "%19.10E"%data
            output.write(output_line+"\n")

    def writeShort(self):
        for line in self.data:
            if line.has_key("header"):
                l = string.split(line["header"])
                if len(l) > 1 and l[1]=='PT':
                    output_line = "   0    PT  TY  LAB "
                    l=line["header"]
                    n=20
                    while n+14 <= len(l):
                        output_line = output_line + "%14s"%l[n:n+14]
                        n=n+19
                    sys.stdout.write(output_line+"\n")
                else:
                    sys.stdout.write(line["header"])
            output_line = "%4d%6d%4d%5d"%(line["BR"],line["PT"],
                                          line["TY number"],line["LAB"])
            for data in line["data"]:
                output_line = output_line + "%14.5E"%data
            sys.stdout.write(output_line+"\n")

    def summary(self):
        s = ""
        for line in self.data:
            if line.has_key("header"):
                l = line["header"]
                n = string.find(l," PT ")
                if n > -1:
                    output_line = "\n  BR    PT  TY  LAB "
                    n = n + 13
                    while n+14 <= len(l):
                        output_line = output_line + "%14s"%l[n:n+14]
                        n = n+19
                    s = s + output_line + "\n"
            if line["TY name"]!="No Label":
                ty_name = line["TY name"]
                if ty_name=='RG':
                    ty_name = '  '
                output_line = "%4d%6d%4s%5d"%(abs(line["BR"]),abs(line["PT"]),
                                              ty_name,line["LAB"])
                for data in line["data"]:
                    output_line = output_line + "%14.5E"%data
                s = s + output_line+"\n"
        return s

    def writeScreen(self):
        sys.stdout.write(self.summary())

    def writeFilename(self,filename):
	output = open(filename,"w")
	self.write(output)
	output.close()

    def read(self,inputfile,screen_lines=0):
        data=inputfile.readlines()
        header = ""
        section = 0
        self.data=[]
        for input_line in data:
            line = string.split(input_line)
            if len(line) > 0 and int(line[0]) != 0:
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
                tynumber = int(line[2])
                if tynumber == 0 and screen_lines:
                    continue
                pt = int(line[1])
                if abs(pt) == 1:
                    section = section + 1
                try:
                    fdata = map(float, line[4:])
                except:
                    fdata = map(AUTOatof, line[4:])
                item = {"BR": int(line[0]),
                        "PT": pt,
                        "TY number": tynumber,
                        "TY name": type_translation(tynumber)["short name"],
                        "LAB": int(line[3]),
                        "data": fdata,
                        "section": section,
                        "index": len(self.data) }
                if header != "":
                    item["header"] = header
                    header = ""
                self.data.append(item)
            else:
                header = header + input_line
    def readFilename(self,filename,screen_lines=0):
	inputfile = open(filename,"r")
	self.read(inputfile,screen_lines)
	inputfile.close()

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


    print "Testing label maninpulation"
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








