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
from AUTOExceptions import *
import types

# A little dictionary to transform types to human readable strings
def type_translation(type):
    type_translation_dict = {}

    type_translation_dict[0]  = {"long name" : "No Label","short name" : "No Label"}
    type_translation_dict[1]  = {"long name" : "Branch point (algebraic problem)","short name" : "BP"}
    type_translation_dict[2]  = {"long name" : "Fold (algebraic problem)","short name" : "LP"}
    type_translation_dict[3]  = {"long name" : "Hopf bifurcation (algebraic problem)","short name" : "HB"}
    type_translation_dict[4]  = {"long name" : "Regular point (every NPR steps)","short name" : "RG"}
    type_translation_dict[-4] = {"long name" : "User requested point","short name" : "UZ"}
    type_translation_dict[5]  = {"long name" : "Fold (ODE)","short name" : "LP"}
    type_translation_dict[6]  = {"long name" : "Bifurcation point (ODE)","short name" : "BP"}
    type_translation_dict[7]  = {"long name" : "Period doubling bifurcation (ODE)","short name" : "PD"}
    type_translation_dict[8]  = {"long name" : "Bifurcation to invarient torus (ODE)","short name" : "TR"}
    type_translation_dict[9]  = {"long name" : "Normal begin or end","short name" : "EP"}
    type_translation_dict[-9] = {"long name" : "Abnormal termination","short name" : "MX"}

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

class parseB:
    def __init__(self,filename=None):
        self.data=[]
	if filename:
	    self.readFilename(filename)

    def __str__(self):
        rep=""
        for x in self.data:
            if x["TY number"] != 0:
                rep=rep+"TY:  "+type_translation(x["TY number"])["long name"]+" "
                rep=rep+"LAB: "+str(x["LAB"])+"\n"
                rep=rep+str(x["data"])+"\n"
                
        return rep

    def __getitem__(self,*index):
        return apply(self.getIndex,index)

    def __call__(self,*label):
        return apply(self.getLabel,label)

    def __len__(self):
        return len(self.data)

    # Removes the first solution with the given label
    def deleteLabel(self,label):
        if type(label)  == types.IntType:
            for i in range(len(self.data)):
                if self.data[i]["LAB"] == label:
                    self.data[i]["LAB"] = 0
                    self.data[i]["TY number"]  = 0
                    self.data[i]["TY name"]    = type_translation(0)["short name"]
        else:
            for j in range(len(label)):
                for i in range(len(self.data)):
                    if self.data[i]["LAB"] == label[j]:
                        self.data[i]["LAB"] = 0
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
        for i in range(len(self)):
            if self.data[i]["LAB"] == label:
                return self.data[i]

    # Given an index, return the correct solution
    def getIndex(self,index):
        return self.data[index]

    # Get all the labels from the solution
    def getLabels(self):
        labels = []
        for i in range(len(self.data)):
            if self.data[i]["LAB"] != 0:
                labels.append(self.data[i]["LAB"])
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
        for line in self.with_header:
            if line.has_key("header"):
                output.write(line["header"])
            else:
                output_line = "%4d%6d%4d%4d"%(line["BR"],line["PT"],
                                              line["TY number"],line["LAB"])
                for data in line["data"]:
                    output_line = output_line + "%14.6E"%data
                output.write(output_line+"\n")

    def writeFilename(self,filename):
	output = open(filename,"w")
	self.write(output)
	output.close()

    def read(self,input):
        data=input.readlines()
        section = 0
        self.with_header=[]
        self.data=[]
        for input_line in data:
            line = string.split(input_line)
            if len(line) > 0 and string.atoi(line[0]) != 0:
                item = {}
                item["BR"] = string.atoi(line[0])
                item["PT"] = string.atoi(line[1])
                item["TY number"] = string.atoi(line[2])
                item["TY name"] = type_translation(item["TY number"])["short name"]
                item["LAB"] = string.atoi(line[3])
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
                if item["PT"] == 1:
                    section = section + 1
                item["section"] = section
                item["data"]=[]
                item["index"] = len(self.data)
                item["data"].extend(map(AUTOatof, line[4:]))
                # We keep two lists of references to the
                # data, one is just the data in the file
                # and the other includes the header.
                self.data.append(item)
                self.with_header.append(item)
            else:
                item = {}
                item["header"] = input_line
                self.with_header.append(item)
    def readFilename(self,filename):
	input = open(filename,"r")
	self.read(input)
	input.close()

def AUTOatof(input_string):
    #Sometimes AUTO messes up the output.  I.e. it gives an
    #invalid floating point number of the form x.xxxxxxxE
    #instead of x.xxxxxxxE+xx.  Here we assume the exponent
    #is 0 and make it into a real real number :-)
    try:
        value=string.atof(input_string)
    except (ValueError):
        try:
            if input_string[-1] == "E":
                #  This is the case where you have 0.0000000E
                value=string.atof(strip(input_string)[0:-1])
            elif input_string[-4] == "-":
                #  This is the case where you have x.xxxxxxxxx-yyy
                value=0.0
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
        raise AUTORegressionError("No TY label")
    if not(a.has_key("TY number")):
        raise AUTORegressionError("No TY label")
    if not(a.has_key("BR")):
        raise AUTORegressionError("No BR label")
    if not(a.has_key("data")):
        raise AUTORegressionError("No data label")
    if not(a.has_key("PT")):
        raise AUTORegressionError("No PT label")
    if not(a.has_key("LAB")):
        raise AUTORegressionError("No LAB label")
    if not(len(a["data"]) == len(b["data"])):
        raise AUTORegressionError("Data sections have different lengths")

def test():
    print "Testing reading from a filename"
    foo = parseB()
    foo.readFilename("test_data/fort.7")    
    if len(foo) != 150:
        raise AUTORegressionError("File length incorrect")
    pointtest(foo.getIndex(0),foo.getIndex(57))

    print "Testing reading from a stream"
    foo = parseB()
    fp = open("test_data/fort.7","r")
    foo.read(fp)
    if len(foo) != 150:
        raise AUTORegressionError("File length incorrect")
    pointtest(foo.getIndex(0),foo.getIndex(57))


    print "Testing label maninpulation"
    labels = foo.getLabels()
    foo.relabel(labels[0],57)
    labels = foo.getLabels()
    if labels[0] != 57:
        raise AUTORegressionError("Error in either relabel or getLabel")
    foo.deleteLabel(labels[1])
    new_labels = foo.getLabels()
    if len(labels) != len(new_labels) + 1:
        raise AUTORegressionError("Error in label deletion")
        
    
    print "parseB passed all tests"

if __name__ == '__main__' :
    test()








