#!/usr/bin/env python
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

from string import *
import os
import sys
from UserDict import *
import cStringIO

line1_comment="NUNSTAB,NSTAB,IEQUIB,ITWIST,ISTART"
line2_comment="NREV,/,IREV(I),I=1,NDIM)"
line3_comment="NFIXED,(/,I,IFIXED(I)),I=1,NFIXED)"
line4_comment="NPSI,(/,I,IPSI(I)),I=1,NPSI)"

# The parseH class parses an AUTO parameter file
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
# Once the data is read in the class provides a dictionary
# interface for manipulating the file.



class parseH(UserDict):

    def __init__(self,filename=None):
	UserDict.__init__(self)
	if filename:
	    self.readFilename(filename)
#        self.dataString=""
        
    def __str__(self):
        string = cStringIO.StringIO()
        self.write(string)
        return string.getvalue()
        
    def readFilename(self,filename):
	input = open(filename,"r")
	self.read(input)
	input.close()

    def writeFilename(self,filename):
	output = open(filename,"w")
	self.write(output)
	output.close()

    def read(self,input):
#        self.dataString = input.read()
	line = input.readline()
	data = split(line)
	self["NUNSTAB"] = atoi(data[0])
	self["NSTAB"] = atoi(data[1])
	self["IEQUIB"] = atoi(data[2])
	self["ITWIST"] = atoi(data[3])
	self["ISTART"] = atoi(data[4])

	line = input.readline()
	data = split(line)
	self["NREV"] = atoi(data[0])
	self["IREV"] = []
        data = []
	if self["NREV"] > 0:
	    line = input.readline()
	    data = split(line)
	for i in data:
	    self["IREV"].append(atoi(i))

	line = input.readline()
	data = split(line)
	self["NFIXED"] = atoi(data[0])
	self["IFIXED"] = []
	if self["NFIXED"] > 0:
	    line = input.readline()
	    data = split(line)
	for i in range(self["NFIXED"]):
	    self["IFIXED"].append(atoi(data[i]))

	line = input.readline()
	data = split(line)
	self["NPSI"] = atoi(data[0])
	self["IPSI"] = []
	if self["NPSI"] > 0:
	    line = input.readline()
	    data = split(line)
	for i in range(self["NPSI"]):
	    self["IPSI"].append(atoi(data[i]))


    def write(self,output):
#        output.write(self.dataString)
	output.write(str(self["NUNSTAB"])+" "+str(self["NSTAB"])+" ")
	output.write(str(self["IEQUIB"]) +" "+str(self["ITWIST"])+" ")
	output.write(str(self["ISTART"]) +" ")
	output.write("          "+line1_comment+"\n")

	output.write(str(self["NREV"])+" ")
	output.write("          "+line2_comment+"\n")
	for i in self["IREV"]:
	    output.write(str(i)+" ")
	if self["NREV"] > 0:
	    output.write("\n")

	output.write(str(self["NFIXED"])+" ")
	output.write("          "+line3_comment+"\n")
	for i in range(self["NFIXED"]):
	    output.write(str(self["IFIXED"][i])+" ")
	if self["NFIXED"] > 0:
	    output.write("\n")

	output.write(str(self["NPSI"])+" ")
	output.write("          "+line4_comment+"\n")
	for i in range(self["NPSI"]):
	    output.write(str(self["IPSI"][i])+" ")
	if self["NPSI"] > 0:
	    output.write("\n")
        
        output.flush()

def pointtest(a):
    keys = ['NUNSTAB', 'NSTAB', 'IEQUIB', 'ITWIST', 'ISTART',
            'NREV', 'IREV', 'NFIXED', 'IFIXED', 'NPSI', 'IPSI']		
    for key in keys:
        if not(a.has_key(key)):
            raise AUTORegressionError("No %s label"%(key,))

def test():
    print "Testing reading from a filename"
    foo = parseH()
    foo.readFilename("test_data/h.cir")    
    pointtest(foo)

    print "Testing reading from a stream"
    foo = parseH()
    fp = open("test_data/h.cir","r")
    foo.read(fp)    
    pointtest(foo)

    print "parseH passed all tests"

if __name__ == '__main__' :
    test()








