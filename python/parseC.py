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

import string
import os
import sys
import UserDict
import cStringIO
import AUTOExceptions

line1_comment="NDIM,IPS,IRS,ILP"
line2_comment="NICP,(ICP(I),I=1 NICP)"
line3_comment="NTST,NCOL,IAD,ISP,ISW,IPLT,NBC,NINT"
line4_comment="NMX,RL0,RL1,A0,A1"
line5_comment="NPR,MXBF,IID,ITMX,ITNW,NWTN,JAC"
line6_comment="EPSL,EPSU,EPSS"
line7_comment="DS,DSMIN,DSMAX,IADS"
line8_comment="NTHL,(/,I,THL(I)),I=1,NTHL)"
line9_comment="NTHU,(/,I,THU(I)),I=1,NTHU)"
line10_comment="NUZR,(/,I,PAR(I)),I=1,NUZR)"

# The parseC class parses an AUTO parameter file
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



class parseC(UserDict.UserDict):
    def __init__(self,filename=None):
	UserDict.UserDict.__init__(self)
        self.__new = 1
        for key in ['NPR', 'EPSS', 'ITMX', 'EPSU', 'ITNW', 'NBC',
            'IADS', 'IPS', 'IID', 'A1', 'DS', 'NMX', 'NTST',
            'NINT', 'NWTN', 'A0', 'EPSL', 'ISP', 'DSMIN', 'MXBF',
            'RL0', 'RL1', 'IPLT', 'ILP', 'NCOL',
            'DSMAX', 'ISW', 'IRS', 'IAD', 'JAC', 'NDIM', 'NPAR',
            'sv', 's']:
            self[key] = None
        for key in ["THL","THU","UZR","ICP"]:
            self[key] = []
	if filename:
	    self.readFilename(filename)

    def __str__(self):
        string = cStringIO.StringIO()
        self.write(string)
        return string.getvalue()

    def __setitem__(self,key,item):
        if key == "ICP":
            self.data["__NICP"] = len(item)
            self.data["ICP"] = item
        elif key in ["THL","THU","UZR"]:
            self.data["__N"+key] = len(item)
            self.data[key] = []
            for x in item:
                self.data[key].append({})
                self.data[key][-1]["PAR index"] = x[0]
                self.data[key][-1]["PAR value"] = x[1]
        else:
            self.data[key] = item
            
    def readFilename(self,filename):
	inputfile = open(filename,"r")
	self.read(inputfile)
	inputfile.close()

    def writeFilename(self,filename):
	output = open(filename,"w")
	self.write(output)
	output.close()

    def read(self,inputfile):
	line = inputfile.readline()
	data = string.split(line)
        while not data[0][0] in string.digits:
            line = string.strip(line)
            if line[0] in ['#','!','_']:
                line = inputfile.readline()
                continue
            pos = string.find(line, '=')
            key = string.strip(line[:pos])
            pos2 = len(line)
            for i in range(pos+1,pos2):
                if line[i] in ['#!_'] or line[i] in string.ascii_uppercase:
                    pos2 = i
                    break
            value = string.strip(line[pos+1:pos2])
            line = string.strip(line[pos2:])
            if key == 'ICP':
                self[key]=map(int,string.split(value[1:-1],','))
            elif key in ['THU','THL','UZR']:
                value = string.strip(string.replace(value[1:-1],']',' '))
                value = string.replace(value,',',' ')
                value = string.split(value[1:],'[')
                list = []
                for valuei in value:
                    valuei = string.split(valuei)
                    list.append([int(valuei[0]),float(valuei[1])])
                self[key]=list
            elif key in self.keys():
                if key[0] in ['I','J','K','L','M','N']:
                    self[key]=int(value)
                else:
                    self[key]=float(value)
            if line == '':
                line = inputfile.readline()
            while line != '' and line[0] == '\n':
                line = inputfile.readline()
            if line == '':
                break
            data = string.split(line)
                
        if line == '':
            return
        
        self.__new = 0
	self["NDIM"] = int(data[0])
	self["IPS"] = int(data[1])
	self["IRS"] = int(data[2])
	self["ILP"] = int(data[3])

	line = inputfile.readline()
	data = string.split(line)
	self.data["__NICP"] = int(data[0])
	self.data["ICP"] = []
	for i in range(self["__NICP"]):
	    self.data["ICP"].append(int(data[i+1]))

	line = inputfile.readline()
	data = string.split(line)
	self["NTST"] = int(data[0])
	self["NCOL"] = int(data[1])
	self["IAD"] = int(data[2])
	self["ISP"] = int(data[3])
	self["ISW"] = int(data[4])
	self["IPLT"] = int(data[5])
	self["NBC"] = int(data[6])
	self["NINT"] = int(data[7])

	line = inputfile.readline()
	data = string.split(line)
	self["NMX"] = int(data[0])
	self["RL0"] = float(data[1])
	self["RL1"] = float(data[2])
	self["A0"] = float(data[3])
	self["A1"] = float(data[4])
	
	line = inputfile.readline()
	data = string.split(line)
	self["NPR"] = int(data[0])
	self["MXBF"] = int(data[1])
	self["IID"] = int(data[2])
	self["ITMX"] = int(data[3])
	self["ITNW"] = int(data[4])
	self["NWTN"] = int(data[5])
	self["JAC"] = int(data[6])
	
	line = inputfile.readline()
	data = string.split(line)
	self["EPSL"] = float(data[0])
	self["EPSU"] = float(data[1])
	self["EPSS"] = float(data[2])

	line = inputfile.readline()
	data = string.split(line)
	self["DS"] = float(data[0])
	self["DSMIN"] = float(data[1])
	self["DSMAX"] = float(data[2])
	self["IADS"] = int(data[3])
	
	line = inputfile.readline()
	data = string.split(line)
	self.data["__NTHL"] = int(data[0])
	self.data["THL"] = []
	for i in range(self["__NTHL"]):
	    self.data["THL"].append({})
	    line = inputfile.readline()
	    data = string.split(line)
	    self.data["THL"][i]["PAR index"] = int(data[0])
	    self.data["THL"][i]["PAR value"] = float(data[1])

	line = inputfile.readline()
	data = string.split(line)
	self.data["__NTHU"] = int(data[0])
	self.data["THU"] = []
	for i in range(self["__NTHU"]):
	    self.data["THU"].append({})
	    line = inputfile.readline()
	    data = string.split(line)
	    self.data["THU"][i]["PAR index"] = int(data[0])
	    self.data["THU"][i]["PAR value"] = float(data[1])

	line = inputfile.readline()
	data = string.split(line)
	self.data["__NUZR"] = int(data[0])
	self.data["UZR"] = []
	for i in range(self["__NUZR"]):
	    self.data["UZR"].append({})
	    line = inputfile.readline()
	    data = string.split(line)
	    self.data["UZR"][i]["PAR index"] = int(data[0])
	    self.data["UZR"][i]["PAR value"] = float(data[1])

    def write(self,output):
        if self.__new:
            for key,value in self.items():
                if key == "ICP":
                    if value != []:
                        output.write(key+"="+str(value)+"\n")
                elif key in ["THL","THU","UZR"]:
                    if value != []:
                        s=key+"=["
                        for item in value:
                            s=s+str([item["PAR index"],item["PAR value"]])+','
                        output.write(s[:-1]+"]\n")
                elif key[0] != '_' and value != None:
                    output.write(key+"="+str(value)+"\n")
            return
            
        for key,value in self.items():
            if value != None and key in ["sv","s","NPAR"]:
                output.write(key+"="+str(value)+"\n")
	output.write(str(self["NDIM"])+" "+str(self["IPS"])+" ")
	output.write(str(self["IRS"]) +" "+str(self["ILP"])+" ")
	output.write("          "+line1_comment+"\n")
	
	output.write(str(self["__NICP"])+" ")
	for i in range(self["__NICP"]):
	    output.write(str(self["ICP"][i])+" ")
	output.write("          "+line2_comment+"\n")
	
	output.write(str(self["NTST"])+" "+str(self["NCOL"])+" ")
	output.write(str(self["IAD"]) +" "+str(self["ISP"])+" ")
	output.write(str(self["ISW"]) +" "+str(self["IPLT"])+" ")
	output.write(str(self["NBC"]) +" "+str(self["NINT"])+" ")
	output.write("          "+line3_comment+"\n")
	
	output.write(str(self["NMX"])+" "+str(self["RL0"])+" ")
	output.write(str(self["RL1"]) +" "+str(self["A0"])+" ")
	output.write(str(self["A1"]) +" ")
	output.write("          "+line4_comment+"\n")

	output.write(str(self["NPR"])+" "+str(self["MXBF"])+" ")
	output.write(str(self["IID"]) +" "+str(self["ITMX"])+" ")
	output.write(str(self["ITNW"]) +" "+str(self["NWTN"])+" ")
	output.write(str(self["JAC"]) +" ")
	output.write("          "+line5_comment+"\n")

	output.write(str(self["EPSL"])+" "+str(self["EPSU"])+" ")
	output.write(str(self["EPSS"]) +" ")
	output.write("          "+line6_comment+"\n")

	output.write(str(self["DS"]) +" "+str(self["DSMIN"])+" ")
	output.write(str(self["DSMAX"]) +" "+str(self["IADS"])+" ")
	output.write("          "+line7_comment+"\n")
	
	output.write(str(self["__NTHL"]))
	output.write("          "+line8_comment+"\n")
	for i in range(self["__NTHL"]):
	    output.write(str(self["THL"][i]["PAR index"])+" ")
	    output.write(str(self["THL"][i]["PAR value"])+"\n")

	output.write(str(self["__NTHU"]))
	output.write("          "+line9_comment+"\n")
	for i in range(self["__NTHU"]):
	    output.write(str(self["THU"][i]["PAR index"])+" ")
	    output.write(str(self["THU"][i]["PAR value"])+"\n")

	output.write(str(self["__NUZR"]))
	output.write("          "+line10_comment+"\n")
	for i in range(self["__NUZR"]):
	    output.write(str(self["UZR"][i]["PAR index"])+" ")
	    output.write(str(self["UZR"][i]["PAR value"])+"\n")
        output.flush()

def pointtest(a):
    keys = ['NPR', 'UZR', 'EPSS', 'ITMX', 'EPSU', 'ITNW', 'NBC',
            'IADS', 'IPS', 'IID', 'A1', 'DS', 'NMX', 'NTST', '__NICP',
            'NINT', 'NWTN', 'A0', 'EPSL', 'ISP', 'DSMIN', 'MXBF', '__NTHL',
            'RL0', 'RL1', 'ICP', '__NTHU', 'IPLT', 'ILP', 'NCOL', 'THL',
            'DSMAX', 'ISW', 'IRS', 'THU', 'IAD', 'JAC', '__NUZR', 'NDIM']
    for key in keys:
        if not(a.has_key(key)):
            raise AUTOExceptions.AUTORegressionError("No %s label"%(key,))

def test():
    print "Testing reading from a filename"
    foo = parseC()
    foo.readFilename("test_data/c.ab")    
    pointtest(foo)

    print "Testing reading from a stream"
    foo = parseC()
    fp = open("test_data/c.ab","r")
    foo.read(fp)    
    pointtest(foo)

    print "parseC passed all tests"

if __name__ == '__main__' :
    test()


