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
import parseB

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
        self.__new = 1
        if filename is not None and type(filename) != type(""):
            if isinstance(filename,UserDict.UserDict):
                self.__new = filename.__new
            UserDict.UserDict.__init__(self,filename)
            return
        UserDict.UserDict.__init__(self)
        for key in ['NPR', 'EPSS', 'ITMX', 'EPSU', 'ITNW', 'NBC',
            'IADS', 'IPS', 'IID', 'A1', 'DS', 'NMX', 'NTST',
            'NINT', 'NWTN', 'A0', 'EPSL', 'ISP', 'DSMIN', 'MXBF',
            'RL0', 'RL1', 'IPLT', 'ILP', 'NCOL',
            'DSMAX', 'ISW', 'IRS', 'IAD', 'JAC', 'NDIM', 'NPAR',
            'NUNSTAB', 'NSTAB', 'IEQUIB', 'ITWIST', 'ISTART',
            'sv', 's', 'dat', 'e', 'unames', 'parnames',
            "THL","THU","UZR","ICP","IREV","IFIXED","IPSI","U","PAR","SP"]:
            self[key] = None
	if filename:
	    self.readFilename(filename)

    def __str__(self):
        string = cStringIO.StringIO()
        self.write(string)
        return string.getvalue()

    def __setitem__(self,key,item):
        if key in ["ICP","IREV","IFIXED","IPSI","SP"]:
            l = 0
            if item is not None:
                l = len(item)
            self.data["__N"+key] = l
            self.data[key] = item
        elif key in ["THL","THU","UZR","U","PAR","unames","parnames"]:
            if item is None:
                self.data["__N"+key] = 0
                self.data[key] = None
                return
            if type(item) == type({}):
                item = item.items()
            self.data["__N"+key] = len(item)
            self.data[key] = []
            for x in item:
                if type(x) == type({}):
                    self.data[key].append([x["PAR index"],x["PAR value"]])
                else:
                    self.data[key].append([x[0],x[1]])
        elif key == "DS" and item == '-':
            if self.data[key] is None:
                self.data[key] = 0.01
            self.data[key] = -self.data[key]
        else:
            self.data[key] = item

    def update(self, d):
        for k, v in d.items():
            self[k] = v
            
    def readFilename(self,filename):
	inputfile = open(filename,"r")
	self.read(inputfile)
	inputfile.close()

    def writeFilename(self,filename,new=False):
	output = open(filename,"w")
	self.write(output,new)
	output.close()

    def scanvalue(self,line,inputfile=None):
        # Scans line for a value
        # returns the value, followed by the rest of the line
        # The value is returned as a flat list of strings if the line 
        # starts with [ and otherwise as a single string
        level = 0
        quote = ' '
        quoteesc = False
        prev = ' '
        npos = 0
        value = []
        start = 0
        isdict = False
        v = ''
        line = string.strip(line)
        i = 0
        while True:
            if i == len(line):
                if level == 0 or inputfile is None:
                    break
                line = string.strip(inputfile.readline())
                i = 0
            npos = i
            c = line[i]
            if quote == ' ':
                if c in [',',' ']:
                    if level == 0:
                        break
                elif c == ':':
                    pass
                elif c in [']','}']:
                    if c == '}':
                        isdict = False
                    if level == 1 and prev in ['[','{']:
                        value = []
                    level = level - 1
                    if v != '':
                        value.append(v)
                    v = ''
                    if c == ']' and isdict:
                        value.append(']')
                else:
                    if prev in [',',' ',':'] and level > 0 and v != '':
                        value.append(v)
                        v = ''
                    if c in ['[','{']:
                        level = level + 1
                        if c == '{':
                            isdict = True
                        elif isdict:
                            value.append('[')
                    elif c in ['"',"'"]:
                        quote = c
                    else:
                        v = v + c
            elif c == quote:
                # ignore "" and ''
                if (i+1 < len(line) and line[i+1] == c) or quoteesc:
                    quoteesc = not quoteesc
                else:
                    quote = ' '
            else:
                v = v + c
            prev = c
            i = i + 1
        if v != '':
            value = v
        while npos < len(line) and line[npos] in [" ",","]:
            npos = npos + 1
        if npos >= len(line) - 1:
            line = ''
        else:
            line = string.strip(line[npos:])
        return value, line

    def parseline(self,line,userspec=False,inputfile=None):
        # parses a new-style constant file line and puts the keys
        # in the dictionary c; also used for the header of a b. file
        while line != "":
            line = string.strip(line)
            if line[0] in ['#','!','_']:
                return
            pos = string.find(line, '=')
            if pos == -1:
                return
            key = string.strip(line[:pos])
            value, line = self.scanvalue(line[pos+1:],inputfile)
            if key in ['ICP','IREV','IFIXED','IPSI']:
                d = []
                for v in value:
                    try:
                        v = int(v)
                    except ValueError:
                        pass
                    d.append(v)
                value = d
            elif key in ['THU','THL','UZR','U','PAR']:
                d = []
                v0s = []
                i = 0
                while i < len(value):
                    try:
                        v0 = int(value[i])
                    except ValueError:
                        v0 = value[i]
                    if value[i+1] == '[':
                        i = i + 1
                        v1 = []
                        while i+1 < len(value) and value[i+1] != ']':
                            v1.append(parseB.AUTOatof(value[i+1]))
                            i = i + 1
                    else:
                        v1 = parseB.AUTOatof(value[i+1])
                        if v0 in v0s:
                            v1 = [v1]
                    if v0 in v0s:
                        # add to list when parameter was already encountered
                        try:
                            d[v0s.index(v0)][1].extend(v1)
                        except AttributeError:
                            d[v0s.index(v0)][1] = [d[v0s.index(v0)][1]]
                            d[v0s.index(v0)][1].extend(v1)
                    else:
                        v0s.append(v0)
                        d.append([v0,v1])
                    i = i + 2
                value = d
            elif key in ['unames','parnames']:
                d = []
                for i in range(0,len(value),2):
                    d.append([int(value[i]),value[i+1]])
                value = d                
            elif key in ['s','dat','sv','e','SP']:
                pass
            elif key in self.keys():
                if key[0] in ['I','J','K','L','M','N']:
                    value=int(value)
                else:
                    value=parseB.AUTOatof(value)
            else:
                value = None
            if value is not None:
                if userspec:
                    self["Active "+key] = self[key]
                self[key] = value

    def read(self,inputfile):
	line = inputfile.readline()
	data = string.split(line)
        while not data[0][0] in string.digits:
            self.parseline(line,inputfile=inputfile)
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
            d = data[i+1]
            try:
                d = int(d)
            except ValueError:
                pass
	    self.data["ICP"].append(d)

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
	self["RL0"] = parseB.AUTOatof(data[1])
	self["RL1"] = parseB.AUTOatof(data[2])
	self["A0"] = parseB.AUTOatof(data[3])
	self["A1"] = parseB.AUTOatof(data[4])
	
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
	self["EPSL"] = parseB.AUTOatof(data[0])
	self["EPSU"] = parseB.AUTOatof(data[1])
	self["EPSS"] = parseB.AUTOatof(data[2])

	line = inputfile.readline()
	data = string.split(line)
	self["DS"] = parseB.AUTOatof(data[0])
	self["DSMIN"] = parseB.AUTOatof(data[1])
	self["DSMAX"] = parseB.AUTOatof(data[2])
	self["IADS"] = int(data[3])
	
	line = inputfile.readline()
	data = string.split(line)
	self.data["__NTHL"] = int(data[0])
	self.data["THL"] = []
	for i in range(self["__NTHL"]):
	    self.data["THL"].append({})
	    line = inputfile.readline()
	    data = string.split(line)
	    self.data["THL"][i][0] = int(data[0])
	    self.data["THL"][i][1] = parseB.AUTOatof(data[1])

	line = inputfile.readline()
	data = string.split(line)
	self.data["__NTHU"] = int(data[0])
	self.data["THU"] = []
	for i in range(self["__NTHU"]):
	    self.data["THU"].append({})
	    line = inputfile.readline()
	    data = string.split(line)
	    self.data["THU"][i][0] = int(data[0])
	    self.data["THU"][i][1] = parseB.AUTOatof(data[1])

	line = inputfile.readline()
	data = string.split(line)
	self.data["__NUZR"] = int(data[0])
	self.data["UZR"] = []
	for i in range(self["__NUZR"]):
	    self.data["UZR"].append({})
	    line = inputfile.readline()
	    data = string.split(line)
            d = data[0]
            try:
                d = int(d)
            except ValueError:
                pass
	    self.data["UZR"][i][0] = d
	    self.data["UZR"][i][1] = parseB.AUTOatof(data[1])

    def write(self,output,new=False):
        def compactstr(value):
            """check if we can use more compact output than str..."""
            try:
                str1 = "%.5g"%value
                str2 = str(value)
                if float(str1) == float(str2) and 'e' in str1:
                    return str1
                return str2
            except TypeError:
                l = []
                for v in value:
                    l.append(compactstr(v))
                return '['+string.join(l,", ")+']'
            
        wdth2keys = ["A0","A1"]
        wdth3keys = ["RL0","RL1","NMX","NPR","NBC","JAC","e"]
        wdth5keys = ["EPSU","EPSS"]
        if self.__new or new:
            lines = [
                ["e","s","dat","sv"],
                ["unames","parnames"],
                ["U","PAR"],
                ["NDIM","IPS","IRS","ILP"],
                ["ICP"],
                ["NTST","NCOL","IAD","ISP","ISW","IPLT","NBC","NINT"],
                ["NMX","RL0","RL1","A0","A1"],
                ["NPR","MXBF","IID","ITMX","ITNW","NWTN","JAC"],
                ["EPSL","EPSU","EPSS"],
                ["DS","DSMIN","DSMAX","IADS"],
                ["NPAR","THL","THU"],
                ["UZR"],
                ["SP"],
                ["NUNSTAB","NSTAB","IEQUIB","ITWIST","ISTART"],
                ["IREV","IFIXED","IPSI"]]
        else:
            lines = [
                ["e","s","dat","sv"],
                ["unames","parnames"],
                ["U","PAR"],
                ["NPAR"],
                ["SP"],
                ["NUNSTAB","NSTAB","IEQUIB","ITWIST","ISTART"],
                ["IREV","IFIXED","IPSI"]]
        for line in lines:
            pos = 0
            for key in line:
                value = self.get(key)
                if value is None:
                    continue
                if pos > 0:
                    output.write(", ")
                pos = pos + 1
                if key in wdth2keys:
                    output.write("%-2s="%key)
                elif key in wdth3keys:
                    output.write("%-3s="%key)
                elif key in wdth5keys:
                    output.write("%-5s="%key)
                else:
                    output.write("%-4s="%key)
                if key in ["ICP","IREV","IFIXED","IPSI","SP"]:
                    output.write("  "+str(value))
                elif key in ["THL","THU","UZR","U","PAR"]:
                    l=[]
                    for k,v in value:
                        l.append(repr(k)+": "+compactstr(v))
                    output.write("  {"+string.join(l,", ")+"}")
                elif key in ["unames","parnames"]:
                    l=[]
                    for k,v in value:
                        l.append(str(k)+": "+repr(v))
                    output.write("{"+string.join(l,", ")+"}")
                elif key in ["sv","s","dat","e"]:
                    value = "'"+str(value)+"'"
                    if key in wdth3keys:
                        output.write("%5s"%value)
                    else:
                        output.write("%4s"%value)
                elif key[0] in ["A", "D", "E", "R"]:
                    value = compactstr(value)
                    output.write("%6s"%value)
                elif pos > 4:
                    output.write("%2s"%value)
                elif key in wdth3keys:
                    output.write("%5s"%value)
                else:
                    output.write("%4s"%value)
            if pos > 0:
                output.write("\n")
        if self.__new or new:
            return
            
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
	    output.write(str(self["THL"][i][0])+" ")
	    output.write(str(self["THL"][i][1])+"\n")

	output.write(str(self["__NTHU"]))
	output.write("          "+line9_comment+"\n")
	for i in range(self["__NTHU"]):
	    output.write(str(self["THU"][i][0])+" ")
	    output.write(str(self["THU"][i][1])+"\n")

	output.write(str(self["__NUZR"]))
	output.write("          "+line10_comment+"\n")
	for i in range(self["__NUZR"]):
	    output.write(str(self["UZR"][i][0])+" ")
	    output.write(str(self["UZR"][i][1])+"\n")
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


