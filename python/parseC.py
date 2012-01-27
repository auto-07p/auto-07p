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

import sys
import AUTOExceptions
import AUTOutil
import parseB

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

class parseC(dict):

    line_comments = ["NDIM,IPS,IRS,ILP",
                     "NICP,(ICP(I),I=1 NICP)",
                     "NTST,NCOL,IAD,ISP,ISW,IPLT,NBC,NINT",
                     "NMX,RL0,RL1,A0,A1",
                     "NPR,MXBF,IID,ITMX,ITNW,NWTN,JAC",
                     "EPSL,EPSU,EPSS",
                     "DS,DSMIN,DSMAX,IADS",
                     "NTHL,(/,I,THL(I)),I=1,NTHL)",
                     "NTHU,(/,I,THU(I)),I=1,NTHU)",
                     "NUZR,(/,I,PAR(I)),I=1,NUZR)"]

    # These keys are preserved when reading in a new constants file,
    # and the keys are not in the constants file
    # Other keys are set to "None"
    special_keys = ["unames", "parnames", "e", "homcont"]

    def __init__(self, filename=None, **kw):
        self["new"] = True
        if filename is not None and type(filename) != type(""):
            dict.__init__(self, filename)
            self.update(**kw)
            return
        dict.__init__(self)
        for key in ['NPR', 'EPSS', 'ITMX', 'EPSU', 'ITNW', 'NBC',
            'IADS', 'IPS', 'IID', 'IIS', 'A1', 'DS', 'NMX', 'NTST',
            'NINT', 'NWTN', 'A0', 'EPSL', 'ISP', 'DSMIN', 'MXBF',
            'RL0', 'RL1', 'IPLT', 'ILP', 'NCOL',
            'DSMAX', 'ISW', 'IRS', 'IAD', 'JAC', 'NDIM', 'NPAR',
            'IBR', 'LAB', 'TY',
            'NUNSTAB', 'NSTAB', 'IEQUIB', 'ITWIST', 'ISTART',
            'sv', 's', 'dat',
            "THL","THU","UZR","UZSTOP","ICP","IREV","IFIXED","IPSI","U","PAR",
            "SP","STOP"]:
            self[key] = None
        if filename:
            self.readFilename(filename)
        self.update(**kw)

    def __setitem__(self, key, item):
        if key in ["THL", "THU", "UZR", "UZSTOP", "U", "PAR", "unames",
                   "parnames"]:
            if item is not None:
                if key == "PAR":
                    import parseS
                    if isinstance(item,parseS.AUTOParameters):
                        item = [(k,item(k)) for k in range(1,len(item)+1)]
                if isinstance(item, dict):
                    item = item.items()
                new = []
                for x in item:
                    if isinstance(x, dict):
                        new.append([x["PAR index"], x["PAR value"]])
                    else:
                        new.append([x[0], x[1]])
                item = new
        elif key == "DS" and item == '-':
            if self[key] is None:
                item = -0.01
            else:
                item = -self[key]
        dict.__setitem__(self, key, item)

    def update(self, d=None, **kw):
        """     Change the options for this parseC object"""
        dct = d
        if dct is None:
            dct = {}
        dct.update(kw)
        for key in dct:
            value = dct[key]
            if self.get("homcont") is not None and key in self["homcont"]:
                self["homcont"][key] = value
            elif key in self or key in self.special_keys:
                self[key] = value
            elif key[:7] != 'Active ':
                raise AUTOExceptions.AUTORuntimeError(
                    "Unknown option: %s"%(key,))
            
    def setdefault(self, *args):
        k = args[0]
        if len(args) > 1:
            d = args[1]
        else:
            d = None
        if k not in self:
            self[k] = d
        return self.get(*args)
            
    def readFilename(self, filename):
        inputfile = open(filename, "r")
        self.read(inputfile)
        inputfile.close()

    def writeFilename(self, filename, new=False):
        output = open(filename, "w")
        self.write(output, new)
        output.close()

    def scanvalue(self, line, inputfile=None):
        # Scans line for a value
        # returns the value, followed by the rest of the line
        # The value is returned as a flat list of strings if the line 
        # starts with [ and otherwise as a single string
        level = 0
        quote = ' '
        quoteesc = False
        prev = ' '
        npos = 0
        value = ''
        isdict = False
        v = ''
        line = line.strip()
        i = 0
        while True:
            if i == len(line):
                if level == 0 or inputfile is None:
                    break
                line = inputfile.readline().strip()
                i = 0
            npos = i
            c = line[i]
            if quote == ' ':
                if c in [',', ' ']:
                    if level == 0:
                        break
                elif c == ':':
                    pass
                elif c in [']', '}']:
                    if c == '}':
                        isdict = False
                    if level == 1 and prev in ['[', '{']:
                        value = []
                    level = level - 1
                    if v != '':
                        value.append(v)
                    v = ''
                    if c == ']' and isdict:
                        value.append(']')
                else:
                    if prev in [',', ' ', ':'] and level > 0 and v != '':
                        value.append(v)
                        v = ''
                    if c in ['[','{']:
                        if level == 0:
                            value = []
                        level = level + 1
                        if c == '{':
                            isdict = True
                        elif isdict:
                            value.append('[')
                    elif c in ['"', "'"]:
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
        while npos < len(line) and line[npos] in [" ", ","]:
            npos = npos + 1
        if npos >= len(line) - 1:
            line = ''
        else:
            line = line[npos:].strip()
        return value, line

    def parseline(self, line, userspec=False, inputfile=None, lineno=None):
        # parses a new-style constant file line and puts the keys
        # in the dictionary c; also used for the header of a b. file
        while line != "":
            line = line.strip()
            if line[0] in ['#', '!', '_']:
                return
            pos = line.find('=')
            if pos == -1:
                return
            key = line[:pos].strip()
            value, line = self.scanvalue(line[pos+1:],inputfile)
            if key in ['ICP', 'IREV', 'IFIXED', 'IPSI']:
                d = []
                for v in value:
                    try:
                        v = int(v)
                    except ValueError:
                        pass
                    d.append(v)
                value = d
            elif key in ['THU', 'THL', 'UZR', 'UZSTOP', 'U', 'PAR']:
                d = []
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
                    d.append([v0,v1])
                    i = i + 2
                value = self.__compactindexed(d)
            elif key in ['unames', 'parnames']:
                value = [[int(value[i]),value[i+1]] 
                         for i in range(0,len(value),2)]
            elif key in ['s', 'dat', 'sv', 'e', 'SP', 'STOP', 'TY']:
                pass
            elif key in self:
                if key[0] in 'IJKLMN':
                    if value[0] == '*':
                        value = 10**len(value)
                    elif key == 'IRS':
                        try:
                            value = int(value)
                        except ValueError:
                            pass
                    else:
                        value = int(value)
                else:
                    value=parseB.AUTOatof(value)
            elif lineno is not None:
                raise AUTOExceptions.AUTORuntimeError(
                    "Unknown AUTO constant %s on line %d"%(key,lineno))
            else:
                value = None
            if value is not None:
                if userspec:
                    self["Active "+key] = self[key]
                self[key] = value

    def read(self, inputfile):
        line = inputfile.readline()
        lineno = 1
        while line != '':
            data = line.split()
            if len(data) > 0:
                if data[0][0].isdigit():
                    break
                self.parseline(line,inputfile=inputfile,lineno=lineno)
            line = inputfile.readline()
            lineno += 1
                
        if line == '':
            return
        
        self["new"] = False
        self["NDIM"] = int(data[0])
        self["IPS"] = int(data[1])
        try:
            self["IRS"] = int(data[2])
        except ValueError:
            self["IRS"] = data[2]            
        self["ILP"] = int(data[3])

        line = inputfile.readline()
        data = line.split()
        icp = []
        for i in range(int(data[0])):
            d = data[i+1]
            try:
                d = int(d)
            except ValueError:
                pass
            icp.append(d)
        self["ICP"] = icp

        for line_comment in self.line_comments[2:7]:
            line = inputfile.readline()
            data = line.split()
            for d, key in zip(data, line_comment.split(",")):
                if key[0] in 'IJKLMN':
                    self[key] = int(d)
                else:
                    self[key] = parseB.AUTOatof(d)

        for key in ["THL", "THU", "UZR"]:
            line = inputfile.readline()
            data = line.split()
            item = []
            for i in range(int(data[0])):
                line = inputfile.readline()
                data = line.split()
                d = data[0]
                try:
                    d = int(d)
                except ValueError:
                    pass
                item.append([d, parseB.AUTOatof(data[1])])
            self[key] = item

    def __compactindexed(self, value):
        """compact THL/THU/UZR lists"""
        d = []
        v0s = []
        for v0, v1 in value:
            if v0 in v0s:
                if not AUTOutil.isiterable(v1):
                    v1 = [v1]
                # add to list when parameter was already encountered
                try:
                    d[v0s.index(v0)][1].extend(v1)
                except AttributeError:
                    d[v0s.index(v0)][1] = [d[v0s.index(v0)][1]]
                    d[v0s.index(v0)][1].extend(v1)
            else:
                v0s.append(v0)
                d.append([v0,v1])
        return d

    def __compactstr(self, value):
        """check if we can use more compact output than str..."""
        try:
            str1 = "%.5g" % value
            str2 = str(value)
            if float(str1) == float(str2) and 'e' in str1:
                return str1
            return str2
        except TypeError:
            l = []
            for v in value:
                l.append(self.__compactstr(v))
            return '['+", ".join(l)+']'
            
    def __newstr(self, lines=None):
        wdth3keys = ["NMX", "NPR", "NBC", "JAC", "e"]
        wdth5keys = ["EPSU", "EPSS"]
        if lines is None:
            lines = [
                ["e", "s", "dat", "sv"],
                ["unames", "parnames"],
                ["U", "PAR"],
                ["NDIM", "IPS", "IRS", "ILP"],
                ["ICP"],
                ["NTST", "NCOL", "IAD", "ISP", "ISW", "IPLT", "NBC", "NINT"],
                ["NMX", "NPR", "MXBF", "IID", "ITMX", "ITNW", "NWTN", "JAC"],
                ["EPSL", "EPSU", "EPSS"],
                ["DS", "DSMIN", "DSMAX", "IADS"],
                ["NPAR", "THL", "THU"],
                ["IIS", "IBR", "LAB", "TY"],
                ["UZR"],
                ["UZSTOP"],
                ["STOP"],
                ["SP"],
                ["RL0", "RL1", "A0", "A1"],
                ["NUNSTAB", "NSTAB", "IEQUIB", "ITWIST", "ISTART"],
                ["IREV", "IFIXED", "IPSI"]]
        olist = []
        for line in lines:
            pos = 0
            for key in line:
                value = self.get(key)
                if value is None:
                    continue
                if pos > 0:
                    olist.append(", ")
                pos = pos + 1
                if key in wdth3keys:
                    s = "%-3s=" % key
                elif key in wdth5keys:
                    s = "%-5s=" % key
                else:
                    s = "%-4s=" % key
                olist.append(s)
                if key in ["ICP", "IREV", "IFIXED", "IPSI", "STOP", "SP"]:
                    s = "  "+str(value)
                elif key in ["THL", "THU", "UZR", "UZSTOP", "U", "PAR"]:
                    l = [repr(k)+": "+self.__compactstr(v) for k, v in value]
                    s = "  {"+", ".join(l)+"}"
                elif key in ["unames", "parnames"]:
                    l = [str(k)+": "+repr(v) for k, v in value]
                    s = "{"+", ".join(l)+"}"
                elif key in ["sv", "s", "dat", "e", "TY"]:
                    value = "'"+str(value)+"'"
                    if key in wdth3keys:
                        s = "%5s" % value
                    else:
                        s = "%4s" % value
                elif key[0] in ["A", "D", "E", "R"]:
                    value = self.__compactstr(value)
                    s = "%6s" % value
                elif pos > 4:
                    s = "%2s" % value
                elif key in wdth3keys:
                    s = "%5s" % value
                else:
                    s = "%4s" % value
                olist.append(s)
            if pos > 0:
                olist.append("\n")
        return "".join(olist)

    def __oldstr(self):
        olist = [self.__newstr([
                ["e", "s", "dat", "sv"],
                ["unames", "parnames"],
                ["U", "PAR"],
                ["NPAR", "IBR", "LAB"],
                ["STOP"],
                ["SP"],
                ["NUNSTAB", "NSTAB", "IEQUIB", "ITWIST", "ISTART"],
                ["IREV", "IFIXED", "IPSI"]])]
            
        for j, line_comment in enumerate(self.line_comments):
            if j==1:
                s = " ".join([str(len(self["ICP"]))]+map(str,self["ICP"]))
            elif j==7:
                s = str(len(self["THL"]))
            elif j==8:
                s = str(len(self["THU"]))
            elif j==9:
                uzrlist = []
                for k, v in self["UZR"] or []:
                    if not AUTOutil.isiterable(v):
                        v = [v]
                    for vv in v:
                        uzrlist.append([k, vv])
                s = str(len(uzrlist))
            else:
                s = " ".join([str(self[d]) for d in line_comment.split(",")])
            olist.append(s+" "*(max(24-len(s),1))+line_comment+"\n")
            if j==7:
                for k, v in self["THL"] or []:
                    olist.append("%s %s\n" % (k, v))
            elif j==8:
                for k, v in self["THU"] or []:
                    olist.append("%s %s\n" % (k, v))
            elif j==9:
                for k, v in uzrlist:
                    olist.append("%s %s\n" % (k, v))

        return "".join(olist)

    def __str__(self):
        if self["new"]:
            return self.__newstr()
        else:
            return self.__oldstr()

    def write(self, output, new=False):
        if new and not self["new"]:
            for key in ["UZR", "THL", "THU"]:
                self[key] = self.__compactindexed(self[key])
            output.write(self.__newstr())
        else:
            output.write(str(self))

def pointtest(a):
    keys = ['NPR', 'UZR', 'EPSS', 'ITMX', 'EPSU', 'ITNW', 'NBC',
            'IADS', 'IPS', 'IID', 'A1', 'DS', 'NMX', 'NTST',
            'NINT', 'NWTN', 'A0', 'EPSL', 'ISP', 'DSMIN', 'MXBF',
            'RL0', 'RL1', 'ICP', 'IPLT', 'ILP', 'NCOL', 'THL',
            'DSMAX', 'ISW', 'IRS', 'THU', 'IAD', 'JAC', 'NDIM']
    for key in keys:
        if key not in a:
            raise AUTOExceptions.AUTORegressionError("No %s label"%(key,))

def test():
    print("Testing reading from a filename")
    foo = parseC()
    foo.readFilename("test_data/c.ab")    
    pointtest(foo)
    foo.write(sys.stdout, new=True)

    print("Testing reading from a stream")
    foo = parseC()
    fp = open("test_data/c.ab", "r")
    foo.read(fp)    
    pointtest(foo)

    print("parseC passed all tests")

if __name__ == '__main__' :
    test()


