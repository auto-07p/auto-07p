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

import AUTOExceptions

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



class parseH(dict):

    line1_comment = "NUNSTAB,NSTAB,IEQUIB,ITWIST,ISTART"
    line2_comment = "NREV,/,IREV(I),I=1,NDIM)"
    line3_comment = "NFIXED,(/,I,IFIXED(I)),I=1,NFIXED)"
    line4_comment = "NPSI,(/,I,IPSI(I)),I=1,NPSI)"

    def __init__(self, filename=None):
        if filename is not None and type(filename) != type(""):
            dict.__init__(self, filename)
            return
        dict.__init__(self)
        for key in ['NUNSTAB', 'NSTAB', 'IEQUIB', 'ITWIST', 'ISTART',
                    'NREV', 'NFIXED', 'NPSI',
                    'IREV', 'IFIXED', 'IPSI']:
            self[key] = None
        if filename:
            self.readFilename(filename)
        
    def readFilename(self, filename):
        inputfile = open(filename, "r")
        self.read(inputfile)
        inputfile.close()

    def writeFilename(self, filename):
        output = open(filename, "w")
        self.write(output)
        output.close()

    def read(self, inputfile):
        line = inputfile.readline()
        data = line.split()
        self["NUNSTAB"] = int(data[0])
        self["NSTAB"] = int(data[1])
        self["IEQUIB"] = int(data[2])
        self["ITWIST"] = int(data[3])
        self["ISTART"] = int(data[4])

        line = inputfile.readline()
        data = line.split()
        nrev = int(data[0])
        data = []
        if nrev > 0:
            line = inputfile.readline()
            data = line.split()
        self["IREV"] = map(int, data)
        self["NREV"] = nrev

        line = inputfile.readline()
        data = line.split()
        nfixed = int(data[0])
        data = []
        if nfixed > 0:
            line = inputfile.readline()
            data = line.split()
        self["IFIXED"] = map(int, data[:nfixed])
        self["NFIXED"] = nfixed

        line = inputfile.readline()
        data = line.split()
        npsi = int(data[0])
        data = []
        if npsi > 0:
            line = inputfile.readline()
            data = line.split()
        self["IPSI"] = map(int, data[:npsi])
        self["NPSI"] = npsi

    def __str__(self):
        olist = ["%s %s %s %s %s           %s\n" %
                 (self["NUNSTAB"], self["NSTAB"], self["IEQUIB"],
                  self["ITWIST"], self["ISTART"], self.line1_comment)]

        nrev = 0
        if len(self["IREV"]) > 0:
            nrev = 1
        olist.append("%s           %s\n"%(nrev, self.line2_comment))
        if nrev > 0:
            olist.append(" ".join(map(str, self["IREV"]))+"\n")

        olist.append("%s           %s\n"%(len(self["IFIXED"]),
                                          self.line3_comment))
        if len(self["IFIXED"]) > 0:
            olist.append(" ".join(map(str, self["IFIXED"]))+"\n")

        olist.append("%s           %s\n"%(len(self["IPSI"]),
                                          self.line4_comment))
        if len(self["IPSI"]) > 0:
            olist.append(" ".join(map(str, self["IPSI"]))+"\n")
        return "".join(olist)
                
    def write(self, output):
        output.write(str(self))

def pointtest(a):
    keys = ['NUNSTAB', 'NSTAB', 'IEQUIB', 'ITWIST', 'ISTART',
            'NREV', 'IREV', 'NFIXED', 'IFIXED', 'NPSI', 'IPSI']         
    for key in keys:
        if key not in a:
            raise AUTOExceptions.AUTORegressionError("No %s label"%(key,))

def test():
    print("Testing reading from a filename")
    foo = parseH()
    foo.readFilename("test_data/h.cir")    
    pointtest(foo)

    print("Testing reading from a stream")
    foo = parseH()
    fp = open("test_data/h.cir","r")
    foo.read(fp)    
    pointtest(foo)

    print("parseH passed all tests")

if __name__ == '__main__' :
    test()








