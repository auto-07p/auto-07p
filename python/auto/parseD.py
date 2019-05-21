#! /usr/bin/env python

# This a class to parse fort.9 files, i.e. the diagnostics from AUTO
# NOTE:  This is nowhere near done, and it currently only finds the
# reduced jacobians and puts them into a list.

import re
import sys
try:
    from UserList import UserList
except ImportError: # Python 3
    from collections import UserList
import getopt
import AUTOutil
import parseB

class parseD(UserList):
    def __init__(self,filename=None):
        if isinstance(filename, str):
            UserList.__init__(self)
            self.readFilename(filename)
        else:
            UserList.__init__(self,filename)

    def __getitem__(self,index):
        return self.getIndex(index)

    def __call__(self,label):
        return self.getLabel(label)

    def __str__(self):
        s = []
        for d in self.data:
            s.append(d["Text"])
        return "".join(s)

    def getIndex(self,index):
        self.__readAll()
        return self.data[index]

    def getLabel(self,label):
        self.__readAll()
        for x in self.data:
            if x["Label"] == label:
                return x

    def __readAll(self):
        for item in self.data:
            solution = item["Text"]
            item.update({
                    "Branch number": 0,
                    "Point number": 0,
                    "Label": 0,
                    "Eigenvalues": [],
                    "Multipliers": []})
            lines = solution.splitlines()
            if len(lines) < 3:
                continue
            i = 0
            for line in lines:
                sp = line.split()
                if len(sp) > 0 and sp[0] == 'BR':
                    break
                i = i + 1
            if i + 1 >= len(lines):
                continue
            sp = lines[i+1].split()
            if len(sp) < 2:
                continue
            item["Branch number"] = int(sp[0])
            item["Point number"] = int(sp[1])
            labline = 0
            for line in lines:
                sp = line.split()
                if labline and len(sp) > 3:
                    if sp[2] != '0':
                        try:
                            item["Label"] = int(sp[2])
                        except:
                            item["Label"] = int(sp[3])
                    break
                if sp[0:4] == ['BR', 'PT', 'TY', 'LAB']:
                    labline = 1
            result = re.findall("Eigenvalue\s.*",solution)
            for eigenvalue_string in result:
                eigenvalue_string = eigenvalue_string.split()
                real_part = parseB.AUTOatof(eigenvalue_string[2])
                imag_part = parseB.AUTOatof(eigenvalue_string[3])
                item["Eigenvalues"].append([real_part,imag_part])
            result = re.findall("Multiplier\s.*",solution)
            for multiplier_string in result:
                multiplier_string = multiplier_string.split()
                # "inaccurate" or "accurate"
                if multiplier_string[1][-1] == "e":
                    continue
                real_part = parseB.AUTOatof(multiplier_string[2])
                imag_part = parseB.AUTOatof(multiplier_string[3])
                item["Multipliers"].append([real_part,imag_part])
        
    def read(self,input):
        data = input.read()
        divstr = "===============================================\n"
        data=data.split(divstr)
        if len(data) == 1:
            divstr = "--------------------------------------------------------------------------------------------\n"
            data=data[0].split(divstr)
        self.data=[]
        for solution in data:
            self.data.append({"Text": solution + divstr})
        self.data[-1]["Text"] = self.data[-1]["Text"][:-len(divstr)]

    def readFilename(self,filename):
        inputfile = AUTOutil.openFilename(filename, "r")
        self.read(inputfile)
        
    def write(self,output):
        output.write(self.__str__())

    def writeFilename(self,filename,append=False):
        if append:
            output = open(filename,"a")
        else:
            output = open(filename,"w")
        self.write(output)
        output.close()


def test():
    pass

if __name__ == "__main__":
    #Parse command line arguements
    opts_list,args=getopt.getopt(sys.argv[1:],"i:")
    opts={}
    for x in opts_list:
        opts[x[0]]=x[1]

    foo = parseD(args[0])
    foo.printMatrix(int(opts["-i"]),sys.stdout)

    


