#! /usr/bin/env python

# This a class to parse fort.9 files, i.e. the diagnostics from AUTO
# NOTE:  This is nowhere near done, and it currently only finds the
# reduced jacobians and puts them into a list.

import re
import sys
import UserList
import string
import getopt
import math
import AUTOExceptions
import parseB
import types

class parseD(UserList.UserList):
    def __init__(self,filename=None):
        if type(filename) == types.StringType:
            UserList.UserList.__init__(self)
            self.readFilename(filename)
        else:
            UserList.UserList.__init__(self,filename)

    def __call__(self,label):
        return self.getLabel(label)

    def __str__(self):
        s = ""
        for d in self.data:
            s = s + d["Text"]
        return s

    def getIndex(self,index):
        return self.data[index]

    def getLabel(self,label):
        for x in self.data:
            if x["Label"] == label:
                return x

    def read(self,input):
        data = input.read()
        divstr = "--------------------------------------------------------------------------------------------\n"
        data=string.split(data,divstr)
        if len(data) == 1:
            divstr = "===============================================\n"
            data=string.split(data[0],divstr)
        self.data=[]
        for solution in data:
            item = {"Text": solution + divstr,
                    "Branch number": 0,
                    "Point number": 0,
                    "Label": 0,
                    "Eigenvalues": [],
                    "Multipliers": []}
            lines = string.split(solution,'\n')
            if len(lines) < 3:
                self.data.append(item)
                continue
            i = 0
            for line in lines:
                sp = string.split(line)
                if len(sp) > 0 and sp[0] == 'BR':
                    break
                i = i + 1
            if i + 1 >= len(lines):
                self.data.append(item)
                continue
            sp = string.split(lines[i+1])
            if len(sp) < 2:
                self.data.append(item)
                continue
            item["Branch number"] = int(sp[0])
            item["Point number"] = int(sp[1])
            labline = 0
            for line in lines:
                sp = string.split(line)
                if labline:
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
                eigenvalue_string = string.split(eigenvalue_string)
                real_part = parseB.AUTOatof(eigenvalue_string[2])
                imag_part = parseB.AUTOatof(eigenvalue_string[3])
                item["Eigenvalues"].append([real_part,imag_part])
            result = re.findall("Multiplier\s.*",solution)
            for multiplier_string in result:
                multiplier_string = string.split(multiplier_string)
                # "inaccurate" or "accurate"
                if multiplier_string[1][-1] == "e":
                    continue
                real_part = parseB.AUTOatof(multiplier_string[2])
                imag_part = parseB.AUTOatof(multiplier_string[3])
                item["Multipliers"].append([real_part,imag_part])
            self.data.append(item)
        self.data[-1]["Text"] = self.data[-1]["Text"][:-len(divstr)]

    def readFilename(self,filename):
        self.read(open(filename,"r"))
        
    def write(self,output):
        output.write(self.__str__())

    def writeFilename(self,filename):
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

    


