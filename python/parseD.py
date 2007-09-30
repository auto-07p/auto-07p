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

class parseD(UserList.UserList):
    def __init__(self,filename):
        UserList.UserList.__init__(self)
        self.__data=[]
        if filename:
            self.readFilename(filename)

    def __getitem__(self,index):
        return self.getIndex(index)

    def __call__(self,label):
        return self.getLabel(label)

    def getIndex(self,index):
        return self.__data[index]

    def getLabel(self,label):
        for x in self.__data:
            if x["Label"] == label:
                return x

    def read(self,input):
        data=input.read()
        data=string.split(data,"--------------------------------------------------------------------------------------------")
        if len(data) == 1:
            data=string.split(data[0],"===============================================\n")
        self.__data=[]
        for solution in data:
            self.__data.append({})
            self.__data[-1]["Text"] = solution
            lines = string.split(solution,'\n')
            self.__data[-1]["Branch number"] = 0
            self.__data[-1]["Point number"] = 0
            self.__data[-1]["Label"] = 0
            self.__data[-1]["Eigenvalues"] = []
            self.__data[-1]["Multipliers"] = []
            if len(lines) < 3:
                continue
            i = 0
            for line in lines:
                sp = string.split(line)
                if len(sp) > 0 and sp[0] == 'BR':
                    break
                i = i + 1
            if i + 1 >= len(lines):
                continue
            sp = string.split(lines[i+1])
            if len(sp) < 2:
                continue
            self.__data[-1]["Branch number"] = int(sp[0])
            self.__data[-1]["Point number"] = int(sp[1])
            labline = False
            for line in lines:
                sp = string.split(line)
                if labline:
                    if sp[2] != '0':
                        try:
                            self.__data[-1]["Label"] = int(sp[2])
                        except:
                            self.__data[-1]["Label"] = int(sp[3])
                    break
                if sp[0:4] == ['BR', 'PT', 'TY', 'LAB']:
                    labline = True
            result = re.findall("Eigenvalue\s.*",solution)
            for eigenvalue_string in result:
                eigenvalue_string = string.split(eigenvalue_string)
                real_part = float(eigenvalue_string[2])
                imag_part = float(eigenvalue_string[3])
                self.__data[-1]["Eigenvalues"].append([real_part,imag_part])
            result = re.findall("Multiplier\s.*",solution)
            for multiplier_string in result:
                multiplier_string = string.split(multiplier_string)
                real_part = float(multiplier_string[2])
                imag_part = float(multiplier_string[3])
                self.__data[-1]["Multipliers"].append([real_part,imag_part])

    def readFilename(self,filename):
        self.read(open(filename,"r"))
        
    def write(self,output):
        raise AUTOExceptions.AUTORuntimeError("parseD does not have a write routine")

    def writeFilename(self,filename):
        self.write(open(filename,"w"))


def test():
    raise AUTOExceptions.AUTORuntimeError("parseD does not have a write routine")

if __name__ == "__main__":
    #Parse command line arguements
    opts_list,args=getopt.getopt(sys.argv[1:],"i:")
    opts={}
    for x in opts_list:
        opts[x[0]]=x[1]

    foo = parseD(args[0])
    foo.printMatrix(int(opts["-i"]),sys.stdout)

    


