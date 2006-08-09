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
        #The first two parts are header, perhaps later we will keep
        #it, but for now we just discard it
        data = data[2:-1]
        self.__data=[]
        for solution in data:
            self.__data.append({})
            self.__data[-1]["Text"] = solution
            self.__data[-1]["Label"] = string.split(solution)[4]
            self.__data[-1]["Eigenvalues"] = []
            result = re.findall("Eigenvalue\s.*",solution)
            for eigenvalue_string in result:
                eigenvalue_string = string.split(eigenvalue_string)
                real_part = string.atof(eigenvalue_string[2])
                imag_part = string.atof(eigenvalue_string[3])
                self.__data[-1]["Eigenvalues"].append([real_part,imag_part])

    def readFilename(self,filename):
        self.read(open(filename,"r"))
        
    def write(self,output):
        AUTORuntimeError("parseD does not have a write routine")

    def writeFilename(self,filename):
        self.write(open(filename,"w"))


def test():
    AUTORuntimeError("No regression test")

if __name__ == "__main__":
    #Parse command line arguements
    opts_list,args=getopt.getopt(sys.argv[1:],"i:")
    opts={}
    for x in opts_list:
        opts[x[0]]=x[1]

    foo = parseD(args[0])
    foo.printMatrix(string.atoi(opts["-i"]),sys.stdout)

    


