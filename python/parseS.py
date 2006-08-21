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

from string import *
import os
import sys
import UserDict
from AUTOExceptions import *
import types
import copy
import parseB

# End of data exception definition
PrematureEndofData = "Premature End of Data"

# This is the number of parameters.  Should be read from auto.h...
# This is not required anymore in AUTO97, since the last entry in
# the header line is this number
NPAR = 20

# The parseS class parses an AUTO fort.8 file
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
# in the fort.8 file.

class parseS:
    def __init__(self,filename=None):
        self.data=[]
        if not (filename is None):
            self.__setFile(open(filename,"rb"))

    def __str__(self):
        rep = ""
        rep = rep + "Number of solutions :" + str(len(self)) + "\n"
        labels = self.getLabels()
        rep = rep + "Labels: "
        for i in range(len(labels)):
            rep = rep + str(labels[i]) + " "
        rep = rep + "\n"
        return rep

    def __getitem__(self,index):
        return self.getIndex(index)

    def __call__(self,label):
        return self.getLabel(label)

    def __len__(self):
        return len(self.data)

    def __setFile(self,file):
        # Remember the offsets we compute
        offsets = []
        # Go to the beginning...
        file.seek(0,0)
        # Read in the first solution
        solution = AUTOSolution()
        solution.readAll(file)
        self.data.append(solution)
        # So we can get figure out how many bytes the first solution takes.
        # We will use this as a guess for the rest
        offsets.append(0) 
        guess_at_size = file.tell()

        # We now go through the file and compute the rest of
        # the offsets.
        while 1:
            start_of_current_solution = offsets[-1]
            # See if the guess for the solution size is correct
            file.seek(start_of_current_solution+guess_at_size+1,0)
            data = file.readline()
            data = split(data)
            # This is where we detect the end of the file
            if len(data) == 0:
                data = file.read(1)
            if len(data) == 0:
                self.data[-1]._setEnd(start_of_current_solution+guess_at_size+1)
                break
            else:
                try:
                    # Check length of line...
                    if len(data) != 12:
                        raise "Incorrect header length"
                    # and the fact they are all integers
                    map(atoi,data)
                    # If it passes both these tests we say it is a header line
                    # and we update the offsets
                    offsets.append(start_of_current_solution+guess_at_size)
                    self.data[-1]._setEnd(start_of_current_solution+guess_at_size)
                    solution = AUTOSolution()
                    solution.read(file,start_of_current_solution+guess_at_size+1)
                    self.data.append(solution)
                except:
                    # Ok, the guess for the size of the solution was wrong...
                    # So, we just read it in and get the size from that.
                    # The assumption is that this does not happen very often.

                    # We access the data to get the object to parse everthing
                    self.data[-1]._forceParse()
                    # Something wierd is going on here, that I don't
                    # quite understand.  self.data[-1]._getEnd() is getting
                    # messed up my the solution.read(file,end) and I am
                    # not sure why.  I need to record the good value here
                    # to fix it.
                    end = self.data[-1]._getEnd()
                    # If this read ends up in an exception we are in the case
                    # where the LAST solution is the one which has a
                    # different size and it is LONGER then the previous
                    # solutions.  In this case, "data" points into the
                    # middle of the solution.
                    solution = AUTOSolution()
                    try:
                        solution.read(file,end)
                    except PrematureEndofData:
                        return
                    offsets.append(end)
                    guess_at_size = end - start_of_current_solution
                        
                    self.data.append(solution)

    # This function needs a little explanation
    # It trys to read a new point from the input file, and if
    # it cannot (because the file ends prematurely) is sets the
    # file pointer back to the way it was when it started and returns
    # This will be used to check a file to see if it has a new point
    # and it will ignore a partially created solution.
    # Basically it for an VBM kind of program.
    def tryNextPointRead(self,input):
	current_position = input.tell()
	try:
	    self.data.append(AUTOSolution(input))
	except PrematureEndofData:
	    input.seek(current_position)

    def read(self,input):
        self.__setFile(input)

    def write(self,output):
        for x in self.data:
            x.write(output)
        output.flush()

    def readFilename(self,filename):
	input = open(filename,"rb")
        self.__setFile(input)

    def writeFilename(self,filename):
	output = open(filename,"w")
        self.write(output)
	output.close()

    # Removes the first solution with the given label
    def deleteLabel(self,label):
        if type(label)  == types.IntType:
            for i in range(len(self)):
                if self.data[i]["Label"] == label:
                    del self.data[i]
                    break
        else:
            for j in range(len(label)):
                for i in range(len(self)):
                    if self.data[i]["Label"] == label[j]:
                        del self.data[i]
                        break
            
    # Relabels the first solution with the given label
    def relabel(self,old_label,new_label):
        if type(old_label)  == types.IntType:
            for i in range(len(self)):
                if self.data[i]["Label"] == old_label:
                    self.data[i]["Label"] = new_label
        else:
            for j in range(len(old_label)):
                for i in range(len(self)):
                    if self.data[i]["Label"] == old_label[j]:
                        self.data[i]["Label"] = new_label[j]

    # Make all labels in the file unique and sequential
    def uniquelyLabel(self):
        for i in range(len(self)):
            self.data[i]["Label"] = i+1

    # Given a label, return the correct solution
    def getLabel(self,label):
        for i in range(len(self)):
            if self.data[i]["Label"] == label:
                return self.data[i]

    def getIndex(self,index):
        return self.data[index]

    # Return a list of all the labels in the file.
    def getLabels(self):
        labels = []
        for x in self.data:
            labels.append(x["Label"])
        return labels

# The AUTOsolution class parses an AUTO fort.8 file
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
# Used by itself is only reads in ONE solution from the file
# for example readFilename will only read the first solution
# Commonly it will be used in a container class only using the
# read and write methods and letting the outside class take care
# of opening the file.

class AUTOSolution(UserDict.UserDict):
    def __init__(self,input=None,offset=None):
	UserDict.UserDict.__init__(self)
        self.__start_of_header = None
        self.__start_of_data   = None
        self.__end              = None
        self.__input           = None
        self.__fullyParsed     = 0
	if input:
	    self.read(input,offset)

    def __str__(self):
        keys = self.keys()
        keys.sort()
        rep=""
        for key in keys:
            if key != "data":
                rep=rep+str(key)+": "+str(self[key])+"\n"
        return rep
    def __getitem__(self,key):
        big_data_keys = ["data","Free Parameters","Parameter NULL vector","Parameters","parameters","p"]
        if type(key) == types.IntType:
            if not(self.__fullyParsed):
                self.__readAll()
            return self["data"][key]
        else:
            if key in big_data_keys and not(self.__fullyParsed):
                self.__readAll()
            return UserDict.UserDict.__getitem__(self,key)
    
    def type(self):
	return parseB.type_translation(self["Type number"])["long name"]

    def readAllFilename(self,filename):
	input = open(filename,"rb")
	self.readAll(input)
	input.close()

    def readFilename(self,filename):
	input = open(filename,"rb")
	self.read(input)
	input.close()

    def writeFilename(self,filename):
	output = open(filename,"w")
	self.write(output)
        output.flush()
	output.close()

    def writeRawFilename(self,filename):
	output = open(filename,"w")
	self.writeRaw(output)
        output.flush()
	output.close()
        
    def toArray(self):
        array = []
        for vector in self["data"]:
            array.append([])
            array[-1].append(vector["t"])
            for point in vector["u"]:
                array[-1].append(point)
        return array

    def writeRaw(self,output):
        for vector in self["data"]:
            output.write(str(vector["t"])+" ")
            for point in vector["u"]:
                output.write(str(point)+" ")
            output.write("\n")
            
    def read(self,input,start=None,end=None):
        self.__input = input
        if not(start is None):
            self.__start_of_header = start
        else:
            self.__start_of_header = 0
        self.__end = end
        self.__readHeader()
    
    def readAll(self,input,start=None,end=None):
        self.read(input,start,end)
        self.__readAll()

    def _setEnd(self,end):
        self.__end = end

    def _getEnd(self):
        return self.__end

    def _forceParse(self):
        self.__readAll()

    def __readHeader(self):
        input = self.__input
        input.seek(self.__start_of_header)
	line = input.readline()
	if not line: raise PrematureEndofData
	data = split(line)
        try:
            self["Branch number"] = atoi(data[0])
            self["Point number"] = atoi(data[1])
            self["Type number"] = atoi(data[2])
            self["Type name"] = parseB.type_translation(self["Type number"])["short name"]
            self["Label"] = atoi(data[3])
            self.__numChangingParameters = atoi(data[4])
            self["ISW"] = atoi(data[5])
            self.__numSValues = atoi(data[6])
            self.__numEntriesPerBlock = atoi(data[7])
            self.__numLinesPerEntry = atoi(data[8])
            self["NTST"] = atoi(data[9])
            self["NCOL"] = atoi(data[10])
            if len(data)==12:
                # This is the case for AUTO97 and beyond
                self.__numFreeParameters = atoi(data[11])
            else:
                # This is the case for AUTO94 and before
                self.__numFreeParameters = NPAR
        except IndexError:
            raise PrematureEndofData
        self.__start_of_data = self.__input.tell()
        
    def __readAll(self):
        self.__fullyParsed = 1
        input = self.__input
        input.seek(self.__start_of_data)
	self["data"] = []
        solution = self["data"]
	for i in range(self.__numSValues):
	    solution.append({})
            point = solution[i]
	    line = input.readline()
	    if not line: raise PrematureEndofData
	    data = split(line)
            pointu = point["u"] = []
            if len(data) > 0:
                point["t"] = AUTOatof(data[0])
                data = data[1:]
            j = 0
	    while len(pointu) < self.__numEntriesPerBlock - 1:
                if pointu != []:
                    line = input.readline()
		    if not line: raise PrematureEndofData
		    data = split(line)
                pointu.extend(map(AUTOatof, data))
	# I am using the value of NTST to test to see if it is an algebraic or
	# ODE problem.
	if self["NTST"] != 0:
	    self["Free Parameters"] = []
	    line = input.readline()
	    if not line: raise PrematureEndofData
            self["Free Parameters"].extend(map(atoi, split(line)))
	    self["Parameter NULL vector"] = []
            while len(self["Parameter NULL vector"]) < len(self["Free Parameters"]):
                line = input.readline()
                if not line: raise PrematureEndofData
                self["Parameter NULL vector"].extend(map(AUTOatof, split(line)))

	    if len(self["Parameter NULL vector"]) != len(self["Free Parameters"]):
		print "BEWARE!! size of parameter NULL vector and number of changing"
		print "parameters are not equal.  This is probably because of these"
		print "arrays being on multiple lines in the fort.8 file"
		exit(1)

	    for i in range(self.__numSValues):
                udot = self["data"][i]["u dot"] = []
		j = 0
		while len(udot) < self.__numEntriesPerBlock-1:
		    line = input.readline()
		    if not line: raise PrematureEndofData
		    data = split(line)
                    udot.extend(map(AUTOatof, data))

	parameters = self["Parameters"] = []
	while len(parameters) < self.__numFreeParameters:
	    line = input.readline()
	    if not line: raise PrematureEndofData
            parameters.extend(map(AUTOatof, split(line)))
	    
        self.__end = input.tell()
        self["parameters"] = self["Parameters"]
        self["p"] = self["Parameters"]


    def write(self,output):
	line = "%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d" % (self["Branch number"],
                                                         self["Point number"],
                                                         self["Type number"],
                                                         self["Label"],
                                                         self.__numChangingParameters,
                                                         self["ISW"],
                                                         self.__numSValues,
                                                         self.__numEntriesPerBlock,
                                                         self.__numLinesPerEntry,
                                                         self["NTST"],
                                                         self["NCOL"],
                                                         self.__numFreeParameters
                                                         )
	output.write(line+"\n")
        # If the file isn't already parsed, and we happen to know the position of
        # the end of the solution we can just copy from the input file into the
        # output file.
        if not(self.__fullyParsed) and not(self.__end is None):
            self.__input.seek(self.__start_of_data)
            output.write(self.__input.read(self.__end - self.__start_of_data))
            output.flush()
        # Otherwise we do a normal write.  NOTE: if the solution isn't already
        # parsed it will get parsed here.
        else:
            for i in range(self.__numSValues):
                num = "%.10E" % (self["data"][i]["t"])
                if AUTOatof(num) < 0:
                    line = "     "+num
                else:
                    line = "      "+num
                for j in range(len(self["data"][i]["u"])):
                    num = "%.10E" % (self["data"][i]["u"][j])
                    if AUTOatof(num) < 0:
                        line = line + " " + num
                    else:
                        line = line + "  " + num
                    if (j+2)%7==0:
                        output.write(line+"\n")
                        line = "    "
                if (j+2)%7!=0:
                    output.write(line+"\n")
            # I am using the value of NTST to test to see if it is an algebraic or
            # ODE problem.
            if self["NTST"] != 0:
                for i in range(len(self["Free Parameters"])):
                    line = "%5d" % (self["Free Parameters"][i])
                    output.write(line)
                output.write("\n")

                line = "    "
                for i in range(len(self["Parameter NULL vector"])):
                    num = "%.10E" % (self["Parameter NULL vector"][i])
                    if i != 0 and i%7==0:
                        line = line + "\n    "
                    if AUTOatof(num) < 0:
                        line = line + " " + num
                    else:
                        line = line + "  " + num
                output.write(line+"\n")

                for i in range(self.__numSValues):
                    line = "    "
                    for j in range(len(self["data"][i]["u dot"])):
                        num = "%.10E" % (self["data"][i]["u dot"][j])
                        if AUTOatof(num) < 0:
                            line = line + " " + num
                        else:
                            line = line + "  " + num
                        if (j+1)%7==0:
                            output.write(line+"\n")
                            line = "    "
                    if (j+1)%7!=0:
                        output.write(line+"\n")

            line = "    "
            for j in range(self.__numFreeParameters):
                num = "%.10E" % (self["Parameters"][j])
                if AUTOatof(num) < 0:
                    line = line + " " + num 
                else:
                    line = line + "  " + num 
                if (j+1)%7==0:
                    output.write(line+"\n")
                    line = "    "
            if (j+1)%7!=0:
                output.write(line+"\n")
            output.flush()

def AUTOatof(input_string):
    #Sometimes AUTO messes up the out.  I.e. it gives an
    #invalid floating point number of the form x.xxxxxxxE
    #instead of x.xxxxxxxE+xx.  Here we assume the exponent
    #is 0 and make it into a real real number :-)
    try:
        value=atof(input_string)
    except (ValueError):
        try:
            if input_string[-1] == "E":
                #  This is the case where you have 0.0000000E
                value=atof(strip(input_string)[0:-1])
            elif input_string[-4] == "-":
		# there is a bug on the SGI which prints out numbers of the 
		# for x.xxxxxxxx-323 and x.xxxxxx-324.  When I get these
		# I reset to 0.
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
    keys = ['Type number', 'Type name', 'Parameter NULL vector',
            'Free Parameters', 'Branch number',
            'data', 'NCOL', 'Label', 'ISW', 'NTST',
            'Point number', 'Parameters']

    for key in keys:
        if not(a.has_key(key)):
            raise AUTORegressionError("No %s label"%(key,))
    if not(len(a["data"]) == len(b["data"])):
        raise AUTORegressionError("Data sections have different lengths")


def test():
    print "Testing reading from a filename"
    foo = parseS()
    foo.readFilename("test_data/fort.8")    
    if len(foo) != 5:
        raise AUTORegressionError("File length incorrect")
    pointtest(foo.getIndex(0),foo.getIndex(3))

    print "Testing reading from a stream"
    foo = parseS()
    fp = open("test_data/fort.8","rb")
    foo.read(fp)    
    if len(foo) != 5:
        raise AUTORegressionError("File length incorrect")
    pointtest(foo.getIndex(0),foo.getIndex(3))

    
    
    print "parseS passed all tests"

if __name__ == '__main__' :
    test()









