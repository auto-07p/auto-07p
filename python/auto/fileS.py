#! /usr/bin/env python
# File I/O utilities for reading AUTO fort.8 files
# Extracted to break cyclic imports

import os
import sys
import struct
try:
    from UserDict import UserDict
    from UserList import UserList
except ImportError: #Python 3
    from collections import UserDict, UserList
try:
    file
except NameError: #Python 3
    from io import IOBase as file
from auto import AUTOExceptions
import copy
from auto import parseB
from auto import parseC
from auto import Points
from auto import AUTOutil
import gzip
import types

# End of data exception definition
class PrematureEndofData(Exception):
    pass

class IncorrectHeaderLength(Exception):
    pass

# This is the number of parameters.  Should be read from auto.h...
# This is not required anymore in AUTO97, since the last entry in
# the header line is this number
NPAR = 20

class fileS(object):
    def __init__(self, filename):
        if isinstance(filename, str):
            inputfile = AUTOutil.openFilename(filename,"rb")
        else:
            inputfile = filename
        self.inputfile = inputfile
        self.name = inputfile.name
        self.solutions = []

        # We now go through the file and read the solutions.
        prev = None
        # for fort.8 we need to read everything into memory; otherwise load the
        # data on demand from disk when we really need it
        # on Windows always load everything because deleting open files is
        # impossible there
        inmemory = (os.path.basename(inputfile.name) == 'fort.8' or
                    sys.platform in ['cygwin', 'win32'])
        while len(inputfile.read(1)) > 0:
            line = inputfile.readline()
            if not line: raise PrematureEndofData
            try:
                header = list(map(int, line.split()))
            except ValueError:
                raise PrematureEndofData
            if len(header) < 10:
                raise PrematureEndofData
            if len(header) == 10:
                # This is the case for AUTO94 and before
                header = header + [NPAR]
            numLinesPerEntry = header[8]
            start_of_data = inputfile.tell()
            if prev is not None and all(
                [header[i] == prevheader[i] for i in [4, 6, 7, 8, 11]]):
                # guess the end from the previous solution
                end += inputfile.tell() - prev
                # See if the guess for the solution end is correct
                inputfile.seek(end)
                data = inputfile.readline().split()
                # This is where we detect the end of the file
                if len(data) == 0:
                    data = inputfile.read(1)
                if len(data) != 0:
                    try:
                        # Check length of line...
                        if len(data) != 12 and len(data) != 16:
                            raise IncorrectHeaderLength
                        # and the fact they are all integers
                        map(int,data)
                        # and the fact that NCOL*NTST+1=NTPL
                        if int(data[9])*int(data[10])+1 != int(data[6]):
                            end = None
                        # If it passes all these tests we say it is a header line
                        # and we can read quickly
                    except:
                        # otherwise the guessed end is not valid
                        end = None
            else:
                end = None
            data = None
            if end is None:
                # We skip the correct number of lines in the entry to
                # determine its end.
                inputfile.seek(start_of_data)
                if inmemory:
                    data = "".encode("ascii").join([inputfile.readline()
                                   for i in range(numLinesPerEntry)])
                else:
                    for i in range(numLinesPerEntry):
                        inputfile.readline()
                end = inputfile.tell()
            elif inmemory:
                inputfile.seek(start_of_data)
                data = inputfile.read(end - start_of_data)
            else:
                inputfile.seek(end)
            if data is None:
                data = (start_of_data, end)
            self.solutions.append({'header': header, 'data': data})
            prev = start_of_data
            prevheader = header

    def readstr(self, i):
        solution = self.solutions[i]
        data = solution['data']
        if not isinstance(data, tuple):
            return data
        start = data[0]
        end = data[1]
        self.inputfile.seek(start)
        solution['data'] = self.inputfile.read(end - start)
        return solution['data']

    def readfloats(self, i, total):
        if not Points.numpyimported:
            Points.importnumpy()       
        N = Points.N
        data = self.readstr(i)
        if hasattr(N, "ndarray") and isinstance(data, N.ndarray):
            return data
        fromstring = Points.fromstring
        if fromstring:
            fdata = []
            if "D".encode("ascii") not in data:
                fdata = fromstring(data, dtype=float, sep=' ')
            if fdata.tolist() == [] or len(fdata) != total:
                fdata = N.array(list(map(parseB.AUTOatof,
                                    data.split())), 'd')
            else:
                #make sure the last element is correct
                #(fromstring may not do this correctly for a
                #string like -2.05071-106)
                fdata[-1] = parseB.AUTOatof(
                    data[data.rfind(" ".encode("ascii"))+1:].strip())
        else:
            data = data.split()
            try:
                fdata = N.array(list(map(float, data)), 'd')
            except ValueError:
                fdata = N.array(list(map(parseB.AUTOatof, data)), 'd')
        if total != len(fdata):
            raise PrematureEndofData
        del self.solutions[i]['data']
        self.solutions[i]['data'] = fdata
        return fdata

    def conditionalclose(self):
        # if everything is in memory, close the file
        for s in self.solutions:
            if isinstance(s['data'], tuple):
                return
        self.inputfile.close()
