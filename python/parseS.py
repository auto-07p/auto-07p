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
import AUTOExceptions
import copy
import parseB
import parseC
import Points
import runAUTO
import gzip
import AUTOutil

# End of data exception definition
class PrematureEndofData(Exception):
    pass

class IncorrectHeaderLength(Exception):
    pass

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

class parseS(list):
    def __init__(self,filename=None):
        if isinstance(filename, str):
            list.__init__(self)
            self.readFilename(filename)
        else:
            if filename is None:
                list.__init__(self)
            else:
                list.__init__(self,filename)

    def __str__(self):
        rep = ""
        rep = rep + "Number of solutions: " + str(len(self)) + "\n"
        labels = self.getLabels()
        rep = rep + "Labels: "
        for label in labels:
            rep = rep + str(label) + " "
        rep = rep + "\n"
        return rep

    def __call__(self,label=None):
        return self.getLabel(label)

    def load(self,**kw):
        """Load solution with the given AUTO constants.
        Returns a shallow copy with a copied set of updated constants
        """
        irs = kw.get("IRS")
        if irs is None:
            irs = (kw.get("constants") or {}).get("IRS")
        sol = None
        if irs is not None:
            if irs != 0:
                try:    
                    sol = self(irs)
                except KeyError:
                    pass
        elif len(self) > 0:
            sol = self[-1]
        if sol is None:
            return AUTOSolution(**kw)            
        else:
            return AUTOSolution.load(sol,**kw)

    # This function needs a little explanation
    # It trys to read a new point from the input file, and if
    # it cannot (because the file ends prematurely) is sets the
    # file pointer back to the way it was when it started and returns
    # This will be used to check a file to see if it has a new point
    # and it will ignore a partially created solution.
    # Basically it for an VBM kind of program.
    def tryNextPointRead(self,inputfile):
        current_position = inputfile.tell()
        try:
            self.append(AUTOSolution(inputfile,name=inputfile.name))
        except PrematureEndofData:
            inputfile.seek(current_position)

    def read(self,inputfile=None):
        # We now go through the file and read the solutions.
        if inputfile is None:
            # read everything into memory
            for solution in self:
                solution.read()
            return
        prev = None
        while len(inputfile.read(1)) > 0:
            solution = AUTOSolution(inputfile,prev,inputfile.name)
            self.append(solution)
            prev = solution
        if len(self) > 0:
            mbr, mlab = 0, 0
            for d in self:
                if d["BR"] > mbr: mbr = d["BR"]
                if d["LAB"] > mlab: mlab = d["LAB"]
            for d in self:
                d._mbr, d._mlab = mbr, mlab

    def write(self,output,mlab=False):
        for i in range(len(self)):
            x = self[i]
            if i == len(self) - 1:
                # maybe write a header after the last solution so that AUTO can
                # pickup a new branch and solution label number
                x.write(output,mlab)
            else:
                x.write(output)
        output.flush()

    def readFilename(self,filename):
        inputfile = AUTOutil.openFilename(filename,"rb")
        self.read(inputfile)
        if os.path.basename(filename) == 'fort.8':
            inputfile.close()
        # else don't close but garbage collect

    def writeFilename(self,filename,append=False,mlab=False):
        # read all solutions because we may overwrite
        self.read()
        if append:
            output = open(filename,"ab")
        else:
            output = open(filename,"wb")
        self.write(output,mlab)
        output.close()

    # Removes solutions with the given labels or type name
    def deleteLabel(self,label=None,keep=0):
        if label == None:
            label=['BP','LP','HB','PD','TR','EP','MX']
        if isinstance(label, (str, int)):
            label = [label]
        indices = []
        for i in range(len(self)):
            x = self[i]
            if ((not keep and (x["Label"] in label or x["Type name"] in label)
                 or (keep and not x["Label"] in label and 
                              not x["Type name"] in label))):
                indices.append(i)
        indices.reverse()
        for i in indices:
            del self[i]
        if len(self) > 0:
            maxlab = max(self.getLabels())
            for d in self:
                d._mlab = maxlab
            
    # Relabels the first solution with the given label
    def relabel(self,old_label=None,new_label=None):
        if old_label is None and new_label is None:
            i = 1
            new = parseS()
            for d in self:
                news = d.__class__(d)
                news["LAB"] = i
                news._mlab = len(self)
                i = i + 1
                new.append(news)
            return new
        if isinstance(old_label, int):
            old_label = [old_label]
            new_label = [new_label]
        for j in range(len(old_label)):
            for d in self:
                if d["Label"] == old_label[j]:
                    d["Label"] = new_label[j]
        if len(self) > 0:
            maxlab = max(self.getLabels())
            for d in self:
                d._mlab = maxlab

    # Make all labels in the file unique and sequential
    def uniquelyLabel(self):
        i = 1
        for d in self:
            d["Label"] = i
            d._mlab = len(self)
            i = i + 1

    # Given a label, return the correct solution
    def getLabel(self,label):
        if label is None:
            return self
        if isinstance(label, int):
            for d in self:
                if d["Label"] == label:
                    return d
            raise KeyError("Label %s not found"%label)
        if isinstance(label, str) and len(label) > 2:
            number = int(label[2:])
            i = 0
            for d in self:
                if d["Type name"] == label[:2]:
                    i = i + 1
                    if i == number:
                        return d
            raise KeyError("Label %s not found"%label)
        if isinstance(label, (str, int)):
            label = [label]        
        data = []
        counts = [0]*len(label)
        for d in self:
            ap = None
            if d["Label"] in label or d["Type name"] in label:
                ap = d
            for i in range(len(label)):
                lab = label[i]
                if (isinstance(lab, str) and len(lab) > 2 and
                    d["Type name"] == lab[:2]):
                    counts[i] = counts[i] + 1
                    if counts[i] == int(lab[2:]):
                        ap = d
            if ap is not None:
                data.append(ap)
        return self.__class__(data)

    def getIndex(self,index):
        return self[index]

    # Return a list of all the labels in the file.
    def getLabels(self):
        labels = []
        for x in self:
            labels.append(x["Label"])
        return labels

# an old-style point and point keys within an AUTOSolution
class SLPointKey(UserList):
    def __init__(self, solution=None, index=None, coords=None):
        if coords=="u dot":
            self.solution = solution["udotps"]
        else:
            self.solution = solution
        self.index = index
    def __getattr__(self, attr):
        if attr == 'data':
            data = []
            for i in range(len(self.solution.coordarray)):
                data.append(self.solution.coordarray[i][self.index])
            return data
        return super(SLPointKey, self).__getattribute__(attr)
    def __setitem__(self, i, item):
        self.solution.coordarray[i][self.index] = item
    def __str__(self):
        return str(self.data)
    def append(self, item):
        self.enlarge(1)
        self.solution.coordarray[-1][self.index] = item
        self.data.append(item)
    def extend(self, other):
        self.enlarge(len(other))
        for i in range(len(other)):
            self.solution.coordarray[-len(other)+i][self.index] = other[i]
    def enlarge(self, ext):
        # enlarges the dimension of coordarray and coordnames
        s = self.solution
        if s._dims is None:
            s._dims = [s.dimension]*len(s)
            c = []
            s0 = s.coordnames[0]
            for i in range(ext):
                c.append(s0[0:s0.find('(')+1]+str(s.dimension+i+1)+')')
            s.extend(c)
        s._dims[self.index] = s.dimension
        if min(s._dims) == max(s._dims):
            s._dims = None

class SLPoint(Points.Point):
    def __init__(self, p, solution=None, index=None):
        Points.Point.__init__(self, p)
        self.index = index
        self.solution = solution

    def __contains__(self, key):
        return key in ["u", "u dot", "t"] or Points.Point.has_key(self,key)

    def has_key(self, key):
        return self.__contains__(key)

    def __getitem__(self, coords):
        if coords == "t":
            return self.solution.indepvararray[self.index]
        if coords in ["u", "u dot"]:
            return SLPointKey(self.solution, self.index, coords)
        return Points.Point.__getitem__(self, coords)

    def __setitem__(self, coords, item):
        if coords == 't':
            self.solution.indepvararray[self.index] = item
        Points.Point.__setitem__(self, coords, item)

    def __str__(self):
        return str({ "t" : self["t"], "u" : self["u"], "u dot" : self["u dot"]})

    __repr__ = __str__

class AUTOParameters(Points.Point):
    def __init__(self, kwd=None, **kw):
        if isinstance(kwd, self.__class__):
            for k,v in kwd.__dict__.items():
                self.__dict__[k] = v
            return
        if kwd is None and kw == {}:
            self.coordnames = []
            self.dimension = 0
            self.parnames = []
            return
        coordnames = kw.get("coordnames",[])[:]
        self.parnames = coordnames[:]
        if "coordarray" in kw:
            for i in range(len(coordnames),len(kw["coordarray"])):
                coordnames.append("PAR("+str(i+1)+")")
            kw["coordtype"] = Points.float64
            kw["coordnames"] = coordnames
        Points.Point.__init__(self,kwd,**kw)

    def __call__(self,index):
        return self.coordarray[index-1]

    def __str__(self):
        parnames = self.parnames
        rep = ""
        for i in range(1,len(self)+1,5):
            j = min(i+4,len(self))
            rep = rep+"\nPAR(%-7s "%(str(i)+':'+str(j)+"):")
            if parnames is not None and i<=len(parnames):
                j2 = min(j,len(parnames))
                for k in range(i,j2+1):
                    rep = rep+"    %-15s"%parnames[k-1]
                rep = rep+"\n            "
            for k in range(i,j+1):
                rep = rep+"%19.10E"%self(k)
        return rep[1:]

# The AUTOsolution class parses an AUTO fort.8 file
# THESE EXPECT THE FILE TO HAVE VERY SPECIFIC FORMAT!
# it provides 4 methods:
# read and write take as an argument either and input or output
#    stream (basically any object with has the method "readline"
#    for reading and "write" for writing)
#    
# readFilename and writeFilename take as an argument a filename
#    in which to read/write the parameters (basically it opens the
#    file and then calles "read" or "write"
#    
# Used by itself is only reads in ONE solution from the file
# for example readFilename will only read the first solution
# Commonly it will be used in a container class only using the
# read and write methods and letting the outside class take care
# of opening the file.

class AUTOSolution(UserDict,Points.Pointset):

    data_keys = ["PT", "BR", "TY number", "TY", "LAB",
                 "ISW", "NTST", "NCOL", "Active ICP", "rldot",
                 "udotps", "NPARI", "NDIM", "IPS", "IPRIV"]

    long_data_keys = {
        "Parameters": "p",
        "parameters": "p",
        "Parameter NULL vector": "rldot",
        "Free Parameters": "Active ICP",
        "Point number": "PT",
        "Branch number": "BR",
        "Type number": "TY number",
        "Type name": "TY",
        "TY name": "TY",
        "Label": "LAB"}

    def __init__(self,input=None,offset=None,name=None,**kw):
        kwdata = {}
        for k in self.data_keys:
            if k in kw and k not in ["ISW","NTST","NCOL","NDIM","IPS"]:
                kwdata[k] = kw[k]
                del kw[k]
        c = kw.get("constants",{}) or {}
        par = None
        coordarray = None
        if isinstance(input,self.__class__):
            for k,v in input.__dict__.items():
                self.__dict__[k] = v
            self.c = parseC.parseC(self.c, **kw)
            self.data = self.data.copy()
            names = kw.get("unames",c.get("unames"))
            if names is not None:
                if type(names) != type({}):
                    names = dict(names)
                for i in range(len(self.coordnames)):
                    self.coordnames[i] = names.get(i+1,'U('+str(i+1)+')')
            names = kw.get("parnames",c.get("parnames"))
            if names is not None:
                if type(names) != type({}):
                    names = dict(names)
                if names != {}:
                    l = len(self.__parnames)
                    self.__parnames = []
                    for i in range(1,max(names)+1,l+1):
                        self.__parnames.append(names.get(i,'PAR('+str(i)+')'))
            if self.__fullyParsed:
                self.PAR = AUTOParameters(coordnames=self.__parnames,
                                          coordarray=Points.array(self.PAR),
                                          name=self.name)
        else:
            UserDict.__init__(self)
            self.c = parseC.parseC(**kw)
            self.__start_of_header = None
            self.__start_of_data   = None
            self.__end             = None
            self.__fullyParsed     = False
            self._dims            = None
            self._mbr             = 0
            self._mlab            = 0
            self.name = name
            self.data.update({"BR":1, "PT":1, "TY number":9, "LAB":0,
                              "ISW":1, "NTST": 1, "NCOL": 0,
                              "NPARI":0, "NDIM": 0, "IPS": None, "IPRIV":0})
            names = kw.get("unames",c.get("unames"))
            self.indepvarname = 't'
            self.coordnames = []
            if names is not None:
                if type(names) != type({}):
                    names = dict(names)
                if names != {}:
                    for i in range(1,max(names)+1):
                        self.coordnames.append(names.get(i,'U('+str(i)+')'))
            names = kw.get("parnames",c.get("parnames"))
            self.__parnames = []
            if names is not None:
                if type(names) != type({}):
                    names = dict(names)
                if names != {}:
                    for i in range(1,max(names)+1):
                        self.__parnames.append(names.get(i,'PAR('+str(i)+')'))
            if input is None:
                pass
            elif isinstance(input,(file,gzip.GzipFile)):
                self.read(input,offset)
            else:
                par = kw.get("PAR",c.get("PAR")) or []
                if type(par) == type({}):
                    par = par.items()
                #init from array
                if not Points.numpyimported:
                    Points.importnumpy()        
                N = Points.N
                if not hasattr(input[0],'__len__'):
                    # point
                    indepvararray = [0.0]
                    coordarray = []
                    ncol = 0
                    ntst = 1
                    for d in input:
                        coordarray.append([d])
                else:
                    # time + solution
                    if "t" in kw:
                        indepvararray = kw["t"]
                        coordarray = input
                    else:
                        indepvararray = input[0]
                        coordarray = input[1:]
                    ncol = 1
                    ntst = len(indepvararray)-1
                    t0 = indepvararray[0]
                    period = indepvararray[-1] - t0
                    if period != 1.0 or t0 != 0.0:
                        #scale to [0,1]
                        for i in range(len(indepvararray)):
                            indepvararray[i] = (indepvararray[i] - t0)/period
                    if 11 not in dict(par):
                        par = [[11,period]] + par
                indepvarname = "t"
                coordnames = []
                for i in range(len(coordarray)):
                    coordnames.append("U("+str(i+1)+")")
                names = kw.get("unames",c.get("unames")) or {}
                if type(names) == type({}):
                    names = names.items()
                for k,v in names:
                    if k > 0 and k <= len(coordnames):
                        coordnames[k-1] = v
                ndim = len(coordarray)
                if ndim < len(self.coordnames):
                    self.coordnames = self.coordnames[:ndim]
                pdict = {"indepvararray": indepvararray,
                         "indepvarname": indepvarname,
                         "coordarray": coordarray,
                         "coordnames": coordnames}
                if "equation" in kw:
                    pdict["name"] = kw["equation"][14:]
                Points.Pointset.__init__(self,pdict)
                self.__fullyParsed = True
                self.data.update({"NTST": ntst, "NCOL": ncol, "LAB": 1,
                                  "NDIM": ndim})
                if par != []:
                    m = len(self.__parnames)
                    for k in dict(par):
                        if not isinstance(k,str):
                            m = max(k,m)
                    p = m*[0.0]
                    self.PAR = AUTOParameters(coordnames=self.__parnames,
                                          coordarray=p, name=self.name)
        if self.name is None or os.path.basename(self.name) == 'fort.8':
            if "equation" in kw:
                self.name = kw["equation"][14:]
            elif kw.get("e") is not None:
                self.name = kw["e"]
            elif c.get("e") is not None:
                self.name = c["e"]
            elif self.name is None:
                self.name = ''
        for k,v in kwdata.items():
            self[k] = v
        if par is None:
            par = kw.get("PAR",c.get("PAR"))
        if (par is not None and
            (self.__start_of_header is not None or self.__fullyParsed)):
            self["PAR"] = par
        if coordarray is None:
            u = kw.get("U",c.get("U"))
            if u is not None:
                self["U"] = u

    def __getstate__(self):
        # For pickle: read everything
        if not self.__fullyParsed and self.__start_of_header is not None:
            self.__readAll()
        return Points.Pointset.__getstate__(self)

    def __str__(self):
        if not self.__fullyParsed and self.__start_of_header is not None:
            self.__readAll()
        keys = list(self.data)
        for key in ["BR","PT","LAB","TY number","ISW","NTST","NCOL","NDIM",
                    "IPS","IPRIV","NPARI"]:
            keys.remove(key)
        keys.sort()
        rep="  BR    PT  TY  LAB ISW NTST NCOL"
        #add corresponding L2-NORM, etc, from fort.7
        if self["IPS"] is not None:
            rep = rep+" NDIM IPS IPRIV"
        for key in keys:
            if key not in self.data_keys:
                rep = rep+"%19s"%key
        rep=rep+ "\n%4d%6d%4s%5d%4d%5d%5d" % (self["BR"], self["PT"],
                                              self["TY"], self["LAB"],
                                              self["ISW"], self["NTST"],
                                              self["NCOL"])
        if self["IPS"] is not None:
            rep = rep+"%5d%4d%6d"%(self["NDIM"],self["IPS"],self["IPRIV"])
        for key in list(keys):
            if key not in self.data_keys:
                rep = rep+"%19.10E"%self[key]
                keys.remove(key)
        if self.__start_of_header is not None or self.__fullyParsed:
            rep=rep+"\n"+Points.Pointset.__repr__(self)
        for key in keys:
            v = self[key]
            if isinstance(v,Points.Pointset):
                v = repr(v)
            elif type(v) not in [type(1),type(1.0),Points.float64,type("")]:
                v = list(v)
            if type(v) == type([]) and type(v[0]) not in [type(1),type(1.0),
                                                          Points.float64]:
                v = map(str,v)
            rep=rep+"\n"+str(key)+": "+str(v)
        if self.__start_of_header is not None or self.__fullyParsed:
            rep=rep+"\n"+str(self.PAR)
        return rep

    def __repr__(self):
        result = id(self)
        if result < 0:
            # avoid negative addresses and Python 2.3 warnings
            result += 256 ** struct.calcsize('P')
        return "<_=%s instance at %#010x>"%(self.__class__.__name__,result)

    def __len__(self):
        return Points.Pointset.__len__(self)

    def __setitem__(self,key,value):
        if (type(key) == type("") and not key in self.coordnames and
            key != self.indepvarname and not key in self.__parnames):
            shortkey = self.long_data_keys.get(key,key)
            if shortkey in self.data_keys:
                if shortkey == "TY":
                    value = parseB.reverse_type_translation(value)
                    shortkey = "TY number"
                elif shortkey == "BR":
                    self._mbr = 0
                elif shortkey == "LAB":
                    self._mlab = 0
                self.data[shortkey] = value
                return
            if shortkey == "PAR":
                if type(value) == type({}):
                    value = value.items()
                for k,v in value:
                    if isinstance(k,str):
                        self.PAR[k] = v
                    else:
                        self.PAR[k-1] = v
                return
            if shortkey == "p":
                self.PAR = AUTOParameters(coordnames=self.__parnames,
                                          coordarray=value)
                return
            if shortkey == "U":
                if type(value) == type({}):
                    value = value.items()
                for i,(k,v) in enumerate(value):
                    if isinstance(k,str):
                        value[i] = self.coordnames.index(k)
                if len(self.coordarray[0]) > 1:
                    # reduce solution to one point
                    del self.coordarray
                    del self.indepvararray
                    self.coordarray = Points.N.array([[0.0]]*max(dict(value)))
                    self.indepvararray = Points.N.array([0.0])
                    self.data.update({"NTST": 1, "NCOL": 0})
                    del self.data["Active ICP"]
                    del self.data["rldot"]
                    del self.data["udotps"]
                for k,v in value:
                    self.coordarray[k-1][0] = v
                return
        try:
            Points.Pointset.__setitem__(self,key,value)
        except (TypeError, ValueError, KeyError, IndexError):
            self.PAR[key] = value

    def __getitem__(self,key):
        big_data_keys = ["data","Active ICP","rldot","p","udotps"]
        if (isinstance(key,str) and key not in self.coordnames and
            key != self.indepvarname and key not in self.__parnames):
            shortkey = self.long_data_keys.get(key,key)
            if shortkey in big_data_keys and not self.__fullyParsed:
                self.__readAll()
            if shortkey in self.data_keys:
                if shortkey == "TY":
                    return parseB.type_translation(
                        self.data["TY number"])["short name"]
                return self.data[shortkey]
            if shortkey == "p":
                return self.PAR
            if shortkey == "data":
                return self
        if isinstance(key,str) and hasattr(self,"b") and key in self.b:
            if not self.__fullyParsed or not key in self.PAR:
                if key not in self.coordnames:
                    return self.b[key]
        if not self.__fullyParsed and self.__start_of_header is not None:
            self.__readAll()
        if isinstance(key, str):
            try:
                return Points.Pointset.__getitem__(self,key)
            except:
                try:
                    return self.PAR[key]
                except:
                    return self.data[key]
        ret = Points.Pointset.__getitem__(self,key)
        if not isinstance(key, int):
            return ret
        return SLPoint(ret, self, key)

    def __call__(self, p=None, coords=None):
        if p is None:
            return(str(self))
        return Points.Pointset.__call__(self, p, coords)

    def __copy__(self):
        return self.__class__(self)

    def copy(self):
        return self.__copy__()

    def __contains__(self,key):
        return (key == "data" or key in self.long_data_keys or
                key in self.__parnames or
                (not self.__fullyParsed and key in self.data_keys) or
                (self.__fullyParsed and key in self.data) or
                (hasattr(self,"b") and key in self.b) or
                Points.Pointset.has_key(self,key))

    def has_key(self, key):
        return self.__contains__(key)

    def get(self, key, failobj=None):
        if key in self:
            return self[key]
        return failobj

    def type(self):
        return parseB.type_translation(self["Type number"])["long name"]

    def load(self,**kw):
        """Load solution with the given AUTO constants.
        Returns a shallow copy with a copied set of updated constants
        """
        if "equation" in kw:
            kw["e"] = kw["equation"][14:]
            del kw["equation"]
        if self["LAB"] != 0 and self["LAB"] != self.c.get("IRS"):
            kw["IRS"] = self["LAB"] 
        return AUTOSolution(self,**kw)

    def run(self,**kw):
        """Run AUTO.

        Run AUTO from the solution with the given AUTO constants.
        Returns a bifurcation diagram of the result.
        """
        c = self.c
        if kw != {}:
            c = parseC.parseC(c, **kw)
        if c.get("e") in ["", None]:
            raise AUTOExceptions.AUTORuntimeError(
                "The equation file argument is missing.")
        runner = runAUTO.runAUTO(equation="EQUATION_NAME=%s"%c["e"],
                                 solution=self,
                                 constants=c,
                                 homcont=c.get("homcont"),
                                 auto_dir=c.get("auto_dir"))
        return runner.run()

    def readAllFilename(self,filename):
        inputfile = AUTOutil.openFilename(filename,"rb")
        self.readAll(inputfile)
        inputfile.close()

    def readFilename(self,filename):
        inputfile = AUTOutil.openFilename(filename,"rb")
        self.read(inputfile)
        if self.__input is None:
            inputfile.close()
        # else don't close the input file but garbage collect

    def writeFilename(self,filename,mlab=False):
        output = open(filename,"wb")
        self.write(output,mlab)
        output.flush()
        output.close()

    def writeRawFilename(self,filename):
        output = open(filename,"w")
        self.writeRaw(output)
        output.flush()
        output.close()
        
    def toArray(self):
        return [
            [vector["t"]] + [point for point in vector["u"]]
            for vector in self["data"]]

    def writeRaw(self,output):
        data = self.toArray()
        output.write("\n".join(["".join(["%24.15E"%v for v in d]) for d in data])+"\n")
            
    def read(self,input=None,prev=None):
        if input is None:
            # read data into memory
            if self.__input is not None:
                input = self.__input
                input.seek(self.__start_of_data)
                self.__data = input.read(self.__end - self.__start_of_data)
                self.__input = None
            return
        # for fort.8 we need to read into self.__data; otherwise load the
        # data on demand from disk when we really need it
        self.__input = None
        if os.path.basename(input.name) != 'fort.8':
            self.__input = input
        if prev is None:
            self.__start_of_header = 0
        else:
            self.__start_of_header = prev._getEnd()
        self.__readHeader(input)
        end = None
        if not prev is None and self.__equalSize(prev):
            # guess the end from the previous solution
            end = input.tell() + prev._getEnd() - prev._getStartOfData()
            # See if the guess for the solution end is correct
            input.seek(end)
            data = input.readline()
            data = data.split()
            # This is where we detect the end of the file
            if len(data) == 0:
                data = input.read(1)
            if len(data) != 0:
                try:
                    # Check length of line...
                    if len(data) != 12 and len(data) != 16:
                        raise IncorrectHeaderLength
                    # and the fact they are all integers
                    map(int,data)
                    # If it passes both these tests we say it is a header line
                    # and we can read quickly
                except:
                    # otherwise the guessed end is not valid
                    end = None
        if end is None:
            # We skip the correct number of lines in the entry to
            # determine its end.
            slist = []
            input.seek(self.__start_of_data)
            for i in range(self.__numLinesPerEntry):
                slist.append(input.readline())
            if self.__input is None:
                self.__data = "".encode("ascii").join(slist)
            end = input.tell()
        elif self.__input is None:
            input.seek(self.__start_of_data)
            self.__data = input.read(end - self.__start_of_data)
        else:
            input.seek(end)
        self.__end = end
    
    def readAll(self,input,start=None,end=None):
        self.read(input,start,end)
        self.__readAll()

    def _setEnd(self,end):
        self.__end = end

    def _getEnd(self):
        return self.__end

    def _getStartOfData(self):
        return self.__start_of_data

    def __equalSize(self,other):
        return (
            self.__numEntriesPerBlock == other.__numEntriesPerBlock and
            self.__numFreeParameters == other.__numFreeParameters and
            self.__numChangingParameters == other.__numChangingParameters and
            self.__numSValues == other.__numSValues)

    def __readHeader(self,inputfile):
        inputfile.seek(self.__start_of_header)
        line = inputfile.readline()
        if not line: raise PrematureEndofData
        data = line.split()
        try:
            self.indepvarname = 't'
            self.__numEntriesPerBlock = int(data[7])
            ndim = self.__numEntriesPerBlock-1
            if ndim < len(self.coordnames):
                self.coordnames = self.coordnames[:ndim]
            for i in range(len(self.coordnames),
                           self.__numEntriesPerBlock-1):
                self.coordnames.append("U("+str(i+1)+")")
 
            self["BR"] = int(data[0])
            self["PT"] = int(data[1])
            self["TY number"] = int(data[2])
            self["LAB"] = int(data[3])
            self.__numChangingParameters = int(data[4])
            self["ISW"] = int(data[5])
            self.__numSValues = int(data[6])
            self.__numLinesPerEntry = int(data[8])
            self["NTST"] = int(data[9])
            self["NCOL"] = int(data[10])
            self["NPARI"] = 0
            self["NDIM"] = 0
            self["IPS"] = None
            self["IPRIV"] = 0
            if len(data)>=12:
                # This is the case for AUTO97 and beyond
                self.__numFreeParameters = int(data[11])
                if len(data)>=16:
                    self["NPARI"] = int(data[12])
                    self["NDIM"] = int(data[13])
                    self["IPS"] = int(data[14])
                    self["IPRIV"] = int(data[15])
            else:
                # This is the case for AUTO94 and before
                self.__numFreeParameters = NPAR
        except IndexError:
            raise PrematureEndofData
        self.__start_of_data = inputfile.tell()
        
    def __readAll(self):
        if not Points.numpyimported:
            Points.importnumpy()        
        fromstring = Points.fromstring
        N = Points.N
        self.__fullyParsed = True
        n = self.__numEntriesPerBlock
        total = n * self.__numSValues + self.__numFreeParameters
        if self["NTST"] != 0:
            total = (total + 2 * self.__numChangingParameters +
                     (n-1) * self.__numSValues)
        solution = []
        j = 0
        nrows = self.__numSValues
        if self.__input is None:
            sdata = self.__data
            del self.__data
        else:
            input = self.__input
            input.seek(self.__start_of_data)
            sdata = input.read(self.__end - self.__start_of_data)
            self.__input = None

        if hasattr(N,"transpose"):
            if fromstring:
                fdata = []
                if sdata.find("D") == -1:
                    fdata = fromstring(sdata, dtype=float, sep=' ')
                if fdata == [] or len(fdata) != total:
                    fdata = N.array(map(parseB.AUTOatof,
                                        sdata.split()), 'd')
                else:
                    #make sure the last element is correct
                    #(fromstring may not do this correctly for a
                    #string like -2.05071-106)
                    fdata[-1] = parseB.AUTOatof(
                        sdata[sdata.rfind(" ")+1:].strip())
            else:
                data = sdata.split()
                try:
                    fdata = N.array(map(float, data), 'd')
                except ValueError:
                    fdata = N.array(map(parseB.AUTOatof, data), 'd')
            if total != len(fdata):
                raise PrematureEndofData
            ups = N.reshape(fdata[:n * nrows],(nrows,n))
            self.indepvararray = ups[:,0]
            self.coordarray = N.transpose(ups[:,1:])
        else: #no numpy
            data = sdata.split()
            try:
                fdata = map(float, data)
            except ValueError:
                fdata = map(parseB.AUTOatof, data)
            if not isinstance(fdata,list): # Python 3
                try:
                    fdata = list(fdata)
                except ValueError:
                    fdata = list(map(parseB.AUTOatof, data))
            if total != len(fdata):
                raise PrematureEndofData
            self.coordarray = []
            try:
                self.indepvararray = N.array(fdata[:n*nrows:n])
                for i in range(1,n):
                    self.coordarray.append(N.array(fdata[i:n*nrows:n]))
            except TypeError:
                self.indepvararray = N.array([fdata[k] for k in xrange(0,n*nrows,n)])
                for i in range(1,n):
                    self.coordarray.append(N.array([fdata[k] for k in xrange(i,n*nrows,n)]))
        del sdata
        j = j + n * nrows

        # I am using the value of NTST to test to see if it is an algebraic or
        # ODE problem.
        if self["NTST"] != 0:
            nfpr = self.__numChangingParameters
            self["Active ICP"] = list(map(int,fdata[j:j+nfpr]))
            j = j + nfpr
            self["rldot"] = fdata[j:j+nfpr]
            j = j + nfpr
            n = n - 1
            if hasattr(N,"transpose"):
                self["udotps"] = N.transpose(
                     N.reshape(fdata[j:j+n * self.__numSValues],(-1,n)))
            else:
                self["udotps"] = []
                try:
                    for i in range(n):
                        self["udotps"].append(N.array(fdata[i:n*nrows:n]))
                except TypeError:
                    for i in range(n):
                        self["udotps"].append(N.array([fdata[k] for k in xrange(i,n*nrows,n)]))
            udotnames = []
            if self["NTST"] > 0:
                for i in range(self.__numEntriesPerBlock-1):
                    udotnames.append("UDOT("+str(i+1)+")")
            self["udotps"] = Points.Pointset({
                "coordarray": self["udotps"],
                "coordnames": udotnames,
                "name": self.name})
            self["udotps"]._dims = None
            j = j + n * nrows

        self.PAR = AUTOParameters(coordnames=self.__parnames,
                               coordarray=fdata[j:j+self.__numFreeParameters])
        Points.Pointset.__init__(self,{
                "indepvararray": self.indepvararray,
                "indepvarname": self.indepvarname,
                "coordarray": self.coordarray,
                "coordnames": self.coordnames,
                "name": self.name})

    def __getattr__(self,attr):
        if self.__start_of_header is None:
            raise AUTOExceptions.AUTORuntimeError("Solution without data.")
        if not self.__fullyParsed and attr != "__del__":
            self.__readAll()
        return super(AUTOSolution, self).__getattribute__(attr)

    def write(self,output,mlab=False):
        try:
            "".encode("ascii") + ""
            def write_enc(s):
                #write encoded
                output.write(s)
        except TypeError: #Python 3.0
            def write_enc(s):
                #write encoded
                output.write(s.encode("ascii"))

        if self.__fullyParsed:
            ndim = len(self.coordarray)
            npar = len(self["Parameters"])
            ntpl = len(self)
        else:
            if self.__start_of_header is None:
                return
            ndim = self.__numEntriesPerBlock-1
            npar = self.__numFreeParameters
            ntpl = self.__numSValues

        if self["NTST"] != 0:
            if self.__fullyParsed:
                nfpr = len(self.get("Active ICP",[0]))
            else:
                nfpr = self.__numChangingParameters
            nrd = 2 + ndim//7 + (ndim-1)//7
            nrowpr = (nrd * (self["NCOL"] * self["NTST"] + 1) +
                      (nfpr-1)//7+1 + (npar-1)//7+1 + (nfpr-1)//20+1)
        else:
            nrowpr = ndim//7+1 + (npar-1)//7+1
            nfpr = self.__numChangingParameters
            
        line = "%6d%6d%6d%6d%6d%6d%8d%6d%8d%5d%5d%5d" % (self["BR"],
                                                         self["PT"],
                                                         self["TY number"],
                                                         self["LAB"],
                                                         nfpr,
                                                         self["ISW"],
                                                         ntpl,
                                                         ndim+1,
                                                         nrowpr,
                                                         self["NTST"],
                                                         self["NCOL"],
                                                         npar
                                                         )
        if self["IPS"] is not None:
            line += "%5d%5d%5d%5d" % (self["NPARI"],self["NDIM"],self["IPS"],
                                      self["IPRIV"])
        write_enc(line+os.linesep)
        # If the file isn't already parsed, and we happen to know the position of
        # the end of the solution we can just copy from the input file into the
        # output file or copy from the raw data for ./fort.8.
        if not self.__fullyParsed and self.__end is not None:
            if self.__input is None:
                output.write(self.__data)
            else:
                input = self.__input
                input.seek(self.__start_of_data)
                output.write(input.read(self.__end - self.__start_of_data))
        # Otherwise we do a normal write.  NOTE: if the solution isn't already
        # parsed it will get parsed here.
        else:
            slist = []
            for i in range(len(self.indepvararray)):
                slist.append("    "+"%19.10E" % (self.indepvararray[i]))
                for j in range(1,len(self.coordarray)+1):
                    if j%7==0:
                        slist.append(os.linesep+"    ")
                    slist.append("%19.10E" % (self.coordarray[j-1][i]))
                slist.append(os.linesep)
            write_enc("".join(slist))
            # I am using the value of NTST to test to see if it is an algebraic or
            # ODE problem.
            if self["NTST"] != 0:
                j = 0
                for parameter in self.get("Active ICP",[0]):
                    write_enc("%5d" % (parameter))
                    j = j + 1
                    if j%20==0:
                        write_enc(os.linesep)
                if j%20!=0:
                    write_enc(os.linesep)

                line = "    "
                i = 0
                for vi in self.get("rldot",[1.0]):
                    num = "%19.10E" % (vi)
                    if i != 0 and i%7==0:
                        line = line + os.linesep + "    "
                    line = line + num
                    i = i + 1
                write_enc(line+os.linesep)

                # write UDOTPS
                slist = []
                if "udotps" in self.data:
                    c = self["udotps"].coordarray
                    l = len(c)
                else:
                    l = 0
                for i in range(len(self.indepvararray)):
                    slist.append("    ")
                    for j in range(len(self.coordarray)):
                        if j!=0 and j%7==0:
                            slist.append(os.linesep+"    ")
                        if j<l:
                            slist.append("%19.10E" %c[j][i])
                        else:
                            slist.append("%19.10E" %0)
                    slist.append(os.linesep)
                write_enc("".join(slist))

            line = "    "
            j = 0
            for parameter in self.PAR.toarray():
                num = "%19.10E" % (parameter)
                line = line + num 
                j = j + 1
                if j%7==0:
                    write_enc(line+os.linesep)
                    line = "    "
            if j%7!=0:
                write_enc(line+os.linesep)
        if mlab and (self._mbr > 0 or self._mlab > 0) and not (
            self._mbr == self["BR"] and self._mlab == self["LAB"]):
            # header for empty solution so that AUTO can pickup the maximal
            # label and branch numbers.
            if self["IPS"] is not None:
                write_enc("%6d%6d%6d%6d%6d%6d%8d%6d%8d%5d%5d%5d%5d%5d%5d%5d%s"%
                      ((self._mbr, 0, 0, self._mlab) + 12*(0,) + (os.linesep,)))
            else:
                write_enc("%6d%6d%6d%6d%6d%6d%8d%6d%8d%5d%5d%5d%s"%
                      ((self._mbr, 0, 0, self._mlab) + 8*(0,) + (os.linesep,)))
        output.flush()

def pointtest(a,b):
    keys = ['Type number', 'Type name', 'Parameter NULL vector',
            'Free Parameters', 'Branch number',
            'data', 'NCOL', 'Label', 'ISW', 'NTST',
            'Point number', 'Parameters']

    # make sure the solutions are fully parsed...
    scratch=a['Parameters']
    scratch=b['Parameters']
    for key in keys:
        if key not in a:
            raise AUTOExceptions.AUTORegressionError("No %s label"%(key,))
    if len(a["data"]) != len(b["data"]):
        raise AUTOExceptions.AUTORegressionError("Data sections have different lengths")


def test():
    print("Testing reading from a filename")
    foo = parseS()
    foo.readFilename("test_data/fort.8")    
    if len(foo) != 5:
        raise AUTOExceptions.AUTORegressionError("File length incorrect")
    pointtest(foo.getIndex(0),foo.getIndex(3))

    print("Testing reading from a stream")
    foo = parseS()
    fp = open("test_data/fort.8","rb")
    foo.read(fp)    
    if len(foo) != 5:
        raise AUTOExceptions.AUTORegressionError("File length incorrect")
    pointtest(foo.getIndex(0),foo.getIndex(3))

    
    
    print("parseS passed all tests")

if __name__ == '__main__' :
    test()









