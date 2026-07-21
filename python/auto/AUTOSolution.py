#! /usr/bin/env python

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


import os
import sys
import struct
try:
    from UserDict import UserDict
    from UserList import UserList
except ImportError: #Python 3
    from collections import UserDict
    from collections import UserList
try:
    file
except NameError: #Python 3
    from io import IOBase as file
from auto import AUTOExceptions
import copy
from auto import parseB
from auto import parseC
from auto import Points
from auto.AUTOutil import format19_10E3
from auto.parseCommon import AUTOParameters

import gzip


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
            return [point[self.index] for point in self.solution.coordarray]
        raise AttributeError(attr)
    def __setitem__(self, i, item):
        self.solution.coordarray[i,self.index] = item
    def __str__(self):
        return str(self.data)
    def append(self, item):
        self.enlarge(1)
        self.solution.coordarray[-1,self.index] = item
        self.data.append(item)
    def extend(self, other):
        self.enlarge(len(other))
        for i in range(len(other)):
            self.solution.coordarray[-len(other)+i,self.index] = other[i]
    def enlarge(self, ext):
        # enlarges the dimension of coordarray and coordnames
        s = self.solution
        if s._dims is None:
            s._dims = [s.dimension]*len(s)
            s0 = s.coordnames[0]
            s.extend([s0[0:s0.find('(')+1]+str(s.dimension+i+1)+')'
                      for i in range(ext)])
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


class AUTOSolution(UserDict,Points.Pointset):

    data_keys = set(["PT", "BR", "TY number", "TY", "LAB",
                 "ISW", "NTST", "NCOL", "Active ICP", "rldot",
                 "udotps", "NPARI", "NDIM", "IPS", "IPRIV"])

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

    def __init__(self,input=None,index=None,name=None,t=None,**kw):
        if isinstance(input,self.__class__):
            for k,v in input.__dict__.items():
                self.__dict__[k] = v
            UserDict.__init__(self,input)
        else:
            UserDict.__init__(self)
            self.c = None
            self.__input          = None
            self.__fullyParsed    = False
            self._dims            = None
            self._mbr             = 0
            self._mlab            = 0
            self.name = name
            self.data.update({"BR":1, "PT":1, "TY number":9, "LAB":0,
                              "ISW":1, "NTST": 1, "NCOL": 0,
                              "NPARI":0, "NDIM": 0, "IPS": None, "IPRIV":0})
            self.indepvarname = 't'
            self.coordnames = []
            self.__parnames = []

            try:
                from auto.fileS import fileS
            except ImportError:
                fileS = None

            self.fileS = fileS 
            if isinstance(input,(file,gzip.GzipFile)) and self.fileS is not None:
                input = self.fileS(input)
            if self.fileS is not None and isinstance(input,self.fileS):
                self.read(input,index)
            elif input is not None:
                self.__readarray(input,t)
        self.update(**kw)

    def update(self, dct=None, **kw):
        par = None
        if "constants" in kw:
            self.c = kw["constants"]
            del kw["constants"]
            unames = dict(self.c.get("unames") or [])
            self.coordnames = [unames.get(i+1,'U(%d)'%(i+1))
                               for i in range(len(self.coordnames))]
            parlen = len(self.__parnames)
            parnames = dict(self.c.get("parnames") or [])
            if parnames != {}:
                parlen = max(parlen, max(parnames))
            par = dict(self.c.get("PAR") or [])
            for key in par:
                if key not in parnames.values():
                    parlen = max(key, parlen)
            self.__parnames = [parnames.get(i,"PAR(%d)"%i)
                               for i in range(1,parlen+1)]
            if ((self.name is None or os.path.basename(self.name) == 'fort.8')
                and self.c.get("e") is not None):
                self.name = self.c["e"]

        if self.name is None:
            self.name = ''
        if self.__fullyParsed:
            self.makeIxMaps()
            self.PAR = AUTOParameters(coordnames=self.__parnames,
                                      coordarray=Points.array(self.PAR),
                                      name=self.name)
        if dct is not None:
            for k,v in dct.items():
                self.data[k] = v
        for k,v in kw.items():
            self[k] = v

        if par is not None and not self.__nodata():
            self["PAR"] = par
            u = self.c.get("U")
            if u is not None:
                self["U"] = u

    def __nodata(self):
        return self.__input is None and not self.__fullyParsed

    def __getstate__(self):
        # For pickle: read everything
        self.__readAll()
        return Points.Pointset.__getstate__(self)

    def __str__(self):
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
                rep = rep+format19_10E3(self[key])
                keys.remove(key)
        if not self.__nodata():
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
        if not self.__nodata():
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
                                          coordarray=value, name=self.name)
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
                    self.coordarray[k-1,0] = v
                return
        try:
            Points.Pointset.__setitem__(self,key,value)
        except (TypeError, ValueError, KeyError, IndexError):
            if self.__nodata():
                raise AUTOExceptions.AUTORuntimeError("Unknown option: %s"%key)
            self.PAR[key] = value

    def __getitem__(self,key):
        big_data_keys = ["data","Active ICP","rldot","p","udotps"]
        if (isinstance(key,str) and key not in self.coordnames and
            key != self.indepvarname and key not in self.__parnames):
            shortkey = self.long_data_keys.get(key,key)
            if shortkey in big_data_keys:
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
        return (key in ["data","TY"] or key in self.long_data_keys or
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
        constants = kw.get("constants")
        if "constants" in kw:
            del kw["constants"]
        c = parseC.parseC(self.c)
        datakw = {}
        for key in self.data_keys:
            if key in kw and (key not in c or key in ["LAB", "TY"]):
                datakw[key] = kw[key]
                if key not in ["LAB", "TY"]:
                    del kw[key]
        oldirs = c["IRS"]
        c.update(constants, **kw)
        if oldirs is not None and c["IRS"] == 0:
            return AUTOSolution(constants=c, **datakw)
        if self["LAB"] != 0:
            c["IRS"] = self["LAB"]
        return AUTOSolution(self, constants=c, **datakw)


    def readAllFilename(self,filename):
        inputfile = self.fileS(filename)
        self.readAll(inputfile)
        inputfile.close()

    def readFilename(self,filename):
        inputfile = self.fileS(filename)
        self.read(inputfile)
        inputfile.conditionalclose()
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
        s = "%24.15E"*(self.coordarray.shape[0]+1)
        for i in range(self.coordarray.shape[1]):
            output.write(s%((self.indepvararray[i],)+
                            tuple(self.coordarray[:,i]))+"\n")
            
    def read(self, inputfile=None, index=0):
        if self.__fullyParsed:
            return
        if inputfile is None:
            # read data into memory
            self.__input.readstr(self.__index)
            self.__input.conditionalclose()
            return
        if not isinstance(inputfile, self.fileS):
            inputfile = self.fileS(inputfile)
        self.__input = inputfile
        self.__index = index
        self.__readHeader()
    
    def readAll(self, inputfile):
        self.read(inputfile)
        self.__readAll()

    def __readHeader(self):
        header = self.__input.solutions[self.__index]['header']
        self.indepvarname = 't'
        self.__numEntriesPerBlock = header[7]
        ndim = self.__numEntriesPerBlock-1
        if ndim < len(self.coordnames):
            self.coordnames = self.coordnames[:ndim]
        for i in range(len(self.coordnames),
                       self.__numEntriesPerBlock-1):
            self.coordnames.append("U(%d)"%(i+1))

        self.update({"NPARI": 0, "NDIM": 0, "IPS": None, "IPRIV": 0})
        for i, key in enumerate(["BR", "PT", "TY number", "LAB", "",
                                 "ISW", "", "", "", "NTST",
                                 "NCOL", "", "NPARI", "NDIM", "IPS",
                                 "IPRIV"]):
            if key and i < len(header):
                self[key] = header[i]
        self.__numChangingParameters = header[4]
        self.__numSValues = header[6]
        self.__numLinesPerEntry = header[8]
        self.__numFreeParameters = header[11]
        
    def __readAll(self):
        if self.__fullyParsed or self.__nodata():
            return
        if not Points.numpyimported:
            Points.importnumpy()
        fromstring = Points.fromstring
        N = Points.N
        self.__fullyParsed = True
        n = self.__numEntriesPerBlock
        nrows = self.__numSValues
        total = n * nrows + self.__numFreeParameters
        nlinessmall = (((n-1)/7+1) * nrows + (self.__numFreeParameters+6)/7)
        if self["NTST"] != 0 and self.__numLinesPerEntry > nlinessmall:
            total += 2 * self.__numChangingParameters + (n-1) * nrows
        fdata = self.__input.readfloats(self.__index, total)
        ups = N.reshape(fdata[:n * nrows],(nrows,n))
        self.indepvararray = ups[:,0]
        self.coordarray = N.transpose(ups[:,1:])
        j = n * nrows

        # Check if direction info is given
        if self["NTST"] != 0 and self.__numLinesPerEntry > nlinessmall:
            nfpr = self.__numChangingParameters
            self["Active ICP"] = list(map(int,fdata[j:j+nfpr]))
            j = j + nfpr
            self["rldot"] = fdata[j:j+nfpr]
            j = j + nfpr
            n = n - 1
            self["udotps"] = N.transpose(
                N.reshape(fdata[j:j+n * self.__numSValues],(-1,n)))
            udotnames = ["UDOT(%d)"%(i+1) for i in
                         range(self.__numEntriesPerBlock-1)]
            self["udotps"] = Points.Pointset({
                "coordarray": self["udotps"],
                "coordnames": udotnames,
                "name": self.name})
            self["udotps"]._dims = None
            j = j + n * nrows

        self.PAR = fdata[j:j+self.__numFreeParameters]
        Points.Pointset.__init__(self,{
                "indepvararray": self.indepvararray,
                "indepvarname": self.indepvarname,
                "coordarray": self.coordarray,
                "coordnames": self.coordnames,
                "name": self.name})
        self.update()

    def __readarray(self,coordarray,indepvararray=None):
        #init from array
        if not Points.numpyimported:
            Points.importnumpy()        
        N = Points.N
        if not hasattr(coordarray[0],'__len__'):
            # point
            indepvararray = [0.0]
            ncol = 0
            ntst = 1
            coordarray = [[d] for d in coordarray]
            pararray = []
        else:
            # time + solution
            if indepvararray is None:
                indepvararray = coordarray[0]
                coordarray = coordarray[1:]
            ncol = 1
            ntst = len(indepvararray)-1
            t0 = indepvararray[0]
            period = indepvararray[-1] - t0
            if period != 1.0 or t0 != 0.0:
                #scale to [0,1]
                for i in range(len(indepvararray)):
                    indepvararray[i] = (indepvararray[i] - t0)/period
            # set PAR(11) to period
            pararray = 10*[0.0] + [period]
        indepvarname = "t"
        ndim = len(coordarray)
        coordnames = ["U(%d)"%(i+1) for i in range(ndim)]
        Points.Pointset.__init__(self,{"indepvararray": indepvararray,
                                       "indepvarname": indepvarname,
                                       "coordarray": coordarray,
                                       "coordnames": coordnames})
        self.__fullyParsed = True
        self.data.update({"NTST": ntst, "NCOL": ncol, "LAB": 1, "NDIM": ndim})
        self.__numChangingParameters = 1
        self.PAR = pararray

    def __getattr__(self,attr):
        if self.__nodata():
            raise AUTOExceptions.AUTORuntimeError("Solution without data.")
        if not self.__fullyParsed and attr != "__del__":
            self.__readAll()
            return getattr(self,attr)
        raise AttributeError(attr)

    def write(self,output,mlab=False):
        if self.__nodata():
            return
        try:
            "".encode("ascii") + ""
            #write encoded
            def write_enc(s):
                output.write(s)
        except TypeError: #Python 3.0
            if hasattr(output, "encoding"):
                # output is a text stream
                if os.linesep == "\n":
                    def write_enc(s):
                        output.write(s)
                else:
                    def write_enc(s):
                        output.write(s.replace(os.linesep, "\n"))
            else:
                # output is a binary stream (common case)
                def write_enc(s):
                    output.write(s.encode("ascii"))

        if self.__fullyParsed:
            ndim = len(self.coordarray)
            npar = len(self["Parameters"])
            ntpl = len(self)
            nrowpr = (ndim//7+1) * ntpl + (npar+6)//7
            nfpr = self.__numChangingParameters
            if "Active ICP" in self.data:
                nfpr = len(self.get("Active ICP",[0]))
                nrowpr += (nfpr+19)//20 + (nfpr+6)//7 + (ndim+6)//7 * ntpl
        else:
            ndim = self.__numEntriesPerBlock-1
            npar = self.__numFreeParameters
            ntpl = self.__numSValues
            nfpr = self.__numChangingParameters
            nrowpr = self.__numLinesPerEntry

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
        # If the file isn't already parsed, we can just copy from the input
        # file into the output file
        if not self.__fullyParsed:
            inputsolution = self.__input.readstr(self.__index)
            if hasattr(output, "encoding") and hasattr(inputsolution, "decode"):
                inputsolution = inputsolution.decode("ascii")
            output.write(inputsolution)
        # Otherwise we do a normal write.  NOTE: if the solution isn't already
        # parsed it will get parsed here.
        else:
            slist = []
            for i in range(len(self.indepvararray)):
                slist.append("    "+format19_10E3(self.indepvararray[i]))
                for j in range(1,len(self.coordarray)+1):
                    if j%7==0:
                        slist.append(os.linesep+"    ")
                    slist.append(format19_10E3(self.coordarray[j-1,i]))
                slist.append(os.linesep)
            write_enc("".join(slist))
            if "Active ICP" in self.data:
                # Solution contains derivative information.
                j = 0
                for parameter in self["Active ICP"]:
                    write_enc("%5d" % (parameter))
                    j = j + 1
                    if j%20==0:
                        write_enc(os.linesep)
                if j%20!=0:
                    write_enc(os.linesep)

                line = "    "
                i = 0
                for vi in self["rldot"]:
                    num = format19_10E3(vi)
                    if i != 0 and i%7==0:
                        line = line + os.linesep + "    "
                    line = line + num
                    i = i + 1
                write_enc(line+os.linesep)

                # write UDOTPS
                slist = []
                c = self["udotps"].coordarray
                l = len(c)
                for i in range(len(self.indepvararray)):
                    slist.append("    ")
                    for j in range(len(self.coordarray)):
                        if j!=0 and j%7==0:
                            slist.append(os.linesep+"    ")
                        if j<l:
                            slist.append(format19_10E3(c[j,i]))
                        else:
                            slist.append(format19_10E3(0))
                    slist.append(os.linesep)
                write_enc("".join(slist))

            line = "    "
            j = 0
            for parameter in self.PAR.toarray():
                num = format19_10E3(parameter)
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
