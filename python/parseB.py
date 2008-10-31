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

import string
import os
import sys
import AUTOExceptions
import AUTOutil
import UserList
import types
import parseC
import Points

type_translation_dict = {
       0: {"long name" : "No Label","short name" : "No Label"},
       1: {"long name" : "Branch point (algebraic problem)","short name" : "BP"},
       2: {"long name" : "Fold (algebraic problem)","short name" : "LP"},
       3: {"long name" : "Hopf bifurcation (algebraic problem)","short name" : "HB"},
       4: {"long name" : "Regular point (every NPR steps)","short name" : "RG"},
      -4: {"long name" : "User requested point","short name" : "UZ"},
       5: {"long name" : "Fold (ODE)","short name" : "LP"},
       6: {"long name" : "Bifurcation point (ODE)","short name" : "BP"},
       7: {"long name" : "Period doubling bifurcation (ODE)","short name" : "PD"},
       8: {"long name" : "Bifurcation to invarient torus (ODE)","short name" : "TR"},
       9: {"long name" : "Normal begin or end","short name" : "EP"},
      -9: {"long name" : "Abnormal termination","short name" : "MX"}}

all_point_types = ["No Label","BP","LP","HB","RG","UZ","PD","TR","EP","MX"]

# A little dictionary to transform types to human readable strings
def type_translation(type):
    if type>=0:
        type=type%10
    else:
        type=-((-type)%10)
    if type in type_translation_dict.keys():
        return type_translation_dict[type]
    else:
        return {"long name" : "Unknown type",
                "short name" : "Unknown type"}
    
# The parseB and AUTOBranch classes parse an AUTO fort.7 file
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
# in the fort.7 file.

# a point within an AUTOBranch
class BDPointData(UserList.UserList):
    def __init__(self, branch=None, index=None):
        self.branch = branch
        self.index = index
    def __getattr__(self, attr):
        if attr == 'data':
            data = []
            for i in range(len(self.branch.coordarray)):
                data.append(self.branch.coordarray[i][self.index])
            return data
        raise AttributeError
    def __setitem__(self, i, item):
        self.branch.coordarray[i][self.index] = item
    def __str__(self):
        return str(self.data)
class BDPoint(Points.Point):
    def __init__(self, p, branch=None, index=None):
        Points.Point.__init__(self, p)
        self.index = index
        self.branch = branch
    
    def has_key(self, key):
        if key in ["TY name","data"] or Points.Point.has_key(self,key):
            return True
        for k,v in self.labels.items():
            if k in all_point_types:
                return v.has_key(key)
        return False

    def __setitem__(self, ixarg, val):
        """Change coordinate array values."""
        if type(ixarg) == type(""):
            for k,v in self.labels.items():
                if k in all_point_types:
                    if v.has_key(ixarg):
                        label[ixarg] = val
                        return
        Points.Point.__setitem__(self, ixarg, val)

    def __getitem__(self, coords):
        if type(coords) == type(""):
            for k,v in self.labels.items():
                if k in all_point_types:
                    if v.has_key(coords):
                        return v[coords]
                    elif coords == "TY name":
                        return k
            if coords == "data":
                return BDPointData(self.branch, self.index)
        return Points.Point.__getitem__(self, coords)

    def __str__(self):
        for k,v in self.labels.items():
            if k in all_point_types:
                ty_name = k
                label = v
                break
        return str({"BR": label["BR"],
                "PT": label["PT"],
                "TY number": label["TY number"],
                "TY name": ty_name,
                "LAB": label["LAB"],
                "data": list(self.coordarray),
                "section": 0,
                "index": label["index"]})

    __repr__ = __str__

# a branch within the parseB class
class AUTOBranch(Points.Pointset):
    def __init__(self,input=None,prevline=None,coordnames=[]):
        self.__fullyParsed = True
        if input is not None:
            self.read(input,prevline)
            self.__fullyParsed = False

    def __getattr__(self,attr):
        if self.__fullyParsed:
            raise AttributeError
        self.__parse()
        return getattr(self,attr)

    def _gettypelabel(self,idx):
        for k,v in self.labels[idx].items():
            if k in all_point_types:
                return k,v

    def __parse(self):
        if self.__fullyParsed:
            return
        global N
        if not Points.numpyimported:
            Points.importnumpy()
        self.__fullyParsed = True
        fromstring = Points.fromstring
        N = Points.N
        datalist = self.__datalist
        del self.__datalist
        line0 = string.split(datalist[0])
        self.BR = int(line0[0])
        ncolumns = len(line0)
        nrows = len(datalist)
        datalist = string.join(datalist,"")
        if fromstring: #numpy
            data = []
            if string.find(datalist, "D") == -1:
                data = fromstring(datalist, dtype=float, sep=' ')
            if len(data) != nrows * ncolumns:
                data = N.array(map(AUTOatof,datalist.split()), 'd')
            coordarray = self.__parsenumpy(ncolumns,data)
        else: #numarray, Numeric, array
            datalist = string.split(datalist)
            try:
                data = map(float, datalist)
            except:
                data = map(AUTOatof, datalist)
            if hasattr(N,"transpose"):
                data = N.array(data,'d')
                coordarray = self.__parsenumpy(ncolumns,data)
            else:
                coordarray = self.__parsearray(ncolumns,data)
        Points.Pointset.__init__(self,{
            "coordarray": coordarray,
            "coordnames": self.coordnames,
            "labels": self.labels,
            })

    def __parsenumpy(self,ncolumns,data):
        global N
        data.shape = (-1,ncolumns)
        coordarray = N.transpose(data[:,4:]).copy()
        points = data[:,1]
        # self.stability gives a list of point numbers where the stability
        # changes: the end point of each part is stored
        stab = N.concatenate((N.nonzero(N.less(points[:-1]*points[1:],0)),
                              [len(points)-1]))
        points = N.less(N.take(points,stab),0)
        stab = stab + 1
        self.stability = N.where(points,-stab,stab)
        return coordarray

    def __parsearray(self,ncolumns,data):
        global N
        # for those without numpy...
        coordarray = []
        try:
            for i in range(4,ncolumns):
                coordarray.append(N.array(data[i::ncolumns]),'d')
        except TypeError:
            for i in range(4,ncolumns):
                coordarray.append(N.array(map(lambda j, d=data: 
                                            d[j], xrange(i,len(data),ncolumns)),
                               'd'))
        self.stability = []
        prevpt = data[1]
        stab = []
        for j in xrange(1,len(data),ncolumns):
            pt = int(data[j])
            if pt * prevpt < 0:
                p = j/ncolumns
                if prevpt < 0:
                    p = -p
                self.stability.append(p)
            prevpt = pt
        p = len(data)/ncolumns
        if pt < 0:
            p = -p
        self.stability.append(p)
        return coordarray

    def __str__(self):
        return self.summary()

    def __getitem__(self,index):
        return self.getIndex(index)

    def __call__(self,label=None):
        return self.getLabel(label)

    def __len__(self):
        if not self.__fullyParsed:
            return len(self.__datalist)
        if self.coordarray == []:
            return 0
        return Points.Pointset.__len__(self)

    # Removes solutions with the given labels or type names
    def deleteLabel(self,label=None,keepTY=0,keep=0):
        if label == None:
            label=['BP','LP','HB','PD','TR','EP','MX']
        if type(label) != types.ListType:
            label = [label]
        for idx in self.labels.getIndices():
            ty_name,v = self._gettypelabel(idx)
            if ((not keep and (v["LAB"] in label or ty_name in label)) or
               (keep and not v["LAB"] in label and not ty_name in label)):
                v["LAB"] = 0
                if not keepTY:
                    v["TY number"] = 0
                if not self.__fullyParsed:
                    self.__patchline(self.__datalist,idx,3,0)
                    if not keepTY:
                        self.__patchline(self.__datalist,idx,2,0)
                if v["TY number"] == 0:
                    self.labels.remove(idx)
            
    # Relabels the first solution with the given label
    def relabel(self,old_label,new_label):
        labels = self.labels
        if type(old_label)  == types.IntType:
            old_label = [old_label]
            new_label = [new_label]
        for j in range(len(old_label)):
            for index in self.labels.getIndices():
                v = self._gettypelabel(index)[1]
                if v["LAB"] == old_label[j]:
                    v["LAB"] = new_label[j]
                    if not self.__fullyParsed:
                        self.__patchline(self.__datalist,index,3,new_label[j])

    # Make all labels in the file unique and sequential
    def uniquelyLabel(self,label=1):
        for index in self.labels.getIndices():
            v = self._gettypelabel(index)[1]
            if v["LAB"] != 0:
                v["LAB"] = label
                if not self.__fullyParsed:
                    self.__patchline(self.__datalist,index,3,label)
                label = label + 1

    # Given a label, return the correct solution
    def getLabel(self,label):
        if label is None:
            return self
        if type(label) == types.IntType:
            for k in self.labels.getIndices():
                v = self._gettypelabel(k)[1]
                if v["LAB"] == label:
                    return self.getIndex(k)
            raise KeyError("Label %s not found"%label)
        if type(label) == types.StringType:
            for k,v in self.labels.sortByIndex():
                if label in v.keys():
                    return self.getIndex(k)
            raise KeyError("Label %s not found"%label)
        labels = {}
        for k,val in self.labels.sortByIndex():
            ty_name,v = self._gettypelabel(k)
            if v["LAB"] in label or ty_name in label:
                labels[k] = val
        new = self.__class__()
        new.BR = self.BR
        new.headerlist = self.headerlist
        new.headernames = self.headernames
        new.labels = labels
        new.stability = self.stability
        if labels == {}:
            return new
        Points.Pointset.__init__(new,{
                "coordarray": self.coordarray,
                "coordnames": self.coordnames,
                "labels": labels})
        return new

    # Return a parseB style line item; if given a string, return the
    # relevant column
    def getIndex(self,index):
        ret = Points.Pointset.__getitem__(self,index)
        if (not isinstance(ret, Points.Point) or
            isinstance(ret, Points.Pointset)):
            return ret
        label = {}
        for k,v in ret.labels.items():
            if k in all_point_types:
                label = v
                break
        if label != {}:
            label["index"] = index
            label["BR"] = self.BR
            label["section"] = 0
        else:
            pt = index+1
            for p in self.stability:
                if abs(p) >= pt:
                    if p < 0:
                        pt = -pt
                    break
            if pt < 0:
                pt = -((-pt-1) % 9999) - 1
            else:
                pt = ((pt-1) % 9999) + 1
            ret.labels["No Label"] = {"BR": self.BR, "PT": pt, "TY number": 0,
                                      "LAB": 0, "index": index, "section": 0}
        return BDPoint({'coordarray': ret.coordarray,
                        'coordnames': ret.coordnames,
                        'labels': ret.labels},self,index)

    # Get all the labels from the solution
    def getLabels(self):
        labels = []
        for index in self.labels.getIndices():
            x = self._gettypelabel(index)[1]
            if x["LAB"] != 0:
                labels.append(x["LAB"])
        return labels

    def writeRawFilename(self,filename):
	output = open(filename,"w")
	self.writeRaw(output)
        output.flush()
	output.close()
        
    def toArray(self):
        array = []
        data = self.coordarray
        for i in range(len(data[0])):
            row = []
            for j in range(len(data)):
                row.append(data[j][i])
            array.append(row)
        return array

    def writeRaw(self,output):
        data = self.coordarray
        for i in range(len(data[0])):
            for j in range(len(data)):
                output.write(str(data[j][i])+" ")
            output.write("\n")
                
    def write(self,output,columnlen=19):
        if columnlen == 19 and not self.__fullyParsed:
            output.write(string.join(self.headerlist,""))
            output.write(string.join(self.__datalist,""))
            return
        format = "%"+str(-columnlen)+"s"
        if self.headerlist != []:
            for l in self.headerlist:
                if string.find(l," PT ") == -1:
                    output.write(l)
        if self.headernames != []:
            output_line = ["   0    PT  TY  LAB "]
            for name in self.headernames:
                output_line.append(format%name)
            output.write(string.join(output_line,"")+'\n')
        br = self.BR
        data = self.coordarray
        istab = 0
        format = "%"+str(columnlen)+"."+str(columnlen-9)+"E"
        for i in range(len(data[0])):
            pt = i+1
            if self.stability[istab] < 0:
                pt = -pt
            tynumber = 0
            lab = 0
            if i in self.labels.by_index.keys():
                for k,label in self.labels[i].items():
                    if k in all_point_types:
                        pt = label["PT"]
                        tynumber = label["TY number"]
                        lab = label["LAB"]
                        break
            if pt == self.stability[istab]:
                istab = istab + 1
            if pt < 0:
                pt = -((-pt-1) % 9999) - 1
            else:
                pt = ((pt-1) % 9999) + 1
            output_line = "%4d%6d%4d%5d"%(br,pt,tynumber,lab)
            for j in range(len(data)):
                output_line = output_line + format%data[j][i]
            output.write(output_line+"\n")

    def writeShort(self):
        self.write(sys.stdout,columnlen=14)

    def summary(self):
        slist = []
        data = self.coordarray
        output_line = ["\n  BR    PT  TY  LAB "]
        if self.headernames != []:
            for name in self.headernames:
                output_line.append("%-14s"%name)
        slist.append(string.join(output_line,""))
        for index,l in self.labels.sortByIndex():
            label = {}
            for k,v in l.items():
                if k in all_point_types:
                    label = v
                    break
            ty_number = label["TY number"]
            if ty_number == 0:
                continue
            ty_name = type_translation(ty_number)["short name"]
            if ty_name=='RG':
                ty_name = '  '
            output_line = "%4d%6d%4s%5d"%(abs(self.BR),abs(label["PT"]),
                                          ty_name,label["LAB"])
            for i in range(len(data)):
                output_line = output_line + "%14.5E"%data[i][index]
            slist.append(output_line)
        return string.join(slist,"\n")

    def writeScreen(self):
        sys.stdout.write(self.summary())

    def writeFilename(self,filename,append=False):
        if append:
            output = open(filename,"a")
        else:
            output = open(filename,"w")
	self.write(output)
	output.close()

    def __patchline(self,datalist,lineno,column,new):
        #patch column of line with new integer value
        newsp = 0
        line = datalist[lineno]
        l = len(line)
        for i in range(column+1):
            oldsp = newsp
            start = l - len(string.lstrip(line[oldsp:]))
            newsp = string.find(line,' ',start)
        datalist[lineno] = (line[:oldsp]+
                            '%'+str(newsp-oldsp)+'d'+line[newsp:])%new

    def __checknorotate(self,datalist):
        # Sometimes the point numbers rotate, like
        # 9996, 9997, 9998, 9999, 1, 2, ...
        # -9996, -9997, 1, 0, -1, -2, ... (an AUTO bug)
        # do not define a new branch if that happens
        prevpt = int(string.split(datalist[-2],None,2)[1])
        if prevpt not in [9999,-9999,9997,-9997,0]:
            return True
        # do some corrections
        if prevpt in [-9997,9997]:
            self.__patchline(datalist,-1,1,-9998)
        elif prevpt == 0:
            self.__patchline(datalist,-2,1,-9999)
        return False

    def read(self,inputfile,prevline=None):
        # We now go through the file and read the branches.
        # read the branch header
        # A section is defined as a part of a fort.7
        # file between "headers", i.e. those parts
        # of the fort.7 file which start with a 0
        # and contain information about the branch
        # FIXME:  I am not sure of this is the correct
        # fix to having multiple sections of a fort.7
        # file with the same branch number.  What it comes
        # dowm to is keeping the fort.7 and fort.8 files
        # in sync.  I.e. I could make sure that
        # this branch numbers are unique here, but then
        # the fort.7 file will not match the fort.8 file.
        # Another way for a section to start is with a point number
        # equal to 1.
        self._lastline = None
        split = string.split
        if hasattr(str,"split"):
            split = str.split
        if prevline:
            line = prevline
        else:
            if not hasattr(inputfile,"next"):
                inputfile = AUTOutil.myreadlines(inputfile)
            line = inputfile.next()
        headerlist = []
        columns = split(line,None,2)
        if columns[0] == '0':
            headerlist.append(line)
            for line in inputfile:
                columns = split(line,None,2)
                if columns[0] != '0':
                    self._lastline = line
                    break
                headerlist.append(line)
        datalist = []
        labels = {}
        if columns[0] != '0':
            self._lastline = None
            datalist = [line]
            if columns[2][0] != '0': #type
                columns = split(line,None,4)
                pt = int(columns[1])
                ty = int(columns[2])
                lab = int(columns[3])
                key = type_translation(ty)["short name"]
                labels[len(datalist)-1] = {key: {"LAB":lab,"TY number":ty,
                                                 "PT":pt}}
            for line in inputfile:
                datalist.append(line)
                columns = split(line,None,2)
                if (columns[0] == '0' or
                    ((columns[1] == '-1' or columns[1] == '1') and
                     self.__checknorotate(datalist))):
                    self._lastline = datalist.pop()
                    break
                if columns[2][0] != '0': #type
                    columns = split(datalist[-1],None,4)
                    pt = int(columns[1])
                    ty = int(columns[2])
                    lab = int(columns[3])
                    key = type_translation(ty)["short name"]
                    labels[len(datalist)-1] = {key: {"LAB":lab,"TY number":ty,
                                                     "PT":pt}}
        self.labels = Points.PointInfo(labels)
        self.__datalist = datalist
        self.c = self.parseHeader(headerlist)

    def readFilename(self,filename):
        try:
            inputfile = open(filename,"r")
        except IOError:
            import gzip
            inputfile = gzip.open(filename+".gz","r")
	self.read(inputfile)
	inputfile.close()

    def parseHeader(self,headerlist):
        self.headerlist = headerlist
        split = string.split
        if hasattr(str,"split"):
            split = str.split
        line = ""
        if headerlist != []:
            line = headerlist[-1]
        ncolumns = len(split(self.__datalist[0])) - 4
        self.headernames = []
        if string.find(line, " PT ") != -1:
            self.coordnames = []
            linelen = len(self.__datalist[0])
            columnlen = (linelen - 19) / ncolumns
            n = linelen - columnlen * ncolumns
            for i in range(ncolumns):
                self.headernames.append(string.rstrip(line[n:n+columnlen]))
                self.coordnames.append(string.strip(line[n:n+columnlen]))
                n = n + columnlen
        if self.coordnames == []:
            self.coordnames = map(str,range(ncolumns))                
        dict = parseC.parseC()
        i = 0
        words = split(headerlist[0])
        if len(words) < 5:
            return
        for key in ["RL0","RL1","A0","A1"]:
            i = i + 1
            dict[key] = AUTOatof(words[i])
        key = ""
        userspec = False
        for line in headerlist[1:]:
            words = split(line)
            if len(words) < 2:
                break
            if words[-1] == "constants:":
                userspec = True
                continue
            if words[1] in ["User-specified", "Active"]:
                line = string.replace(line,"s:",":")
                for ind in range(2,len(words)):
                    if words[ind] in ["parameter:","parameters:"]:
                        index = ind + 1
                        break
                if words[1][0] == "U":
                    key = "ICP"
                else:
                    key = "Active ICP"
                d = []
                for w in words[index:]:
                    try:
                        w = int(w)
                    except ValueError:
                        if ((w[0] == "'" and w[-1] == "'") or
                            (w[0] == '"' and w[-1] == '"')):
                            w = w[1:-1]
                    d.append(w)
                dict[key] = d
                continue
            dict.parseline(string.join(words[1:]),userspec)
        return dict

class parseBR(UserList.UserList,AUTOBranch):
    def __init__(self,filename=None):
        if type(filename) == types.StringType:
            UserList.UserList.__init__(self)
            self.readFilename(filename)
        else:
            UserList.UserList.__init__(self,filename)

    # Removes solutions with the given labels or type names
    def deleteLabel(self,label=None,keepTY=0,keep=0):
        for d in self.data:
            d.deleteLabel(label,keepTY,keep)
            
    # Relabels the first solution with the given label
    def relabel(self,old_label,new_label):
        for d in self.data:
            d.relabel(old_label,new_label)

    # Make all labels in the file unique and sequential
    def uniquelyLabel(self):
        label = 1
        for d in self.data:
            d.uniquelyLabel(label)
            for idx,val in d.labels.sortByIndex():
                for k,v in val.items():
                    if k in all_point_types and v["TY number"] != 0:
                        label = v["LAB"]
            label = label + 1
            
    # Given a label, return the correct solution
    def getLabel(self,label):
        if type(label) in [types.IntType, types.StringType]:
            i = 0
            section = 0
            for d in self.data:
                l = len(d.coordarray[0])
                try:
                    item = d.getLabel(label)
                    item["index"] = item["index"] + i
                    item["section"] = section
                    return item
                except:
                    pass
                i = i + l
                section = section + 1
            raise KeyError("Label %s not found"%label)
        new = self.__class__()
        new.data = []
        for d in self.data:
            newbranch = d.getLabel(label)
            if newbranch:
                new.data.append(newbranch)
        return new

    # Given an index, return the correct solution
    # Return a parseB style line item
    def getIndex(self,index):
        if type(index) != type(0):
            return self.data[0].getIndex(index)
        section = 0
        i = index
        for d in self.data:
            l = len(d.coordarray[0])
            if i < l:
                item = d.getIndex(i)
                item.labels["section"] = section
                item.labels["index"] = index
                return item
            i = i - l
            section = section + 1
        raise IndexError

    # Get all the labels from the solution
    def getLabels(self):
        labels = []
        for d in self.data:
            labels.extend(d.getLabels())
        return labels

    def toArray(self):
        array = []
        for d in self.data:
            array.extend(d.toArray())
        return array

    def writeRaw(self,output):
        for d in self.data:
            d.writeRaw(output)
            output.write("\n")
                
    def write(self,output):
        for d in self.data:
            d.write(output)

    def writeShort(self):
        for d in self.data:
            d.writeShort()

    def summary(self):
        slist = []
        for branch in self.data:
            slist.append(branch.__str__())
        return string.join(slist,"\n")+"\n"

    def read(self,inputfile):
        # We now go through the file and read the branches.
        prevline = None
        coordnames = []
        if not hasattr(inputfile,"next"):
            inputfile = AUTOutil.myreadlines(inputfile)
        while 1:
            branch = AUTOBranch(inputfile,prevline,coordnames)
            prevline = branch._lastline
            coordnames = branch.coordnames
            self.data.append(branch)
            if prevline is None:
                break

class parseB(AUTOBranch):
    #compatibility class for dg()
    def __init__(self,filename=None):
        self.branches = parseBR(filename)
        if len(self.branches) > 0:
            self.coordnames = self.branches[0].coordnames
        self.deleteLabel = self.branches.deleteLabel
        self.relabel = self.branches.relabel
        self.uniquelyLabel = self.branches.uniquelyLabel
        self.getIndex = self.branches.getIndex
        self.getLabels = self.branches.getLabels
        self.toArray = self.branches.toArray
        self.writeRaw = self.branches.writeRaw
        self.write = self.branches.write
        self.writeShort = self.branches.writeShort
        self.summary = self.branches.summary
    def __len__(self):
        l = 0
        for d in self.branches:
            l = l + len(d)
        return l
    def getLabel(self,label):
        if type(label) in [types.IntType,types.StringType]:
            return self.branches.getLabel(label)
        new = self.__class__()
        new.branches = self.branches.getLabel(label)
        return new
    def read(self,inputfile):
        self.branches.read(inputfile)
        if len(self.branches) > 0:
            self.coordnames = self.branches[0].coordnames

def AUTOatof(input_string):
    #Sometimes AUTO messes up the output.  I.e. it gives an
    #invalid floating point number of the form x.xxxxxxxE
    #instead of x.xxxxxxxE+xx.  Here we assume the exponent
    #is 0 and make it into a real real number :-)
    try:
        value=float(input_string)
    except (ValueError):
        try:
            if input_string[-1] == "E":
                #  This is the case where you have 0.0000000E
                value=float(string.strip(input_string)[0:-1])
            elif input_string[-4] in ["-","+"]:
                #  This is the case where you have x.xxxxxxxxx-yyy
                #  or x.xxxxxxxxx+yyy (standard Fortran but not C)
                value=float(input_string[:-4]+'E'+input_string[-4:])
            elif input_string[-4] == "D":
                #  This is the case where you have x.xxxxxxxxxD+yy
                #  or x.xxxxxxxxxD-yy (standard Fortran but not C)
                value=float(input_string[:-4]+'E'+input_string[-3:])
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
    if not(a.has_key("TY name")):
        raise AUTOExceptions.AUTORegressionError("No TY label")
    if not(a.has_key("TY number")):
        raise AUTOExceptions.AUTORegressionError("No TY label")
    if not(a.has_key("BR")):
        raise AUTOExceptions.AUTORegressionError("No BR label")
    if not(a.has_key("data")):
        raise AUTOExceptions.AUTORegressionError("No data label")
    if not(a.has_key("PT")):
        raise AUTOExceptions.AUTORegressionError("No PT label")
    if not(a.has_key("LAB")):
        raise AUTOExceptions.AUTORegressionError("No LAB label")
    if not(len(a["data"]) == len(b["data"])):
        raise AUTOExceptions.AUTORegressionError("Data sections have different lengths")

def test():
    print "Testing reading from a filename"
    foo = parseB()
    foo.readFilename("test_data/fort.7")    
    if len(foo) != 150:
        raise AUTOExceptions.AUTORegressionError("File length incorrect")
    pointtest(foo.getIndex(0),foo.getIndex(57))

    print "Testing reading from a stream"
    foo = parseB()
    fp = open("test_data/fort.7","r")
    foo.read(fp)
    if len(foo) != 150:
        raise AUTOExceptions.AUTORegressionError("File length incorrect")
    pointtest(foo.getIndex(0),foo.getIndex(57))


    print "Testing label manipulation"
    labels = foo.getLabels()
    foo.relabel(labels[0],57)
    labels = foo.getLabels()
    if labels[0] != 57:
        raise AUTOExceptions.AUTORegressionError("Error in either relabel or getLabel")
    foo.deleteLabel(labels[1])
    new_labels = foo.getLabels()
    if len(labels) != len(new_labels) + 1:
        raise AUTOExceptions.AUTORegressionError("Error in label deletion")
        
    
    print "parseB passed all tests"

if __name__ == '__main__' :
    test()








