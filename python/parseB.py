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
import AUTOExceptions
import AUTOutil
try:
    from UserList import UserList
except ImportError: # Python 3
    from collections import UserList
import parseC
import Points

try:
    next
except NameError: #Python < 3
    def next(it):
        return it.next()

type_translation_dict = {
       0: {"long name" : "No Label","short name" : "No Label"},
       1: {"long name" : "Branch point (algebraic problem)","short name" : "BP"},
      -2: {"long name" : "Cusp (algebraic problem)","short name" : "CP"},
       2: {"long name" : "Fold (algebraic problem)","short name" : "LP"},
       3: {"long name" : "Hopf bifurcation (algebraic problem)","short name" : "HB"},
      -3: {"long name" : "Zero-Hopf bifurcation (algebraic problem)","short name" : "ZH"},
      -1: {"long name" : "Bogdanov-Takens bifurcation (algebraic problem)","short name" : "BT"},
      -32: {"long name" : "Generalized Hopf bifurcation (algebraic problem)","short name" : "GH"},
       4: {"long name" : "Regular point (every NPR steps)","short name" : "RG"},
      -4: {"long name" : "User requested point","short name" : "UZ"},
       5: {"long name" : "Fold (ODE)","short name" : "LP"},
       6: {"long name" : "Bifurcation point (ODE)","short name" : "BP"},
       7: {"long name" : "Period doubling bifurcation (ODE)","short name" : "PD"},
       8: {"long name" : "Bifurcation to invariant torus (ODE)","short name" : "TR"},
      -5: {"long name" : "1:1 Resonance bifurcation (ODE)","short name" : "R1"},
      -6: {"long name" : "1:2 Resonance bifurcation (ODE)","short name" : "R2"},
      -7: {"long name" : "1:3 Resonance bifurcation (ODE)","short name" : "R3"},
      -8: {"long name" : "1:4 Resonance bifurcation (ODE)","short name" : "R4"},
      23: {"long name" : "Fold-torus bifurcation (maps)","short name" :"LTR"},
      77: {"long name" : "Flip-torus bifurcation (maps)","short name" :"PTR"},
      28: {"long name" : "Fold-flip bifurcation (maps)","short name" :"LPD"},
      88: {"long name" : "Torus-torus bifurcation (maps)","short name" :"TTR"},
       9: {"long name" : "Normal begin or end","short name" : "EP"},
      -9: {"long name" : "Abnormal termination","short name" : "MX"}}

all_point_types = ["No Label",
                   "BP","LP","HB","BT","RG","UZ","PD","TR","EP","MX","ZH",
                   "CP","GH","R1","R2","R3","R4","LPD","LTR","PTR","TTR"]

def type_translation(type):
    """A little dictionary to transform types to human readable strings"""
    if type in [28,78]: # LPD
        type=28
    elif type in [23,83]: # LTR
        type=23
    elif type in [77,87]: # PTR
        type=77
    elif type not in [-32,88]:
        if type>=0:
            type=type%10
        else:
            type=-((-type)%10)
    return type_translation_dict.get(type,
                                     {"long name" : "Unknown type",
                                      "short name" : "Unknown type"})
    
def reverse_type_translation(type):
    """A little dictionary to transform human readable strings to types"""
    for k,v in type_translation_dict.items():
        if v["short name"] == type or v["long name"] == type:
            return k
    return type
    
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

class parseBMixin(object):
    def __str__(self):
        return self.summary()

    def __getitem__(self,index):
        return self.getIndex(index)

    def __call__(self,label=None):
        return self.getLabel(label)

    def dsp(self,label=None):
        """Removes solutions with the given labels or type names"""
        return self.deleteLabel(label,copy=1)

    def ksp(self,label=None):
        """Keeps solutions with the given labels or type names"""
        return self.deleteLabel(label,keep=1,copy=1)

    def dlb(self,label=None):
        """Removes solutions with the given labels or type names"""
        return self.deleteLabel(label,keepTY=1,copy=1)

    def klb(self,label=None):
        """Keeps solutions with the given labels or type names"""
        return self.deleteLabel(label,keepTY=1,keep=1,copy=1)

    def readFilename(self,filename):
        inputfile = AUTOutil.openFilename(filename,"r")
        self.read(inputfile)
        inputfile.close()

    def writeFilename(self,filename,append=False):
        if append:
            output = open(filename,"a")
        else:
            output = open(filename,"w")
        self.write(output)
        output.close()

    def writeRawFilename(self,filename):
        output = open(filename,"w")
        self.writeRaw(output)
        output.flush()
        output.close()
        
    def writeScreen(self):
        sys.stdout.write(self.summary())

# a point within an AUTOBranch
class BDPointData(UserList):
    def __init__(self, branch=None, idx=None):
        self.branch = branch
        self.idx = idx
    def __getattr__(self, attr):
        if attr == 'data':
            return [d[self.idx] for d in self.branch.coordarray]
        return super(BDPointData, self).__getattribute__(attr)
    def __setitem__(self, i, item):
        self.branch.coordarray[i][self.idx] = item
    def __str__(self):
        return str(self.data)
class BDPoint(Points.Point):
    def __init__(self, p, branch=None, idx=None, pt=None):
        if isinstance(p['coordarray'], list):
            # not fully parsed
            self.__fullyParsed = False
            self.p = p
            self.coordnames = p['coordnames']
            self.name_map = dict(zip(p['coordnames'],
                                     range(len(p['coordnames']))))
        else:
            self.__fullyParsed = True
            Points.Point.__init__(self, p)
        self.idx = idx
        self.pt = pt
        self.branch = branch
        self.section = 0
        self.index = idx
    
    def __getattr__(self, attr):
        if not self.__fullyParsed:
            p = self.p
            p['coordtype'] = type(p['coordarray'][0])
            del self.name_map, self.p
            Points.Point.__init__(self, p)
            self.__fullyParsed = True
        return super(BDPoint, self).__getattribute__(attr)

    def __contains__(self, key):
        if (key in ["TY","TY name","data","BR","PT","section","index"] or
            Points.Point.has_key(self,key)):
            return True
        for k,v in self.labels.items():
            if k in all_point_types:
                return key in v
        return False

    def has_key(self, key):
        return self.__contains__(key)

    def __setitem__(self, ixarg, val):
        """Change coordinate array values."""
        if type(ixarg) == type(""):
            if ixarg == "BR":
                self.branch["BR"] = val
                return
            elif ixarg == "PT":
                if not self.__fullyParsed:
                    self.pt = val
                # else ignore
                return
            elif ixarg == "section":
                self.section = val
                return
            elif ixarg == "index":
                self.index = val
                return
            for k,v in self.labels.items():
                if "LAB" in v:
                    v[ixarg] = val
                    return
        Points.Point.__setitem__(self, ixarg, val)

    def __getitem__(self, coords):
        if type(coords) == type(""):
            if not self.__fullyParsed:
                try:
                    return self.p['coordarray'][self.name_map[coords]]
                except KeyError:
                    pass
            if coords == "BR":
                return self.branch["BR"]
            elif coords == "PT":
                if self.__fullyParsed:
                    pt = self.idx+1
                    for p in self.branch.stability():
                        if abs(p) >= pt:
                            if p < 0:
                                pt = -pt
                            break
                    if pt < 0:
                        pt = -((-pt-1) % 9999) - 1
                    else:
                        pt = ((pt-1) % 9999) + 1
                else:
                    pt = self.pt
                return pt
            elif coords == "section":
                return self.section
            elif coords == "index":
                return self.index
            for k,v in self.labels.items():
                if k in all_point_types:
                    if coords in v:
                        return v[coords]
                    elif coords in ("TY name", "TY"):
                        return k
            if coords == "data":
                return BDPointData(self.branch, self.idx)
        return Points.Point.__getitem__(self, coords)

    def __str__(self):
        for k,v in self.labels.items():
            if k in all_point_types:
                ty_name = k
                label = v
                break
        return str({"BR": self["BR"],
                "PT": self["PT"],
                "TY number": label["TY number"],
                "TY name": ty_name,
                "LAB": label["LAB"],
                "data": list(self.coordarray),
                "section": self.section,
                "index": self.index})

    __repr__ = __str__

# a branch within the parseB class
class AUTOBranch(parseBMixin, Points.Pointset):
    def __init__(self,input=None,prevline=None,coordnames=[]):
        self.__fullyParsed = True
        if isinstance(input,AUTOBranch):
            for k,v in input.__dict__.items():
                self.__dict__[k] = v
        elif input is not None:
            self.read(input,prevline)
            self.__fullyParsed = False

    def __getattr__(self,attr):
        if self.__fullyParsed or attr == "__del__":
            raise AttributeError
        self.__parse()
        return super(AUTOBranch, self).__getattribute__(attr)

    def _gettypelabel(self,idx):
        for k,v in self.labels[idx].items():
            if "LAB" in v:
                return k,v
        return 'No Label', {}

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
        line0 = datalist[0].split()
        self.BR = int(line0[0])
        ncolumns = len(line0)
        nrows = len(datalist)
        datalist = "".join(datalist)
        if fromstring: #numpy
            data = []
            if "D" not in datalist:
                data = fromstring(datalist, dtype=float, sep=' ')
            if len(data) != nrows * ncolumns:
                data = N.array(map(AUTOatof,datalist.split()), 'd')
            else:
                #make sure the last element is correct
                #(fromstring may not do this correctly for a
                #string like -2.05071-106)
                data[-1] = AUTOatof(datalist[datalist.rfind(' ')+1:].strip())
        else: #numarray, Numeric, array
            datalist = datalist.split()
            try:
                data = N.array(map(float, datalist), 'd')
            except ValueError:
                data = N.array(map(AUTOatof, datalist), 'd')
        data.shape = (-1,ncolumns)
        coordarray = N.transpose(data[:,4:]).copy()
        points = data[:,1]
        if hasattr(N,"concatenate"):
            stability = self.__parsenumpy(points)
        else:
            stability = self.__parsearray(points)
        # add stability info labels
        branchtype = type_translation(self.TY)["short name"]
        for i in stability:
            if i < 0:
                stab = "S"
            else:
                stab = "U"
            self.labels.update(abs(i)-1, branchtype, {"stab": stab})
        # sometimes the columns names are the same: add spaces to those
        for i in range(len(self.coordnames)):
            name = self.coordnames[i]
            if self.coordnames.count(name) > 1:
                for j in range(i+1,len(self.coordnames)):
                    if self.coordnames[j] == name:
                        self.coordnames[j] = name + ' '
        Points.Pointset.__init__(self,{
            "coordarray": coordarray,
            "coordnames": self.coordnames,
            "labels": self.labels,
            })

    def __parsenumpy(self,points):
        global N
        # stability gives a list of point numbers where the stability
        # changes: the end point of each part is stored
        stab = N.concatenate((N.nonzero(N.less(points[:-1]*points[1:],0)),
                              [len(points)-1]))
        points = N.less(N.take(points,stab),0)
        stab = stab + 1
        stability = N.where(points,-stab,stab)
        return stability

    def __parsearray(self,points):
        global N
        # for those without numpy...
        stability = []
        prevpt = points[0]
        for p, pt in enumerate(points):
            if pt * prevpt < 0:
                if prevpt < 0:
                    p = -p
                stability.append(p)
            prevpt = pt
        p = len(points)
        if pt < 0:
            p = -p
        stability.append(p)
        return stability

    def __setitem__(self, item, value):
        if item in ("BR", "TY", "TY number") and item not in self.coordnames:
            if item == "BR":
                br = self["BR"]
                if value != br:
                    self.__parse()
                    self.BR = value
            else:
                if item == "TY":
                    value = reverse_type_translation(value)
                self.TY = value
                # sync solution TYs
                for k,x in map(self._gettypelabel, self.labels.getIndices()):
                    v = x["TY number"]
                    if v>=0:
                        v=v%10
                    else:
                        v=-((-v)%10)
                    x["TY number"] = value*10 + v
        else:
            Points.Pointset.__setitem__(self, item, value)

    def __len__(self):
        if not self.__fullyParsed:
            return len(self.__datalist)
        if len(self.coordarray) == 0:
            return 0
        return Points.Pointset.__len__(self)

    def deleteLabel(self,label=None,keepTY=0,keep=0,copy=0):
        """Removes solutions with the given labels or type names"""
        if label is None:
            label = [lab for lab in all_point_types if lab not in
                     ["No Label", "RG", "UZ"]]
        if isinstance(label, (str, int)):
            label = [label]
        if copy:
            new = self.__class__(self)
            new.labels = Points.PointInfo(self.labels.by_index.copy())
            if not self.__fullyParsed:
                new.__datalist = self.__datalist[:]
        else:
            new = self
        for idx in new.labels.getIndices():
            ty_name,v = new._gettypelabel(idx)
            if "LAB" not in v:
                continue
            if ((not keep and (v["LAB"] in label or ty_name in label)) or
               (keep and not v["LAB"] in label and not ty_name in label)):
                if copy:
                    new.labels[idx][ty_name] = v.copy()
                new.labels[idx][ty_name]["LAB"] = 0
                if not keepTY:
                    new.labels[idx][ty_name]["TY number"] = 0
                if not new.__fullyParsed:
                    new.__patchline(new.__datalist,idx,3,0)
                    if not keepTY:
                        new.__patchline(new.__datalist,idx,2,0)
                if v["TY number"] == 0:
                    new.labels.remove(idx)
        if copy:
            return new

    def relabel(self,old_label=1,new_label=None):
        """Relabels the first solution with the given label"""
        if new_label is None:
            label = old_label
            new = self.__class__(self)
            labels = {}
            if not self.__fullyParsed:
                new.__datalist = self.__datalist[:]
            for index in self.labels.getIndices():
                labels[index] = self.labels[index].copy()
                ty_name,v = self._gettypelabel(index)
                if v.get("LAB",0) != 0:
                    labels[index][ty_name] = v.copy()
                    labels[index][ty_name]["LAB"] = label
                    if not self.__fullyParsed:
                        self.__patchline(new.__datalist,index,3,label)
                    label = label + 1
            new.labels = Points.PointInfo(labels)
            return new
        labels = self.labels
        if isinstance(old_label, int):
            old_label = [old_label]
            new_label = [new_label]
        for j in range(len(old_label)):
            for index in self.labels.getIndices():
                v = self._gettypelabel(index)[1]
                if "LAB" not in v:
                    continue
                if v["LAB"] == old_label[j]:
                    v["LAB"] = new_label[j]
                    if not self.__fullyParsed:
                        self.__patchline(self.__datalist,index,3,new_label[j])

    def uniquelyLabel(self,label=1):
        """Make all labels in the file unique and sequential"""
        for index in self.labels.getIndices():
            v = self._gettypelabel(index)[1]
            if v.get("LAB",0) != 0:
                v["LAB"] = label
                if not self.__fullyParsed:
                    self.__patchline(self.__datalist,index,3,label)
                label = label + 1

    def getLabel(self,label):
        """Given a label, return the correct solution"""
        if label is None:
            return self
        if isinstance(label, int):
            for k in self.labels.getIndices():
                v = self._gettypelabel(k)[1]
                if v.get("LAB",0) == label:
                    return self.getIndex(k)
            raise KeyError("Label %s not found"%label)
        if isinstance(label, str) and len(label) > 2 and label[-1].isdigit():
            j = 2
            if not label[2].isdigit():
                j = 3
            number = int(label[j:])
            i = 0
            for k,v in self.labels.sortByIndex():
                if label[:j] in v:
                    i  = i + 1
                    if i == number:
                        return self.getIndex(k)
            raise KeyError("Label %s not found"%label)
        if not AUTOutil.isiterable(label):
            label = [label]
        labels = {}
        counts = [0]*len(label)
        for k,val in self.labels.sortByIndex():
            ty_name,v = self._gettypelabel(k)
            if "LAB" not in v:
                continue
            for i in range(len(label)):
                lab = label[i]
                j = 2
                if len(lab) > 2 and not lab[2].isdigit():
                    j = 3
                if (isinstance(lab, str) and len(lab) > j and
                    ty_name == lab[:j]):
                    counts[i] = counts[i] + 1
                    if counts[i] == int(lab[j:]):
                        labels[k] = val
            if v["LAB"] in label or ty_name in label:
                labels[k] = val
                continue
        new = self.__class__(self)
        new.labels = Points.PointInfo(labels)
        return new

    def getIndex(self,index):
        """Return a parseB style line item; if given a string, return the
        relevant column"""
        if isinstance(index,tuple) and len(index)==2 and not self.__fullyParsed:
            # retrieve data without parsing everything else
            i, j = index
            if isinstance(j, str):
                j = self.coordnames.index(j)
            return AUTOatof(self.__datalist[i].split()[4+j])
        if index in ("BR", "TY", "TY number") and index not in self.coordnames:
            if index == "BR":
                if self.__fullyParsed:
                    return self.BR
                else:
                    return int(self.__datalist[0].split(None,1)[0])
            elif index == "TY":
                return type_translation(self.TY)["short name"]
            else: #"TY number"
                return self.TY
        if isinstance(index, int) and index < 0:
            index = len(self) + index
        if self.__fullyParsed or not isinstance(index, int):
            if not Points.numpyimported:
                Points.importnumpy()
            ret = Points.Pointset.__getitem__(self,index)
            if isinstance(ret, Points.Pointset):
                r = self.__class__(self)
                r.coordarray = ret.coordarray
                r.labels = ret.labels
                # adjust stability info
                if isinstance(index, slice):
                    prev = 0
                    stability = []
                    for p in self.stability():
                        if p < 0:
                            stab = "S"
                        else:
                            stab = "U"
                        stability.append((abs(p)-prev)*stab)
                        prev = abs(p)
                    stability = "".join(stability)[index]
                    branchtype = type_translation(self.TY)["short name"]
                    prev = stability[0]
                    for i, stab in enumerate(stability):
                        if stab != prev:
                            r.labels.update(i-1, branchtype, {"stab": prev})
                        prev = stab
                    r.labels.update(len(r)-1, branchtype,{"stab":stability[-1]})
                return r
            if not isinstance(ret, Points.Point):
                return ret
            labels = ret.labels
            coordnames = ret.coordnames
            coordarray = ret.coordarray
            pt = None
        else:
            labels = self.labels[index]
            coordnames = self.coordnames
            coordarray = self.__datalist[index].split()
            pt = int(coordarray[1])
            coordarray = list(map(AUTOatof, coordarray[4:]))
        label = None
        for v in labels.values():
            if "LAB" in v:
                label = v
                break
        if label is None:
            label = {"TY number": 0, "LAB": 0}
            if "No Label" in labels:
                labels["No Label"].update(label)
            else:
                labels["No Label"] = label
        return BDPoint({'coordarray': coordarray,
                        'coordnames': coordnames,
                        'labels': labels},self,index)

    def getLabels(self):
        """Get all the labels from the solution"""
        labels = []
        for index in self.labels.getIndices():
            x = self._gettypelabel(index)[1]
            if x.get("LAB",0) != 0:
                labels.append(x["LAB"])
        return labels

    def stability(self):
        """Returns a list of point numbers where the stability
        changes: the end point of each part is stored."""
        if not self.__fullyParsed:
            self.__parse()
        stab = []
        prevpt = 0
        branchtype = type_translation(self.TY)["short name"]
        for idx,val in self.labels.sortByIndex():
            if branchtype in val and "stab" in val[branchtype]:
                if val[branchtype]["stab"] == "U":
                    pt = idx+1
                else:
                    pt = -idx-1
                if pt * prevpt <= 0:
                    stab.append(pt)
                else:
                    stab[-1] = pt
                prevpt = pt
        return stab

    def subtract(self,other,ref,pt=None):
        """Subtracts branch branches using interpolation with respect to other
        with monotonically increasing or decreasing reference coordinate ref,
        and starting point pt"""
        global N
        if not Points.numpyimported:
            Points.importnumpy()
        N = Points.N
        if pt is None:
            index = 0
        elif type(pt) == type(1):
            index = abs(pt) - 1
        else:
            index = pt["index"]
        coordarray = N.array(self.coordarray)
        if not isinstance(other,Points.Pointset):
            other = other[0]
        b0 = other[ref]
        if b0[index] > b0[index+1]:
            # decreasing array: take first part until it increases
            k = index+1
            while k < len(b0) and b0[k] <= b0[k-1]:
                k = k + 1
            b0 = b0[:k]
        else:
            k = index+1
            while k < len(b0) and b0[k] >= b0[k-1]:
                k = k + 1
            b0 = b0[:k]
        r = 0
        for i in range(len(other.coordnames)):
            if other.coordnames[i] == ref:
                r = i
        a0 = self.coordarray[r]
        k = index+1
        for j in range(len(a0)):
            if b0[index+1] > b0[index]:
                #find k so that b0[k-1] < a0[j] <= b0[k]
                if a0[j] > b0[k]:
                    while k < len(b0)-1 and a0[j] > b0[k]:
                        k = k + 1
                elif b0[k-1] >= a0[j]:
                    while k > index+1 and b0[k-1] >= a0[j]:
                        k = k - 1
            else:
                #find k so that b0[k-1] > a0[j] >= b0[k]
                if a0[j] < b0[k]:
                    while k < len(b0)-1 and a0[j] < b0[k]:
                        k = k + 1
                elif b0[k-1] <= a0[j]:
                    while k > index+1 and b0[k-1] <= a0[j]:
                        k = k - 1
            #do extrapolation if past the boundaries...
            for i in range(len(self.coordnames)):
                if i == r or i >= len(other.coordarray):
                    continue
                a = coordarray[i]
                b = other.coordarray[i]
                a[j]=a[j]-b[k-1]-(a0[j]-b0[k-1])*(b[k]-b[k-1])/(b0[k]-b0[k-1])
        new = self.__class__(self)
        Points.Pointset.__init__(new, coordarray = coordarray,
                                 coordnames = self.coordnames,
                                 labels = self.labels)
        return new

    def toArray(self):
        return list(map(list,zip(*self.coordarray)))

    def writeRaw(self,output):
        data = self.toArray()
        output.write("\n".join(["".join(["%24.15E"%v for v in d]) for d in data])+"\n")
                
    def write(self, output, columnlen=19):
        if columnlen == 19 and not self.__fullyParsed:
            output.writelines(self.headerlist)
            output.writelines(self.__datalist)
            return
        format = "%"+str(-columnlen)+"s"
        if self.headerlist != []:
            for l in self.headerlist:
                if " PT " not in l:
                    output.write(l)
        if self.headernames != []:
            output_line = ["   0    PT  TY  LAB "]
            for name in self.headernames:
                output_line.append(format%name)
            output.writelines(output_line)
            output.write('\n')
        br = self.BR
        data = self.coordarray
        istab = 0
        format = "%"+str(columnlen)+"."+str(columnlen-9)+"E"
        stability = self.stability()
        for i in range(len(data[0])):
            pt = i+1
            if stability[istab] < 0:
                pt = -pt
            tynumber = 0
            lab = 0
            if i in self.labels.by_index:
                for k,label in self.labels[i].items():
                    if "LAB" in label:
                        tynumber = label["TY number"]
                        lab = label["LAB"]
                        break
            if pt == stability[istab]:
                istab = istab + 1
            if pt < 0:
                pt = -((-pt-1) % 9999) - 1
            else:
                pt = ((pt-1) % 9999) + 1
            output_line = "%4d%6d%4d%5d"%(br,pt,tynumber,lab)
            for j in range(len(data)):
                output_line = output_line + format%data[j,i]
            output.write(output_line+"\n")

    def writeShort(self):
        self.write(sys.stdout, columnlen=14)

    def summary(self):
        slist = []
        if self.__fullyParsed:
            data = self.coordarray
        first = True
        for index,l in self.labels.sortByIndex():
            label = {}
            for k,v in l.items():
                if "LAB" in v:
                    label = v
                    break
            ty_number = label.get("TY number",0)
            if ty_number == 0 or label["LAB"] == 0:
                continue
            if first:
                slist.append("".join(
                        ["\n  BR    PT  TY  LAB "] +
                        ["%-14s"%name for name in self.headernames]))
                first = False
            ty_name = type_translation(ty_number)["short name"]
            if ty_name=='RG':
                ty_name = '  '
                
            if self.__fullyParsed:
                linelist = (["%4d%6d%4s%5d"%(abs(self.BR),(index % 9999) + 1,
                                             ty_name,label["LAB"])]+
                            ["%14.5E"%d[index] for d in data])
            else:
                linedata = self.__datalist[index].split()
                linelist = (
                    ["%4d%6d%4s%5d"%(abs(int(linedata[0])),abs(int(linedata[1])),
                                     ty_name,label["LAB"])] +
                    ["%14.5E"%AUTOatof(d) for d in linedata[4:]])
            slist.append("".join(linelist))
        return "\n".join(slist)

    def __patchline(self,datalist,lineno,column,new):
        #patch column of line with new integer value
        newsp = 0
        line = datalist[lineno]
        l = len(line)
        for i in range(column+1):
            oldsp = newsp
            start = l - len(line[oldsp:].lstrip())
            newsp = line.find(' ',start)
        datalist[lineno] = (line[:oldsp]+
                            '%'+str(newsp-oldsp)+'d'+line[newsp:])%new

    def __checknorotate(self,datalist):
        # Sometimes the point numbers rotate, like
        # 9996, 9997, 9998, 9999, 1, 2, ...
        # -9996, -9997, 1, 0, -1, -2, ... (an AUTO bug)
        # do not define a new branch if that happens
        prevpt = int(datalist[-2].split(None,2)[1])
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
        if hasattr(str,"split"):
            split = str.split
        else:
            import string
            split = string.split
        if prevline:
            line = prevline
        else:
            try:
                line = next(inputfile)
            except StopIteration:
                self.labels = None
                return
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
                if pt < 0:
                    stab = 'S'
                else:
                    stab = 'U'
                ty = int(columns[2])
                lab = int(columns[3])
                key = type_translation(ty)["short name"]
                labels[len(datalist)-1] = {key: {"LAB":lab,"TY number":ty}}
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
                    labels[len(datalist)-1] = {key: {"LAB":lab,"TY number":ty}}
        self.labels = Points.PointInfo(labels)
        self.__datalist = datalist
        self.c = self.parseHeader(headerlist)

    def parseHeader(self,headerlist):
        self.headerlist = headerlist
        if hasattr(str,"split"):
            split = str.split
        else:
            import string
            split = string.split
        line = ""
        if headerlist != []:
            line = headerlist[-1]
        ncolumns = len(split(self.__datalist[0])) - 4
        self.headernames = []
        self.coordnames = []
        self.TY = 0
        if " PT " in line:
            linelen = len(self.__datalist[0])
            columnlen = (linelen - 19) // ncolumns
            n = linelen - columnlen * ncolumns
            for i in range(ncolumns):
                self.headernames.append(line[n:n+columnlen].rstrip())
                self.coordnames.append(line[n:n+columnlen].strip())
                n = n + columnlen
        if self.coordnames == []:
            self.coordnames = ["COLUMN(%d)"%(i+1) for i in range(ncolumns)]
        dict = parseC.parseC()
        i = 0
        if len(headerlist) == 0:
            return
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
            if words[-1] in ["constants:", "above:"]:
                userspec = True
                continue
            if words[1] == 'Branch':
                self.TY = int(words[3])
            if words[1] in ["User-specified", "Active"]:
                line = line.replace("s:",":")
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
            dict.parseline(" ".join(words[1:]),userspec)
        return dict

class parseBR(parseBMixin, UserList):
    def __init__(self,filename=None):
        if isinstance(filename, str):
            UserList.__init__(self)
            self.readFilename(filename)
        else:
            UserList.__init__(self,filename)

    # Removes solutions with the given labels or type names
    def deleteLabel(self,label=None,keepTY=0,keep=0,copy=0):
        if copy:
            data = []
            for d in self.data:
                data.append(d.deleteLabel(label,keepTY,keep,copy))
            return self.__class__(data)
        for d in self.data:
            d.deleteLabel(label,keepTY,keep)
            
    # Relabels the first solution with the given label
    def relabel(self,old_label=None,new_label=None):
        if new_label is None:
            label = 1
            new = []
            for d in self.data:
                newd = d.relabel(label)
                for idx,val in newd.labels.sortByIndex():
                    for k,v in val.items():
                        if v.get("LAB",0) != 0:
                            label = v["LAB"]
                new.append(newd)
                label = label + 1
            return self.__class__(new)
        for d in self.data:
            d.relabel(old_label,new_label)

    # Make all labels in the file unique and sequential
    def uniquelyLabel(self):
        label = 1
        for d in self.data:
            d.uniquelyLabel(label)
            for idx,val in d.labels.sortByIndex():
                for k,v in val.items():
                    if v.get("LAB",0) != 0:
                        label = v["LAB"] + 1
            
    # Given a label, return the correct solution
    def getLabel(self,label):
        if isinstance(label, int):
            i = 0
            section = 0
            for d in self.data:
                l = len(d.coordarray[0])
                try:
                    item = d.getLabel(label)
                    item["index"] = item["index"] + i
                    item["section"] = section
                    return item
                except (KeyError, IndexError):
                    pass
                i = i + l
                section = section + 1
            raise KeyError("Label %s not found"%label)
        elif isinstance(label, str) and len(label) > 2 and label[-1].isdigit():
            j = 2
            if not label[2].isdigit():
                j = 3
            number = int(label[j:])
            for d in self.data:
                l = len(d.getLabel(label[:j]).getLabels())
                if number <= l:
                    return d.getLabel(label[:j]+str(number))
                number -= l
            raise KeyError("Label %s not found"%label)
        new = []
        label = label[:]
        for d in self.data:
            newbranch = d.getLabel(label)
            if newbranch:
                new.append(newbranch)
            for i, lab in enumerate(label):
                if isinstance(lab, str) and len(lab) > 2 and lab[-1].isdigit():
                    j = 2
                    if not lab[2].isdigit():
                        j = 3
                    number = int(lab[j:])
                    l = len(d.getLabel(lab[:j]).getLabels())
                    if number >= l:
                        number -= l
                        label[i] = lab[:j]+str(number)
        return self.__class__(new)

    def __getitem__(self,index):
        try:
            return UserList.__getitem__(self,index)
        except TypeError:
            return self.data[0][index]

    # Given an index, return the correct solution
    # Return a parseB style line item
    def getIndex(self,index):
        if type(index) != type(0):
            return self.data[0].getIndex(index)
        section = 0
        i = index
        for d in self.data:
            l = len(d)
            if i < l:
                item = d.getIndex(i)
                item["section"] = section
                item["index"] = index
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

    # Merges branches
    def merge(self):
        fw = None
        data = []
        for bw in self.data:
            data.append(bw)
            if fw is None or fw.BR != bw.BR or (fw.c is not bw.c and
                                      fw.c["DS"] * bw.c["DS"] > 0):
                fw = bw
                continue
            f0 = fw[0]
            b0 = bw[0]
            if len(f0) != len(b0):
                fw = bw
                continue
            cont = False
            for i in range(len(f0)):
                if f0[i] != b0[i]:
                    fw = bw
                    cont = True
                    break
            if cont:
                continue
            #now we know that the branches have the same starting point:
            #merge them
            new = bw[::-1]
            new.extend(fw[1:])
            new.c = fw.c
            new.headerlist = fw.headerlist
            data[-1] = new

            # adjust label numbers
            lab = min(fw.getLabels()+bw.getLabels())
            for idx,val in new.labels.sortByIndex():
                for k,v in val.items():
                    if v.get("LAB",0) > 0:
                        val[k] = v.copy()
                        val[k]["LAB"] = lab
                        lab = lab+1

            if hasattr(fw,"diagnostics"):
                if hasattr(bw,"diagnostics"):
                    new.diagnostics = fw.diagnostics + bw.diagnostics
                else:
                    new.diagnostics = fw.diagnostics

            del data[-2]
            #reset for further search
            fw = None
        return self.__class__(data)

    def subtract(self,other,ref,pt=None):
        """Subtracts branch branches using interpolation with respect to other
        with monotonically increasing or decreasing reference coordinate ref,
        and starting point pt"""
        new = []
        for d in self.data:
            new.append(d.subtract(other,ref,pt))
        return self.__class__(new)

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
            s = branch.__str__()
            if s != "":
                slist.append(s)
        return "\n".join(slist)+"\n"

    def __repr__(self):
        slist = []
        for branch in self.data:
            slist.append(branch.__repr__())
        return "\n".join(slist)+"\n"

    def read(self,inputfile):
        # We now go through the file and read the branches.
        prevline = None
        coordnames = []
        lastc = None
        lastTY = 0
        while True:
            branch = AUTOBranch(inputfile,prevline,coordnames)
            if branch.labels is None:
                break
            prevline = branch._lastline
            coordnames = branch.coordnames
            self.data.append(branch)
            if branch.c is None:
                #for header-less branches use constants of last branch header
                branch.c = lastc
                branch.TY = lastTY
            else:
                lastc = branch.c
                lastTY = branch.TY
            if prevline is None:
                break

class parseB(parseBMixin):
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
            l += len(d)
        return l
    def getLabel(self,label):
        if isinstance(label, int) or (
            isinstance(label, str) and len(label) > 2 and label[-1].isdigit()):
            return self.branches.getLabel(label)
        return self.__class__(self.branches.getLabel(label))
    def read(self,inputfile):
        self.branches.read(inputfile)
        if len(self.branches) > 0:
            self.coordnames = self.branches[0].coordnames
    def __repr__(self):
        return self.branches.__repr__()

def AUTOatof(input_string):
    #Sometimes AUTO messes up the output.  I.e. it gives an
    #invalid floating point number of the form x.xxxxxxxE
    #instead of x.xxxxxxxE+xx.  Here we assume the exponent
    #is 0 and make it into a real real number :-)
    try:
        return float(input_string)
    except ValueError:
        try:
            if not isinstance(input_string, str):
                input_string = input_string.decode('ascii')
            if input_string[-1] == "E":
                #  This is the case where you have 0.0000000E
                return float(input_string.strip()[0:-1])
            if len(input_string) >= 5:
                if input_string[-4] in ["-","+"]:
                    #  This is the case where you have x.xxxxxxxxx-yyy
                    #  or x.xxxxxxxxx+yyy (standard Fortran but not C)
                    return float(input_string[:-4]+'E'+input_string[-4:])
                if input_string[-4] == "D":
                    #  This is the case where you have x.xxxxxxxxxD+yy
                    #  or x.xxxxxxxxxD-yy (standard Fortran but not C)
                    return float(input_string[:-4]+'E'+input_string[-3:])
            input_string = input_string.replace("D","E")
            input_string = input_string.replace("d","e")
            try:
                return float(input_string)
            except ValueError:
                i = input_string.find("-", 1)
                if i == -1:
                    i = input_string.find("+", 1)
                if i != -1 and input_string[i-1] not in ["e", "E"]:
                    # form x.xxx+yy or x.xxx-yy (standard Fortran but not C)
                    return float(input_string[:i]+'E'+input_string[i:])
            print("Encountered value I don't understand")
            print(input_string)
            print("Setting to 0")
            return 0.0
        except ValueError:
            print("Encountered value which raises an exception while processing!!!")
            print(input_string)
            print("Setting to 0")
            return 0.0
            
            
def pointtest(a,b):
    if "TY name" not in a:
        raise AUTOExceptions.AUTORegressionError("No TY label")
    if "TY number" not in a:
        raise AUTOExceptions.AUTORegressionError("No TY label")
    if "BR" not in a:
        raise AUTOExceptions.AUTORegressionError("No BR label")
    if "data" not in a:
        raise AUTOExceptions.AUTORegressionError("No data label")
    if "PT" not in a:
        raise AUTOExceptions.AUTORegressionError("No PT label")
    if "LAB" not in a:
        raise AUTOExceptions.AUTORegressionError("No LAB label")
    if len(a["data"]) != len(b["data"]):
        raise AUTOExceptions.AUTORegressionError("Data sections have different lengths")

def test():
    print("Testing reading from a filename")
    foo = parseB()
    foo.readFilename("test_data/fort.7")    
    if len(foo) != 150:
        raise AUTOExceptions.AUTORegressionError("File length incorrect")
    pointtest(foo.getIndex(0),foo.getIndex(57))

    print("Testing reading from a stream")
    foo = parseB()
    fp = open("test_data/fort.7","r")
    foo.read(fp)
    if len(foo) != 150:
        raise AUTOExceptions.AUTORegressionError("File length incorrect")
    pointtest(foo.getIndex(0),foo.getIndex(57))


    print("Testing label manipulation")
    labels = foo.getLabels()
    foo.relabel(labels[0],57)
    labels = foo.getLabels()
    if labels[0] != 57:
        raise AUTOExceptions.AUTORegressionError("Error in either relabel or getLabel")
    foo.deleteLabel(labels[1])
    new_labels = foo.getLabels()
    if len(labels) != len(new_labels) + 1:
        raise AUTOExceptions.AUTORegressionError("Error in label deletion")
        
    bar = foo.relabel()
    old_labels = foo.getLabels()
    new_labels = bar.getLabels()
    if old_labels[0] != 57 or new_labels[0] != 1:
        raise AUTOExceptions.AUTORegressionError("Error in relabelling")
    print("parseB passed all tests")

if __name__ == '__main__' :
    test()








