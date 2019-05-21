#! /usr/bin/env python
# this is an enhanced list of bifurcation branches:
# the labels contain the solutions and diagnostics as well
import AUTOutil
import parseB
import parseC
import parseH
import parseS
import parseD
import Points
import AUTOExceptions
import gzip
import types
import sys
import struct

class bifDiag(parseB.parseBR):

    # some constants must not be preserved from run to run. These are:
    nonekeys = set(["IRS", "PAR", "U", "sv", "s", "dat", "TY", "LAB", "IBR"])

    def __init__(self,fort7_filename=None,fort8_filename=None,
                 fort9_filename=None,constants=None):
        if (constants is not None and constants.get('sv') is not None and
            type(fort7_filename) in (type(""),type(None)) and
            type(fort8_filename) in (type(""),type(None)) and
            type(fort9_filename) in (type(""),type(None))):
            #filebased
            self.c = constants
            self.filenames = [fort7_filename,fort8_filename,fort9_filename]
        else:
            self.__realinit(fort7_filename,fort8_filename,fort9_filename,
                            constants)

    def __realinit(self,fort7_filename,fort8_filename,fort9_filename,constants):
        ioerrors = []
        try:
            parseB.parseBR.__init__(self,fort7_filename)
            for i, d in enumerate(self):
                self[i] = bifDiagBranch(d)
        except IOError:
            ioerrors.append(str(sys.exc_info()[1]))
            parseB.parseBR.__init__(self)
            fort7_filename = None
        if isinstance(fort8_filename, parseS.AUTOSolution):
            fort8_filename = [fort8_filename]
        try:
            solution = parseS.parseS(fort8_filename)
        except IOError:
            ioerrors.append(str(sys.exc_info()[1]))
            solution = None
            if fort7_filename is None:
                raise AUTOExceptions.AUTORuntimeError('\n'.join(ioerrors))
        if fort7_filename is None and fort8_filename is not None:
            # simulate a bifurcation diagram
            labels = {}
            for s in solution:
                br = s["Branch number"]
                if labels == {} or br != branch.BR:
                    if labels != {}:
                        branch.labels = Points.PointInfo(labels)
                    branch = bifDiagBranch()
                    self.append(branch)
                    branch.BR = br
                    branch.coordarray = []
                    branch.coordnames = []
                    branch.headernames = []
                    branch.headerlist = []
                    branch.c = constants
                    labels = {}
                    base = 0
                    i = 0
                pt = s["PT"]
                lab = s["LAB"]
                ty = s["TY number"]
                if i >= base + pt - 1:
                    # point numbers wrapped around
                    base += 9999
                i = base + pt - 1
                labels[i] = {s["TY"]: {"LAB":lab,"TY number":ty,"PT":pt}}
            if labels != {}:
                branch.labels = Points.PointInfo(labels)
        if fort9_filename is not None and self.data != []:
            try:
                # for now just attach diagnostics information to first branch
                self[0].diagnostics = parseD.parseD(fort9_filename)
            except IOError:
                pass
        i = 0
        if solution is not None:
            for d in self:
                if constants is None and d.c is not None:
                    constants = d.c
                constants = parseC.parseC(constants)
                for k in constants:
                    if k in self.nonekeys:
                        constants[k] = None
                for ind in d.labels.getIndices():
                    if i >= len(solution):
                        break
                    x = d._gettypelabel(ind)[1]
                    s = solution[i]
                    if x.get("LAB",0) != 0 or s["LAB"] == 0:
                        i = i+1
                        s = x["solution"] = parseS.AUTOSolution(
                            s, constants=constants)
                        if d.coordnames != []:
                            s.b = d[ind]

    #delayed file-based reading to save memory if sv= is used in run()
    def __getattr__(self,attr):
        if attr == 'c':
            if len(self)>0 and self[0].c is not None:
                return self[0].c
        elif attr == 'data':
            if hasattr(self,'filenames'):
                self.__realinit(self.filenames[0], self.filenames[1],
                                self.filenames[2], self.c)
                del self.filenames
                del self.c
                return self.data
        return super(bifDiag, self).__getattribute__(attr)
        
    def __repr__(self):
        result = id(self)
        if result < 0:
            # avoid negative addresses and Python 2.3 warnings
            result += 256 ** struct.calcsize('P')
        return "<_=%s instance at %#010x>"%(self.__class__.__name__,result)

    def getLabel(self,label):
        sols = parseS.parseS()
        #adjust maximum label/branch
        mbr = max([abs(d["BR"]) for d in self] or [None])
        mlab = max(self.getLabels() or [None])
        for d in self:
            sols.extend(d.getLabel(None,mbr=mbr,mlab=mlab))
        return sols(label)

    def __call__(self,label=None):
        return self.getLabel(label)

    def load(self,**kw):
        """Load solution with the given AUTO constants.
        Returns a shallow copy with a copied set of updated constants
        """
        return self().load(**kw)

    def read(self,fort7_input,fort8_input=None,fort9_input=None):
        parseB.parseBR.read(self,fort7_input)
        for i, d in enumerate(self):
            self[i] = bifDiagBranch(d)
        if fort8_input is not None and (
            isinstance(fort8_input, (file, gzip.GzipFile))):
            solution = parseS.parseS()
            solution.read(fort8_input)
            i = 0
            for d in self:
                for k,x in map(d._gettypelabel, d.labels.getIndices()):
                    if x.get("LAB",0) != 0:
                        x["solution"] = solution[i]
                        i = i+1
        if fort9_input is not None:
            diagnostics = parseD.parseD()
            diagnostics.read(fort9_input)
            # for now just attach diagnostics information to the first branch
            self[0].diagnostics = diagnostics

    def write(self,fort7_output,fort8_output=None,fort9_output=None):
        parseB.parseBR.write(self,fort7_output)
        if fort8_output is not None:
            self().write(fort8_output)
        if fort9_output is not None:
            for d in self:
                if hasattr(d,"diagnostics"):
                    d.diagnostics.write(fort9_output)

    def readFilename(self,fort7_filename,fort8_filename=None,fort9_filename=None):
        parseB.parseBR.readFilename(self,fort7_filename)
        for i, d in enumerate(self):
            self[i] = bifDiagBranch(d)
        if fort8_filename is not None and isinstance(fort8_filename, str):
            solution = parseS.parseS(fort8_filename)
            i = 0
            for d in self:
                for k,x in map(d._gettypelabel, d.labels.getIndices()):
                    if x.get("LAB",0) != 0:
                        x["solution"] = solution[i]
                        i = i+1
        if not fort9_filename is None:
            # for now just attach diagnostics information to the first branch
            self[0].diagnostics = parseD.parseD(fort9_filename)

    def writeFilename(self,fort7_filename,fort8_filename=None,fort9_filename=None,append=False):
        #if only one filename is given, then just save the solutions file
        if fort8_filename is None:
            fort8_filename = fort7_filename
        elif len(self) > 0 and len(self[0]) > 0:
            parseB.parseBR.writeFilename(self,fort7_filename,append)
        if fort8_filename != '':
            self().writeFilename(fort8_filename,append)
        if not fort9_filename is None:
            for d in self:
                if hasattr(d,"diagnostics"):
                    d.diagnostics.writeFilename(fort9_filename,append)
                    append=True

    # Removes solutions with the given labels or type names
    def deleteLabel(self,label=None,keepTY=0,keep=0,copy=0):
        # accept a user-defined boolean function
        if isinstance(label, types.FunctionType):
            deletesols = self(label)
            data = []
            for d in self:
                label = [s["LAB"] for s in deletesols if s.b.branch is d]
                data.append(d.deleteLabel(label,keepTY,keep,copy))
            if copy:
                return self.__class__(data)
            return
        return parseB.parseBR.deleteLabel(self,label,keepTY,keep,copy)

class bifDiagBranch(parseB.AUTOBranch):
    def __init__(self,input=None):
        parseB.AUTOBranch.__init__(self,input)

    def __getattr__(self,attr):
        if attr == "diagnostics":
            raise AttributeError
        return super(bifDiagBranch, self).__getattr__(attr)

    def getLabel(self,label,mbr=None,mlab=None):
        sols = []
        for idx in self.labels.getIndices():
            x = self._gettypelabel(idx)[1]
            if "solution" in x:
                br = abs(self["BR"])
                pt = idx%9999 + 1
                lab = x["LAB"]
                ty = x["TY number"]
                sol = x["solution"]
                if (sol._mlab != mlab or sol._mbr != mbr or
                    br != sol["BR"] or pt != sol["PT"] or
                    ty != sol["TY number"] or lab != sol["LAB"]):
                    sol = sol.__class__(sol, BR=br, PT=pt,
                                        LAB=lab, TY=ty)
                    sol._mlab = mlab
                    sol._mbr = mbr
                sols.append(sol)
        return parseS.parseS(sols)(label)

    def load(self,**kw):
        """Load solution with the given AUTO constants.
        Returns a shallow copy with a copied set of updated constants
        """
        return self().load(**kw)

    def write(self,fort7_output,fort8_output=None,fort9_output=None):
        parseB.AUTOBranch.write(self,fort7_output)
        if fort8_output is not None:
            self().write(fort8_output)
        if fort9_output is not None:
            if hasattr(self,"diagnostics"):
                self.diagnostics.write(fort9_output)

    def writeFilename(self,fort7_filename,fort8_filename=None,fort9_filename=None,append=False):
        #if only one filename is given, then just save the solutions file
        if fort8_filename is None:
            fort8_filename = fort7_filename
        elif len(self) > 0:
            parseB.AUTOBranch.writeFilename(self,fort7_filename,append)
        if fort8_filename != '':
            self().writeFilename(fort8_filename,append)
        if not fort9_filename is None:
            if hasattr(self,"diagnostics"):
                self.diagnostics.writeFilename(fort9_filename,append)
                append=True

    def deleteLabel(self,label=None,keepTY=0,keep=0,copy=0):

        # accept a user-defined boolean function
        if isinstance(label, types.FunctionType):
            label = [s["LAB"] for s in self(label)]
        new = parseB.AUTOBranch.deleteLabel(self,label,keepTY,keep,copy)
        if keepTY:
            if not copy:
                new = self
            for idx in new.labels.getIndices():
                x = new._gettypelabel(idx)[1]
                if x["LAB"] ==0 and "solution" in x:
                    del x["solution"]
        if copy:
            if hasattr(self,"diagnostics"):
                new.diagnostics = self.diagnostics
            return new

    def relabel(self,old_label=1,new_label=None):
        """Relabels the first solution with the given label"""
        if new_label is None:
            new = self.__class__(parseB.AUTOBranch.relabel(self,old_label))
            if hasattr(self,"diagnostics"):
                new.diagnostics = self.diagnostics
            return new
        parseB.AUTOBranch.relabel(self,old_label,new_label)
    
def pointtest7(a,b):
    if "TY name" not in a:
        raise AUTOExceptions.AUTORegressionError("No TY name label")
    if "TY number" not in a:
        raise AUTOExceptions.AUTORegressionError("No TY number label")
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
   
    
def pointtest8(a,b):
    keys = ['Type number', 'Type name',
            'Free Parameters',  'Branch number', 'Parameter NULL vector',
            'data', 'NCOL', 'Label', 'ISW', 'NTST',
            'Point number', 'Parameters']

    scratch=a['Parameters']
    scratch=b['Parameters']
    for key in keys:
        if key not in a:
            raise AUTOExceptions.AUTORegressionError("No %s label"%(key,))
    if len(a["data"]) != len(b["data"]):
        raise AUTOExceptions.AUTORegressionError("Data sections have different lengths")


def test():
    foo = bifDiag()
    foo.readFilename("test_data/fort.7","test_data/fort.8")
    if len(foo[0]) != 150:
        raise AUTOExceptions.AUTORegressionError("File length incorrect")
    pointtest7(foo[0].getIndex(0),foo[0].getIndex(57))
    if len(foo()) != 5:
        raise AUTOExceptions.AUTORegressionError("File length incorrect")
    pointtest8(foo().getIndex(0),foo().getIndex(3))

    if len(foo.getLabels()) != 5:
        raise AUTOExceptions.AUTORegressionError("Incorrect number of labels")

    print("Deleting labels")
    foo.deleteLabel(range(6,9))
    
    if len(foo.getLabels()) != 2:
        raise AUTOExceptions.AUTORegressionError("Incorrect number of labels")

    print("Relabeling")
    foo.relabel(9,57)

    for i in range(len(foo[0])):
        if foo[0].getIndex(0)["TY number"] != 0:
            if foo[0].getIndex(0)["LAB"] != 57:
                raise AUTOExceptions.AUTORegressionError("Incorrect label")
            break
    if foo().getIndex(0)["Label"] != 57:
        raise AUTOExceptions.AUTORegressionError("Incorrect label")

    print("Making labels unique")
    foo.uniquelyLabel()

    for i in range(len(foo[0])):
        if foo[0].getIndex(0)["TY number"] != 0:
            if foo[0].getIndex(0)["LAB"] != 1:
                raise AUTOExceptions.AUTORegressionError("Incorrect label")
            break
    if foo().getIndex(0)["Label"] != 1:
        raise AUTOExceptions.AUTORegressionError("Incorrect label")

    print("bifDiag passed all tests")

if __name__ == '__main__' :
    test()
