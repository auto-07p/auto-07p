#! /usr/bin/env python
# this is an enhanced list of bifurcation branches:
# the labels contain the solutions and diagnostics as well
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

# some constants must not be preserved from run to run. These are:
nonekeys = ["IRS", "PAR", "U", "sv", "s", "dat"]

class bifDiag(parseB.parseBR):
    def __init__(self,fort7_filename=None,fort8_filename=None,
                 fort9_filename=None,**kw):
        if (kw != {} and
            kw.get("constants") is not None and
            kw["constants"]['sv'] is not None and
            type(fort7_filename) in (type(""),type(None)) and
            type(fort8_filename) in (type(""),type(None)) and
            type(fort9_filename) in (type(""),type(None))):
            #filebased
            self.options = kw
            self.filenames = [fort7_filename,fort8_filename,fort9_filename]
        else:
            self.__realinit(fort7_filename,fort8_filename,fort9_filename,kw)

    def __realinit(self,fort7_filename,fort8_filename,fort9_filename,options):
        ioerrors = []
        try:
            parseB.parseBR.__init__(self,fort7_filename)
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
                    branch = parseB.AUTOBranch()
                    self.append(branch)
                    branch.BR = br
                    branch.coordarray = []
                    branch.coordnames = []
                    branch.headernames = []
                    branch.headerlist = []
                    branch.c = options.get("constants")
                    labels = {}
                    i = 0
                pt = s["Point number"]
                ty = s["Type number"]
                lab = s["Label"]
                key = parseB.type_translation(ty)["short name"]
                labels[i] = {key: {"LAB":lab,"TY number":ty,"PT":pt}}
                i = i+1
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
            if "constants" not in options:
                options["constants"] = None
            for d in self:
                if options["constants"] is None and d.c is not None:
                    if d.c["e"] is not None and options.get("equation") is None:
                        options["equation"] = "EQUATION_NAME="+d.c["e"]
                    options["constants"] = d.c
                if options["constants"] is not None:
                    options = options.copy()
                    c = parseC.parseC(options["constants"])
                    options["constants"] = c
                    for k in c:
                        if k in nonekeys:
                            c[k] = None
                for ind in d.labels.getIndices():
                    if i >= len(solution):
                        break
                    x = d._gettypelabel(ind)[1]
                    s = solution[i]
                    if x["LAB"] != 0 or s["LAB"] == 0:
                        i = i+1
                        s = x["solution"] = parseS.AUTOSolution(s, **options)
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
                                self.filenames[2], self.options)
                del self.filenames
                del self.options
                return self.data
        raise AttributeError
        
    def __repr__(self):
        result = id(self)
        if result < 0:
            # avoid negative addresses and Python 2.3 warnings
            result += 256 ** struct.calcsize('P')
        return "<_=%s instance at %#010x>"%(self.__class__.__name__,result)

    def getLabel(self,label):
        sols = []
        for d in self:
            for k,x in map(d._gettypelabel, d.labels.getIndices()):
                if "solution" in x:
                    sols.append(x["solution"])
        solution = parseS.parseS(sols)
        s = solution(label)
        #adjust maximum label/branch
        mbr, mlab = 0, 0
        for d in solution:
            if d["BR"] > mbr: mbr = d["BR"]
            if d["LAB"] > mlab: mlab = d["LAB"]
        if isinstance(s,parseS.parseS):
            for i in range(len(s)):
                if s[i]._mlab != mlab or s[i]._mbr != mbr:
                    s[i] = s[i].__class__(s[i])
                    s[i]._mlab = mlab
                    s[i]._mbr = mbr
        elif s._mlab != mlab or s._mbr != mbr:
            s = s.__class__(s)
            s._mlab = mlab
            s._mbr = mbr
        return s

    def __call__(self,label=None):
        return self.getLabel(label)

    def load(self,**kw):
        """Load solution with the given AUTO constants.
        Returns a shallow copy with a copied set of updated constants
        """
        return self().load(**kw)

    def read(self,fort7_input,fort8_input=None,fort9_input=None):
        parseB.parseBR.read(self,fort7_input)
        if fort8_input is not None and (
            isinstance(fort8_input, (file, gzip.GzipFile))):
            solution = parseS.parseS()
            solution.read(fort8_input)
            i = 0
            for d in self:
                for k,x in map(d._gettypelabel, d.labels.getIndices()):
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
        if fort8_filename is not None and isinstance(fort8_filename, str):
            solution = parseS.parseS(fort8_filename)
            i = 0
            for d in self:
                for k,x in map(d._gettypelabel, d.labels.getIndices()):
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

    def deleteLabel(self,label=None,keepTY=0,keep=0,copy=0):

        # accept a user-defined boolean function
        if isinstance(label, types.FunctionType):
            f = label
            cnt = getattr(f,"func_code",getattr(f,"__code__",None)).co_argcount
            if cnt == 1:
                # function takes just one parameter
                label = [s["LAB"] for s in self() if f(s)]
            elif cnt == 2:
                # function takes two parameters: compare all solutions
                # with each other
                sols = self()
                indices = set([])
                for i1, s1 in enumerate(sols):
                    if i1 in indices:
                        continue
                    for i2 in range(i1+1,len(sols)):
                        if i2 not in indices and f(s1, sols[i2]):
                            indices.add(i2)
                label = [sols[i]["LAB"] for i in sorted(indices)]
            else:
                raise AUTOExceptions.AUTORuntimeError(
                    "Invalid number of arguments for %s."%f.__name__)

        new = parseB.parseBR.deleteLabel(self,label,keepTY,keep,copy)
        if copy:
            for i in range(len(self)):
                if hasattr(self[i],"diagnostics"):
                    new[i].diagnostics = self[i].diagnostics
        else:
            new = self
        newlabels = new.getLabels()
        if len(newlabels) > 0:
            maxlab = max(newlabels)
        for d in new:
            for k,x in map(d._gettypelabel, d.labels.getIndices()):
                if "solution" in x:
                    if x["LAB"] == 0:
                        del x["solution"]
                    elif x["solution"]._mlab != maxlab:
                        if copy:
                            news = x["solution"].__class__(x["solution"])
                            x["solution"] = news
                        x["solution"]._mlab = maxlab
        if copy:
            return new

    def relabel(self,old_label=None,new_label=None):
        if old_label is None and new_label is None:
            new = parseB.parseBR.relabel(self)
            label = 0
            for i in range(len(self)):
                if hasattr(self[i],"diagnostics"):
                    new[i].diagnostics = self[i].diagnostics
            for d in new:
                for k,x in map(d._gettypelabel, d.labels.getIndices()):
                    if "solution" in x and x["LAB"] != 0:
                        label = label + 1
                        news = x["solution"].__class__(x["solution"])
                        news["LAB"] = label
                        x["solution"] = news
            for d in new:
                for k,x in map(d._gettypelabel, d.labels.getIndices()):
                    if "solution" in x and x["LAB"] != 0:
                        x["solution"]._mlab = label
            return new
        parseB.parseBR.relabel(self,old_label,new_label)
        self().relabel(old_label,new_label)
    
    def uniquelyLabel(self):
        parseB.parseBR.uniquelyLabel(self)
        self().uniquelyLabel()

    def merge(self):
        # Merges branches and then sync solution
        new = parseB.parseBR.merge(self)
        mlab = max(self.getLabels())
        for d in new:
            for k,x in map(d._gettypelabel, d.labels.getIndices()):
                if "solution" in x:
                    news = x["solution"].__class__(x["solution"])
                    news._mlab = mlab
                    news["PT"] = abs(x["PT"])
                    news["LAB"] = x["LAB"]
                    x["solution"] = news
        return new

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
