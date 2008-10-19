#! /usr/bin/env python
# this is an enhanced list of bifurcation branches:
# the labels contain the solutions and diagnostics as well
import parseB
import parseC
import parseS
import parseD
import types
import AUTOExceptions

class bifDiag(parseB.parseBR):
    def __init__(self,fort7_filename=None,fort8_filename=None,fort9_filename=None,
                 options=None):
        self.options = options
        if options is not None and options["constants"]['sv'] is not None:
            #filebased
            self.filenames = [fort7_filename,fort8_filename,fort9_filename]
            return
        parseB.parseBR.__init__(self,fort7_filename)
        if type(fort7_filename) == types.ListType:
            return
        if (fort8_filename is None) and not(fort7_filename is None) and not(
            type(fort7_filename) == types.ListType):
            raise AUTOExceptions.AUTORuntimeError("Must set both both filenames")
        diagnostics = None
        if type(fort7_filename) == types.StringType or fort7_filename is None:
            solution = parseS.parseS(fort8_filename)
            for s in solution:
                s.options = options.copy()
                if options is not None:
                    s.options["constants"] = parseC.parseC(options["constants"])
                s.options["solution"] = s
            if not fort9_filename is None:
                diagnostics = parseD.parseD(fort9_filename)
        else:
            solution = fort8_filename
            diagnostics = fort9_filename
        if diagnostics is None:
            diagnostics = []
        i = 0
        for d in self:
            for k,x in map(d._gettypelabel, d.labels.getIndices()):
                x["solution"] = solution[i]
                i = i+1
        if self.data != []:
            # for now just attach diagnostics information to the first branch
            self[0].diagnostics = diagnostics

    #delayed file-based reading to save memory if sv= is used in run()
    def __getattr__(self,attr):
        if attr == 'data' and hasattr(self,'filenames'):
            self.__init__(self.filenames[0], self.filenames[1],
                          self.filenames[2])
            del self.filenames
            return self.data
        raise AttributeError
        
    def __repr__(self):
        return ""

    def getLabel(self,label):
        sols = []
        for d in self:
            for k,x in map(d._gettypelabel, d.labels.getIndices()):
                sols.append(x["solution"])
        solution = parseS.parseS(sols)
        return solution(label)

    def read(self,fort7_input,fort8_input=None,fort9_input=None):
        parseB.parseBR.read(self,fort7_input)
        if fort8_input is not None and type(fort8_input) == types.FileType:
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
        if fort8_filename is not None and type(fort8_filename) == types.StringType:
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
        else:
            parseB.parseBR.writeFilename(self,fort7_filename,append)
        self().writeFilename(fort8_filename,append)
        if not fort9_filename is None:
            for d in self:
                if hasattr(d,"diagnostics"):
                    d.diagnostics.writeFilename(fort9_filename,append)
                    append=True

    def deleteLabel(self,label=None,keepTY=0,keep=0):
        parseB.parseBR.deleteLabel(self,label,keepTY,keep)
        for d in self:
            for k,x in map(d._gettypelabel, d.labels.getIndices()):
                if x.has_key("solution") and x["LAB"] == 0:
                    del x["solution"]

    def relabel(self,old_label=None,new_label=None):
        if old_label is None and new_label is None:
            self.uniquelyLabel()
            return self
        parseB.parseBR.relabel(self,old_label,new_label)
        self().relabel(old_label,new_label)
    
    def uniquelyLabel(self):
        parseB.parseBR.uniquelyLabel(self)
        self().uniquelyLabel()

def pointtest7(a,b):
    if not(a.has_key("TY name")):
        raise AUTOExceptions.AUTORegressionError("No TY name label")
    if not(a.has_key("TY number")):
        raise AUTOExceptions.AUTORegressionError("No TY number label")
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
   
    
def pointtest8(a,b):
    keys = ['Type number', 'Type name',
            'Free Parameters',  'Branch number', 'Parameter NULL vector',
            'data', 'NCOL', 'Label', 'ISW', 'NTST',
            'Point number', 'Parameters']

    scratch=a['Parameters']
    scratch=b['Parameters']
    for key in keys:
        if not(a.has_key(key)):
            raise AUTOExceptions.AUTORegressionError("No %s label"%(key,))
    if not(len(a["data"]) == len(b["data"])):
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

    print "Deleting labels"
    foo.deleteLabel(range(6,9))
    
    if len(foo.getLabels()) != 2:
        raise AUTOExceptions.AUTORegressionError("Incorrect number of labels")

    print "Relabeling"
    foo.relabel(9,57)

    for i in range(len(foo[0])):
        if foo[0].getIndex(0)["TY number"] != 0:
            if foo[0].getIndex(0)["LAB"] != 57:
                raise AUTOExceptions.AUTORegressionError("Incorrect label")
            break
    if foo().getIndex(0)["Label"] != 57:
        raise AUTOExceptions.AUTORegressionError("Incorrect label")

    print "Making labels unique"
    foo.uniquelyLabel()

    for i in range(len(foo[0])):
        if foo[0].getIndex(0)["TY number"] != 0:
            if foo[0].getIndex(0)["LAB"] != 1:
                raise AUTOExceptions.AUTORegressionError("Incorrect label")
            break
    if foo().getIndex(0)["Label"] != 1:
        raise AUTOExceptions.AUTORegressionError("Incorrect label")

    print "bifDiag passed all tests"

if __name__ == '__main__' :
    test()
