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
import parseB
import parseS
import parseD
import AUTOExceptions

class parseBandS:
    def __init__(self,fort7_filename=None,fort8_filename=None,fort9_filename=None):
        self.diagnostics = None
        if isinstance(fort7_filename, list):
            self.diagram = None
            self.solution = parseS.parseS(fort7_filename)
        elif fort8_filename is None and fort7_filename is not None:
            raise AUTOExceptions.AUTORuntimeError("Must set both both filenames")
        elif isinstance(fort7_filename, str) or fort7_filename is None:
            self.diagram = parseB.parseB(fort7_filename)
            self.solution = parseS.parseS(fort8_filename)
            if not fort9_filename is None:
                self.diagnostics = parseD.parseD(fort9_filename)
        else:
            self.diagram = fort7_filename
            self.solution = fort8_filename
            self.diagnostics = fort9_filename
        if self.diagnostics is None:
            self.diagnostics = []
        self.dg = self.diagram
        self.sl = self.solution

    def __str__(self):
        return self.diagram.summary()
        
    def __repr__(self):
        return ""
        
    def getLabel(self,label):
        if isinstance(label, int):
            return self.solution(label)
        return parseBandS(self.diagram(label),self.solution(label),
                          self.diagnostics)

    def __add__(self,x):
        if x == []:
            x = parseBandS()
        add = parseBandS(self.diagram + x.diagram,
                         self.solution + x.solution,
                         self.diagnostics + x.diagnostics)
        add.uniquelyLabel()
        return add

    def __radd__(self,x):
        if x == []:
            x = parseBandS()
        add = parseBandS(x.diagram + self.diagram,
                         x.solution + self.solution,
                         x.diagnostics + self.diagnostics)
        add.uniquelyLabel()
        return add

    def __iadd__(self,x):
        if x != []:
            self.diagram.__iadd__(x.diagram)
            self.solution.__iadd__(x.solution)
            self.diagnostics.__iadd__(x.diagnostics)
        self.uniquelyLabel()
        return self

    def extend(self,x):
        self.__iadd__(x)

    def read(self,fort7_input,fort8_input,fort9_input=None):
        self.diagram.read(fort7_input)
        self.solution.read(fort8_input)
        if not fort9_input is None:
            self.diagnostics.read(fort9_input)

    def write(self,fort7_output,fort8_output,fort9_output=None):
        self.diagram.write(fort7_output)
        self.solution.write(fort8_output)
        if not fort9_output is None:
            self.diagnostics.write(fort9_output)

    def save(self,output_filename):
        self.writeFilename('b.'+output_filename,
                           's.'+output_filename,
                           'd.'+output_filename)
        return self

    def readFilename(self,fort7_filename,fort8_filename,fort9_filename=None):
        self.diagram.readFilename(fort7_filename)
        self.solution.readFilename(fort8_filename)
        if not fort9_filename is None:
            self.diagnostics.readFilename(fort9_filename)

    def writeFilename(self,fort7_filename,fort8_filename=None,fort9_filename=None):
        #if only one filename is given, then just save the solutions file
        if fort8_filename is None:
            fort8_filename = fort7_filename
        else:
            self.diagram.writeFilename(fort7_filename)
        self.solution.writeFilename(fort8_filename)
        if not fort9_filename is None:
            self.diagnostics.writeFilename(fort9_filename)

    def deleteLabel(self,label=None,keepTY=0,keep=0):
        self.diagram.deleteLabel(label,keepTY,keep)
        self.solution.deleteLabel(label,keep)

    def relabel(self,old_label=None,new_label=None):
        if old_label is None and new_label is None:
            self.diagram.uniquelyLabel()
            self.solution.uniquelyLabel()
            return self
        self.diagram.relabel(old_label,new_label)
        self.solution.relabel(old_label,new_label)
    
    def uniquelyLabel(self):
        self.diagram.uniquelyLabel()
        self.solution.uniquelyLabel()

    def getLabels(self):
        return self.diagram.getLabels()

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
    foo = parseBandS()
    foo.readFilename("test_data/fort.7","test_data/fort.8")
    if len(foo.diagram) != 150:
        raise AUTOExceptions.AUTORegressionError("File length incorrect")
    pointtest7(foo.diagram.getIndex(0),foo.diagram.getIndex(57))
    if len(foo.solution) != 5:
        raise AUTOExceptions.AUTORegressionError("File length incorrect")
    pointtest8(foo.solution.getIndex(0),foo.solution.getIndex(3))

    if len(foo.getLabels()) != 5:
        raise AUTOExceptions.AUTORegressionError("Incorrect number of labels")

    print("Deleting labels")
    foo.deleteLabel(range(6,9))
    
    if len(foo.getLabels()) != 2:
        raise AUTOExceptions.AUTORegressionError("Incorrect number of labels")

    print("Relabeling")
    foo.relabel(9,57)

    for i in range(len(foo.diagram)):
        if foo.diagram.getIndex(0)["TY number"] != 0:
            if foo.diagram.getIndex(0)["LAB"] != 57:
                raise AUTOExceptions.AUTORegressionError("Incorrect label")
            break
    if foo.solution.getIndex(0)["Label"] != 57:
        raise AUTOExceptions.AUTORegressionError("Incorrect label")

    print("Making labels unique")
    foo.uniquelyLabel()

    for i in range(len(foo.diagram)):
        if foo.diagram.getIndex(0)["TY number"] != 0:
            if foo.diagram.getIndex(0)["LAB"] != 1:
                raise AUTOExceptions.AUTORegressionError("Incorrect label")
            break
    if foo.solution.getIndex(0)["Label"] != 1:
        raise AUTOExceptions.AUTORegressionError("Incorrect label")

    print("parseBandS passed all tests")

if __name__ == '__main__' :
    test()
