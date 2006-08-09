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

class parseBandS:
    def __init__(self,fort7_filename=None,fort8_filename=None):
        if (fort8_filename is None) and not(fort8_filename is None):
            raise AUTORuntimeError("Must set both both filenames")
        self.diagram = parseB.parseB()
        self.solution = parseS.parseS()
        self.dg = self.diagram
        self.sl = self.solution
        if not(fort7_filename is None):
            self.diagram.readFilename(fort7_filename)
            self.solution.readFilename(fort8_filename)

    def __str__(self):
        return self.diagram.__str__() + self.solution.__str__()
        
    def read(self,fort7_input,fort8_input):
        self.diagram.read(fort7_input)
        self.solution.read(fort8_input)

    def write(self,fort7_output,fort8_output):
        self.diagram.write(fort7_output)
        self.solution.write(fort8_output)

    def readFilename(self,fort7_filename,fort8_filename):
        self.diagram.readFilename(fort7_filename)
        self.solution.readFilename(fort8_filename)

    def writeFilename(self,fort7_filename,fort8_filename):
        self.diagram.writeFilename(fort7_filename)
        self.solution.writeFilename(fort8_filename)

    def deleteLabel(self,label):
        self.diagram.deleteLabel(label)
        self.solution.deleteLabel(label)

    def relabel(self,old_label,new_label):
        self.diagram.relabel(old_label,new_label)
        self.solution.relabel(old_label,new_label)
    
    def uniquelyLabel(self):
        self.diagram.uniquelyLabel()
        self.solution.uniquelyLabel()

    def getLabels(self):
        return self.diagram.getLabels()

def pointtest7(a,b):
    if not(a.has_key("TY name")):
        raise AUTORegressionError("No TY name label")
    if not(a.has_key("TY number")):
        raise AUTORegressionError("No TY number label")
    if not(a.has_key("BR")):
        raise AUTORegressionError("No BR label")
    if not(a.has_key("data")):
        raise AUTORegressionError("No data label")
    if not(a.has_key("PT")):
        raise AUTORegressionError("No PT label")
    if not(a.has_key("LAB")):
        raise AUTORegressionError("No LAB label")
    if not(len(a["data"]) == len(b["data"])):
        raise AUTORegressionError("Data sections have different lengths")
   
    
def pointtest8(a,b):
    keys = ['Type number', 'Type name',
            'Free Parameters',  'Branch number', 'Parameter NULL vector',
            'data', 'NCOL', 'Label', 'ISW', 'NTST',
            'Point number', 'Parameters']

    for key in keys:
        if not(a.has_key(key)):
            raise AUTORegressionError("No %s label"%(key,))
    if not(len(a["data"]) == len(b["data"])):
        raise AUTORegressionError("Data sections have different lengths")


def test():
    foo = parseBandS()
    foo.readFilename("test_data/fort.7","test_data/fort.8")
    if len(foo.diagram) != 150:
        raise AUTORegressionError("File length incorrect")
    pointtest7(foo.diagram.getIndex(0),foo.diagram.getIndex(57))
    if len(foo.solution) != 5:
        raise AUTORegressionError("File length incorrect")
    pointtest8(foo.solution.getIndex(0),foo.solution.getIndex(3))

    if len(foo.getLabels()) != 5:
        raise AUTORegressionError("Incorrect number of labels")

    print "Deleting labels"
    foo.deleteLabel(range(6,9))
    
    if len(foo.getLabels()) != 2:
        raise AUTORegressionError("Incorrect number of labels")

    print "Relabeling"
    foo.relabel(9,57)

    for i in range(len(foo.diagram)):
        if foo.diagram.getIndex(0)["TY number"] != 0:
            if foo.diagram.getIndex(0)["LAB"] != 57:
                raise AUTORegressionError("Incorrect label")
            break
    if foo.solution.getIndex(0)["Label"] != 57:
        raise AUTORegressionError("Incorrect label")

    print "Making labels unique"
    foo.uniquelyLabel()

    for i in range(len(foo.diagram)):
        if foo.diagram.getIndex(0)["TY number"] != 0:
            if foo.diagram.getIndex(0)["LAB"] != 1:
                raise AUTORegressionError("Incorrect label")
            break
    if foo.solution.getIndex(0)["Label"] != 1:
        raise AUTORegressionError("Incorrect label")

    print "parseBandS passed all tests"

if __name__ == '__main__' :
    test()
















