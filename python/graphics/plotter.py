#! /usr/bin/env python
import grapher
import Tkinter
import parseB
import parseS
import AUTOutil
import types
import copy
import string
import Pmw
import ConfigParser
import os

class plotter(grapher.GUIGrapher):
    def __init__(self,parent=None,cnf={},**kw):

        optionDefaults = {}
        # The kind of diagram (single solution vs. bifur diagram)
        optionDefaults["type"]     = ("bifurcation",self.__optionCallback)  
        # The X column
        optionDefaults["bifurcation_x"] = ([0],self.__optionCallback)
        optionDefaults["solution_x"]    = (["t"],self.__optionCallback)
        # The Y column
        optionDefaults["bifurcation_y"] = ([1],self.__optionCallback)
        optionDefaults["solution_y"]    = ([0],self.__optionCallback)
        # Sets of labels that the user is likely to want to use
        optionDefaults["label_defaults"]   = (None,self.__optionCallback)
        # Sets of columns that the user is likely to want to use
        optionDefaults["bifurcation_column_defaults"]   = (None,self.__optionCallback)
        optionDefaults["solution_column_defaults"]   = (None,self.__optionCallback)
        # The index of the solution we wish to draw
        optionDefaults["index"]    = ([0],self.__optionCallback)
        # The label of the solution we wish to draw
        optionDefaults["label"]    = ([0],self.__optionCallback)

        # Already parsed data structures
        optionDefaults["bifurcation_diagram"]          = (parseB.parseB(),self.__optionCallback)
        optionDefaults["solution"]          = (parseS.parseS(),self.__optionCallback)
        optionDefaults["bifurcation_diagram_filename"] = ("",self.__optionCallback)
        optionDefaults["solution_filename"] = ("",self.__optionCallback)
        optionDefaults["runner"]         = (None,self.__optionCallback)
        optionDefaults["mark_t"]         = (None,self.__optionCallback)

        optionDefaults["bifurcation_symbol"]     = ("B",self.__optionCallback)
        optionDefaults["limit_point_symbol"]     = ("L",self.__optionCallback)
        optionDefaults["hopf_symbol"]            = ("H",self.__optionCallback)
        optionDefaults["period_doubling_symbol"] = ("D",self.__optionCallback)
        optionDefaults["torus_symbol"]           = ("T",self.__optionCallback)
        optionDefaults["user_point_symbol"]      = ("U",self.__optionCallback)
        optionDefaults["error_symbol"]           = ("X",self.__optionCallback)
        optionDefaults["use_labels"]             = (1,self.__optionCallback)


        parser = ConfigParser.ConfigParser()
        parser.add_section("AUTO_plotter")
        if(os.path.exists(os.path.expandvars("$AUTO_DIR/.autorc"))):
            parser.read(os.path.expandvars("$AUTO_DIR/.autorc"))
        if(os.path.exists(os.path.expandvars("$HOME/.autorc"))):
            parser.read(os.path.expandvars("$HOME/.autorc"))
        if(os.path.exists("./.autorc")):
            parser.read("./.autorc")

        for option in parser.options("AUTO_plotter"):
            optionDefaults[option] = (eval(parser.get("AUTO_plotter",option)),self.__optionCallback)

        self.__needsPlot = None
        apply(grapher.GUIGrapher.__init__,(self,parent))

        dict = AUTOutil.cnfmerge((cnf,kw))
        self.addOptions(optionDefaults)
        plotter.config(self,dict)

    def config(self,cnf=None,**kw):
        if type(cnf) == types.StringType or (cnf is None and len(kw) == 0):
            return self._configNoDraw(cnf)
        else:
            self._configNoDraw(AUTOutil.cnfmerge((cnf,kw)))
        if self.__needsPlot:
            self._plotNoDraw()
            self.__needsPlot = None
        self.clear()
        self.computeXRange()
        self.computeYRange()	
        self.draw()
        self.update()
        
    configure=config

    def _configNoDraw(self,cnf=None,**kw):
        if type(cnf) == types.StringType or (cnf is None and len(kw) == 0):
            return grapher.GUIGrapher._configNoDraw(self,cnf)
        else:
            grapher.GUIGrapher._configNoDraw(self,AUTOutil.cnfmerge((cnf,kw)))
    _configureNoDraw = _configNoDraw

    def __optionCallback(self,key,value,options):
        if key == "runner":
            self.cget("bifurcation_diagram").read(value.getBifurcation_diagram())
            self.cget("solution").read(value.getSolution())
        elif key == "bifurcation_diagram_filename":
            if os.path.exists(value):
                self.cget("bifurcation_diagram").readFilename(value)
        elif key == "solution_filename":
            if os.path.exists(value):
                self.cget("solution").readFilename(value)
        elif key == "label":
            labels = self.cget("solution").getLabels()
            options["index"] =[]
            for i in range(len(value)):
                for j in range(len(labels)):
                    if labels[j] == value[i]:
                        options["index"].append(j)
        elif key == "index":
            options["label"] =[]
            for i in range(len(value)):
                labels = self.cget("solution").getLabels()
                options["label"].append(labels[value[i]])
        # We only recreate the data if one of the above options gets set
        # We can't just recreate the data for any option since:
        #   1)  It is inefficient
        #   2)  The range gets recomputed when we create new data, so
        #       minx, miny, maxx, and maxy all never get used.
        self.__needsPlot = "yes"

    def _plotNoDraw(self):
        self.delAllData()
        if self.cget("type") == "bifurcation" and len(self.cget("bifurcation_diagram")) > 0:
            self.__plot7()
        if self.cget("type") == "solution" and len(self.cget("solution")) > 0:
            self.__plot8()

    def plot(self):
        self._plotNoDraw()
        self.clear()
        self.computeXRange()
        self.computeYRange()
        self.draw()

    def __plot7(self):
        xcolumns = self.cget("bifurcation_x")
        ycolumns = self.cget("bifurcation_y")
        self.delAllData()
        solution = self.cget("bifurcation_diagram")
        if len(xcolumns) == len(ycolumns):
            for j in range(len(xcolumns)):
                x = []
                y = []
                labels = []
                current_index = 0
                for i in range(len(solution)):
                    if i > 0 and (solution.getIndex(i-1)["section"] != solution.getIndex(i)["section"]):
                        if len(x) > 0:
                            self.addArrayNoDraw((x,y))
                        for label in labels:
                            self.addLabel(len(self)-1,label["index"],label["text"],label["symbol"])
                        x = []
                        y = []
                        labels = []
                        current_index = 0
                    x.append(solution.getIndex(i)["data"][xcolumns[j]]) 
                    y.append(solution.getIndex(i)["data"][ycolumns[j]])
                    
                    if solution.getIndex(i)["LAB"] != 0:
                        labels.append({})
                        labels[-1]["index"] = current_index
                        if self.cget("use_labels") == 1:
                            labels[-1]["text"] = "%d"%solution.getIndex(i)["LAB"]
                        else:
                            labels[-1]["text"] = ""
                        if solution.getIndex(i)["TY number"] == 4 or solution.getIndex(i)["TY number"] == 9:
                            labels[-1]["symbol"] = None
                        elif solution.getIndex(i)["TY number"] == 1 or solution.getIndex(i)["TY number"] == 6: 
                            labels[-1]["symbol"] = self.cget("bifurcation_symbol")
                        elif solution.getIndex(i)["TY number"] == 2 or solution.getIndex(i)["TY number"] == 5: 
                            labels[-1]["symbol"] = self.cget("limit_point_symbol")
                        elif solution.getIndex(i)["TY number"] == 3: 
                            labels[-1]["symbol"] = self.cget("hopf_symbol")
                        elif solution.getIndex(i)["TY number"] == 7: 
                            labels[-1]["symbol"] = self.cget("period_doubling_symbol")
                        elif solution.getIndex(i)["TY number"] == 8: 
                            labels[-1]["symbol"] = self.cget("torus_symbol")
                        elif solution.getIndex(i)["TY number"] == -4: 
                            labels[-1]["symbol"] = self.cget("user_point_symbol")
                        else:
                            labels[-1]["symbol"] = self.cget("error_symbol")
                    current_index = current_index + 1
                if len(x) > 0:
                    self.addArrayNoDraw((x,y))
                for label in labels:
                    self.addLabel(len(self)-1,label["index"],label["text"],label["symbol"])
        
        # Call the base class config
        grapher.GUIGrapher._configNoDraw(self,
                                         xlabel="Column %d"%xcolumns[0],
                                         ylabel="Column %d"%ycolumns[0])

    def __plot8(self):
        self.delAllData()
        xcolumns = self.cget("solution_x")
        ycolumns = self.cget("solution_y")

        if len(xcolumns) == len(ycolumns):
            xnames="Error"
            ynames="Error"
            for index in self.cget("index"):
                solution = self.cget("solution").getIndex(index)["data"]
                r = range(len(solution))
                xnames = ""
                ynames = ""
                for j in range(len(xcolumns)):
                    labels = []
                    xc = xcolumns[j]
                    yc = ycolumns[j]
                    if xc == "t":
                        x = map(lambda i, s=solution: s[i]["t"], r)
                    else:
                        x = map(lambda i, s=solution, c = xc: s[i]["u"][c], r)
                    if yc == "t":
                        y = map(lambda i, s=solution: s[i]["t"], r)
                    else:
                        y = map(lambda i, s=solution, c = yc: s[i]["u"][c], r)

                    if not(self.cget("mark_t") is None):
                        for i in r:
                            if i != 0 and solution[i-1]["t"] <= self.cget("mark_t") < solution[i]["t"]:
                                labels.append({})
                                labels[-1]["index"] = i
                                labels[-1]["text"] = ""
                                labels[-1]["symbol"] = "fillcircle"
                    # Call the base class config
                    if len(x) > 0:
                        self.addArrayNoDraw((x,y))
                    for label in labels:
                        self.addLabel(len(self)-1,label["index"],label["text"],label["symbol"])

                    xnames = xnames + " " + str(xcolumns[j])
                    ynames = ynames + " " + str(ycolumns[j])
                grapher.GUIGrapher._configNoDraw(self,
                                                 xlabel="Columns %s"%xnames,
                                                 ylabel="Columns %s"%ynames)


def test():
    import parseB
    import parseS
    import sys
    foo = plotter(bifurcation_diagram_filename="../test_data/fort.7",
                  solution_filename="../test_data/fort.8")

    foo.plot()
    foo.pack()
    foo.update()
    print "Hit return to continue"
    raw_input()
    foo.clear()
    foo.update()
    print "Hit return to continue"
    raw_input()
    foo.clear()
    foo.config(type="solution",label=[6])
    foo.plot()
    foo.update()
    print "Hit return to continue"
    raw_input()
    foo.clear()
    foo.config(index=[3])
    foo.plot()
    foo.update()
    print "Hit return to continue"
    raw_input()

if __name__ == "__main__":
    test()





