#! /usr/bin/env python
try:
    import grapher_mpl as grapher
except:
    import grapher
import parseB
import parseS
import AUTOutil
import types
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

        optionDefaults["ps_colormode"]           = ("color",self.__optionCallback)
        optionDefaults["stability"]              = (0,self.__optionCallback)

        parser = ConfigParser.ConfigParser()
        parser.add_section("AUTO_plotter")
        if(os.path.exists(os.path.expandvars("$AUTO_DIR/.autorc"))):
            parser.read(os.path.expandvars("$AUTO_DIR/.autorc"))
        if(os.path.exists(os.path.expandvars("$HOME/.autorc"))):
            parser.read(os.path.expandvars("$HOME/.autorc"))
        if(os.path.exists("./.autorc")):
            parser.read("./.autorc")

        optionDefaultsRC = {}
        for option in parser.options("AUTO_plotter"):
            v = eval(parser.get("AUTO_plotter",option))
            optionDefaultsRC[option] = v

        self.__needsPlot = None
        apply(grapher.GUIGrapher.__init__,(self,parent,optionDefaultsRC))

        dict = AUTOutil.cnfmerge((cnf,kw))
        self.addOptions(optionDefaults)
        self.addRCOptions(optionDefaultsRC)
        plotter._configNoDraw(self,optionDefaultsRC)
        plotter._configNoDraw(self,dict)
        self._plotNoDraw()
        self.__needsPlot = None
        self.computeXRange()
        self.computeYRange()
        grapher.GUIGrapher.plot(self)

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
            grapher.GUIGrapher.plot(self)
        else:
            self.clear()
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
            if self.cget("label") != [0]:
                self.__optionCallback("label",self.cget("label"),options)
        elif key == "label":
            labels = self.cget("solution").getLabels()
            options["index"] =[]
            for v in value:
                for j in range(len(labels)):
                    if labels[j] == v:
                        options["index"].append(j)
        elif key == "index":
            options["label"] =[]
            for v in value:
                labels = self.cget("solution").getLabels()
                options["label"].append(labels[v])
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
        grapher.GUIGrapher.plot(self)
        self.draw()

    def __plot7(self):
        xcolumns = self.cget("bifurcation_x")
        ycolumns = self.cget("bifurcation_y")
        self.delAllData()
        solution = self.cget("bifurcation_diagram")
        dp = self.cget("stability")
        if len(solution) > 0 and len(xcolumns) == len(ycolumns):
            for j in range(len(xcolumns)):
                x = []
                y = []
                labels = []
                current_index = 0
                prevsol = solution[0]
                for sol in solution:
                    newsect = prevsol["section"] != sol["section"]
                    if (newsect or
                        (dp and current_index > 1 and
                         (prevsol['PT'] > 0) != (sol['PT'] > 0))):
                        if len(x) > 0:
                            if dp:
                                self.addArrayNoDraw((x,y),newsect,
                                                    stable=prevsol['PT'] < 0)
                            else:
                                self.addArrayNoDraw((x,y),newsect)
                        for label in labels:
                            self.addLabel(len(self)-1,label["index"],label["text"],label["symbol"])
                        if newsect:
                            current_index = 0
                            x = []
                            y = []
                        else:
                            current_index = 1
                            x = [prevsol["data"][xcolumns[j]]]
                            y = [prevsol["data"][ycolumns[j]]]
                        labels = []
                    x.append(sol["data"][xcolumns[j]]) 
                    y.append(sol["data"][ycolumns[j]])
                    
                    TYnumber = sol["TY number"]
                    lab = sol["LAB"]
                    if TYnumber != 0 or lab != 0:
                        if lab != 0:
                            text = str(lab)
                        else:
                            text = ""
                        if TYnumber == 4 or TYnumber == 9:
                            symbol = None
                        elif TYnumber == 1 or TYnumber == 6: 
                            symbol = self.cget("bifurcation_symbol")
                        elif TYnumber == 2 or TYnumber == 5: 
                            symbol = self.cget("limit_point_symbol")
                        elif TYnumber == 3: 
                            symbol = self.cget("hopf_symbol")
                        elif TYnumber == 7: 
                            symbol = self.cget("period_doubling_symbol")
                        elif TYnumber == 8: 
                            symbol = self.cget("torus_symbol")
                        elif TYnumber == -4: 
                            symbol = self.cget("user_point_symbol")
                        elif TYnumber != 0:
                            symbol = self.cget("error_symbol")
                        labels.append({"index": current_index, "text": text,
                                       "symbol": symbol})
                    current_index = current_index + 1
                    prevsol = sol
                if len(x) > 0:
                    if dp:
                        self.addArrayNoDraw((x,y),stable=prevsol['PT'] < 0)
                    else:
                        self.addArrayNoDraw((x,y))
                for label in labels:
                    self.addLabel(len(self)-1,label["index"],label["text"],label["symbol"])
        
        # Call the base class config
        xlabel = self["xlabel"]
        if self.config("xlabel")[3] is None:
            xlabel = "Column %d"%xcolumns[0]
        ylabel = self["ylabel"]
        if self.config("ylabel")[3] is None:
            ylabel = "Column %d"%ycolumns[0]
        grapher.GUIGrapher._configNoDraw(self,xlabel=xlabel,ylabel=ylabel)

    def __plot8(self):
        self.delAllData()
        xcolumns = self.cget("solution_x")
        ycolumns = self.cget("solution_y")

        if len(xcolumns) == len(ycolumns):
            xnames="Error"
            ynames="Error"
            index = 9
            for ind in self.cget("index"):
                sol = self.cget("solution").getIndex(ind)
                solution = sol["data"]
                label = sol["Label"]
                xnames = ""
                ynames = ""
                for j in range(len(xcolumns)):
                    labels = []
                    xc = xcolumns[j]
                    yc = ycolumns[j]
                    if xc == "t":
                        x = map(lambda s: s["t"], solution)
                    else:
                        x = map(lambda s, c = xc: s["u"][c], solution)
                    if yc == "t":
                        y = map(lambda s: s["t"], solution)
                    else:
                        y = map(lambda s, c = yc: s["u"][c], solution)

                    if not(self.cget("mark_t") is None):
                        for i in range(len(solution)):
                            if i != 0 and solution[i-1]["t"] <= self.cget("mark_t") < solution[i]["t"]:
                                labels.append({"index": i,
                                               "text": "",
                                               "symbol": "fillcircle"})
                    if len(solution) <= 15:
                        index = 1
                        if len(solution) <= 1:
                            index = 0
                    labels.append({"index": index, "text": str(label), "symbol": ""})
                    # Call the base class config
                    if len(x) > 0:
                        self.addArrayNoDraw((x,y))
                    for label in labels:
                        self.addLabel(len(self)-1,label["index"],label["text"],label["symbol"])

                    xnames = xnames + " " + str(xcolumns[j])
                    ynames = ynames + " " + str(ycolumns[j])
                index = index + 10
                if index > len(solution):
                    index = 14
            xlabel = self["xlabel"]
            if self.config("xlabel")[3] is None:
                xlabel = "Columns %s"%xnames
            ylabel = self["ylabel"]
            if self.config("ylabel")[3] is None:
                ylabel = "Columns %s"%ynames
            grapher.GUIGrapher._configNoDraw(self,xlabel=xlabel,ylabel=ylabel)


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





