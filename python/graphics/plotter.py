#! /usr/bin/env python
try:
    import grapher_mpl
    grapher = grapher_mpl
except ImportError:
    import grapher
    print "Using plain TkInter for plotting. You can obtain better quality graphics"
    print "using matplotlib (http://matplotlib.sf.net)."
import parseB
import parseS
import AUTOutil
import types
import os

class plotter(grapher.GUIGrapher):
    def __init__(self,parent=None,cnf={},**kw):

        optionDefaults = {}
        # The kind of diagram (single solution vs. bifur diagram)
        optionDefaults["type"]     = ("bifurcation",self.__optionCallback)  
        # The X column
        optionDefaults["bifurcation_x"] = ([0],self.__optionCallback)
        optionDefaults["solution_x"]    = ([-1],self.__optionCallback)
        # The Y column
        optionDefaults["bifurcation_y"] = ([1],self.__optionCallback)
        optionDefaults["solution_y"]    = ([0],self.__optionCallback)
        # The coordinate names
        optionDefaults["bifurcation_coordnames"] = (None,self.__optionCallback)
        optionDefaults["solution_indepvarname"]  = (None,self.__optionCallback)
        optionDefaults["solution_coordnames"]    = (None,self.__optionCallback)
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

        parser = AUTOutil.getAUTORC("AUTO_plotter")
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
            try:
                self.cget("bifurcation_diagram").readFilename(value)
            except IOError:
                pass
        elif key == "solution_filename":
            try:
                self.cget("solution").readFilename(value)
            except IOError:
                pass
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
        elif key in ["bifurcation_x","bifurcation_y",
                     "solution_x","solution_y"]:
            if type(value) != type([]):
                value = [value]
        # We only recreate the data if one of the above options gets set
        # We can't just recreate the data for any option since:
        #   1)  It is inefficient
        #   2)  The range gets recomputed when we create new data, so
        #       minx, miny, maxx, and maxy all never get used.
        self.__needsPlot = "yes"

    def _plotNoDraw(self):
        self.delAllData()
        ty = self.cget("type")
        columns = [self.cget(ty+"_x"),self.cget(ty+"_y")]
        for coord in range(2):
            if type(columns[coord]) != type([]):
                columns[coord] = [columns[coord]]
            if len(columns[coord]) == 1:
                columns[coord] = columns[coord] * len(columns[1-coord])
        [xcolumns,ycolumns] = columns
        xnames = None
        if (ty == "bifurcation" and len(self.cget("bifurcation_diagram")) and
            len(self.cget("bifurcation_diagram")[0])):
            xnames, ynames = self.__plot7(columns[0],columns[1])
        if ty == "solution" and len(self.cget("solution")) > 0:
            xnames, ynames = self.__plot8(columns[0],columns[1])
        if xnames is not None:
            label = {}
            for coord in ["x","y"]:
                label[coord] = self[coord+"label"]
                if self.config(coord+"label")[3] is None:
                    names = {"x": xnames, "y": ynames}[coord]
                    columns = {"x": xcolumns, "y": ycolumns}[coord]
                    origcolumns = self.cget(ty+"_"+coord)
                    if type(origcolumns) != type([]) or len(origcolumns) == 1:
                        label[coord] = names[2:len(names)/len(columns)]
                    else:
                        label[coord] = names[2:]
            grapher.GUIGrapher._configNoDraw(self,xlabel=label["x"],
                                             ylabel=label["y"])

    def plot(self):
        self._plotNoDraw()
        self.clear()
        self.computeXRange()
        self.computeYRange()
        grapher.GUIGrapher.plot(self)
        self.draw()

    def __plot7(self,xcolumns,ycolumns):
        symbollist = [
            [[1,6], "bifurcation_symbol"],
            [[2,5], "limit_point_symbol"],
            [[3],   "hopf_symbol"],
            [[7],   "period_doubling_symbol"],
            [[8],   "torus_symbol"],
            [[-4],  "user_point_symbol"]]
        self.delAllData()
        solution = self.cget("bifurcation_diagram")
        coordnames = []
        if hasattr(solution,"coordnames"):
            coordnames = solution.coordnames
            branches = solution.branches
        else:
            if len(solution) > 0:
                coordnames = solution[0].coordnames
            branches = solution
        if self.cget("bifurcation_coordnames"):
            coordnames = self.cget("bifurcation_coordnames")
        dp = self.cget("stability")
        xnames="Error"
        ynames="Error"
        if len(solution) > 0 and len(xcolumns) == len(ycolumns):
            xnames = ""
            ynames = ""
            for j in range(len(xcolumns)):
                cols = []
                for col in [xcolumns[j],ycolumns[j]]:
                    if type(col) != type(1):
                        try:
                            col = coordnames.index(col)
                        except ValueError:
                            print "Unknown column name: %s"%(col)
                            col = 0
                    cols.append(col)
                [xcol,ycol] = cols
                for branch in branches:
                    try:
                        x = branch.coordarray[xcol]
                    except IndexError:
                        print "The x-coordinate (set to column %s) is out of range"%xcol
                        break
                    try:
                        y = branch.coordarray[ycol]
                    except IndexError:
                        print "The y-coordinate (set to column %s) is out of range"%ycol
                        break
                    if dp:
                        #look at stability:
                        newsect = 1
                        old = 0
                        for pt in branch.stability:
                            abspt = abs(pt)
                            if abspt > 1 or pt == branch.stability[-1]:
                                self.addArrayNoDraw((x[old:abspt],y[old:abspt]),
                                                    newsect,stable=pt<0)
                                old = abspt - 1
                                newsect = 0
                    else:
                        self.addArrayNoDraw((x,y),1)
                    for i,l in branch.labels.sortByIndex():
                        for k,v in l.items():
                            if k in parseB.all_point_types:
                                label = v
                                break
                        lab = label["LAB"]
                        TYnumber = label["TY number"]
                        if TYnumber>=0:
                            TYnumber=TYnumber%10
                        else:
                            TYnumber=-((-TYnumber)%10)
                        text = ""
                        if lab != 0:
                            text = str(lab)
                        symbol = None
                        for item in symbollist:
                            if TYnumber in item[0]:
                                symbol = self.cget(item[1])
                        if not symbol and TYnumber not in [0,4,9]:
                            symbol = self.cget("error_symbol")
                        self.addLabel(len(self)-1,[x[i],y[i]],text,symbol)
                xnames = xnames + ", " + coordnames[xcol]
                ynames = ynames + ", " + coordnames[ycol]
        return xnames, ynames
        
    def __plot8(self,xcolumns,ycolumns):
        self.delAllData()
        indepvarname = self.cget("solution").indepvarname
        if self.cget("solution_indepvarname"):
            indepvarname = self.cget("solution_indepvarname")
        coordnames = self.cget("solution").coordnames
        if self.cget("solution_coordnames"):
            coordnames = self.cget("solution_coordnames")

        xnames="Error"
        ynames="Error"
        if len(xcolumns) == len(ycolumns):
            index = 9
            for ind in self.cget("index"):
                sol = self.cget("solution").getIndex(ind)
                tm = sol[sol.indepvarname]
                label = sol["Label"]
                xnames = ""
                ynames = ""
                for j in range(len(xcolumns)):
                    labels = []
                    cols = []
                    xycols = []
                    for col in [xcolumns[j],ycolumns[j]]:
                        if type(col) == type(1):
                            if col == -1:
                                col = indepvarname
                            else:
                                col = coordnames[col]
                        if indepvarname == col:
                            xy = tm
                        else:
                            try:
                                xy = sol[sol.coordnames[coordnames.index(col)]]
                            except ValueError:
                                print "Unknown column name: %s"%(col)
                                xy = tm
                                col = 't'
                        cols.append(col)
                        xycols.append(xy)
                    [x,y] = xycols
                    if not(self.cget("mark_t") is None):
                        for i in range(len(tm)):
                            if i != 0 and tm[i-1] <= self.cget("mark_t") < tm[i]:
                                labels.append({"index": i,
                                               "text": "",
                                               "symbol": "fillcircle"})
                    if len(tm) <= 15:
                        index = 1
                        if len(tm) <= 1:
                            index = 0
                    labels.append({"index": index, "text": str(label), "symbol": ""})
                    # Call the base class config
                    if len(x) > 0:
                        self.addArrayNoDraw((x,y))
                    for lab in labels:
                        self.addLabel(len(self)-1,
                                      [x[lab["index"]],y[lab["index"]]],
                                      lab["text"],lab["symbol"])

                    xnames = xnames + ", " + cols[0]
                    ynames = ynames + ", " + cols[1]
                index = index + 10
                if index > len(tm):
                    index = 14
        return xnames,ynames


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





