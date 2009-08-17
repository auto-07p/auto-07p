#! /usr/bin/env python
try:
    from graphics import grapher_mpl as grapher
except ImportError:
    from graphics import grapher
    print("Using plain TkInter for plotting. You can obtain better quality graphics")
    print("using matplotlib (http://matplotlib.sf.net).")
import parseB
import parseS
import AUTOutil
import os

class plotter(grapher.GUIGrapher):
    def __init__(self,parent=None,**kw):

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
        if kw.has_key("hide"):
            optionDefaultsRC["hide"] = kw["hide"]

        self.__needsPlot = None
        grapher.GUIGrapher.__init__(self,parent,**optionDefaultsRC)

        self.addOptions(**optionDefaults)
        self.addRCOptions(**optionDefaultsRC)
        plotter._configNoDraw(self,**optionDefaultsRC)
        plotter._configNoDraw(self,**kw)
        self._plotNoDraw()
        self.__needsPlot = None
        if "minx" not in kw or "maxx" not in kw:
            self.computeXRange()
        if "miny" not in kw or "maxy" not in kw:
            self.computeYRange()
        grapher.GUIGrapher.plot(self)

    def config(self,cnf=None,**kw):
        rval = self._configNoDraw(cnf,**kw)
        if isinstance(cnf, str) or (cnf is None and not kw):
            return rval
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
        return grapher.GUIGrapher._configNoDraw(self,cnf,**kw)
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
            if type(value) != type([]) and type(value) != type(()):
                value = [value]
            options["index"] = [j for v in value
                    for j, l in enumerate(labels) if l == v]
        elif key == "index":
            labels = self.cget("solution").getLabels()
            options["label"] = [labels[v] for v in value]
        elif key in ["bifurcation_x","bifurcation_y",
                     "solution_x","solution_y"]:
            if type(value) != type([]) and type(value) != type(()):
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
            if (type(columns[coord]) != type([]) and
                type(columns[coord]) != type(())):
                columns[coord] = [columns[coord]]
        names, columns = self.__makeaxistitles(columns[0],columns[1])
        m = max([len(column) for column in columns])
        for coord, column in enumerate(columns):
            if column is not None and len(column) == 1:
                columns[coord] = column * m
        plot = True
        if ty == "bifurcation":
            sol = self.cget(ty+"_diagram")
        else:
            sol = self.cget(ty)
        if ty == "bifurcation" and len(sol) and len(sol[0]):
            self.__plot7(columns[0],columns[1])
        elif ty == "solution" and len(sol) > 0:
            self.__plot8(columns[0],columns[1])
        else:
            plot = False
        if plot:
            label = {}
            for coord in ["x","y"]:
                label[coord] = self[coord+"label"]
                if self.config(coord+"label")[3] is None:
                    label[coord] = ", ".join(names[{"x": 0, "y": 1}[coord]])
            grapher.GUIGrapher._configNoDraw(self,xlabel=label["x"],
                                             ylabel=label["y"])

    def plot(self):
        self._plotNoDraw()
        self.clear()
        self.computeXRange()
        self.computeYRange()
        grapher.GUIGrapher.plot(self)
        self.draw()

    def __makeaxistitles(self,xcolumns,ycolumns):
        # parse coordinate names from bifurcation diagram/solution
        # then construct the titles from these names
        ty = self.cget("type")
        indepvarname = None
        if ty == "bifurcation":
            solution = self.cget(ty+"_diagram")
            # use the "branches" member for parseB objects
            solution = getattr(solution,"branches",solution)
        else:
            solution = self.cget(ty)
        parsecoordnames = []
        # first go by column
        for s in solution:
            # check for any "MAX"-style member and replace, e.g., U(1) by
            # MAX U(1)
            if ty == "bifurcation":
                for i in range(min(len(parsecoordnames),len(s.coordnames))):
                    name = s.coordnames[i]
                    namelist = name.split(None,1)
                    if (len(namelist) > 1 and 
                        namelist[0] in ["MAX", "MIN", "INTEGRAL", "L2-NORM"] and
                        parsecoordnames[i] == namelist[1].strip()):
                        parsecoordnames[i] = name.strip()
            elif indepvarname is None:
                indepvarname = s.indepvarname
            if len(s.coordnames) > len(parsecoordnames):
                parsecoordnames.extend([name.strip() for name in
                                        s.coordnames[len(parsecoordnames):]])
        # then add anything not already there
        for s in solution:
            for name in s.coordnames:
                sname = name.strip()
                if sname not in parsecoordnames:
                    parsecoordnames.append(sname)
                                    
        # override coordnames with user/autorc provided ones
        if ty == "solution":
            if self.cget("solution_indepvarname"):
                indepvarname = self.cget("solution_indepvarname")
            elif indepvarname is None:
                indepvarname = "t"
        coordnames = self.cget(ty+"_coordnames") or []
        # but if that list is too short, extend it
        if len(coordnames) < len(parsecoordnames):
            coordnames.extend(parsecoordnames[len(coordnames):])
        self._coordnames = coordnames

        # construct titles
        names = ["Error","Error"]
        lx = len(xcolumns)
        ly = len(ycolumns)
        if len(solution) > 0 and (lx == ly or lx == 1 or ly == 1):
            names = [[],[]]
            for j in range(2):
                columns = [xcolumns,ycolumns][j]
                for col in columns:
                    if type(col) == type(1):
                        # numerical column: check limits
                        if indepvarname is not None and col == -1:
                            col = indepvarname
                        else:
                            for s in solution:
                                if col < len(s.coordnames):
                                    col = coordnames[col]
                                    break
                            if type(col) == type(1):
                                print("The %s-coordinate (set to column %s) "
                                      "is out of range"%(["x","y"][j],col))
                                col = "Error"
                    elif col not in coordnames and (indepvarname is None or
                                                    col != indepvarname):
                        print("Unknown column name: %s"%(col))
                        col = "Error"
                    names[j].append(col)

        # translate xcolumns/ycolumns to use parsed coordnames
        ucoordnames = self.cget(ty+"_coordnames") or []
        cols = [xcolumns,ycolumns]
        if ucoordnames != []:
            cols = []
            for columns in [xcolumns,ycolumns]:
                ncol = []
                for col in columns:
                    try:
                        indx = ucoordnames.index(col)
                        col = parsecoordnames[indx]
                    except ValueError:
                        pass
                    ncol.append(col)
                cols.append(ncol)
        return names,cols

    def __plot7branch(self,branch,xcolumns,ycolumns):
        symbollist = [
            [[1,6], "bifurcation_symbol"],
            [[2,5], "limit_point_symbol"],
            [[3],   "hopf_symbol"],
            [[7],   "period_doubling_symbol"],
            [[8],   "torus_symbol"],
            [[-4],  "user_point_symbol"]]
        dp = self.cget("stability")
        coordnames = branch.coordnames
        for j in range(len(xcolumns)):
            xycols = []
            for col in [xcolumns[j],ycolumns[j]]:
                if type(col) != type(1):
                    try:
                        col = coordnames.index(col)
                    except ValueError:
                        # check if we have an item that starts with
                        # MAX, MIN, INTEGRAL, or L2-NORM
                        # in that case also plot U(1) if given MAX U(1)
                        namelist = col.split(None,1)
                        if len(namelist) < 2 or (namelist[0] not in 
                                  ["MAX", "MIN", "INTEGRAL", "L2-NORM"]):
                            break
                        try:
                            col = coordnames.index(namelist[1])
                        except ValueError:
                            break
                try:
                    xy = branch.coordarray[col]
                except IndexError:
                    break
                xycols.append(xy)
            if len(xycols) < 2:
                continue
            [x,y] = xycols
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

    def __plot7(self,xcolumns,ycolumns):
        self.delAllData()
        solution = self.cget("bifurcation_diagram")
        branches = getattr(solution,"branches",solution)
        if len(xcolumns) == len(ycolumns):
            for branch in branches:
                self.__plot7branch(branch,xcolumns,ycolumns)

    def __plot8solution(self,sol,index,xcolumns,ycolumns):
        indepvarname = sol.indepvarname
        tm = sol[indepvarname]
        label = sol["Label"]
        if self.cget("solution_indepvarname"):
            indepvarname = self.cget("solution_indepvarname")
        coordnames = sol.coordnames
        for j in range(len(xcolumns)):
            labels = []
            xycols = []
            for col in [xcolumns[j],ycolumns[j]]:
                if type(col) != type(1):
                    if indepvarname == col:
                        col = -1
                    else:
                        try:
                            col = coordnames.index(col)
                        except ValueError:
                            break
                if col == -1:
                    xy = tm
                else:
                    try:
                        xy = sol.coordarray[col]
                    except IndexError:
                        break
                xycols.append(xy)
            if len(xycols) < 2:
                continue
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

        index = index + 10
        if index > len(tm):
            index = 14
        return index

    def __plot8(self,xcolumns,ycolumns):
        self.delAllData()
        solution = self.cget("solution")
        if len(xcolumns) == len(ycolumns):
            index = 9
            for ind in self.cget("index"):
                sol = solution.getIndex(ind)
                index = self.__plot8solution(sol,index,xcolumns,ycolumns)



def test():
    import parseB
    import parseS
    import sys
    foo = plotter(bifurcation_diagram_filename="../test_data/fort.7",
                  solution_filename="../test_data/fort.8")

    foo.plot()
    foo.pack()
    foo.update()
    print("Hit return to continue")
    raw_input()
    foo.clear()
    foo.update()
    print("Hit return to continue")
    raw_input()
    foo.clear()
    foo.config(type="solution",label=[6])
    foo.plot()
    foo.update()
    print("Hit return to continue")
    raw_input()
    foo.clear()
    foo.config(index=[3])
    foo.plot()
    foo.update()
    print("Hit return to continue")
    raw_input()

if __name__ == "__main__":
    test()





