#! /usr/bin/env python
try:
    from graphics import grapher_mpl as grapher
except ImportError:
    from graphics import grapher
    print("Using plain TkInter for plotting. You can obtain better quality graphics")
    print("using matplotlib (http://matplotlib.sf.net).")
import parseB
import parseS
import parseC
import AUTOutil
import os
import gc
Axes3D = grapher.Axes3D

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
        # The Z column
        optionDefaults["bifurcation_z"] = (None,self.__optionCallback)
        optionDefaults["solution_z"]    = (None,self.__optionCallback)
        # The coordinate names
        optionDefaults["bifurcation_coordnames"] = (None,self.__optionCallback)
        optionDefaults["solution_indepvarname"]  = (None,self.__optionCallback)
        optionDefaults["solution_coordnames"]    = (None,self.__optionCallback)
        optionDefaults["labelnames"]             = (None,self.__optionCallback)
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

        optionDefaults["letter_symbols"] = (True,self.__optionCallback)
        optionDefaults["bifurcation_symbol"]     = ("B",self.__optionCallback)
        optionDefaults["limit_point_symbol"]     = ("L",self.__optionCallback)
        optionDefaults["hopf_symbol"]            = ("H",self.__optionCallback)
        optionDefaults["zero_hopf_symbol"]       = ("ZH",self.__optionCallback)
        optionDefaults["bogdanov_takens_symbol"] = ("BT",self.__optionCallback)
        optionDefaults["cusp_symbol"]            = ("CP",self.__optionCallback)
        optionDefaults["generalized_hopf_symbol"]= ("GH",self.__optionCallback)
        optionDefaults["1_1_resonance_symbol"]   = ("R1",self.__optionCallback)
        optionDefaults["1_2_resonance_symbol"]   = ("R2",self.__optionCallback)
        optionDefaults["1_3_resonance_symbol"]   = ("R3",self.__optionCallback)
        optionDefaults["1_4_resonance_symbol"]   = ("R4",self.__optionCallback)
        optionDefaults["fold_flip_symbol"]       = ("LPD",self.__optionCallback)
        optionDefaults["fold_torus_symbol"]      = ("LTR",self.__optionCallback)
        optionDefaults["flip_torus_symbol"]      = ("PTR",self.__optionCallback)
        optionDefaults["torus_torus_symbol"]     = ("TTR",self.__optionCallback)
        optionDefaults["period_doubling_symbol"] = ("D",self.__optionCallback)
        optionDefaults["torus_symbol"]           = ("T",self.__optionCallback)
        optionDefaults["user_point_symbol"]      = ("U",self.__optionCallback)
        optionDefaults["error_symbol"]           = ("X",self.__optionCallback)

        optionDefaults["ps_colormode"]           = ("color",self.__optionCallback)
        optionDefaults["stability"]              = (False,self.__optionCallback)
        optionDefaults["coloring_method"]        = ("curve",self.__optionCallback)

        parser = AUTOutil.getAUTORC("AUTO_plotter")
        optionDefaultsRC = {}
        c = parseC.parseC()
        for option in parser.options("AUTO_plotter"):
            optionDefaultsRC[option] = self.parseoption(
                option,parser.get("AUTO_plotter",option),c)
        # Let these override the RC options, if specified.
        for key in ["hide","xlabel","ylabel","zlabel"]:
            if key in kw:
                optionDefaultsRC[key] = kw[key]

        self.__needsPlot = None
        grapher.GUIGrapher.__init__(self,parent,**optionDefaultsRC)

        self.addOptions(**optionDefaults)
        self.addRCOptions(**optionDefaultsRC)
        for options in [optionDefaultsRC, kw]:
            if "letter_symbols" in options:
                self.__optionCallback("letter_symbols",
                                      options["letter_symbols"], options)
                del options["letter_symbols"]
        plotter._configNoDraw(self,**optionDefaultsRC)
        plotter._configNoDraw(self,**kw)
        self._plotNoDraw()
        self.__needsPlot = None
        for coord in 'x', 'y', 'z':
            if "min"+coord not in kw or "max"+coord not in kw:
                self.computeRange(coord,kw.get("min"+coord),
                                  kw.get("max"+coord))
        grapher.GUIGrapher.plot(self)

    def parselist(self,option,v,c,sep):
        quote = ' '
        i = 0
        vlist = []
        level = 0
        for j, ch in enumerate(v):
            if ch == '[' and level == 0 and option[-9:] == "_defaults":
                level += 1
                vlist1 = []
                i = j+1
                continue
            if level == 1 and quote == ' ' and ch == ']':
                s = v[i:j]
                i = j+1
            elif j == len(v) - 1:
                s = v[i:]
            elif ch in sep and quote == ' ':
                s = v[i:j]
                i = j+1
            else:
                if quote == ' ':
                    if ch in ['"',"'"]:
                        quote = ch
                elif ch == quote:
                    quote = ' '
                continue
            s = s.strip()
            if len(s) == 0 or s[0] not in ['"',"'"]:
                try:
                    int(s)
                except ValueError:
                    s = "'"+s+"'"
            if level == 1:
                vlist1.append(s)
                if ch == ']':
                    level -= 1
                    vlist1 = c.scanvalue("["+",".join(vlist1)+"]")[0]
                    for i1, l1 in enumerate(vlist1):
                        try:
                            vlist1[i1] = int(l1)
                        except ValueError:
                            pass
                    vlist.append(vlist1)
            elif option[-9:] != "_defaults":
                vlist.append(s)
        if option[-9:] == "_defaults":
            return vlist
        v = c.scanvalue("["+",".join(vlist)+"]")[0]
        for i, l in enumerate(v):
            try:
                v[i] = int(l)
            except ValueError:
                pass
        return v

    def parseoption(self,option,v,c):
        special = {'None': None, 'True': True, 'False': False,
                   'Yes': True, 'No': False}
        i = 0
        quoted = False
        while i < len(v) and v[i] in ['"',"'"]:
            q = v[i]
            # look for endquote
            j = v.find(q,i+1)
            if j < 0:
                break
            # remove quotes
            quoted = True
            v = v[:i]+v[i+1:j]+v[j+1:]
            i = j - 1
        # remove comment after last quote
        i = v.find('#',i+1)
        if i >= 0:
            v = v[:i].strip()
        if quoted:
            return v
        w = v.capitalize()
        if w in special:
            v = special[w]
        elif (option[-2:] in ['_x','_y','_z'] or
              option[-9:] == "_defaults" or
              option[-11:] == "_coordnames" or
              option in ["label","index","dashes","labelnames"] or
              (option[0] == 'd' and len(option) == 2)):
            # convert to list, quote args if not numbers,
            # then use the same method as for constant files
            if ((option[0] == 'd' and len(option) == 2) or
                option == "labelnames"):
                brackets = ['{']
                sep = [',',':']
            else:
                brackets = ['[','(']
                sep = [',']
            if len(v) > 0 and v[0] in brackets:
                endbracket = {'{':'}','[':']','(':')'}[v[0]]
                v = v[1:]
                if len(v) > 0 and v[-1] == endbracket:
                    v = v[:-1]
            v = self.parselist(option,v,c,sep)
            if ((option[0] == 'd' and len(option) == 2) or
                option == "labelnames"):
                v = dict([(v[i],v[i+1]) for i in range(0,len(v),2)])
                if option != "labelnames":
                    # Convert "True"/"False"/"yes"/"no" to True/False
                    for k in v:
                        try:
                            w = v[k].capitalize()
                            if w in special:
                                v[k] = special[w]
                        except AttributeError:
                            pass
        else: # simple value
            try:
                v = int(v)
            except ValueError:
                try:
                    v = float(v)
                except ValueError:
                    pass
        return v

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
            self.computeZRange()
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
        gc.collect()
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
        elif key == "solution":
            if self.cget("label") != [0]:
                self.__optionCallback("label",self.cget("label"),options)
        elif key == "label":
            labels = self.cget("solution").getLabels()
            if not isinstance(value, (list, tuple)):
                value = [value]
            if value in [["all"], ["All"]]:
                value = labels
            options["index"] = [j for v in value
                    for j, l in enumerate(labels) if l == v]
        elif key == "index":
            labels = self.cget("solution").getLabels()
            if not isinstance(value, (list, tuple)):
                value = [value]
            options["label"] = [labels[v] for v in value]
        elif key in ["bifurcation_x","bifurcation_y","bifurcation_z",
                     "solution_x","solution_y","solution_z"]:
            if not isinstance(value, (list, tuple)):
                value = [value]
        elif key == "letter_symbols":
            keys = ["bifurcation_symbol", "limit_point_symbol",
                    "hopf_symbol", "period_doubling_symbol","torus_symbol",
                    "user_point_symbol", "error_symbol",
                    "1_1_resonance_symbol", "1_2_resonance_symbol",
                    "1_3_resonance_symbol", "1_4_resonance_symbol",
                    "bogdanov_takens_symbol", "cusp_symbol",
                    "generalized_hopf_symbol", "zero_hopf_symbol",
                    "fold_flip_symbol", "fold_torus_symbol",
                    "flip_torus_symbol", "torus_torus_symbol"]
            if value:
                values = ["B", "L", "H", "D", "T", "U", "X",
                          "R1", "R2", "R3", "R4", "BT", "CP", "GH", "ZH",
                          "LPD", "LTR", "PTR", "TTR"]
            else:
                values = ["square", None,
                          "fillsquare", "diamond", "filldiamond",
                          None, None,
                          "filldiamond", "filldiamond",
                          "filldiamond", "filldiamond",
                          "circle", None,
                          "triangle", "doubletriangle",
                          "doubletriangle", "doubletriangle",
                          "doubletriangle", "doubletriangle"]
            options.update(dict(zip(keys, values)))

        # We only recreate the data if one of the above options gets set
        # We can't just recreate the data for any option since:
        #   1)  It is inefficient
        #   2)  The range gets recomputed when we create new data, so
        #       minx, miny, maxx, and maxy all never get used.
        self.__needsPlot = "yes"

    def _plotNoDraw(self):
        self.delAllData()
        ty = self.cget("type")
        columns = [self.cget(ty+"_x"),self.cget(ty+"_y"),self.cget(ty+"_z")]
        for coord in range(3):
            if (type(columns[coord]) != type([]) and
                type(columns[coord]) != type(())):
                columns[coord] = [columns[coord]]
        names, columns = self.__makeaxistitles(*columns)
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
            self.__plot7(*columns)
        elif ty == "solution" and len(sol) > 0:
            self.__plot8(*columns)
        else:
            plot = False
        if plot:
            label = {}
            for coord in "x","y","z":
                label[coord] = self[coord+"label"]
                if self.config(coord+"label")[3] is None:
                    namescoord = names[{"x": 0, "y": 1, "z": 2}[coord]]
                    if namescoord is None:
                        label["z"] = None
                    else:
                        label[coord] = ", ".join(namescoord)
            grapher.GUIGrapher._configNoDraw(self,xlabel=label["x"],
                                             ylabel=label["y"],
                                             zlabel=label["z"])

    def plot(self):
        self._plotNoDraw()
        self.clear()
        self.computeXRange()
        self.computeYRange()
        self.computeZRange()
        grapher.GUIGrapher.plot(self)
        self.draw()

    def __makeaxistitles(self,xcolumns,ycolumns,zcolumns):
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
        coordnames = coordnames[:]
        # but if that list is too short, extend it
        if len(coordnames) < len(parsecoordnames):
            coordnames.extend(parsecoordnames[len(coordnames):])
        self._coordnames = coordnames

        # construct translation dictionary for unames/parnames in autorc
        labelnames = self.cget("labelnames") or {}
        labelnames = labelnames.copy()
        for key in list(labelnames):
            for prefix in "MIN ", "MAX ", "INTEGRAL ", "L2-NORM ":
                if prefix+key not in labelnames:
                    labelnames[prefix+key] = prefix+labelnames[key]

        # construct titles
        names = [["Error"],["Error"]]
        lx = len(xcolumns)
        ly = len(ycolumns)
        lz = len(zcolumns)
        if lx == 1: lx = max(ly, lz)
        if ly == 1: ly = max(lx, lz)
        if lz == 1: lz = max(lx, ly)
        if (len(solution) > 0 and lx == ly == lz and 
            (ty == "solution" or len(solution[0]) > 0)):
            names = [[],[],[]]
            for j in range(3):
                columns = [xcolumns,ycolumns,zcolumns][j]
                for col in columns:
                    if col is None:
                        names[j] = None
                        continue
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
                                      "is out of range"%(["x","y","z"][j],col))
                                col = "Error"
                    elif col not in coordnames and (indepvarname is None or
                                                    col != indepvarname):
                        print("Unknown column name: %s"%(col))
                        col = "Error"
                    names[j].append(labelnames.get(col, col))

        # translate xcolumns/ycolumns to use parsed coordnames
        ucoordnames = self.cget(ty+"_coordnames") or []
        cols = [xcolumns,ycolumns,zcolumns]
        if ucoordnames != []:
            cols = []
            for columns in [xcolumns,ycolumns,zcolumns]:
                ncol = []
                for col in columns:
                    try:
                        indx = ucoordnames.index(col)
                        col = parsecoordnames[indx]
                    except (ValueError, TypeError):
                        pass
                    ncol.append(col)
                cols.append(ncol)
        return names,cols

    def __plot7branch(self,branch,xcolumns,ycolumns,zcolumns):
        symbollist = [
            [[1,6], "bifurcation_symbol"],
            [[2,5], "limit_point_symbol"],
            [[3],   "hopf_symbol"],
            [[-1],  "bogdanov_takens_symbol"],
            [[-2],  "cusp_symbol"],
            [[-32], "generalized_hopf_symbol"],
            [[-3],  "zero_hopf_symbol"],
            [[7],   "period_doubling_symbol"],
            [[8],   "torus_symbol"],
            [[-5],  "1_1_resonance_symbol"],
            [[-6],  "1_2_resonance_symbol"],
            [[-7],  "1_3_resonance_symbol"],
            [[-8],  "1_4_resonance_symbol"],
            [[23,83], "fold_torus_symbol"],
            [[77,87], "flip_torus_symbol"],
            [[28,78], "fold_flip_symbol"],
            [[88],  "torus_torus_symbol"],
            [[-4],  "user_point_symbol"]]
        specialsymbols = [-32,23,83,77,87,28,78,88]
        dp = self.cget("stability")
        coordnames = branch.coordnames
        for j in range(len(xcolumns)):
            labels = []
            for i,l in branch.labels.sortByIndex():
                label = None
                for k in l:
                    v = l[k]
                    if "LAB" in v:
                        label = v
                        break
                if label is None:
                    continue
                lab = label["LAB"]
                TYnumber = label["TY number"]
                if TYnumber not in specialsymbols:
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
                        symbol = item[1]
                if symbol is not None:
                    symbol = self.cget(symbol)
                elif TYnumber not in [0,4,9]:
                    symbol = self.cget("error_symbol")
                labels.append([i, text, symbol])
            xycols = []
            for col in [xcolumns[j],ycolumns[j],zcolumns[j]]:
                if col is None:
                    break
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
            if len(xycols) < 2 or (zcolumns[j] is not None and len(xycols) < 3):
                continue
            [x,y] = xycols[:2]
            if len(xycols) == 3:
                z = xycols[2]
            else:
                z = None
            if self.cget("coloring_method") == "branch":
                color = abs(branch.BR)-1
            elif self.cget("coloring_method") == "type":
                color = branch.TY
            else:
                color = None
            if dp:
                #look at stability:
                newsect = 1
                old = 0
                stability = branch.stability()
                addedlabels = []
                for pt in stability:
                    abspt = abs(pt)
                    if abspt > 1 or pt == stability[-1]:
                        if z is None:
                            v = x[old:abspt],y[old:abspt]
                        else:
                            v = x[old:abspt],y[old:abspt],z[old:abspt]
                        self.addArrayNoDraw(v,newsect,color,stable=pt<0)
                        for label in labels:
                            if (old <= label[0] and label[0] < abspt and
                                label not in addedlabels):
                                self.addLabel(len(self)-1, label[0] - old,
                                              label[1], label[2])
                                addedlabels.append(label)
                        old = abspt - 1
                        newsect = 0
            else:
                if z is None:
                    self.addArrayNoDraw((x,y),1,color)
                else:
                    self.addArrayNoDraw((x,y,z),1,color)
                for label in labels:
                    self.addLabel(len(self)-1, *label)

    def __plot7(self,xcolumns,ycolumns,zcolumns):
        self.delAllData()
        solution = self.cget("bifurcation_diagram")
        branches = getattr(solution,"branches",solution)
        if len(xcolumns) == len(ycolumns) == len(zcolumns):
            for branch in branches:
                self.__plot7branch(branch,xcolumns,ycolumns,zcolumns)

    def __plot8solution(self,sol,index,xcolumns,ycolumns,zcolumns):
        indepvarname = sol.indepvarname
        tm = sol[indepvarname]
        label = sol["Label"]
        if self.cget("solution_indepvarname"):
            indepvarname = self.cget("solution_indepvarname")
        coordnames = sol.coordnames
        if self.cget("coloring_method") == "branch":
            color = sol["BR"]-1
        elif self.cget("coloring_method") == "type":
            color = sol["TY number"]
        else:
            color = None
        for j in range(len(xcolumns)):
            labels = []
            xycols = []
            for col in [xcolumns[j],ycolumns[j],zcolumns[j]]:
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
            [x,y] = xycols[:2]
            z = None
            if len(xycols) == 3:
                z = xycols[2]
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
                if z is None:
                    self.addArrayNoDraw((x,y),color=color)
                else:
                    self.addArrayNoDraw((x,y,z),color=color)
            for lab in labels:
                self.addLabel(len(self)-1, lab["index"], lab["text"],
                              lab["symbol"])

        index = index + 10
        if index > len(tm):
            index = 14
        return index

    def __plot8(self,xcolumns,ycolumns,zcolumns):
        self.delAllData()
        solution = self.cget("solution")
        if len(xcolumns) == len(ycolumns) == len(zcolumns):
            index = 9
            for ind in self.cget("index"):
                sol = solution.getIndex(ind)
                index = self.__plot8solution(sol,index,xcolumns,ycolumns,
                                             zcolumns)



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





