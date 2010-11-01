#!/usr/bin/env python
try:
    import Tkinter
    import tkSimpleDialog
    import tkFileDialog
except ImportError:
    import tkinter as Tkinter # Python 3
    from tkinter import simpledialog as tkSimpleDialog
    from tkinter import filedialog as tkFileDialog
from graphics import Pmw
import AUTOutil
from graphics import optionHandler
import math
import sys
import string

GrapherError="GrapherError"
Axes3D=None

class BasicGrapher(optionHandler.OptionHandler,Tkinter.Canvas):
    """Documentation string for Basic Grapher

    A simple graphing widget
    By Randy P."""
    def __init__(self,parent=None,callback=None,**kw):
        self.data = []
        #Get the data from the arguments and then erase the
        #ones which are not used by canvas

        optionDefaults={}
        optionDefaults["minx"] = (0,callback)
        optionDefaults["maxx"] = (0,callback)
        optionDefaults["miny"] = (0,callback)
        optionDefaults["maxy"] = (0,callback)
        optionDefaults["minz"] = (0,callback)
        optionDefaults["maxz"] = (0,callback)
        optionDefaults["azimuth"] = (None,callback)
        optionDefaults["elevation"] = (None,callback)
        optionDefaults["left_margin"] = (80,callback)
        optionDefaults["right_margin"] = (40,callback)
        optionDefaults["top_margin"] = (40,callback)
        optionDefaults["bottom_margin"] = (40,callback)
        optionDefaults["decorations"] = (True,callback)
        optionDefaults["xlabel"] = (None,callback)
        optionDefaults["xlabel_fontsize"] = (None,callback)
        optionDefaults["ylabel"] = (None,callback)
        optionDefaults["ylabel_fontsize"] = (None,callback)
        optionDefaults["zlabel"] = (None,callback)
        optionDefaults["zlabel_fontsize"] = (None,callback)
        optionDefaults["xticks"] = (5,callback)
        optionDefaults["yticks"] = (5,callback)
        optionDefaults["zticks"] = (5,callback)
        optionDefaults["grid"] = (True,callback)
        optionDefaults["tick_label_template"] = ("%.2e",callback)
        optionDefaults["tick_length"] = (0.2,callback)
        optionDefaults["odd_tick_length"] = (0.4,callback)
        optionDefaults["even_tick_length"] = (0.2,callback)
        # background is handled by the Canvas widget
        optionDefaults["foreground"] = ("black",callback)
        optionDefaults["color_list"] = ("black red green blue",callback)
        optionDefaults["symbol_font"] = ("-misc-fixed-*-*-*-*-*-*-*-*-*-*-*-*",callback)
        optionDefaults["symbol_color"] = ("red",callback)
        optionDefaults["smart_label"] = (True,callback)
        optionDefaults["line_width"] = (2,callback)
        optionDefaults["realwidth"] = (1,callback)
        optionDefaults["realheight"] = (1,callback)
        optionDefaults["use_labels"] = (True,callback)
        optionDefaults["use_symbols"] = (True,callback)
        optionDefaults["top_title"] = ("",callback)
        optionDefaults["top_title_fontsize"] = (None,callback)
        optionDefaults["dashes"] = ((6.0,6.0),callback)
        optionDefaults["width"] = (600,callback)
        optionDefaults["height"] = (480,callback)

        optionAliases = {}
        optionAliases["fg"] = "foreground"
        # __parseOptions uses functions from the Canvas
        # widget, so we need to initialize it first
        if kw.get("hide") and 'graphics.grapher_mpl' in sys.modules:
            optionHandler.OptionHandler.__init__(self)
        else:
            Tkinter.Canvas.__init__(self,parent)
            optionHandler.OptionHandler.__init__(self,Tkinter.Canvas)

        for key in list(kw):
            if key not in optionDefaults:
                del kw[key]
        self.addOptions(**optionDefaults)
        self.addAliases(**optionAliases)
        BasicGrapher._configNoDraw(self,**kw)

    def __len__(self):
        return len(self.data)

    def config(self,cnf=None,**kw):
        rval = self._configNoDraw(cnf,**kw)
        if isinstance(cnf, str) or (cnf is None and not kw):
            return rval
        self.clear()
        self.draw()
    configure=config

    # This version can be used to increase efficiency
    # for example, if you want to config, but know you
    # will need to redraw later.
    def _configNoDraw(self,cnf=None,**kw):
        # note: reset xticks/yticks if min/max are set without ticks
        if (cnf is not None or kw) and not isinstance(cnf, str):
            dct = (cnf or {}).copy()
            dct.update(kw)
            for coord in ["x", "y", "z"]:
                minc = "min" + coord
                maxc = "max" + coord
                ticks = coord + "ticks"
                if (minc in dct or maxc in dct) and ticks not in dct:
                    dct[ticks] = None
            return optionHandler.OptionHandler.config(self,**dct)
        return optionHandler.OptionHandler.config(self,cnf,**kw)
    _configureNoDraw = _configNoDraw

    def _addData(self,data,newsect=None,color=None,stable=None):
        for array in data:
            if len(array[0]) != len(array[1]):
                raise GrapherError("Array lengths must match")
            new_array={}
            new_array["x"]=array[0]
            new_array["y"]=array[1]
            if len(array) > 2:
                new_array["z"]=array[2]
            new_array["stable"]=stable
            new_array["newsect"]=newsect
            new_array["color"]=color
            if len(array[0]) > 0:
                new_array["minx"]=min(array[0])
                new_array["maxx"]=max(array[0])
            if len(array[1]) > 0:
                new_array["miny"]=min(array[1])
                new_array["maxy"]=max(array[1])
            if "z" in new_array and len(array[2]) > 0:
                new_array["minz"]=min(array[2])
                new_array["maxz"]=max(array[2])
            self.data.append(new_array)
        
    def addData(self,data):
        self._addData(data)
        self.computeXRange()
        self.computeYRange()
        self.computeZRange()
        self.draw()

    def addArray(self,array):        
        self._addData((array,))
        self.computeXRange()
        self.computeYRange()
        self.computeZRange()
        self.draw()

    def addDataNoDraw(self,data):        
        self._addData(data)

    def addArrayNoDraw(self,array,newsect=None,color=None,stable=None):
        self._addData((array,),newsect,color,stable)

    def _delAllData(self):
        self.data=[]
        # check type for next data
        try:
            zcolumn = self.cget(self.cget("type")+"_z")
        except Tkinter.TclError: #in regression test
            return
        if zcolumn is not None:
            self._configNoDraw({self.cget("type")+"_z":None})
            print("\nWithout matplotlib 3D plots are not supported.")
            print("Plotting only the first two coordinates.")

    def delAllData(self):
        self._delAllData()
        self.clear()

    def _delData(self,index):
        del self.data[index]

    def delData(self,index):
        self._delData(index)
        self.clear()
        self.draw()

    def _round(self,val,increment):
        "This function returns the closest integer multiple to increment"
        quotient = val/increment
        remainder = quotient-math.floor(quotient)
        if remainder < 0.5:
            return math.floor(quotient)*increment
        else:
            return (math.floor(quotient)+1)*increment
        
    def _computeNiceRanges(self,minimum,maximum):
        # This bit of code computes "nice" range values.  Given a
        # minimum and manximum it computes a new minimum, maximum,
        # and number of divisions so that the number of digits in
        # the the numbers in the value at each tick mark
        # in minimized
        therange = maximum - minimum 
        inc = math.pow(10,math.ceil(math.log10(therange) - 1.0))
        if therange / inc <= 2:
            inc = inc / 4
        elif therange / inc <= 4:
            inc = inc / 2
        minimumr = self._round(minimum,inc)
        if minimumr > minimum:
            minimum = minimumr - inc
        else:
            minimum = minimumr
        maximumr = self._round(maximum,inc)
        if maximumr < maximum:
            maximum = maximumr + inc
        else:
            maximum = maximumr ;
        num = int(round(( maximum - minimum ) / inc))
        return {"min": minimum, "max": maximum, "divisions": num + 1}

    def computeRange(self,coordinate,guess_minimum=None,guess_maximum=None):
        minc = "min"+coordinate
        maxc = "max"+coordinate
        if len(self.data) > 0 and minc not in self.data[0]:
            return
        if guess_minimum is None and len(self.data) > 0:
            guess_minimum = min([entry[minc] for entry in self.data])

        if guess_maximum is None and len(self.data) > 0:
            guess_maximum = max([entry[maxc] for entry in self.data])

        if guess_minimum != guess_maximum:
            d = self._computeNiceRanges(guess_minimum,guess_maximum)
            self._configNoDraw(**{minc:d["min"],maxc:d["max"],
                                  coordinate+'ticks':d["divisions"]})
        elif guess_maximum != None:
            self._configNoDraw(**{minc:guess_minimum-1,maxc:guess_maximum+1})
            
    def computeXRange(self,guess_minimum=None,guess_maximum=None):
        self.computeRange("x",guess_minimum,guess_maximum)

    def computeYRange(self,guess_minimum=None,guess_maximum=None):
        self.computeRange("y",guess_minimum,guess_maximum)

    def computeZRange(self,guess_minimum=None,guess_maximum=None):
        self.computeRange("z",guess_minimum,guess_maximum)

    def getXRange(self):
        return [self.cget("minx"),self.cget("maxx")]

    def getYRange(self):
        return [self.cget("miny"),self.cget("maxy")]

    def getZRange(self):
        return [self.cget("minz"),self.cget("maxz")]

    def clear(self):
        for x in self.find_all():
            self.delete(x)

    def plot(self):
        pass

    def draw(self):
        color_list = self.cget("color_list").split()
        minx=self.cget("minx")
        maxx=self.cget("maxx")
        miny=self.cget("miny")
        maxy=self.cget("maxy")
        top_margin = self.cget("top_margin")
        bottom_margin = self.cget("bottom_margin")
        left_margin = self.cget("left_margin")
        right_margin = self.cget("right_margin")
        width = int(self.cget("realwidth"))
        height = int(self.cget("realheight"))

        if self.cget("decorations"):
            # border
            self.create_polygon(left_margin,top_margin,
                                    int(width)-right_margin,top_margin,
                                    int(width)-right_margin,int(height)-bottom_margin,
                                    left_margin,int(height)-bottom_margin,fill="",outline=self.cget("foreground"))
        # data
        line_width=self.cget("line_width")
        adjwidth = width - (left_margin + right_margin)
        adjheight = height - (top_margin + bottom_margin)
        xscale = (float(maxx) - minx) / adjwidth
        yscale = (float(maxy) - miny) / adjheight
        i=-1
        for d in self.data:
            if d["newsect"] is None or d["newsect"]:
                i = i+1
            if d["color"] is None:
                color = i
            else:
                color = d["color"]
            fill=color_list[color%len(color_list)]
            curve="curve:%d"%(i,)
            n=len(d["x"])
            [x,y]=self.__valueToCanvasFast([d["x"][0],d["y"][0]],minx,maxx,miny,maxy,
                                            width,height,left_margin,right_margin,top_margin,bottom_margin)
            # If we only have one point we draw a small circle
            if n == 1:
                self.create_oval(x-3,y-3,x+3,y+3,
                                 tags=("data_point:%d"%(0,),"curve:%d"%(i,),"data"),
                                 fill=fill)
            else:
                line = [x, y]
                xs = d["x"]
                ys = d["y"]
                stable = d["stable"]
                for j in range(1, n):
                    line.append((xs[j]-minx) / xscale + left_margin)
                    line.append((adjheight - (ys[j]-miny) / yscale + top_margin))
                if stable is None or stable:
                    self.create_line(line,width=line_width,tags=(curve,"data"),fill=fill)
                else:
                    self.create_line(line,width=line_width,tags=(curve,"data"),fill=fill,dash=(10,10))

            
        if self.cget("decorations"):
            # clip stuff outside box
            self.create_polygon(0,0,
                                int(width),0,
                                int(width),top_margin-1,
                                0,top_margin-1,
                                fill=self["background"])
            self.create_polygon(0,0,
                                0,int(height),
                                left_margin-1,int(height),
                                left_margin-1,0,
                                fill=self["background"])
            self.create_polygon(int(width),int(height),
                                int(width),0,
                                int(width)-right_margin+1,0,
                                int(width)-right_margin+1,int(height),
                                fill=self["background"])
            self.create_polygon(int(width),int(height),
                                0,int(height),
                                0,int(height)-bottom_margin+1,
                                int(width),int(height)-bottom_margin+1,
                                fill=self["background"])

            # tick marks
            xw=float(width) - (float(left_margin) + float(right_margin))
            yw=float(height) - (float(top_margin) + float(bottom_margin))

            tick_label_template=self.cget("tick_label_template")
            tick_length=self.cget("tick_length")
            odd_tick_length=self.cget("odd_tick_length")
            even_tick_length=self.cget("even_tick_length")

            xticks = self.cget("xticks")
            if xticks is None:
                xticks = self.config("xticks")[3]
            else:
                xticks=int(xticks)
            tick_start_y=yw+bottom_margin
            for i in range(xticks):
                # The odd tick marks should be longer
                if i%2==0:
                    tick_end_y=yw+bottom_margin*(1+even_tick_length)
                else:
                    tick_end_y=yw+bottom_margin*(1+odd_tick_length)
                    
                tick_x=left_margin + float(i)*xw/float(xticks-1)
                self.create_line(tick_x,tick_start_y,tick_x,tick_end_y,fill=self.cget("foreground"))
                val = self.canvasToValue((tick_x,tick_start_y))
                self.create_text(tick_x,tick_end_y,text=tick_label_template%(val[0],),anchor="n",fill=self.cget("foreground"))
                if i != 0 and i != xticks - 1 and self.cget("grid") in ["yes",True]:
                    self.create_line(tick_x,tick_start_y,tick_x,tick_start_y-yw,
                                     fill=self.cget("foreground"),stipple="gray50")
            yticks = self.cget("yticks")
            if yticks is None:
                yticks = self.config("yticks")[3]
            else:
                yticks=int(yticks)
            tick_start_x=left_margin
            tick_end_x=left_margin*(1-tick_length)
            for i in range(yticks):
                tick_y=bottom_margin + float(i)*yw/float(yticks-1)
                self.create_line(tick_start_x,tick_y,tick_end_x,tick_y,fill=self.cget("foreground"))
                val = self.canvasToValue((tick_start_x,tick_y))
                self.create_text(tick_end_x,tick_y,text=tick_label_template%(val[1],),anchor="e",fill=self.cget("foreground"))
                if i != 0 and i != yticks - 1 and self.cget("grid") in ["yes",True]:
                    self.create_line(tick_start_x,tick_y,tick_start_x + xw,tick_y,
                                     fill=self.cget("foreground"),stipple="gray50")

            # Axis labels
            self.create_text(left_margin*0.3,bottom_margin*0.3,
                             text=self.cget("ylabel"),anchor="nw",fill=self.cget("foreground"))
            self.create_text(int(width)-left_margin*0.3,int(height)-bottom_margin*0.1,
                             text=self.cget("xlabel"),anchor="se",fill=self.cget("foreground"))
            # Title
            self.create_text((left_margin-right_margin+int(width))//2,
                             top_margin*0.1,text=self.cget("top_title"),anchor="n",
                             fill=self.cget("foreground"))

        
    def valueToCanvas(self,val):
        if len(val) != 2:
            raise GrapherError("Illegal value choosen for coordinate transformation.  Must be a tuple with 2 elements.")
        # make a few constants shorter
        minx=self.cget("minx")
        maxx=self.cget("maxx")
        miny=self.cget("miny")
        maxy=self.cget("maxy")
        width = int(self.cget("realwidth"))
        height = int(self.cget("realheight"))
        left_margin = self.cget("left_margin")
        right_margin = self.cget("right_margin")
        top_margin = self.cget("top_margin")
        bottom_margin = self.cget("bottom_margin")
        return self.__valueToCanvasFast(val,minx,maxx,miny,maxy,
                                        width,height,left_margin,right_margin,top_margin,bottom_margin)

    def __valueToCanvasFast(self,val,minx,maxx,miny,maxy,
                            width,height,left_margin,right_margin,top_margin,bottom_margin):
        x = val[0]
        y = val[1]
        width = width - (left_margin + right_margin)
        height = height - (top_margin + bottom_margin)
        return [((x-minx)/(maxx-minx))*width + left_margin,
                height - ((y-miny)/(maxy-miny))*height + top_margin]
        
    def transform(self,val):
        [x,y] = self.valueToCanvas(val)
        return [x,self.cget("realheight") - y]

    def transform_seq(self,seqs):
        minx=self.cget("minx")
        maxx=self.cget("maxx")
        miny=self.cget("miny")
        maxy=self.cget("maxy")
        width = int(self.cget("realwidth"))
        height = int(self.cget("realheight"))
        left_margin = self.cget("left_margin")
        right_margin = self.cget("right_margin")
        top_margin = self.cget("top_margin")
        bottom_margin = self.cget("bottom_margin")
        valuetocanvasfast = self.__valueToCanvasFast
        sp2 = 5 #fontsize
        sp4 = 5
        l = []
        for i in range(len(seqs[0])):
            val = [seqs[0][i],seqs[1][i]]
            [x,y] = valuetocanvasfast(val,minx,maxx,miny,maxy,
                width,height,left_margin,right_margin,top_margin,bottom_margin)
            l.append(
                [(x - left_margin) / sp2,((height - y) - bottom_margin) / sp4])
        return l

    def canvasToValue(self,val):
        if len(val) != 2:
            raise GrapherError("Illegal value choosen for coordinate transformation.  Must be a tuple with 2 elements.")
        x = val[0]
        if x < self.cget("left_margin"):
            x = self.cget("left_margin")
        if x > int(self.cget("realwidth")) - self.cget("right_margin"):
            x = int(self.cget("realwidth")) - self.cget("right_margin")
        y = val[1]
        if y < self.cget("top_margin"):
            y = self.cget("top_margin")
        if y > int(self.cget("realheight")) - self.cget("bottom_margin"):
            y = int(self.cget("realheight")) - self.cget("bottom_margin")

        x = x - self.cget("left_margin")
        y = y - self.cget("bottom_margin")
        minx=self.cget("minx")
        maxx=self.cget("maxx")
        miny=self.cget("miny")
        maxy=self.cget("maxy")
        return [minx + (maxx-minx)*float(x)/(float(self.cget("realwidth"))-(self.cget("left_margin")+self.cget("right_margin"))),
                maxy - (maxy-miny)*float(y)/(float(self.cget("realheight"))-(self.cget("bottom_margin")+self.cget("top_margin")))]

class LabeledGrapher(BasicGrapher):
    def __init__(self,parent=None,**kw):
        self.labels=[]
        BasicGrapher.__init__(self,parent,**kw)

    def addLabel(self,i,j,input_text,symbol=None):
        new_label={}
        new_label["j"]=j
        xy = [self.data[i]["x"][j], self.data[i]["y"][j]]
        if "z" in self.data[i]:
            xy.append(self.data[i]["z"][j])
        new_label["xy"]=xy
        new_label["text"]=input_text
        new_label["symbol"]=symbol
        self.labels[i].append(new_label)

    def _delData(self,i):
        del self.labels[i]
        BasicGrapher._delData(self,i)

    def _delAllData(self):
        self.labels=[]
        BasicGrapher._delAllData(self)

    def _addData(self,data,newsect=None,color=None,stable=None):
        self.labels.append([])
        BasicGrapher._addData(self,data,newsect,color,stable)

    def plotlabels(self):
        if self.cget("realwidth") == 1 or self.cget("realheight") == 1:
            return

        if not self.cget("use_labels") or len(self.labels) == 0:
            return

        trans = self.transform_seq
        if self.cget("smart_label"):
            mp = self.inarrs()
            for d in self.data:
                self.map_curve(mp,trans([d["x"],d["y"]]))
        for i, labels in enumerate(self.labels):
            for label in labels:
                if len(label["text"]) == 0:
                    continue
                [x,y] = label["xy"][:2]
                if (x < self["minx"] or x > self["maxx"] or
                    y < self["miny"] or y > self["maxy"]):
                    continue
                data = self.transform([x,y])
                if not(data is None):
                    [x,y] = data
                    if self.cget("smart_label"):
                        [xoffd1,yoffd1,xoffd2,yoffd2,
                         xofft,yofft,pos] = self.findsp(x,y,mp)
                    else:
                        [xoffd1,yoffd1,xoffd2,yoffd2,
                         xofft,yofft,pos] = self.dumblabel(i,label["j"],x,y)
                    anchor = self.getpos(pos)
                    y = self.cget("realheight") - y
                    self.create_line(x+xoffd1,y-yoffd1,x+xoffd2,y-yoffd2,
                                     fill=self.cget("foreground"))
                    self.create_text(x+xofft,y-yofft,text=label["text"],
                                     anchor=anchor,fill=self.cget("foreground"))

    # not-so-smart way of plotting labels
    def dumblabel(self,i,j,x,y):
        #Find a neighbor so I can compute the "slope"
        if j < len(self.data[i]["x"]) - 1:
            first = j
            second = j+1
        else:
            first = j-1
            second = j
        realwidth=self.cget("realwidth")
        realheight=self.cget("realheight")
        left_margin=self.cget("left_margin")
        top_margin=self.cget("top_margin")
        #pick a good direction for the label
        ys = self.data[i]["y"]
        if ys[second] > ys[first]:
            if (x < int(realwidth)-(20+left_margin) and
                y < int(realheight)-(20+top_margin)):
                xoffset = 10
                yoffset = 10
                pos=0
            else:
                xoffset = -10
                yoffset = -10
                pos=4
        else:
            if x > 20+left_margin and y < int(realheight)-(20+top_margin):
                xoffset = -10
                yoffset = 10
                pos=2
            else:
                xoffset = 10
                yoffset = -10
                pos=6

        #self.addtag_overlapping("overlaps",x+xoffset-3,
        #        y+yoffset-3,x+xoffset+3,y+yoffset+3)
        #if len(self.gettags("overlaps")) != 0:
        #    print self.gettags("overlaps")
        #self.dtag("overlaps")
        #print "---------------------------------------------"    
        return [xoffset//10,yoffset//10,xoffset,yoffset,xoffset,yoffset,pos]

    # smarter way to plot labels: ported from old PLAUT
    #-------------------------------------------------------------------
    #   Tries to find an empty space to put the
    #   point or branch label from pnts in by searching for three
    #   unchanged entries in mp.  The search is outward from the
    #   point to be labelled.  If space can't be found the label
    #   will be written out anyway.
    #-------------------------------------------------------------------
    def findsp(self,x1,y1,mp):
        sp1 = self.cget("left_margin")
        minsx = sp1
        maxsx = self.cget("realwidth") - self.cget("right_margin")
        sp2 = 5 #fontsize
        sp3 = self.cget("bottom_margin")
        minsy = sp3
        maxsy = self.cget("realheight") - self.cget("top_margin")
        sp4 = 5
        #C---     *x1, y1 ARE SCREEN COORDINATES
        xi = (x1 - sp1) / sp2
        yi = (y1 - sp3) / sp4
        pi2 = math.pi * 2.0
        start = pi2 / 12.0
        npoint = 16

        hx = len(mp) - 5
        hy = len(mp[0]) - 5
        found = False
        for radius in range(3,71):
            npoint = npoint + 8

            for st in [start + (float(i)/npoint)*pi2 for i in range(npoint)]:
                xd = int(xi + radius * math.cos(st))
                yd = int(yi + radius * math.sin(st))
                #-------------------------------------------------------
                #   Organize the search along two sides of
                #   a square of size 2(xd) with pnt xi, yi in its center.
                #--------------------------------------------------------
                if xd <= hx and xd >= 3 and yd <= hy and yd >= 3:
                    found = True
                    for col in mp[xd-2:xd+3]:
                        if max(col[yd-2:yd+3]) == 1:
                            found = False
                            break
                    if found:
                        r1 = (radius + 3) * sp2
                        ix = x1 + r1 * math.cos(st)
                        iy = y1 + r1 * math.sin(st)
                        if ix>=minsx and ix<=maxsx and iy>=minsy and iy<=maxsy:
                            break
                        found = False
            if found:
                break

        if not found:
            return [1,1,10,10,11,11,4] #if no empty space

        for col in mp[xd-2:xd+3]:
            for i2 in range(yd-1,yd+2):
                col[i2] = 1

        pos = int(st/pi2*8.0 - 0.5)
        radius = radius * sp2
        cosst = math.cos(st)
        sinst = math.sin(st)
        d1dist = self.cget("line_width") + 1
        xoffd1 = d1dist * cosst
        yoffd1 = d1dist * sinst
        xoffd2 = radius * cosst
        yoffd2 = radius * sinst
        xofft = (radius + 2) * cosst
        yofft = (radius + 2) * sinst
        return [xoffd1,yoffd1,xoffd2,yoffd2,xofft,yofft,pos]

    def getpos(self,pos):
        anchor = [ "sw", "s", "se", "e", "ne", "n", "nw", "w", "sw" ]
        return anchor[pos]

    #-----------------------------------------------------------------------
    #    Initializes the map array that is
    #    used to label the plotted curves.
    #-----------------------------------------------------------------------
    def inarrs(self):
        sp1 = self.cget("left_margin")
        sp2 = 5 #fontsize
        sp3 = self.cget("bottom_margin")
        sp4 = 5
        nx = int(self.cget("realwidth")-sp1-self.cget("right_margin"))//sp2
        ny = int(self.cget("realheight")-sp3-self.cget("top_margin"))//sp4
        r = ny*[0]
        return [r[:] for i in range(nx)]

    #-----------------------------------------------------------------------
    #        Maps the curves in mp array
    #-----------------------------------------------------------------------
    def map_curve(self,mp,xys):
        ixmax = len(mp)
        iymax = len(mp[0])
        [x2,y2] = xys[0]
        ix2,iy2 = int(x2),int(y2)
        for xy in xys[1:]:
            x1,y1 = x2,y2
            ix1,iy1 = ix2,iy2
            [x2,y2] = xy
            ix2,iy2 = int(x2),int(y2)
            if ix1 == ix2 and iy1 == iy2:
                #optimize common case of short line segments
                if ix1 >= 0 and ix1 < ixmax and iy1 >= 0 and iy1 < iymax:
                    mp[ix1][iy1] = 1
                continue
            # if we are sure that the line is outside the graph limits we
            # should skip and save a lot of time
            if ((ix1 < 0 and ix2 < 0) or (iy1 < 0 and iy2 < 0) or
                (ix1 >= ixmax and ix2 >= ixmax) or 
                (iy1 >= iymax and iy2 >= iymax)):
                continue
            dx = x2 - x1
            dy = y2 - y1
            index = int(max(abs(dx),abs(dy))) + 1
            ilow  = 0
            ihigh = index
            if dx != 0:
                xlim1 = -x1*index/dx
                xlim2 = (ixmax-x1)*index/dx+1
                ilow  = max(ilow,min(xlim1,xlim2))
                ihigh = min(ihigh,max(xlim1,xlim2))
            if dy != 0:
                ylim1 = -y1*index/dy
                ylim2 = (iymax-y1)*index/dy+1
                ilow  = max(ilow,min(ylim1,ylim2))
                ihigh = min(ihigh,max(ylim1,ylim2))
            for i in range(int(ilow),int(ihigh)):
                f = float(i)/index
                ix = int(x1 + f * dx)
                iy = int(y1 + f * dy)
                try:
                    mp[ix][iy] = 1
                except IndexError:
                    pass

    def draw(self):
        self.plotlabels()
        BasicGrapher.draw(self)
        self.plotsymbols()

    def plotsymbols(self):
        if not self.cget("use_symbols"):
            return

        for i, labels in enumerate(self.labels):
            for label in labels:
                l = label["symbol"]
                if l is None:
                    continue
                data = self.valueToCanvas(label["xy"][:2])
                if data is None:
                    continue
                [x,y] = data
                if (x < self["minx"] or x > self["maxx"] or
                    y < self["miny"] or y > self["maxy"]):
                    continue
                c = self.cget("symbol_color")
                if len(l) <= 3:
                    self.create_text(x,y,font=self.cget("symbol_font"),
                                     fill=c,text=l)
                    continue
                ms = 3
                for i, ch in enumerate(l):
                    if ch in string.digits:
                        ms = int(l[i:])
                        l = l[:i]
                        break
                if l == "fillcircle":
                    self.create_oval(x-ms,y-ms,x+ms,y+ms,fill=c,outline=c)
                elif l == "circle":
                    self.create_oval(x-ms,y-ms,x+ms,y+ms,outline=c)
                elif l == "square":
                    self.create_rectangle(x-ms,y-ms,x+ms,y+ms,outline=c)
                elif l == "crosssquare":
                    self.create_rectangle(x-ms,y-ms,x+ms,y+ms,outline=c)
                    self.create_line(x-ms,y-ms,x+ms,y+ms,fill=c)
                    self.create_line(x-ms,y+ms,x+ms,y-ms,fill=c)
                elif l == "fillsquare":
                    self.create_rectangle(x-ms,y-ms,x+ms,y+ms,fill=c,outline=c)

# FIXME:  No regression tester
class InteractiveGrapher(LabeledGrapher):
    def __init__(self,parent=None,**kw):
        LabeledGrapher.__init__(self,parent,**kw)    

    def unzoom(self):
        self.computeXRange()
        self.computeYRange()
        self.clear()
        self.draw()
        
    def labelPointWrapper(self,e):
        #find all objects near the mouse pointer
        points = self.find_closest(e.x,e.y)
        #find the first one that has a data_point?? tag
        point=None
        curve=None
        for point in points:
            #tags associated with the point
            point_tags = self.gettags(point)
            for tag in point_tags:
                tag=tag.split(":")
                if tag[0] == "data_point":
                    point=int(tag[1])
                if tag[0] == "curve":
                    curve=int(tag[1])
        if not((point is None) or (curve is None)):
            label = tkSimpleDialog.askstring("Label","Enter here")
            self.addLabel(curve,point,label)

    def printTagWrapper(self,e):
        theid=self.find("closest",e.x,e.y)
        print(self.gettags(theid[0]))
        
    def printValueWrapper(self,e):
        self.__printValue((e.x,e.y))

    def __printValue(self,val):
        print(self.canvasToValue(val))

    def zoomRubberBand(self,e):
        self.delete("rubber_band")
        self.create_polygon(e.x,e.y,e.x,self.zoom_starty,
                            self.zoom_startx,self.zoom_starty,
                            self.zoom_startx,e.y,
                            tags=("rubber_band",),fill="",outline="grey")

    def zoomWrapperStart(self,e):
        self.zoom_startx = e.x
        self.zoom_starty = e.y
        
    def zoomWrapperEnd(self,e):
        tolerance = 5

        cx=[self.zoom_startx,e.x]
        cy=[self.zoom_starty,e.y]
        cx.sort()
        cy.sort()
        
        if cx[1] - cx[0] > tolerance and cy[1] - cy[0] > tolerance:
            vx=[0,0]
            vy=[0,0]
            # canvas coordinates start at the top left, values coordinates start at bottom left
            vx[0],vy[0] = self.canvasToValue((cx[0],cy[1]))
            vx[1],vy[1] = self.canvasToValue((cx[1],cy[0]))

            self.computeXRange(vx[0],vx[1])
            self.computeYRange(vy[0],vy[1])
            self.clear()
            self.draw()
#            self.config(minx=vx[0],maxx=vx[1],miny=vy[0],maxy=vy[1])

    def drawWrapper(self,e):
        # Note: we should not update self["width"] and self["height"] here, because
        # those can be 2 or 4 less
        self._configNoDraw(realwidth=e.width,realheight=e.height)
        self.configure()
        self.clear()
        self.draw()

class GUIGrapher(InteractiveGrapher):
    def __init__(self,parent=None,**kw):
        InteractiveGrapher.__init__(self,parent,**kw)
        self.bind("<ButtonPress-3>",self.popupMenuWrapper)
        self.menu=Tkinter.Menu()
        self.menu.add_radiobutton(label="print value",command=self.printValueBindings)
#        self.menu.add_radiobutton(label="print tag",command=self.printTagBindings)
#        self.menu.add_radiobutton(label="label point",command=self.labelPointBindings)
        self.menu.add_radiobutton(label="zoom",command=self.zoomBindings)
        self.menu.invoke('zoom')
        self.menu.add_command(label="Unzoom",command=self.unzoom)
        self.menu.add_command(label="Postscript",command=self.generatePostscript)
        self.menu.add_command(label="Configure...",command=self.__interactiveConfigureDialog)
        self.bind("<Configure>",self.drawWrapper)

    def __interactiveConfigureDialog(self):
        diag = Pmw.Dialog(self,buttons=("Ok","Cancel"))
        options = sorted([self.configure(key)[0] for key in self.configure() 
                          if self._isInternalOption(key)])

        self.optionList = Pmw.ScrolledListBox(diag.interior(),
                                              items=options,
                                              dblclickcommand=self.__updateInteractiveConfigureDialog)
        self.optionList.pack(side="left")

        frame = Tkinter.Frame(diag.interior())
        frame.pack(side="right")
        self.optionLabel = Pmw.EntryField(frame,
                                         labelpos="w",
                                         label_text="Option Name",
                                         entry_state=Tkinter.DISABLED)
        self.optionLabel.pack(side="top")

        self.valueLabel = Pmw.EntryField(frame,
                                         labelpos="w",
                                         label_text="Old Value",
                                         entry_state=Tkinter.DISABLED)
        self.valueLabel.pack(side="top")

        self.valueEntry = Pmw.EntryField(frame,
                                         labelpos="w",
                                         label_text="New Value",
                                         command=self.__modifyOption)
        self.valueEntry.pack(side="top")
        
    def __modifyOption(self):
        key = self.optionList.getcurselection()[0]
        if isinstance(self.cget(key), int):
            self[key] = int(self.valueEntry.get())
        elif isinstance(self.cget(key), float):
            self[key] = float(self.valueEntry.get())
        elif isinstance(self.cget(key), str):
            self[key] = self.valueEntry.get()
        self.valueLabel.setentry(self.cget(key))

    def __updateInteractiveConfigureDialog(self):
        key = self.optionList.getcurselection()[0]
        self.optionLabel.setentry(key)
        self.valueLabel.setentry(self.cget(key))
        self.valueEntry.clear()
        if isinstance(self.cget(key), int):
            self.valueEntry.configure(validate={"validator":"integer"})
        elif isinstance(self.cget(key), float):
            self.valueEntry.configure(validate={"validator":"real"})
        elif isinstance(self.cget(key), str):
            self.valueEntry.configure(validate={"validator":"alphanumeric"})
        
    def generatePostscript(self,filename=None,pscolormode=None):
        """
        Save the plot as postscript.
        """
        if pscolormode is None:
            pscolormode=self.cget("ps_colormode")
        if filename is None:
            filename = tkFileDialog.asksaveasfilename(defaultextension=".eps",title="Save as Postscript File")
        self.update()
        self.postscript(file=filename,colormode=pscolormode)

    savefig = generatePostscript

    def printValueBindings(self):
        self.bind("<ButtonPress-1>",self.printValueWrapper)
        self.unbind("<B1-Motion>")
        self.unbind("<ButtonRelease-1>")
        
    def printTagBindings(self):
        self.bind("<ButtonPress-1>",self.printTagWrapper)
        self.unbind("<B1-Motion>")
        self.unbind("<ButtonRelease-1>")
        
    def labelPointBindings(self):
        self.bind("<ButtonPress-1>",self.labelPointWrapper)
        self.unbind("<B1-Motion>")
        self.unbind("<ButtonRelease-1>")
        
    def zoomBindings(self):
        self.bind("<ButtonPress-1>",self.zoomWrapperStart)
        self.bind("<B1-Motion>",self.zoomRubberBand)
        self.bind("<ButtonRelease-1>",self.zoomWrapperEnd)
        
    def popupMenuWrapper(self,e):
        self.menu.tk_popup(e.x+self.winfo_rootx(),e.y+self.winfo_rooty())


def test(grapher=None):
    if grapher is None:
        grapher = GUIGrapher()
    data = [float(i)*0.1 for i in range(62)]

    grapher.addArray((data,list(map(math.sin,data))))
    grapher.addArray((data,list(map(math.cos,data))))
    grapher.addLabel(0,10,"hello")
    grapher.addLabel(0,30,"world")
    grapher.pack()
    grapher.plot()

    button = Tkinter.Button(text="Quit",command=grapher.quit)
    button.pack()
    button.update()
    print("Press <return> to continue")
    raw_input()

    grapher.delAllData()
    grapher.addArray((data,list(map(math.cos,data))))
    grapher.plot()
    print("Press <return> to continue")
    raw_input()

if __name__=='__main__':
    test()






