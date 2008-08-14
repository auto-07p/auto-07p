#!/usr/bin/env python
import Tkinter
import Pmw
import tkSimpleDialog
import tkFileDialog
import string
import types
import AUTOutil
import optionHandler
import math

GrapherError="GrapherError"

class BasicGrapher(optionHandler.OptionHandler,Tkinter.Canvas):
    """Documentation string for Basic Grapher

    A simple graphing widget
    By Randy P."""
    def __init__(self,parent=None,callback=None,cnf={},**kw):
        self.data = []
        #Get the data from the arguments and then erase the
        #ones which are not used by canvas

        optionDefaults={}
        optionDefaults["minx"] = (0,callback)
        optionDefaults["maxx"] = (0,callback)
        optionDefaults["miny"] = (0,callback)
        optionDefaults["maxy"] = (0,callback)
        optionDefaults["left_margin"] = (80,callback)
        optionDefaults["right_margin"] = (40,callback)
        optionDefaults["top_margin"] = (40,callback)
        optionDefaults["bottom_margin"] = (40,callback)
        optionDefaults["decorations"] = (1,callback)
        optionDefaults["xlabel"] = (None,callback)
        optionDefaults["xlabel_fontsize"] = (None,callback)
        optionDefaults["ylabel"] = (None,callback)
        optionDefaults["ylabel_fontsize"] = (None,callback)
        optionDefaults["xticks"] = (5,callback)
        optionDefaults["yticks"] = (5,callback)
        optionDefaults["grid"] = ("yes",callback)
        optionDefaults["tick_label_template"] = ("%.2e",callback)
        optionDefaults["tick_length"] = (0.2,callback)
        optionDefaults["odd_tick_length"] = (0.4,callback)
        optionDefaults["even_tick_length"] = (0.2,callback)
        # background is handled by the Canvas widget
        optionDefaults["foreground"] = ("black",callback)
        optionDefaults["color_list"] = ("black red green blue",callback)
        optionDefaults["symbol_font"] = ("-misc-fixed-*-*-*-*-*-*-*-*-*-*-*-*",callback)
        optionDefaults["symbol_color"] = ("red",callback)
        optionDefaults["smart_label"] = (1,callback)
        optionDefaults["line_width"] = (2,callback)
        optionDefaults["realwidth"] = (1,callback)
        optionDefaults["realheight"] = (1,callback)
        optionDefaults["use_labels"] = (1,callback)
        optionDefaults["use_symbols"] = (1,callback)
        optionDefaults["top_title"] = ("",callback)
        optionDefaults["top_title_fontsize"] = (None,callback)
        optionDefaults["dashes"] = ((6.0,6.0),callback)

        optionAliases = {}
        optionAliases["fg"] = "foreground"
        # __parseOptions uses functions from the Canvas
        # widget, so we need to initialize it first
        Tkinter.Canvas.__init__(self,parent)
        optionHandler.OptionHandler.__init__(self,Tkinter.Canvas)

        dict = AUTOutil.cnfmerge((cnf,kw))
        for key in dict.keys():
            if not key in optionDefaults.keys():
                del dict[key]
        self.addOptions(optionDefaults)
        self.addAliases(optionAliases)
        BasicGrapher._configNoDraw(self,dict)

    def __len__(self):
        return len(self.data)

    def config(self,cnf=None,**kw):
        if type(cnf) == types.StringType or (cnf is None and len(kw) == 0):
            return self._configNoDraw(cnf)
        else:
            self._configNoDraw(AUTOutil.cnfmerge((cnf,kw)))
            self.clear()
            self.draw()
    configure=config

    # This version can be used to increase efficiency
    # for example, if you want to config, but know you
    # will need to redraw later.
    def _configNoDraw(self,cnf=None,**kw):
        if type(cnf) == types.StringType or (cnf is None and len(kw) == 0):
            return optionHandler.OptionHandler.config(self,cnf)
        else:
            optionHandler.OptionHandler.config(self,AUTOutil.cnfmerge((cnf,kw)))
    _configureNoDraw = _configNoDraw

    def getData(self,i,j):
        if j=="x":
            return self.data[i]["x"]
        elif j=="y":
            return self.data[i]["y"]
        else:
            return [self.data[i]["x"][j],self.data[i]["y"][j]]

    def _addData(self,data,newsect=None,stable=None):
        for array in data:
            if len(array[0]) != len(array[1]):
                raise GrapherError,"Array lengths must match"
            new_array={}
            new_array["x"]=array[0]
            new_array["y"]=array[1]
            new_array["stable"]=stable
            new_array["newsect"]=newsect
            if len(array[0]) > 0:
                new_array["minx"]=min(array[0])
                new_array["maxx"]=max(array[0])
            if len(array[1]) > 0:
                new_array["miny"]=min(array[1])
                new_array["maxy"]=max(array[1])
            self.data.append(new_array)
        
    def addData(self,data):
        self._addData(data)
        self.computeXRange()
        self.computeYRange()
        self.draw()

    def addArray(self,array):        
        self._addData((array,))
        self.computeXRange()
        self.computeYRange()
        self.draw()

    def addDataNoDraw(self,data):        
        self._addData(data)

    def addArrayNoDraw(self,array,newsect=None,stable=None):
        self._addData((array,),newsect,stable)

    def _delAllData(self):
        self.data=[]

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
        returnVal = {}
        # This bit of code computes "nice" range values.  Given a
        # minimum and manximum it computes a new minimum, maximum,
        # and number of divisions so that the number of digits in
        # the the numbers in the value at each tick mark
        # in minimized
        range = maximum - minimum 
        inc = math.pow(10,math.ceil(math.log10(range) - 1.0))
        if range / inc <= 2:
            inc = inc / 4
        elif range / inc <= 4:
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
        returnVal["min"] = minimum
        returnVal["max"] = maximum
        returnVal["divisions"] = num + 1
        return returnVal

    def computeXRange(self,guess_minimum=None,guess_maximum=None):
        if guess_minimum is None:
            minimums=[]
            for entry in self.data:
                minimums.append(entry["minx"])
            if minimums != []:
                guess_minimum = min(minimums)

        if guess_maximum is None:
            maximums=[]
            for entry in self.data:
                maximums.append(entry["maxx"])
            if maximums != []:
                guess_maximum = max(maximums)

        if guess_minimum != guess_maximum:
            dict = self._computeNiceRanges(guess_minimum,guess_maximum)
            self._configNoDraw(minx=dict["min"],maxx=dict["max"])
            self._configNoDraw(xticks=dict["divisions"])
        elif guess_maximum != None:
            self._configNoDraw(minx=guess_minimum-1,maxx=guess_maximum+1)
            self._configNoDraw(xticks=None)
            
    def computeYRange(self,guess_minimum=None,guess_maximum=None):
        if guess_minimum is None:
            minimums=[]
            for entry in self.data:
                minimums.append(entry["miny"])
            if minimums != []:
                guess_minimum = min(minimums)

        if guess_maximum is None:
            maximums=[]
            for entry in self.data:
                maximums.append(entry["maxy"])
            if maximums != []:
                guess_maximum = max(maximums)

        if guess_minimum != guess_maximum:
            dict = self._computeNiceRanges(guess_minimum,guess_maximum)
            self._configNoDraw(miny=dict["min"],maxy=dict["max"])
            self._configNoDraw(yticks=dict["divisions"])
        elif guess_minimum != None:
            self._configNoDraw(miny=guess_minimum-1,maxy=guess_maximum+1)
            self._configNoDraw(yticks=None)

    def getXRange(self):
        return [self.cget("minx"),self.cget("maxx")]

    def getYRange(self):
        return [self.cget("miny"),self.cget("maxy")]

    def clear(self):
        for x in self.find_all():
            self.delete(x)

    def plot(self):
        pass

    def draw(self):
        color_list = string.split(self.cget("color_list"))
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
        i=0
        for d in self.data:
            fill=color_list[i%len(color_list)]
            curve="curve:%d"%(i,)
            n=len(d["x"])
            [x,y]=self.__valueToCanvasFast([d["x"][0],d["y"][0]],minx,maxx,miny,maxy,
                                            width,height,left_margin,right_margin,top_margin,bottom_margin)
            # If we only have one point we draw a small circle
            if n == 1:
                self.create_oval(x-3,y-3,x+3,y+3,
                                 tags=("data_point:%d"%(0,),"curve:%d"%(i,),"data"),
                                 fill=color_list[i%len(color_list)])
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
            if d["newsect"] is None or d["newsect"]:
                i = i+1

            
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
                if i != 0 and i != xticks - 1 and self.cget("grid") == "yes":
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
                if i != 0 and i != yticks - 1 and self.cget("grid") == "yes":
                    self.create_line(tick_start_x,tick_y,tick_start_x + xw,tick_y,
                                     fill=self.cget("foreground"),stipple="gray50")

            # Axis labels
            self.create_text(left_margin*0.3,bottom_margin*0.3,
                             text=self.cget("ylabel"),anchor="nw",fill=self.cget("foreground"))
            self.create_text(int(width)-left_margin*0.3,int(height)-bottom_margin*0.1,
                             text=self.cget("xlabel"),anchor="se",fill=self.cget("foreground"))
            # Title
            self.create_text((left_margin-right_margin+int(width))/2,
                             top_margin*0.1,text=self.cget("top_title"),anchor="n",
                             fill=self.cget("foreground"))

        
    def valueToCanvas(self,val):
        if len(val) != 2:
            raise GrapherError,"Illegal value choosen for coordinate transformation.  Must be a tuple with 2 elements."
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

    def canvasToValue(self,val):
        if len(val) != 2:
            raise GrapherError,"Illegal value choosen for coordinate transformation.  Must be a tuple with 2 elements."
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
    def __init__(self,parent=None,cnf={},**kw):
        kw=AUTOutil.cnfmerge((cnf,kw))
        self.labels=[]
        apply(BasicGrapher.__init__,(self,parent),kw)

    def addLabel(self,i,j,input_text,symbol=None):
        new_label={}
        new_label["j"]=j
        new_label["text"]=input_text
        new_label["symbol"]=symbol
        self.labels[i].append(new_label)

    def _delData(self,i):
        del self.labels[i]
        BasicGrapher._delData(self,i)

    def _delAllData(self):
        self.labels=[]
        BasicGrapher._delAllData(self)

    def _addData(self,data,newsect=None,stable=None):
        self.labels.append([])
        BasicGrapher._addData(self,data,newsect,stable)

    def plotlabels(self):
        if self.cget("realwidth") == 1 or self.cget("realheight") == 1:
            return

        if not self.cget("use_labels"):
            return

        trans = self.transform
        if self.cget("smart_label"):
            mp = self.inarrs()
            for i in range(len(self.data)):
                self.map_curve(mp,self.data[i]["x"],self.data[i]["y"],trans)
        for i in range(len(self.labels)):
            for label in self.labels[i]:
                if len(label["text"]) == 0:
                    continue
                j = label["j"]
                [x,y] = self.getData(i,j)
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
                         xofft,yofft,pos] = self.dumblabel(i,j,x,y)
                    anchor = self.getpos(pos)
                    y = self.cget("realheight") - y
                    self.create_line(x+xoffd1,y-yoffd1,x+xoffd2,y-yoffd2,
                                     fill=self.cget("foreground"))
                    self.create_text(x+xofft,y-yofft,text=label["text"],
                                     anchor=anchor,fill=self.cget("foreground"))

    # not-so-smart way of plotting labels
    def dumblabel(self,i,j,x,y):
        #Find a neighbor so I can compute the "slope"
        if j < len(self.getData(i,"x"))-1:
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
        if self.getData(i,"y")[second] > self.getData(i,"y")[first]:
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
        return [xoffset/10,yoffset/10,xoffset,yoffset,xoffset,yoffset,pos]

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
        rd = math.pi / 180.0
        start = rd * 30.0
        radius = 2
        npoint = 16

        found = 0
        while radius < 70:
            radius = radius + 1
            npoint = npoint + 8
            st     = start
            rinc   = rd * 360.0 / float(npoint)

            for i in range(npoint):
                xd = int(xi + radius * math.cos(st))
                yd = int(yi + radius * math.sin(st))
                if self.emptsp(xd,yd,mp):
                    r1 = (radius + 3) * sp2
                    ix = x1 + r1 * math.cos(st)
                    iy = y1 + r1 * math.sin(st)
                    if (ix>=minsx and ix<=maxsx and iy>=minsy and iy<=maxsy):
                        found = 1
                        break
                st = st + rinc
            if found:
                break

        if radius >= 70:
            return [1,1,10,10,11,11,4] #if no empty space

        for i1 in range(xd-2,xd+3):
            for i2 in range(yd-1,yd+2):
                mp[i1][i2] = 1

        pos = int((st/rd - 22.5) / 45)
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
    #   Organizes the search along two sides of
    #   a square of size 2(xd) with pnt xi, yi in its center.
    #-----------------------------------------------------------------------
    def emptsp(self,ix,iy,mp):
       if ix > len(mp) - 5 or ix < 3:
           return 0
       if iy > len(mp[0]) - 5 or iy < 3:
           return 0
       for i in range(ix-2,ix+3):
           for j in range(iy-2,iy+3):
               if mp[i][j]:
                   return 0
       return 1

    #-----------------------------------------------------------------------
    #    Initializes the map array that is
    #    used to label the plotted curves.
    #-----------------------------------------------------------------------
    def inarrs(self):
        sp1 = self.cget("left_margin")
        sp2 = 5 #fontsize
        sp3 = self.cget("bottom_margin")
        sp4 = 5
        nx = int(self.cget("realwidth")-sp1-self.cget("right_margin"))/sp2
        ny = int(self.cget("realheight")-sp3-self.cget("top_margin"))/sp4
        r = ny*[0]
        mp = []
        for i in range(nx):
            mp.append(r[:])
        return mp

    #-----------------------------------------------------------------------
    #        Maps the curves in mp array
    #-----------------------------------------------------------------------
    def map_curve(self,mp,xs,ys,trans):
        sp1 = self.cget("left_margin")
        sp2 = 5 #fontsize
        sp3 = self.cget("bottom_margin")
        sp4 = 5
        ixmax = len(mp)
        iymax = len(mp[0])
        minx = self["minx"]
        maxx = self["maxx"]
        miny = self["miny"]
        maxy = self["maxy"]
        x,y = xs[0],ys[0]
        for i in range(1,len(xs)):
            oldx,oldy = x,y
            x,y = xs[i],ys[i]
            # if we are sure that the line is outside the graph limits we
            # should skip and save a lot of time
            if ((oldx < minx and x < minx) or
                (oldy < miny and y < miny) or
                (oldx > maxx and x > maxx) or
                (oldy > maxy and y > maxy)):
                continue
            xold,yold = trans((oldx,oldy))
            x1 = (xold - sp1) / sp2
            y1 = (yold - sp3) / sp4
            xnew,ynew = trans((x,y))
            dx = (xnew - xold) / sp2
            dy = (ynew - yold) / sp4
            index = int(max(abs(dx),abs(dy))) + 1
            ilow = 0
            ihigh = index
            if dx != 0 and dy != 0:
                xlim1 = int(-x1*index/dx)
                xlim2 = int((ixmax-x1)*index/dx)+1
                ylim1 = int(-y1*index/dy)
                ylim2 = int((iymax-y1)*index/dy)+1
                ilow  = max(min(xlim1,xlim2),min(ylim1,ylim2),0)
                ihigh = min(max(xlim1,xlim2),max(ylim1,ylim2),index)
            for i in range(ilow,ihigh):
                f = float(i)/index
                ix = int(x1 + f * dx)
                iy = int(y1 + f * dy)
                if ix >= 0 and ix < ixmax and iy >= 0 and iy < iymax:
                    mp[ix][iy] = 1        

    def draw(self):
        self.plotlabels()
        BasicGrapher.draw(self)
        self.plotsymbols()

    def plotsymbols(self):
        if not self.cget("use_symbols"):
            return

        for i in range(len(self.labels)):
            for label in self.labels[i]:
                l = label["symbol"]
                if l is None:
                    continue
                data = self.valueToCanvas(self.getData(i,label["j"]))
                if data is None:
                    continue
                [x,y] = data
                c = self.cget("symbol_color")
                if len(l) == 1:
                    self.create_text(x,y,font=self.cget("symbol_font"),
                                     fill=c,text=l)
                elif l == "fillcircle":
                    self.create_oval(x-3,y-3,x+3,y+3,fill=c,outline=c)
                elif l == "circle":
                    self.create_oval(x-3,y-3,x+3,y+3,outline=c)
                elif l == "square":
                    self.create_rectangle(x-3,y-3,x+3,y+3,outline=c)
                elif l == "crosssquare":
                    self.create_rectangle(x-3,y-3,x+3,y+3,outline=c)
                    self.create_line(x-3,y-3,x+3,y+3,fill=c)
                    self.create_line(x-3,y+3,x+3,y-3,fill=c)
                elif l == "fillsquare":
                    self.create_rectangle(x-3,y-3,x+3,y+3,fill=c,outline=c)

# FIXME:  No regression tester
class InteractiveGrapher(LabeledGrapher):
    def __init__(self,parent=None,cnf={},**kw):
        kw=AUTOutil.cnfmerge((cnf,kw))
        apply(LabeledGrapher.__init__,(self,parent),kw)    

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
                tag=string.split(tag,":")
                if tag[0] == "data_point":
                    point=int(tag[1])
                if tag[0] == "curve":
                    curve=int(tag[1])
        if not((point is None) or (curve is None)):
            label = tkSimpleDialog.askstring("Label","Enter here")
            self.addLabel(curve,point,label)

    def printTagWrapper(self,e):
        id=self.find("closest",e.x,e.y)
        print self.gettags(id[0])
        
    def printValueWrapper(self,e):
        self.__printValue((e.x,e.y))

    def __printValue(self,val):
        print self.canvasToValue(val)

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
        self._configNoDraw(realwidth=e.width)
        self._configNoDraw(realheight=e.height)
        self.configure()
        self.clear()
        self.draw()

class GUIGrapher(InteractiveGrapher):
    def __init__(self,parent=None,cnf={},**kw):
        kw=AUTOutil.cnfmerge((cnf,kw))
        apply(InteractiveGrapher.__init__,(self,parent),kw)
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
        options = []
        for key in self.configure().keys():
            if self._isInternalOption(key):
                options.append(self.configure(key)[0])

        options.sort()
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
        if type(self.cget(key)) == types.IntType:
            self[key] = int(self.valueEntry.get())
        elif type(self.cget(key)) == types.FloatType:
            self[key] = float(self.valueEntry.get())
        elif type(self.cget(key)) == types.StringType:
            self[key] = self.valueEntry.get()
        self.valueLabel.setentry(self.cget(key))

    def __updateInteractiveConfigureDialog(self):
        key = self.optionList.getcurselection()[0]
        self.optionLabel.setentry(key)
        self.valueLabel.setentry(self.cget(key))
        self.valueEntry.clear()
        if type(self.cget(key)) == types.IntType:
            self.valueEntry.configure(validate={"validator":"integer"})
        elif type(self.cget(key)) == types.FloatType:
            self.valueEntry.configure(validate={"validator":"real"})
        elif type(self.cget(key)) == types.StringType:
            self.valueEntry.configure(validate={"validator":"alphanumeric"})
        
    def generatePostscript(self,filename=None,pscolormode=None):
        if pscolormode is None:
            pscolormode=self.cget("ps_colormode")
        if filename is None:
            filename = tkFileDialog.asksaveasfilename(defaultextension=".eps",title="Save as Postscript File")
        self.update()
        self.postscript(file=filename,colormode=pscolormode)

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


def test(grapher):
    import math
    data=[]
    for i in range(62):
        data.append(float(i)*0.1)

    grapher.addArray((data,map(math.sin,data)))
    grapher.addArray((data,map(math.cos,data)))
    grapher.addLabel(0,10,"hello")
    grapher.addLabel(0,30,"world")
    grapher.pack()
    grapher.plot()

    button = Tkinter.Button(text="Quit",command=grapher.quit)
    button.pack()
    button.update()
    print "Press <return> to continue"
    raw_input()

    grapher.delAllData()
    grapher.addArray((data,map(math.cos,data)))
    grapher.plot()
    print "Press <return> to continue"
    raw_input()

if __name__=='__main__':
    test(GUIGrapher())






