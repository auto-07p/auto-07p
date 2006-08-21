#!/usr/bin/env python
import Tkinter
import Pmw
import tkSimpleDialog
import tkFileDialog
import string
import types
import AUTOutil
import copy
import optionHandler
import math

GrapherError="GrapherError"

class BasicGrapher(optionHandler.OptionHandler,Tkinter.Canvas):
    """Documentation string for Basic Grapher

    A simple graphing widget
    By Randy P."""
    def __init__(self,parent=None,cnf={},**kw):
        self.data = []

        #Get the data from the arguements and then erase the
        #ones which are not used by canvas
        optionDefaults={}
        optionDefaults["minx"] = (0,None)
        optionDefaults["maxx"] = (0,None)
        optionDefaults["miny"] = (0,None)
        optionDefaults["maxy"] = (0,None)
        optionDefaults["left_margin"] = (80,None)
        optionDefaults["right_margin"] = (40,None)
        optionDefaults["top_margin"] = (40,None)
        optionDefaults["bottom_margin"] = (40,None)
        optionDefaults["decorations"] = (1,None)
        optionDefaults["xlabel"] = ("",None)
        optionDefaults["ylabel"] = ("",None)
        optionDefaults["xticks"] = (5,None)
        optionDefaults["yticks"] = (5,None)
        optionDefaults["grid"] = ("yes",None)
        optionDefaults["tick_label_template"] = ("%.2e",None)
        optionDefaults["tick_length"] = (0.2,None)
        optionDefaults["odd_tick_length"] = (0.4,None)
        optionDefaults["even_tick_length"] = (0.2,None)
        # background is handled by the Canvas widget
        optionDefaults["foreground"] = ("black",None)
        optionDefaults["color_list"] = ("black red green blue",None)
        optionDefaults["symbol_font"] = ("-misc-fixed-*-*-*-*-*-*-*-*-*-*-*-*",None)
        optionDefaults["symbol_color"] = ("red",None)
        optionDefaults["smart_label"] = (0,None)
        optionDefaults["line_width"] = (2,None)
        optionDefaults["realwidth"] = (1,None)
        optionDefaults["realheight"] = (1,None)

        optionAliases = {}
        optionAliases["fg"] = "foreground"
        # __parseOptions uses functions from the Canvas
        # widget, so we need to initialize it first
        apply(Tkinter.Canvas.__init__,(self,parent))
        optionHandler.OptionHandler.__init__(self,Tkinter.Canvas)

        dict = AUTOutil.cnfmerge((cnf,kw))
        self.addOptions(optionDefaults)
        self.addAliases(optionAliases)
        BasicGrapher.config(self,dict)

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

    def _addData(self,data):
        for array in data:
            if len(array[0]) != len(array[1]):
                raise GrapherError,"Array lengths must match"
            new_array={}
            new_array["x"]=array[0]
            new_array["y"]=array[1]
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

    def addArrayNoDraw(self,array):        
        self._addData((array,))

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
        if range / inc >= 7.5:
            inc = inc * 2
        if range / inc <= 0.5:
            inc = inc / 2
        if range / inc <= 0.5:
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
        num = ( maximum - minimum ) / inc 
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
            self._configNoDraw(minx=dict["min"])
            self._configNoDraw(maxx=dict["max"])
            self._configNoDraw(xticks=dict["divisions"])
        elif guess_maximum != None:
            self._configNoDraw(minx=guess_minimum-1)
            self._configNoDraw(maxx=guess_maximum+1)
            
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
            self._configNoDraw(miny=dict["min"])
            self._configNoDraw(maxy=dict["max"])
            self._configNoDraw(yticks=dict["divisions"])
        elif guess_minimum != None:
            self._configNoDraw(miny=guess_minimum-1)
            self._configNoDraw(maxy=guess_maximum+1)


    def getXRange(self):
        return [self.cget("minx"),self.cget("maxx")]

    def getYRange(self):
        return [self.cget("miny"),self.cget("maxy")]

    def clear(self):
        for x in self.find_all():
            self.delete(x)

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
        xscale = (maxx - minx) / adjwidth
        yscale = (maxy - miny) / adjheight
        for i in range(len(self.data)):
            fill=color_list[i%len(color_list)]
            curve="curve:%d"%(i,)
            n=len(self.getData(i,"x"))
            [x,y]=self.__valueToCanvasFast(self.getData(i,0),minx,maxx,miny,maxy,
                                            width,height,left_margin,right_margin,top_margin,bottom_margin)
            # If we only have one point we draw a small circle
            if len(self.getData(i,"x")) == 1:
                self.create_oval(x-3,y-3,x+3,y+3,
                                 tags=("data_point:%d"%(0,),"curve:%d"%(i,),"data"),
                                 fill=color_list[i%len(color_list)])
            else:
                line = [x, y]
                xs = self.data[i]["x"]
                ys = self.data[i]["y"]
                for j in range(1, n):
                    line.append((xs[j]-minx) / xscale + left_margin)
                    line.append((adjheight - (ys[j]-miny) / yscale + top_margin))
                self.create_line(line,width=line_width,tags=(curve,"data"),fill=fill)
            
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
            
            xticks=int(self.cget("xticks"))
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
            yticks=int(self.cget("yticks"))
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

    def _addData(self,data):
        self.labels.append([])
        BasicGrapher._addData(self,data)

    def draw(self):
        symbols = []
        for i in range(len(self.labels)):
            for label in self.labels[i]:
                j = label["j"]

                #Find a neighbor so I cam compute the "slope"
                if j < len(self.getData(i,"x"))-1:
                    first = j
                    second = j+1
                else:
                    first = j-1
                    second = j                
                data = self.valueToCanvas(self.getData(i,j))
                if not(data is None):
                    x = data[0]
                    y = data[1]
                    #pick a good direction for the label
                    if (self.getData(i,"y")[second] - self.getData(i,"y")[first]) > 0:
                        if (x < int(self.cget("realwidth"))-(20+self.cget("left_margin"))) and (y > (20+self.cget("top_margin"))):
                            xoffset = 10
                            yoffset = -10
                            anchor="sw"
                        else:
                            xoffset = -10
                            yoffset = 10
                            anchor="ne"
                    else:
                        if (x > 20+self.cget("left_margin")) and (y > 20+self.cget("top_margin")):
                            xoffset = -10
                            yoffset = -10
                            anchor="se"
                        else:
                            xoffset = 10
                            yoffset = 10
                            anchor="nw"

                    #self.addtag_overlapping("overlaps",x+xoffset-3,y+yoffset-3,x+xoffset+3,y+yoffset+3)
                    #if len(self.gettags("overlaps")) != 0:
                    #    print self.gettags("overlaps")
                    #self.dtag("overlaps")
                    #print "---------------------------------------------"    

                    if len(label["text"]) > 0:
                        self.create_line(x,y,x+xoffset,y+yoffset,fill=self.cget("foreground"))
                        self.create_text(x+xoffset,y+yoffset,text=label["text"],anchor=anchor,fill=self.cget("foreground"))

                    if not(label["symbol"] is None):
                        symbols.append({})
                        symbols[-1]["x"] = x
                        symbols[-1]["y"] = y
                        symbols[-1]["symbol"] = label["symbol"]

        BasicGrapher.draw(self)

        for symbol in symbols:
            if len(symbol["symbol"]) == 1:
                self.create_text(symbol["x"],symbol["y"],
                                 font=self.cget("symbol_font"),
                                 fill=self.cget("symbol_color"),
                                 text=symbol["symbol"])
            elif symbol["symbol"] == "fillcircle":
                self.create_oval(symbol["x"]-3,symbol["y"]-3,
                                 symbol["x"]+3,symbol["y"]+3,
                                 fill=self.cget("symbol_color"),
                                 outline=self.cget("symbol_color"))
            elif symbol["symbol"] == "circle":
                self.create_oval(symbol["x"]-3,symbol["y"]-3,
                                 symbol["x"]+3,symbol["y"]+3,
                                 outline=self.cget("symbol_color"))
            elif symbol["symbol"]  == "square":
                self.create_rectangle(symbol["x"]-3,symbol["y"]-3,
                                      symbol["x"]+3,symbol["y"]+3,
                                      outline=self.cget("symbol_color"))
            elif symbol["symbol"]  == "crosssquare":
                self.create_rectangle(symbol["x"]-3,symbol["y"]-3,
                                      symbol["x"]+3,symbol["y"]+3,
                                      outline=self.cget("symbol_color"))
                self.create_line(symbol["x"]-3,symbol["y"]-3,
                                symbol["x"]+3,symbol["y"]+3,
                                fill=self.cget("symbol_color"))
                self.create_line(symbol["x"]-3,symbol["y"]+3,
                                symbol["x"]+3,symbol["y"]-3,
                                fill=self.cget("symbol_color"))
            elif symbol["symbol"]  == "fillsquare":
                self.create_rectangle(symbol["x"]-3,symbol["y"]-3,
                                      symbol["x"]+3,symbol["y"]+3,
                                      fill=self.cget("symbol_color"),
                                      outline=self.cget("symbol_color"))
        

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
                    point=string.atoi(tag[1])
                if tag[0] == "curve":
                    curve=string.atoi(tag[1])
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
            self[key] = string.atoi(self.valueEntry.get())
        elif type(self.cget(key)) == types.FloatType:
            self[key] = string.atof(self.valueEntry.get())
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
        
    def generatePostscript(self,filename=None):
        if filename is None:
            filename = tkFileDialog.asksaveasfilename(defaultextension=".eps",title="Save as Postscript File")
        self.update()
        self.postscript(file=filename)

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


def test():
    import math
    data=[]
    for i in range(62):
        data.append(float(i)*0.1)

    grapher = GUIGrapher()
    grapher.addArray((data,map(math.sin,data)))
    grapher.addArray((data,map(math.cos,data)))
    grapher.addLabel(0,10,"hello")
    grapher.addLabel(0,30,"world")
    grapher.pack()

    button = Tkinter.Button(text="Quit",command=grapher.quit)
    button.pack()
    button.update()
    print "Press <return> to continue"
    raw_input()

    grapher.delAllData()
    grapher.addArray((data,map(math.cos,data)))
    print "Press <return> to continue"
    raw_input()

if __name__=='__main__':
    test()






