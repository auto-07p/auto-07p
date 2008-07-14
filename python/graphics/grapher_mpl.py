#!/usr/bin/env python
import matplotlib
matplotlib.use('TkAgg')

from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2TkAgg
from matplotlib.figure import Figure

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

class FigureCanvasTkAggRedraw(FigureCanvasTkAgg):
    def __init__(self,grapher,parent):
        if parent is None:
            parent = Tkinter.Tk()
        parent.wm_title("PyPLAUT")
        FigureCanvasTkAgg.__init__(self,grapher.ax.get_figure(),master=parent)
        
        tkwidget = self.get_tk_widget()

        toolbar = NavigationToolbar2TkAgg( self, parent )
        toolbar.zoom(self)

        self.grapher = grapher

    def redraw(self):
        # recalculate label positions
        self.grapher.plotlabels()
        FigureCanvasTkAgg.draw(self)
        
    def draw(self):
        ax = self.grapher.ax
        [minx,maxx] = ax.get_xlim()
        [miny,maxy] = ax.get_ylim()
        self.grapher._configNoDraw(minx=minx,maxx=maxx,miny=miny,maxy=maxy)
        self.grapher._configNoDraw(xticks=None,yticks=None)
        self.redraw()

    def show(self):
        fig = self.grapher.ax.get_figure() 
        dpi = fig.get_dpi()
        self.grapher._configNoDraw(
            realwidth=fig.get_figwidth()*dpi,
            realheight=fig.get_figheight()*dpi)
        self.redraw()

class BasicGrapher(optionHandler.OptionHandler):
    """Documentation string for Basic Grapher

    A simple graphing widget
    By Randy P."""
    def __init__(self,parent=None,cnf={},**kw):
        self.data = []

        #Get the data from the arguements and then erase the
        #ones which are not used by canvas
        optionDefaults={}
        optionDefaults["minx"] = (0,self.__optionCallback)
        optionDefaults["maxx"] = (0,self.__optionCallback)
        optionDefaults["miny"] = (0,self.__optionCallback)
        optionDefaults["maxy"] = (0,self.__optionCallback)
        optionDefaults["left_margin"] = (80,self.__optionCallback)
        optionDefaults["right_margin"] = (40,self.__optionCallback)
        optionDefaults["top_margin"] = (40,self.__optionCallback)
        optionDefaults["bottom_margin"] = (40,self.__optionCallback)
        optionDefaults["decorations"] = (1,self.__optionCallback)
        optionDefaults["xlabel"] = (None,self.__optionCallback)
        optionDefaults["ylabel"] = (None,self.__optionCallback)
        optionDefaults["xticks"] = (None,self.__optionCallback)
        optionDefaults["yticks"] = (None,self.__optionCallback)
        optionDefaults["grid"] = ("yes",self.__optionCallback)
        optionDefaults["tick_label_template"] = ("%.2e",self.__optionCallback)
        optionDefaults["tick_length"] = (0.2,self.__optionCallback)
        optionDefaults["odd_tick_length"] = (0.4,self.__optionCallback)
        optionDefaults["even_tick_length"] = (0.2,self.__optionCallback)
        optionDefaults["background"] = ("white",self.__optionCallback)
        optionDefaults["foreground"] = ("black",self.__optionCallback)
        optionDefaults["color_list"] = ("black red green blue",self.__optionCallback)
        optionDefaults["symbol_font"] = ("-misc-fixed-*-*-*-*-*-*-*-*-*-*-*-*",self.__optionCallback)
        optionDefaults["symbol_color"] = ("red",self.__optionCallback)
        optionDefaults["smart_label"] = (0,self.__optionCallback)
        optionDefaults["line_width"] = (2,self.__optionCallback)
        optionDefaults["realwidth"] = (1,self.__optionCallback)
        optionDefaults["realheight"] = (1,self.__optionCallback)
        optionDefaults["use_labels"] = (1,self.__optionCallback)
        optionDefaults["use_symbols"] = (1,self.__optionCallback)
        optionDefaults["width"] = (1,self.__optionCallback)
        optionDefaults["height"] = (1,self.__optionCallback)
        optionDefaults["title"] = ("",self.__optionCallback)

        optionAliases = {}
        optionAliases["fg"] = "foreground"
        optionHandler.OptionHandler.__init__(self)

        self.ax = Figure(figsize=(4.3,3.0)).gca()
        self.ax.set_autoscale_on(0)
        self.canvas = FigureCanvasTkAggRedraw(self,parent)
        tk_widget = self.canvas.get_tk_widget()
        self.quit = tk_widget.quit
        self.bind = tk_widget.bind
        self.unbind = tk_widget.unbind
        self.postscript = self.canvas.print_figure
        self.winfo_rootx = tk_widget.winfo_rootx
        self.winfo_rooty = tk_widget.winfo_rooty
        self.redrawlabels = 0

        dict = AUTOutil.cnfmerge((cnf,kw))
        self.addOptions(optionDefaults)
        self.addAliases(optionAliases)
        BasicGrapher._configNoDraw(self,dict)
        for key in ["grid","decorations","xlabel","ylabel"]:
            self.__optionCallback(key,self.cget(key),[])
        matplotlib.rcParams["axes.edgecolor"]=self.cget("foreground")

    def pack(self,**kw):
        self.canvas.get_tk_widget().pack(kw)

    def update(self):
        self.canvas.get_tk_widget().update()
        FigureCanvasTkAgg.draw(self.canvas)

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

    def __optionCallback(self,key,value,options):
        if key in ["minx","maxx","miny","maxy","realwidth","realheight"]:
            self.redrawlabels = 1
            if key == "minx":
                self.ax.set_xlim(xmin=value)
            elif key == "maxx":
                self.ax.set_xlim(xmax=value)
            elif key == "miny":
                self.ax.set_ylim(ymin=value)
            elif key == "maxy":
                self.ax.set_ylim(ymax=value)
            elif key == "realwidth":
                lm = self.cget("left_margin")
                rm = self.cget("right_margin")
                self.ax.get_figure().subplots_adjust(left=lm/value,
                                                     right=1-rm/value)
            elif key == "realheight":
                tm = self.cget("top_margin")
                bm = self.cget("bottom_margin")
                self.ax.get_figure().subplots_adjust(top=1-tm/value,
                                                     bottom=bm/value)
        elif key == "grid":
            self.ax.grid(value == "yes")
        elif key == "width":
            self.canvas.get_tk_widget()[key] = value
        elif key == "height":
            self.canvas.get_tk_widget()[key] = value
        elif key == "title":
            self.ax.set_title(value)
        elif key == "background":
            self.ax.set_axis_bgcolor(value)
        elif key == "decorations":
            if value:
                self.ax.set_axis_on()
            else:
                self.ax.set_axis_off()
        elif key == "use_symbols":
            self.plotsymbols()
        elif key == "use_labels":
            self.plotlabels()
        elif key == "xlabel":
            if value is None:
                value = ""
            self.ax.set_xlabel(value,color=self.cget("foreground"))
        elif key == "ylabel":
            if value is None:
                value = ""
            self.ax.set_ylabel(value,color=self.cget("foreground"))
        elif key in ["xticks","yticks"]:
            if value is None:
                if key == "xticks":
                    self.ax.set_xscale('linear')
                else:
                    self.ax.set_yscale('linear')
            else:
                ticks = []
                min = self.cget("min"+key[0])
                max = self.cget("max"+key[0])
                for i in range(value):
                    ticks.append(min + ((max - min) * i) / (value-1))
                if key == "xticks":
                    self.ax.set_xticks(ticks)
                else:
                    self.ax.set_yticks(ticks)


    # This version can be used to increase efficiency
    # for example, if you want to config, but know you
    # will need to redraw later.
    def _configNoDraw(self,cnf=None,**kw):
        if type(cnf) == types.StringType or (cnf is None and len(kw) == 0):
            return optionHandler.OptionHandler.config(self,cnf)
        else:
            cnf = AUTOutil.cnfmerge((cnf,kw))
            optionHandler.OptionHandler.config(self,cnf)
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
        self._configNoDraw(xticks=None,yticks=None)
        for d in self.data:
            self.ax.lines.remove(d["mpline"])
        self.data=[]

    def delAllData(self):
        self._delAllData()
        self.clear()

    def _delData(self,index):
        self.ax.lines.remove(data[index]["mpline"])
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
        self.ax.get_figure().axes = []

    def draw(self):
        if self.redrawlabels:
            self.plotlabels()
        self.ax.get_figure().axes = [self.ax]
        FigureCanvasTkAgg.draw(self.canvas)

    def plot(self):
        color_list = string.split(self.cget("color_list"))
            
        # data
        line_width=self.cget("line_width")
        i=0
        for d in self.data:
            curve="curve:%d"%(i,)
            fill=color_list[i%len(color_list)]
            if len(d["x"]) == 1:
                # If we only have one point we draw a small circle
                self.ax.plot(d["x"],d["y"],'o'+fill[0])
                #tags=("data_point:%d"%(0,),curve,"data")
            else:
                xs = d["x"]
                ys = d["y"]
                stable = d["stable"]
                #tags=(curve,"data")
                if stable is None or stable:
                    self.ax.plot(xs,ys,color=fill,lw=line_width)
                else:
                    self.ax.plot(xs,ys,color=fill,ls='--',lw=line_width)
            d["mpline"] = self.ax.lines[-1]
            if d["newsect"] is None or d["newsect"]:
                i = i+1
        self.ax.get_figure().axes = [self.ax]
            
    def __setitem__(self,key,value):
        apply(self.configure, (), {key: value})

    def __getitem__(self,key):
        return self.cget(key)

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
        new_label["mpline"]=None
        new_label["mptext"]=None
        new_label["mpsymline"]=None
        new_label["mpsymtext"]=None
        self.labels[i].append(new_label)

    def _delData(self,i):
        del self.labels[i]
        BasicGrapher._delData(self,i)

    def _delAllData(self):
        for l in self.labels:
            for label in l:
                if label["mpline"]:
                    self.ax.lines.remove(label["mpline"])
                if label["mptext"]:
                    self.ax.texts.remove(label["mptext"])
                if label["mpsymline"]:
                    self.ax.lines.remove(label["mpsymline"])
                if label["mpsymtext"]:
                    self.ax.texts.remove(label["mpsymtext"])
        self.labels=[]
        BasicGrapher._delAllData(self)

    def _addData(self,data,newsect=None,stable=None):
        self.labels.append([])
        BasicGrapher._addData(self,data,newsect,stable)

    def plotlabels(self):
        self.redrawlabels = 0
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
                if 'transform' in dir(self.ax.transData):
                    data = self.ax.transData.transform([self.getData(i,j)])[0]
                else:
                    data = self.ax.transData.xy_tup(self.getData(i,j))
                if not(data is None):
                    x = data[0]
                    y = data[1]
                    realwidth=self.cget("realwidth")
                    realheight=self.cget("realheight")
                    left_margin=self.cget("left_margin")
                    top_margin=self.cget("top_margin")
                    y = realheight - y
                    #pick a good direction for the label
                    if (self.getData(i,"y")[second] - self.getData(i,"y")[first]) > 0:
                        if (x < int(self.cget("realwidth"))-(20+self.cget("left_margin"))) and (y > (20+self.cget("top_margin"))):
                            xoffset = 10
                            yoffset = -10
                            va = "bottom"
                            ha = "left"
                        else:
                            xoffset = -10
                            yoffset = 10
                            va = "top"
                            ha = "right"
                    else:
                        if (x > 20+self.cget("left_margin")) and (y > 20+self.cget("top_margin")):
                            xoffset = -10
                            yoffset = -10
                            va = "bottom"
                            ha = "right"
                        else:
                            xoffset = 10
                            yoffset = 10
                            va = "top"
                            ha = "left"

                    #self.addtag_overlapping("overlaps",x+xoffset-3,y+yoffset-3,x+xoffset+3,y+yoffset+3)
                    #if len(self.gettags("overlaps")) != 0:
                    #    print self.gettags("overlaps")
                    #self.dtag("overlaps")
                    #print "---------------------------------------------"    

                    y = realheight - y
                    [x1,y1] = self.getData(i,j)
                    if label["mpline"]:
                        self.ax.lines.remove(label["mpline"])
                    label["mpline"] = None
                    if label["mptext"]:
                        self.ax.texts.remove(label["mptext"])
                    label["mptext"] = None
                    if self.cget("use_labels") and len(label["text"]) > 0:
                        if 'transform' in dir(self.ax.transData):
                            [x2,y2] = self.ax.transData.inverted().transform(
                                (x+xoffset,y-yoffset))
                        else:
                            [x2,y2] = self.ax.transData.inverse_xy_tup(
                                (x+xoffset,y-yoffset))
                        self.ax.plot([x1,x2],[y1,y2],
                                     color=self.cget("foreground"))
                        self.ax.text(x2,y2,label["text"],ha=ha,va=va,
                                     color=self.cget("foreground"))
                        label["mpline"] = self.ax.lines[-1]
                        label["mptext"] = self.ax.texts[-1]

    def plot(self):
        self.plotlabels()
        BasicGrapher.plot(self)
        self.plotsymbols()

    def plotsymbols(self):
        for i in range(len(self.labels)):
            for label in self.labels[i]:
                [x,y] = self.getData(i,label["j"])
                l = label["symbol"]
                if l is None:
                    continue
                c=self.cget("symbol_color")
                mfc=self.ax.get_axis_bgcolor()
                if label["mpsymline"]:
                    self.ax.lines.remove(label["mpsymline"])
                label["mpsymline"] = None
                if label["mpsymtext"]:
                    self.ax.texts.remove(label["mpsymtext"])
                label["mpsymtext"] = None
                if not self.cget("use_symbols"):
                    continue
                elif len(l) == 1:
                    #font=self.cget("symbol_font"),
                    self.ax.text(x,y,l,ha="center",va="center",color=c)
                    label["mpsymtext"] = self.ax.texts[-1]
                    continue
                elif l == "fillcircle":
                    self.ax.plot([x],[y],'o'+c[0])
                elif l == "circle":
                    self.ax.plot([x],[y],'o'+c[0],mfc=mfc)
                elif l == "square":
                    self.ax.plot([x],[y],'s'+c[0],mfc=mfc)
                elif l == "crosssquare":
                    self.ax.plot([x],[y],'x'+c[0])
                elif l == "fillsquare":
                    self.ax.plot([x],[y],'s'+c[0])
                elif l == "diamond":
                    self.ax.plot([x],[y],'D'+c[0],mfc=mfc)
                elif l == "filldiamond":
                    self.ax.plot([x],[y],'D'+c[0])
                elif l == "triangle":
                    self.ax.plot([x],[y],'^'+c[0],mfc=mfc)
                elif l == "doubletriangle":
                    self.ax.plot([x],[y],'^'+c[0])
                else:
                    continue
                label["mpsymline"] = self.ax.lines[-1]
        
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
        
class GUIGrapher(InteractiveGrapher):
    def __init__(self,parent=None,cnf={},**kw):
        kw=AUTOutil.cnfmerge((cnf,kw))
        apply(InteractiveGrapher.__init__,(self,parent),kw)
        self.bind("<ButtonPress-3>",self.popupMenuWrapper)
        self.menu=Tkinter.Menu()
#        self.menu.add_radiobutton(label="print tag",command=self.printTagBindings)
#        self.menu.add_radiobutton(label="label point",command=self.labelPointBindings)
        #self.menu.add_radiobutton(label="zoom",command=self.zoomBindings)
        #self.menu.invoke('zoom')
        self.menu.add_command(label="Unzoom",command=self.unzoom)
        self.menu.add_command(label="Save",command=self.generatePostscript)
        self.menu.add_command(label="Configure...",command=self.__interactiveConfigureDialog)

    def __interactiveConfigureDialog(self):
        widget=self.canvas.get_tk_widget()
        diag = Pmw.Dialog(widget,buttons=("Ok","Cancel"))
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
            filename = tkFileDialog.asksaveasfilename(defaultextension=".eps",title="Save the figure")
        self.update()
        #self.postscript(filename,colormode=pscolormode)
        self.postscript(filename)

    def printTagBindings(self):
        self.bind("<ButtonPress-1>",self.printTagWrapper)
        self.unbind("<B1-Motion>")
        self.unbind("<ButtonRelease-1>")
        
    def labelPointBindings(self):
        self.bind("<ButtonPress-1>",self.labelPointWrapper)
        self.unbind("<B1-Motion>")
        self.unbind("<ButtonRelease-1>")
        
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
    test()






