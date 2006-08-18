#    Visualization Bifurcation Manifolds
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

import Pmw
import Tkinter
import parseS
import parseB
import string
import AUTOutil
import types
import optionHandler
import copy
import math
import os
import ConfigParser
import OpenInventorWrapper

class plotter3D(optionHandler.OptionHandler,Tkinter.Frame):
    def __init__(self,parent=None,cnf={},**kw):
        optionDefaults={}
        optionDefaults["problem"] = ("auto_plotter3d",self.__optionCallback)
        # The kind of diagram (single solution vs. bifur diagram)
        optionDefaults["type"]     = ("bifurcation",self.__optionCallback)  
        # The X column
        optionDefaults["bifurcation_x"]     = ([0],self.__optionCallback)
        optionDefaults["solution_x"]        = (["t"],self.__optionCallback)
        # The Y column
        optionDefaults["bifurcation_y"]     = ([1],self.__optionCallback)
        optionDefaults["solution_y"]        = ([0],self.__optionCallback)
        # The Z column
        optionDefaults["bifurcation_z"]     = ([1],self.__optionCallback)
        optionDefaults["solution_z"]        = ([0],self.__optionCallback)
        # The Color column
        optionDefaults["bifurcation_color"] = ([1],self.__optionCallback)
        optionDefaults["solution_color"]    = ([0],self.__optionCallback)
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
        optionDefaults["mark_t"]         = (None, self.__optionCallback)
        optionDefaults["mark_t_radius"]  = (0.01, self.__optionCallback)
        optionDefaults["mark_t_color"]   = ((1.0,1.0,1.0), self.__optionCallback)


        # Options for the GUI
        optionDefaults["bifurcation_x_scale"]       = (1.0,None)
        optionDefaults["bifurcation_y_scale"]       = (1.0,None)
        optionDefaults["bifurcation_z_scale"]       = (1.0,None)
        optionDefaults["bifurcation_x_trans"]       = (0.0,None)
        optionDefaults["bifurcation_y_trans"]       = (0.0,None)
        optionDefaults["bifurcation_z_trans"]       = (0.0,None)
        optionDefaults["solution_x_scale"]       = (1.0,None)
        optionDefaults["solution_y_scale"]       = (1.0,None)
        optionDefaults["solution_z_scale"]       = (1.0,None)
        optionDefaults["solution_x_trans"]       = (0.0,None)
        optionDefaults["solution_y_trans"]       = (0.0,None)
        optionDefaults["solution_z_trans"]       = (0.0,None)

        optionDefaults["line_width"]        = (1.0,None)
        optionDefaults["cylinder"]     = (0,None)
        optionDefaults["geom_comp"]    = (7,None)
        optionDefaults["light_comp"]   = (7,None)
        optionDefaults["write_points_on"] = (0,None)

        optionDefaults["grid_on"]       = (0,None)
        optionDefaults["grid_lines"]    = (6,None)
        optionDefaults["grid_1_start"]  = (-1.0,None)
        optionDefaults["grid_2_start"]  = (-1.0,None)
        optionDefaults["grid_1_end"]    = (1.0,None)
        optionDefaults["grid_2_end"]    = (1.0,None)
        optionDefaults["grid_width"]    = (1.0,None)
        optionDefaults["grid_R"]        = (1.0,None)
        optionDefaults["grid_G"]        = (1.0,None)
        optionDefaults["grid_B"]        = (1.0,None)
        optionDefaults["grid_cylinder"] = (0,None)
        optionDefaults["grid_direction"]= (["x","y","z"],None)

        optionDefaults["cube_on"]    = (1,None)
        optionDefaults["cube_R"]     = (1.0,None)
        optionDefaults["cube_G"]     = (1.0,None)
        optionDefaults["cube_B"]     = (1.0,None)
        

        optionDefaults["auto_center"]= (1,None)
        optionDefaults["auto_scale"]= (1,None)

        parser = ConfigParser.ConfigParser()
        parser.add_section("AUTO_plotter3D")
        if(os.path.exists(os.path.expandvars("$AUTO_DIR/.autorc"))):
            parser.read(os.path.expandvars("$AUTO_DIR/.autorc"))
        if(os.path.exists(os.path.expandvars("$HOME/.autorc"))):
            parser.read(os.path.expandvars("$HOME/.autorc"))
        if(os.path.exists("./.autorc")):
            parser.read("./.autorc")

        for option in parser.options("AUTO_plotter3D"):
            optionDefaults[option] = (eval(parser.get("AUTO_plotter3D",option)),self.__optionCallback)


        Tkinter.Frame.__init__(self,parent)
        optionHandler.OptionHandler.__init__(self)

        self.user_data = {}

        dict = AUTOutil.cnfmerge((cnf,kw))
        self.addOptions(optionDefaults)
        plotter3D.config(self,dict)


    def __optionCallback(self,key,value,options):
        if key == "runner":
            self.cget("bifurcation_diagram").read(value.getBifurcation_diagram())
            self.cget("solution").read(value.getSolution())
        elif key == "bifurcation_diagram_filename":
            self.cget("bifurcation_diagram").readFilename(value)
        elif key == "solution_filename":
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
        elif key == "problem":
            self.set_problem(value)

    def config(self,cnf=None,**kw):
        if type(cnf) == types.StringType or (cnf is None and len(kw) == 0):
            return self._configNoDraw(cnf)
        else:
            self._configNoDraw(AUTOutil.cnfmerge((cnf,kw)))
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

    def generatePostscript(self):
        pass

    def draw(self):
        send_data = {}
        
        send_data["X"] = []    
        send_data["Y"] = []    
        send_data["Z"] = []    
        send_data["Color"] = []
        if self.cget("type") == "bifurcation":
            data = self.cget("bifurcation_diagram")
            send_data["X"].append([])
            send_data["Y"].append([])
            send_data["Z"].append([])
            send_data["Color"].append([])
            if len(self.cget("bifurcation_x")) == len(self.cget("bifurcation_y")) == len(self.cget("bifurcation_z")) == len(self.cget("bifurcation_color")):
                for i in range(len(self.cget("bifurcation_x"))):
                    for value in data:
                        xval = value["data"][self.cget("bifurcation_x")[i]]
                        yval = value["data"][self.cget("bifurcation_y")[i]]
                        zval = value["data"][self.cget("bifurcation_z")[i]]
                        colorval = value["data"][self.cget("bifurcation_color")[i]]
                        send_data["X"][0].append(self.cget("bifurcation_x_scale")*(xval+self.cget("bifurcation_x_trans")))
                        send_data["Y"][0].append(self.cget("bifurcation_y_scale")*(yval+self.cget("bifurcation_y_trans")))
                        send_data["Z"][0].append(self.cget("bifurcation_z_scale")*(zval+self.cget("bifurcation_z_trans")))
                        send_data["Color"][0].append(colorval)
            send_data["markers"] = []
        else:
            send_data["markers"] = []
            if len(self.cget("solution_x")) == len(self.cget("solution_y")) == len(self.cget("solution_z")) == len(self.cget("solution_color")):
                for k in range(len(self.cget("index"))):
                    data = self.cget("solution")[self.cget("index")[k]]["data"]
                    for i in range(len(self.cget("solution_x"))):
                        send_data["X"].append([])
                        send_data["Y"].append([])
                        send_data["Z"].append([])
                        send_data["Color"].append([])

                        x_coord = self.cget("solution_x")[i]
                        y_coord = self.cget("solution_y")[i]
                        z_coord = self.cget("solution_z")[i]
                        color_coord = self.cget("solution_color")[i]

                        start_s = -999.00
                        for j in range(len(data)):
                            if start_s != -999.00:
                                if start_s < self.cget("mark_t") < data[j]["t"]:
                                    send_data["markers"].append({})
                                    send_data["markers"][-1]["branch"] = i
                                    send_data["markers"][-1]["point"] = j
                                    send_data["markers"][-1]["radius"] = self.cget("mark_t_radius")
                                    send_data["markers"][-1]["R"] = self.cget("mark_t_color")[0]
                                    send_data["markers"][-1]["G"] = self.cget("mark_t_color")[1]
                                    send_data["markers"][-1]["B"] = self.cget("mark_t_color")[2]
                                start_s = data[j]["t"]
                            else:
                                start_s = data[j]["t"]
                            if x_coord == "t":
                                xval = data[j]["t"]
                            else:
                                xval = data[j]["u"][x_coord]

                            if y_coord == "t":
                                yval = data[j]["t"]
                            else:
                                yval = data[j]["u"][y_coord]

                            if z_coord == "t":
                                zval = data[j]["t"]
                            else:
                                zval = data[j]["u"][z_coord]

                            if color_coord == "t":
                                colorval = data[j]["t"]
                            else:
                               colorval = data[j]["u"][color_coord]

                            send_data["X"][-1].append(self.cget("solution_x_scale")*(xval+self.cget("solution_x_trans")))
                            send_data["Y"][-1].append(self.cget("solution_y_scale")*(yval+self.cget("solution_y_trans")))
                            send_data["Z"][-1].append(self.cget("solution_z_scale")*(zval+self.cget("solution_z_trans")))
                            send_data["Color"][-1].append(colorval)


        if self.cget("auto_center") != 0 or self.cget("auto_scale") != 0:
            for x in ("X","Y","Z"):
                max = []
                min = []
                for i in range(len(send_data[x])):
                    sorted = copy.deepcopy(send_data[x][i])
                    sorted.sort()
                    min.append(sorted[0])
                    max.append(sorted[-1])
                max.sort()
                min.sort()
                max = max[-1]
                min = min[0]

                if self.cget("auto_center") != 0:
                    trans = (max+min)/2.0
                else:
                    trans = 0.0
                    
                if self.cget("auto_scale") != 0:
                    scale = 2.0 * math.fabs(max-min)
                else:
                    scale = 1.0
                    
                
                if scale == 0.0:
                    scale = 1.0
                    if x == "X":
                        cube_x_max=0.25
                        cube_x_min=-0.25
                    if x == "Y":
                        cube_y_max=0.25
                        cube_y_min=-0.25
                    if x == "Z":
                        cube_z_max=0.25
                        cube_z_min=-0.25
                else:
                    if x == "X":
                        cube_x_max=(max-trans)*1.0/scale
                        cube_x_min=(min-trans)*1.0/scale
                    if x == "Y":
                        cube_y_max=(max-trans)*1.0/scale
                        cube_y_min=(min-trans)*1.0/scale
                    if x == "Z":
                        cube_z_max=(max-trans)*1.0/scale
                        cube_z_min=(min-trans)*1.0/scale
                    

                for i in range(len(send_data[x])):
                    for j in range(len(send_data[x][i])):
                        send_data[x][i][j] = (send_data[x][i][j] - trans)*1.0/scale

        grid = {}
        cube = {}
        for key in self.simpleOptionDictionary().keys():
            if key[0:5] == "grid_":
                grid[key] = self.cget(key)
            if key[0:5] == "cube_":
                cube[key] = self.cget(key)
        cube["cube_x_max"] = cube_x_max
        cube["cube_x_min"] = cube_x_min 
        cube["cube_y_max"] = cube_y_max
        cube["cube_y_min"] = cube_y_min
        cube["cube_z_max"] = cube_z_max
        cube["cube_z_min"] = cube_z_min
        
        options = "%f %d %d %d" % (self.cget("line_width"),
                                   self.cget("cylinder"),
                                   self.cget("geom_comp"),
                                   self.cget("light_comp"))
        send_data["grid"] = grid
        send_data["cube"] = cube
        send_data["options"] = options
        send_string = str(id(send_data))

        output = open("AUTOtest.iv","w")
        output.write("#Inventor V2.1 ascii\n")
        for j in range(len(send_data["X"])):
            output.write("VertexProperty {\n")
            output.write("    vertex [\n")
            for i in range(len(send_data["X"][j])):
                output.write("            %f %f %f"%(send_data["X"][j][i],
                                         send_data["Y"][j][i],
                                         send_data["Z"][j][i]))
                if i != len(send_data["X"][j])-1:
                    output.write(",")
                output.write("\n")
            output.write("           ]\n")
            output.write("}\n")


            output.write("IndexedLineSet {\n")
            output.write(" coordIndex [\n")
            for i in range(len(send_data["X"][0])):
                output.write("%d"%i)
                if i != len(send_data["X"][0])-1:
                    output.write(",")
                output.write(" ")
            output.write("           ]\n")

            output.write("}\n")


