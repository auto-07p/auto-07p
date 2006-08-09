#    DataViewer visualization package
#    Copyright (C) 1997 Randy Paffenroth and Thomas Stone
#
#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Library General Public
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

#include the definitions from the main Make.include
include ${DV}/Make.include
#This defines 3DLIBS INCLUDE_DIRS LIB_DIRS LIBS CXXFLAGS CXX and SHARED_FLAG
#as well as PYTHON_OBJECTS

# name of directory into which to put the shared library.  You can change this
# to be whatever you like.
BIN_DIR = ../lib/$(DV_ARCH)_$(DV_GRAPHIC_LIB)

#The objects for each problem that needs to be included into the shell
#*************************************
#ADD ANYTHING YOU WANT INCLUDED HERE!!
#*************************************
PROBLEM_OBJECTS = objects/$(DV_ARCH)/DVpython_problems.o objects/$(DV_ARCH)/default_problem.o objects/$(DV_ARCH)/auto_plotter3d.o

all: 
	-mkdir -p objects/$(DV_ARCH)
	$(MAKE) DV_python.so 
#Make for master python shell
#***********************************
#ADD YOUR PROBLEM DIRECTORIES HERE!!
#***********************************
DV_python.so: $(PYTHON_OBJECTS) $(PROBLEM_OBJECTS)  
	$(CXX) $(SHARED_FLAG) $(CXXFLAGS) $(LIB_DIRS) $(PYTHON_OBJECTS) $(PROBLEM_OBJECTS) $(LIBS) -o $(BIN_DIR)/DV_python.so

clean:
	rm -rf objects

objects/$(DV_ARCH)/%.o : %.cc
	$(CXX) $(CXXFLAGS) $(INCLUDE_DIRS) -c $< -o $@










