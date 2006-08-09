/*
    Visualization for Bifurcation Manifolds
    Copyright (C) 1997 Randy Paffenroth and John Maddocks

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU  General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
    MA 02111-1307, USA
*/
#include <iostream.h> 
#include <fstream.h> 
#include <strstream.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "DV.h"
#include "DVselection_container.h"
#include "DVtips_and_hints.h"
#include <Python.h>
#include <graminit.h>
static void make_grid(PyObject *,DVcontainer &);
static void make_cube(PyObject *,DVcontainer &);

int auto_plotter3d(DVcreate_image_arguments &dv)
{ 
  if(!Py_IsInitialized()) {
    cout << "Initializing Python" << endl;
    Py_Initialize();
  }
  // Not exactly sure how good this is to do, but I am going to
  // give it a try.  Basically I am getting the long representation
  // of the point to the ParseVBm object.  I turn it back into a
  // pointer here.
  long dict_pointer;
  sscanf(dv.input_string,"%ld",&dict_pointer);
  // Do not DECREF this!
  PyObject *dict = (PyObject *)dict_pointer;

  // Don't need to DECREF
  PyObject *full_x_array = PyDict_GetItemString(dict,"X");
  PyObject *full_y_array = PyDict_GetItemString(dict,"Y");
  PyObject *full_z_array = PyDict_GetItemString(dict,"Z");
  PyObject *full_color_array = PyDict_GetItemString(dict,"Color");

  for(int j=0;j<PyList_Size(full_x_array);j++) {
    // Don't need to DECREF
    PyObject *x_array = PyList_GetItem(full_x_array,j);
    PyObject *y_array = PyList_GetItem(full_y_array,j);
    PyObject *z_array = PyList_GetItem(full_z_array,j);
    PyObject *color_array = PyList_GetItem(full_color_array,j);
    
    LCVMarray_2D<float> data(PyList_Size(x_array),4);
    for(int i=0;i<PyList_Size(x_array);i++) {
      data[i][0] = (float)PyFloat_AsDouble(PyList_GetItem(x_array,i));
      data[i][1] = (float)PyFloat_AsDouble(PyList_GetItem(y_array,i));
      data[i][2] = (float)PyFloat_AsDouble(PyList_GetItem(z_array,i));
      data[i][3] = (float)PyFloat_AsDouble(PyList_GetItem(color_array,i));
    }

    // FIXME:  This is temporary
    PyObject *markers_array = PyDict_GetItemString(dict,"markers");
    for(int i=0;i<PyList_Size(markers_array);i++) {
      PyObject *marker_dict = PyList_GetItem(markers_array,i);
      if(PyInt_AsLong(PyDict_GetItemString(marker_dict,"branch"))==j){
	// Don't need to DECREF
	int index = PyInt_AsLong(PyDict_GetItemString(marker_dict,"point"));
	DVspheres markers(data[index][0],data[index][1],
			  data[index][2],
			  PyFloat_AsDouble(PyDict_GetItemString(marker_dict,"radius")));
	float R = PyFloat_AsDouble(PyDict_GetItemString(marker_dict,"R"));
	float G = PyFloat_AsDouble(PyDict_GetItemString(marker_dict,"G"));
	float B = PyFloat_AsDouble(PyDict_GetItemString(marker_dict,"B"));
	markers.color[0] = DVcolor(R,G,B);
	markers.geometry_complexity = 8;
	dv.top.copy_child(markers);
      }
    }
    DVlines config(data);
    {
      float width;
      int cylinder,geom_comp,light_comp;
      int write_points_on;
      
      
      // Don't need to DECREF
      PyObject *python_option = PyDict_GetItemString(dict,"options");
      
      istrstream option_string(PyString_AsString(python_option),
			       strlen(PyString_AsString(python_option)));  
      option_string >> width;
      option_string >> cylinder;
      option_string >> geom_comp;
      option_string >> light_comp;
      option_string >> write_points_on;
      config.width[0] = width;
      if(cylinder) 
	config.set_line_status(DV_CYLINDER);
      else
	config.set_line_status(DV_LINE);
      config.geometry_complexity = geom_comp;
      config.light_complexity = light_comp;
      //if (write_points_on)
      //cout << config;
    }
    config.set_x_plot_coordinate(0);
    config.set_y_plot_coordinate(1);
    config.set_z_plot_coordinate(2);
    config.set_color_coordinate(3);
    dv.top.copy_child(config);
  }

  if(PyMapping_HasKeyString(dict,"grid")) {
    // Don't need to DECREF
    PyObject *python_grid = PyDict_GetItemString(dict,"grid");
    make_grid(python_grid,dv.top);
  }
  if(PyMapping_HasKeyString(dict,"cube")) {
    // Don't need to DECREF
    PyObject *python_grid = PyDict_GetItemString(dict,"cube");
    make_cube(python_grid,dv.top);
  }
  
  //Py_Finalize();
  return 1;
}
  
void make_cube(PyObject *python_cube,DVcontainer &top) {
  long cube_on;
  double cube_R,cube_G,cube_B;
  double cube[2][3];

  cube_on        = PyInt_AsLong(PyDict_GetItemString(python_cube,"cube_on"));
  cube_R         = PyFloat_AsDouble(PyDict_GetItemString(python_cube,"cube_R"));
  cube_G         = PyFloat_AsDouble(PyDict_GetItemString(python_cube,"cube_G"));
  cube_B         = PyFloat_AsDouble(PyDict_GetItemString(python_cube,"cube_B"));
  cube[0][0]     = PyFloat_AsDouble(PyDict_GetItemString(python_cube,"cube_x_max"));
  cube[0][1]     = PyFloat_AsDouble(PyDict_GetItemString(python_cube,"cube_y_max"));
  cube[0][2]     = PyFloat_AsDouble(PyDict_GetItemString(python_cube,"cube_z_max"));
  cube[1][0]     = PyFloat_AsDouble(PyDict_GetItemString(python_cube,"cube_x_min"));
  cube[1][1]     = PyFloat_AsDouble(PyDict_GetItemString(python_cube,"cube_y_min"));
  cube[1][2]     = PyFloat_AsDouble(PyDict_GetItemString(python_cube,"cube_z_min"));

  if(cube_on) {
    LCVMarray_3D<float> grid_data(12,2,3);
    int points[12][2][3];
    points[0][0][0] = 0;    points[0][0][1] = 0;    points[0][0][2] = 0;
    points[0][1][0] = 1;    points[0][1][1] = 0;    points[0][1][2] = 0;

    points[1][0][0] = 0;    points[1][0][1] = 0;    points[1][0][2] = 0;
    points[1][1][0] = 0;    points[1][1][1] = 1;    points[1][1][2] = 0;

    points[2][0][0] = 0;    points[2][0][1] = 0;    points[2][0][2] = 0;
    points[2][1][0] = 0;    points[2][1][1] = 0;    points[2][1][2] = 1;

    points[3][0][0] = 1;    points[3][0][1] = 1;    points[3][0][2] = 1;
    points[3][1][0] = 0;    points[3][1][1] = 1;    points[3][1][2] = 1;

    points[4][0][0] = 1;    points[4][0][1] = 1;    points[4][0][2] = 1;
    points[4][1][0] = 1;    points[4][1][1] = 0;    points[4][1][2] = 1;

    points[5][0][0] = 1;    points[5][0][1] = 1;    points[5][0][2] = 1;
    points[5][1][0] = 1;    points[5][1][1] = 1;    points[5][1][2] = 0;

    points[6][0][0] = 1;    points[6][0][1] = 0;    points[6][0][2] = 1;
    points[6][1][0] = 1;    points[6][1][1] = 0;    points[6][1][2] = 0;

    points[7][0][0] = 1;    points[7][0][1] = 1;    points[7][0][2] = 0;
    points[7][1][0] = 1;    points[7][1][1] = 0;    points[7][1][2] = 0;

    points[8][0][0] = 1;    points[8][0][1] = 1;    points[8][0][2] = 0;
    points[8][1][0] = 0;    points[8][1][1] = 1;    points[8][1][2] = 0;

    points[9][0][0] = 1;    points[9][0][1] = 0;    points[9][0][2] = 1;
    points[9][1][0] = 0;    points[9][1][1] = 0;    points[9][1][2] = 1;

    points[10][0][0] = 0;    points[10][0][1] = 1;    points[10][0][2] = 1;
    points[10][1][0] = 0;    points[10][1][1] = 1;    points[10][1][2] = 0;

    points[11][0][0] = 0;    points[11][0][1] = 1;    points[11][0][2] = 1;
    points[11][1][0] = 0;    points[11][1][1] = 0;    points[11][1][2] = 1;


    for(int i=0;i<12;i++) {
      for(int j=0;j<2;j++)
	for(int k=0;k<3;k++) {
	  grid_data[i][j][k] = cube[points[i][j][k]][k];
	}
    }

    DVlines cube(grid_data);
    /*DVboxes cube(cube_x_min,cube_y_min,cube_z_min,cube_x_max,cube_y_max,cube_z_max);*/
    cube.geometry_complexity = 3;
    cube.color[0] = DVcolor(cube_R,cube_G,cube_B);
    top.copy_child(cube);
  }
}

void make_grid(PyObject *python_grid,DVcontainer &top) {
  long grid_on = 1;
  long grid_lines = 10;
  double grid1_start = -1.0;
  double grid1_end = 1.0;
  double grid2_start = -1.0;
  double grid2_end = 1.0;
  double grid_width = 1;
  double grid_R = 1.0;
  double grid_G = 1.0;
  double grid_B = 1.0;
  long grid_cylinder = 0;
  PyObject *grid_direction;
  int i,j;

  grid_on        = PyInt_AsLong(PyDict_GetItemString(python_grid,"grid_on"));
  grid_lines     = PyInt_AsLong(PyDict_GetItemString(python_grid,"grid_lines"));
  grid1_start    = PyFloat_AsDouble(PyDict_GetItemString(python_grid,"grid_1_start"));
  grid2_start    = PyFloat_AsDouble(PyDict_GetItemString(python_grid,"grid_2_start"));
  grid1_end      = PyFloat_AsDouble(PyDict_GetItemString(python_grid,"grid_1_end"));
  grid2_end      = PyFloat_AsDouble(PyDict_GetItemString(python_grid,"grid_2_end"));
  grid_width     = PyFloat_AsDouble(PyDict_GetItemString(python_grid,"grid_width"));
  grid_R         = PyFloat_AsDouble(PyDict_GetItemString(python_grid,"grid_R"));
  grid_G         = PyFloat_AsDouble(PyDict_GetItemString(python_grid,"grid_G"));
  grid_B         = PyFloat_AsDouble(PyDict_GetItemString(python_grid,"grid_B"));
  grid_cylinder  = PyInt_AsLong(PyDict_GetItemString(python_grid,"grid_cylinder"));
  grid_direction = PyDict_GetItemString(python_grid,"grid_direction");

  if(grid_on) {
    for(j=0;j<PyList_Size(grid_direction);j++) {
      int X=0,Y=0,Z=0;
      char *direction;
      direction = PyString_AsString(PyList_GetItem(grid_direction,j));

      if(strcmp(direction,"x") == 0) {
	X = 0;
	Y = 1;
	Z = 2;	
      }
      if(strcmp(direction,"y") == 0) {
	X = 2;
	Y = 0;
	Z = 1;
      }
      if(strcmp(direction,"z") == 0) {
	X = 1;
	Y = 2;
	Z = 0;
      }
      
      LCVMarray_3D<float> grid_data(2*grid_lines+2,2,3);
      for(i=0;i<=grid_lines;i++) {
	float tmp_y = grid1_start + 
	  ((float)i/(float)grid_lines)*(grid1_end-grid1_start);
	grid_data[i][0][X] = 0.0;
	grid_data[i][0][Y] = tmp_y;
	grid_data[i][0][Z] = grid2_start;
	grid_data[i][1][X] = 0.0;
	grid_data[i][1][Y] = tmp_y;
	grid_data[i][1][Z] = grid2_end;
      }
      for(i=0;i<=grid_lines;i++) {
	float tmp_z = grid2_start + 
	  ((float)i/(float)grid_lines)*(grid2_end-grid2_start);
	grid_data[i+grid_lines+1][0][X] = 0.0;
	grid_data[i+grid_lines+1][0][Y] = grid1_start;
	grid_data[i+grid_lines+1][0][Z] = tmp_z;
	grid_data[i+grid_lines+1][1][X] = 0.0;
	grid_data[i+grid_lines+1][1][Y] = grid1_end;
	grid_data[i+grid_lines+1][1][Z] = tmp_z;
      }
      DVlines grid_lines(grid_data);
      grid_lines.width[0] = grid_width;
      grid_lines.color[0] = DVcolor(grid_R,grid_G,grid_B);
      if(grid_cylinder) 
	grid_lines.set_line_status(DV_CYLINDER);
      else
	grid_lines.set_line_status(DV_LINE);
      top.copy_child(grid_lines);
    }
  }
}
  
