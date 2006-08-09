#ifndef _TUBE_
#define _TUBE_

#include <Inventor/nodes/SoSeparator.h>
#include <Inventor/nodes/SoShapeHints.h>
#include <Inventor/nodes/SoCoordinate3.h>
#include <Inventor/nodes/SoQuadMesh.h>
#include <Inventor/nodes/SoFaceSet.h>


// A class that is a placeholder of the routines for creating a tube.
// The proper way would be to define a subclass of SoShape?


class Tube {
  public:
    Tube() {}

    // constructor: sets up variables, doesn't create anything yet
    Tube(const int num_pts, float xyz_path[][3],
         const float radius=1.0, const int num_edges=20);

    // creates a sceen subtree desribing a tube, with some material
    SoSeparator *createTube();

  private:
    // computes the points of the tube, stores them into mesh[][3]
    void createMesh();

    float (*path)[3];  // pointer to the array supplied by the user
    float (*mesh)[3];  // this one is allocated/deallocated internally

    float radius;
    int n_edges;  // # of edges of the tube (vertices of its cross-section)
    int path_pts; // # of points of the path
    int n_segments;  // number of segments the path consists of
    int mesh_pts;    // number of points in quad mesh

    bool open_path;  // true if the given path is not closed (the path is
                     // considered closed if the first and the last points
                     // in the supplied array are exactly the same)
};

#endif //_TUBE_


// To do:
// - add an array for scaling / coloring?
// - define bounding box, compute defualt tube radius as a fraction of 
//   the boundin box size


