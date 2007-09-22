#include "tube.h"
#include "polygon3d.h"


#define COPY3(to,from) \
  to[0] = from[0]; to[1] = from[1]; to[2] = from[2]; 


Tube::Tube(const int num_pts, float xyz_path[][3],
           const float r, const int num_edges)
{
  path = xyz_path;
  // This is for checking the data validity. 
  // if distance betwee two points are too colse, the next
  // point will be skipped.
  // Added by ZCH. Mar 30, 2004
  float distance = 0;
  float pt[3];
  pt[0]=path[0][0];
  pt[1]=path[0][1];
  pt[2]=path[0][2];
//  for(int i =1; i <5; i++)
    int i = 1;
    int j = i;
	int skip = 0;
  do
  {
  distance = sqrt((path[i][0]-pt[0])*(path[i][0]-pt[0]) +
            (path[i][1]-pt[1])*(path[i][1]-pt[1]) +
            (path[i][2]-pt[2])*(path[i][2]-pt[2]));
    if(distance < 1.e-7)
    {
//        printf("Skip i=%i\n", i);
		skip++;
    }
    else
    {
      path[j][0]=path[i][0];
      path[j][1]=path[i][1];
      path[j][2]=path[i][2];

      pt[0]=path[i][0];
      pt[1]=path[i][1];
      pt[2]=path[i][2];
      j++;
     }
     i++;
  } while(i < num_pts ); 

  path_pts = i-skip;//num_pts;
 // path_pts = num_pts;
  // ZCH's modification end here. 
  radius = r;
  n_edges = num_edges;

  n_segments = path_pts - 1;
  mesh_pts = (n_edges + 1) * path_pts;
  open_path = (SbVec3f(path[0]) == SbVec3f(path[n_segments])) == 0;

  // printf("Estimated number of mesh points: %d\n", mesh_pts);
}


SoSeparator* Tube::createTube()
{
   SoSeparator *result = new SoSeparator;
   result->ref();
// this line is added to deal with one or zero point cases.
// added by ZCH. Mar 30, 2004
   if(path_pts <=1) return result;

   // allocate memory for the array that stores vertices of the tube
   mesh = new float[mesh_pts][3];

   // compute the coordinates of the vertices
   createMesh();

   SoShapeHints *shapehints = new SoShapeHints;
   shapehints->vertexOrdering = SoShapeHints::COUNTERCLOCKWISE;
   shapehints->shapeType = SoShapeHints::SOLID;
   shapehints->faceType = SoShapeHints::CONVEX;
   shapehints->creaseAngle = 1.0;
   result->addChild(shapehints);

   // define coordinates for vertices
   SoCoordinate3 *myCoords = new SoCoordinate3;
   myCoords->point.setValues(0, mesh_pts, mesh);
   result->addChild(myCoords);

   // define the QuadMesh
   SoQuadMesh *myQuadMesh = new SoQuadMesh;
   myQuadMesh->verticesPerRow = n_edges + 1; // # of points around the tube
   myQuadMesh->verticesPerColumn = path_pts; // # of points along the tube
   result->addChild(myQuadMesh);

   // if path is not closed, plug a solid polygon at each end of the tube
   if ( open_path ) {

     // for the start of the path, use the first n_edges points of mesh[]
     // to define a polygon

     // for the end of the path, use the last n_edges points from mesh[]
     // with order reversed to get the correct orientation of the polygon

     for (int i=0; i<n_edges; i++) {
       COPY3(mesh[n_edges+i], mesh[mesh_pts-i-1]);
     }

     // set them as coordinate points
     SoCoordinate3 *myCoords1 = new SoCoordinate3;
     myCoords1->point.setValues(0, n_edges*2, mesh);
     result->addChild(myCoords1);

     // define the two polygons
     SoFaceSet *myFaceSet = new SoFaceSet;
     myFaceSet->startIndex.setValue(0);
     myFaceSet->numVertices.set1Value(0, n_edges);
     myFaceSet->numVertices.set1Value(1, n_edges);
     result->addChild(myFaceSet);
   }

   // dealocate memory: don't need the array with vertices any more
   delete[] mesh;

   result->unrefNoDelete();
   return result;
}


void Tube::createMesh()
{
  int i, j;
  int mp = 0; // counter for created mesh points
  SbPlane plane;
  float (*pj)[3] = new float[n_edges][3];
  SbVec3f v1, v2;

  // compute points of the first cross-section:

  // place the polygon at the first point of the path
  // and orient it perpendicular to the first segment
  Polygon3D poly = Polygon3D(SbVec3f(path[0]), 
    SbVec3f(path[0]) - SbVec3f(path[1]), radius, n_edges);

  if ( open_path ) {
    // if the path is open, simply store the polygon vertices as mesh points
    for (i=0; i<n_edges; i++) {
      poly[i].getValue(mesh[mp][0], mesh[mp][1], mesh[mp][2]);
      mp++;
    }
    poly[0].getValue(mesh[mp][0], mesh[mp][1], mesh[mp][2]);
    mp++;
  } 
  else {
    // if the path is closed:

    // define a plane through the first point that bisects the angle between 
    // the last and the first segments
    v1.setValue(path[n_segments-1][0]-path[0][0], 
                path[n_segments-1][1]-path[0][1],
                path[n_segments-1][2]-path[0][2]);
    v1.normalize();
    v2.setValue(path[0][0]-path[1][0], 
                path[0][1]-path[1][1],
                path[0][2]-path[1][2]);
    v2.normalize();
    plane = SbPlane(v2+v1, SbVec3f(path[0]));

    // project the vertices of the polygon onto the plane
    poly.project(plane, pj);

    // store the result as mesh points defining the first cross-section
    for (i=0; i<n_edges; i++) {
      COPY3(mesh[mp], pj[i]);
      mp++;
    }
    COPY3(mesh[n_edges], pj[0]);
    mp++;
  } 

  // compute points of the intermediate cross-sections
  for (j=0; j<n_segments-1; j++) 
  {
    // move the polygon to the j+1 point of the path ...
    poly.moveTo(SbVec3f(path[j+1]));
    
    // and orient it perpendiclar to (j+1,j+2) segment
    poly.rotate(SbVec3f(path[j+1]) - SbVec3f(path[j+2]));
    
    // define a plane through j+1 point that bisects the angle between 
    // (j,j+1) and (j+1,j+2) segments
    v1.setValue(path[j][0]-path[j+1][0], 
                path[j][1]-path[j+1][1],
                path[j][2]-path[j+1][2]);
    v1.normalize();
    v2.setValue(path[j+1][0]-path[j+2][0], 
                path[j+1][1]-path[j+2][1],
                path[j+1][2]-path[j+2][2]);
    v2.normalize();

//////////////////////////////////////////////////////////////////////////////
	// in case v1 and v2 are equal but in opposite direction?
	// added by zch	
	/*
	SbVec3f v1cv2 = v1.cross(v2);
	//if(v1cv2.length()==0) // in case two are exactly opposite this work just fine, but..
	double vdv = v1.dot(v2);
	double theta = acos(v1.dot(v2));
	printf("vdv = %f\n", vdv);
	if(fabs(vdv+1.0) < 1.e-5)
	{
		// only do this when vdv ~= -1
		printf("here \n");
    	plane = SbPlane(v1, SbVec3f(path[j+1]));
	}
	else
    	plane = SbPlane(v2+v1, SbVec3f(path[j+1]));
	*/
	// BUT the above method still does not work in some case.
	// Actually the simplest way to solve this is:
	// NOTE: v1 doesn't work either.
    plane = SbPlane(v2, SbVec3f(path[j+1]));
	
//////////////////////////////////////////////////////////////////////////////
  /*
    // Alternative code that only approximates bisection:
    v1.setValue(path[j]); v2.setValue(path[j+2]);
    plane = SbPlane(v1-v2, SbVec3f(path[j+1]));
  */

    // project (along the (j+1,j+2) segment) the vertices of the polygon 
    // onto the plane ...
    poly.project(plane, pj);

    // and store the result as mesh points
    for (i=0; i<n_edges; i++) 
    {
      COPY3(mesh[mp], pj[i]);
      mp++;
    }
    COPY3(mesh[mp], pj[0]);
    mp++;
  }

  // compute points of the last cross-section

  // move the polygon to the last point of the path ...
  poly.moveTo(SbVec3f(path[n_segments]));
  // and orient it perpendicular to the last segment
  poly.rotate(SbVec3f(path[n_segments-1]) - SbVec3f(path[n_segments]));

  if (  open_path ) {
    // if the path is open, simply store the polygon vertices as mesh points
    for (i=0; i<n_edges; i++) {
      poly[i].getValue(mesh[mp][0], mesh[mp][1], mesh[mp][2]);
      mp++;
    }
    poly[0].getValue(mesh[mp][0], mesh[mp][1], mesh[mp][2]);
    mp++;
  } 
  else {
    // if the path is closed:

    // define a plane through the last point that bisects the angle between 
    // the last and the first segments
    v1.setValue(path[n_segments-1][0]-path[0][0], 
                path[n_segments-1][1]-path[0][1],
                path[n_segments-1][2]-path[0][2]);
    v1.normalize();
    v2.setValue(path[0][0]-path[1][0], 
                path[0][1]-path[1][1],
                path[0][2]-path[1][2]);
    v2.normalize();
    plane = SbPlane(v2+v1, SbVec3f(path[0]));

    // project the vertices of the polygon onto the plane
    poly.project(plane, pj);

    // store the result as mesh points defining the last cross-section
    for (i=0; i<n_edges; i++) {
      COPY3(mesh[mp], pj[i]);
      mp++;
    }
    COPY3(mesh[mp], pj[0]);
    mp++;
/*
    for (i=0; i<n_edges; i++) {
      COPY3(mesh[mp], mesh[i]);
      mp++;
    }
    COPY3(mesh[mp], mesh[0]);
    mp++;
*/  
  } 
  
//  printf("Mesh points generated: %d\n", mp);
  mesh_pts = mp;
  delete[] pj;
}


#undef COPY3

