#ifndef _POLYGON_3D_
#define _POLYGON_3D_

#include <math.h>
#include <Inventor/SbLinear.h>

// this switches on normalization of normal vectors; does it help anything?
// #define _normalize_normals_

//////////////////////////////////////////////////////////////////////////////
//
// Class: Polygon3D
//
// This class describes regular plane polygons placed in 3D space,
// intended to be used to approximate circles
//
// To do: define Polygon3D and then RegularPolygon3D?  Then, a projection is a 
// polygon, etc.
//
//////////////////////////////////////////////////////////////////////////////

class Polygon3D {
  public:
    // Constructs a regular polygon given radius and number of vertices
    // The normal is in the z-direction, the center is the origin
    Polygon3D(float r=1.0, int nv=16);

    // Constructs a regular polygon given center, normal, radius and number of vertices
    Polygon3D(const SbVec3f &c, const SbVec3f &n, float r, int nv);

    // Destructor: frees memory used to store vertices
    ~Polygon3D();

    // Rotates the polygon wrt its center (normal becomes new_normal)
    void rotate(const SbVec3f &new_normal);

    // Parallel translates the polygon by vector t (center becomes center+t)
    void translate(const SbVec3f &t);

    // Parallel translates the polygon to a new center c
    void moveTo(const SbVec3f &c);

    // Scales the polygon by s (radius becomes radius*s)
    void scale(float s);

    // Projects vertices onto a plane along the normal of the polygon
    void project(const SbPlane &p, float pj[][3]);

    // Projects vertices onto a plane along the normal of the plane
    void project1(const SbPlane &p, float pj[][3]);

    // Rotates the vertices n steps (counterclockwise if n>0)
    void rotate(const int n);

    // Accessors
    const float &	getRadius() const  { return radius; }
    const SbVec3f &	getNormal() const  { return normal; }
    const SbVec3f &	getCenter() const  { return center; }

    // Accesses indexed vertices
    SbVec3f &	operator [](int i)  { return (points[i]); }
    const SbVec3f & operator [](int i) const  { return (points[i]); }

    // Prints coordinates of the verteces for testing purposes
    void print();

  protected:
    int n_points;
    float radius;
    SbVec3f normal, center;
    SbVec3f *points;

  private:
    // Common code for constructors
    void init(float r, int nv);
};

#endif /* _POLYGON_3D_ */
