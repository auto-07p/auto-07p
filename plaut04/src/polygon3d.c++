#include <Inventor/SbLinear.h>
#include "polygon3d.h"

void Polygon3D::init(float r, int nv)
{
    n_points = nv;
    radius = r;
    normal.setValue(0.0, 0.0, 1.0);
    center.setValue(0.0, 0.0, 0.0);

    points = new SbVec3f[n_points];
    float dfi = (M_PI * 2) / n_points;
    for (int i=0; i<n_points; i++)
        points[i].setValue(radius*cos(dfi*i), radius*sin(dfi*i), 0.0);
}


Polygon3D::Polygon3D(float r, int nv)
{
    init(r, nv);
}


Polygon3D::Polygon3D(const SbVec3f &c, const SbVec3f &n, float r, int nv)
{
    init(r, nv);
    rotate(n);
    translate(c);
}


Polygon3D::~Polygon3D()
{
    if (points != NULL)
        delete [] points;
}


void Polygon3D::rotate(const SbVec3f &new_normal)
{
    SbRotation rot = SbRotation(normal, new_normal);
    for (int i=0; i<n_points; i++)
    {
        rot.multVec(points[i]-center, points[i]);
        points[i] += center;
    }
    normal = new_normal;

#ifdef _normalize_normals_
    normal.normalize();
#endif
}


void Polygon3D::translate(const SbVec3f &t)
{
    for (int i=0; i<n_points; i++)
        points[i] += t;
    center += t;
}


void Polygon3D::moveTo(const SbVec3f &c)
{
    for (int i=0; i<n_points; i++)
        points[i] += c - center;
    center = c;
}


void Polygon3D::scale(float s)
{
    for (int i=0; i<n_points; i++)
        points[i] = (points[i]-center)*s + center;
}


void Polygon3D::project(const SbPlane &p, float pj[][3])
{
    SbLine line;
    SbVec3f pt; 

    for (int i=0; i<n_points; i++)
    {
        line.setValue(points[i], points[i] + normal);
        p.intersect(line, pt);
        pt.getValue(pj[i][0], pj[i][1], pj[i][2]);
    }
}


void Polygon3D::project1(const SbPlane &p, float pj[][3])
{
    SbLine line;  
    SbVec3f pt;  

    for (int i=0; i<n_points; i++)
    {
        line.setValue(points[i], points[i] + p.getNormal());
        p.intersect(line, pt);
        pt.getValue(pj[i][0], pj[i][1], pj[i][2]);
    }
}


void Polygon3D::rotate(const int n)
{
    if (n == 0) return;
}


void Polygon3D::print()
{
    for (int i=0; i<n_points; i++)
        printf("%12f  %12f  %12f\n", points[i][0], points[i][1], points[i][2]);
}
