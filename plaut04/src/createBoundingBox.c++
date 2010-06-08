#include "createBoundingBox.h"

#include <Inventor/So.h>
///////////////////////////////////////////////////////////////////////////////
//
SoSeparator *
drawLine(SbVec3f pointS, SbVec3f pointE, SbVec3f color, float thickness, bool ticker, bool text, int dir)
//
///////////////////////////////////////////////////////////////////////////////
{

    SoSeparator * aSep = new SoSeparator;

    SoCylinder * aCylinder = new SoCylinder;
    aCylinder->radius = thickness;
    aCylinder->height = (pointS-pointE).length();

    SoMaterial *aMtl = new SoMaterial;
    aMtl->ambientColor.setValue(0.8,0.8,0.8);
    aMtl->diffuseColor.setValue(0.8,0.8,0.8);
    aMtl->specularColor.setValue(0.5,0.5,0.5);
    aMtl->shininess = 0.9;
    aMtl->transparency = 0.0;

    SoTransform * xTrans = new SoTransform;
    if(dir==1)
        xTrans->rotation.setValue(SbVec3f(0, 0, 1), M_PI_2);
    else if(dir == 2)
        xTrans->rotation.setValue(SbVec3f(0, 1, 0), M_PI_2);
    else if(dir == 3)
        xTrans->rotation.setValue(SbVec3f(1, 0, 0), M_PI_2);

    xTrans->translation.setValue((pointE+pointS)/2);

    aSep->addChild(xTrans);
    aSep->addChild(aMtl);
    aSep->addChild(aCylinder);

    return aSep;
}


///////////////////////////////////////////////////////////////////////////////
//
SoSeparator * 
createBoundingBox()
//
///////////////////////////////////////////////////////////////////////////////
{
    SbVec3f pointS, pointE, color;
    float thickness = 0.003;
    float xMin, yMin, zMin, xMax, yMax, zMax;
    xMin = yMin = zMin = -1;
    xMax = yMax = zMax =  1;

    SoSeparator *bdBoxSep = new SoSeparator;
    bdBoxSep->setName("bdBox");
    color.setValue(1,1,1);

    pointS.setValue(xMin,yMin,zMin);
    pointE.setValue(xMax, yMin, zMin);
    bdBoxSep->addChild(drawLine(pointS, pointE, color, thickness, false, false, 1));

    pointS.setValue(xMin, yMax, zMin);
    pointE.setValue(xMax, yMax, zMin);
    bdBoxSep->addChild(drawLine(pointS, pointE, color, thickness, false, false, 1));

    pointS.setValue(xMin, yMax, zMax);
    pointE.setValue(xMax, yMax, zMax);
    bdBoxSep->addChild(drawLine(pointS, pointE, color, thickness, false, false, 1));

    pointS.setValue(xMin, yMin, zMax);
    pointE.setValue(xMax, yMin, zMax);
    bdBoxSep->addChild(drawLine(pointS, pointE, color, thickness, false, false, 1));

    pointS.setValue(xMin, yMin, zMin);
    pointE.setValue(xMin, yMin, zMax);
    bdBoxSep->addChild(drawLine(pointS, pointE, color, thickness, false, false, 3));

    pointS.setValue(xMax, yMin, zMin);
    pointE.setValue(xMax, yMin, zMax);
    bdBoxSep->addChild(drawLine(pointS, pointE, color, thickness, false, false, 3));

    pointS.setValue(xMax, yMax, zMin);
    pointE.setValue(xMax, yMax, zMax);
    bdBoxSep->addChild(drawLine(pointS, pointE, color, thickness, false, false, 3));

    pointS.setValue(xMin, yMax, zMin);
    pointE.setValue(xMin, yMax, zMax);
    bdBoxSep->addChild(drawLine(pointS, pointE, color, thickness, false, false, 3));

    pointS.setValue(xMin, yMin, zMin);
    pointE.setValue(xMin, yMax, zMin);
    bdBoxSep->addChild(drawLine(pointS, pointE, color, thickness, false, false, 2));

    pointS.setValue(xMax, yMin, zMin);
    pointE.setValue(xMax, yMax, zMin);
    bdBoxSep->addChild(drawLine(pointS, pointE, color, thickness, false, false, 2));

    pointS.setValue(xMax, yMin, zMax);
    pointE.setValue(xMax, yMax, zMax);
    bdBoxSep->addChild(drawLine(pointS, pointE, color, thickness, false, false, 2));

    pointS.setValue(xMin, yMin, zMax);
    pointE.setValue(xMin, yMax, zMax);
    bdBoxSep->addChild(drawLine(pointS, pointE, color, thickness, false, false, 2));
    return bdBoxSep;
}
