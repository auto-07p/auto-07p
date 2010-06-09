// This file is unused!!!

#include "drawCoords.h"

#include <Inventor/nodes/SoCylinder.h>
#include <Inventor/nodes/SoNode.h>
#include <Inventor/nodes/SoTransform.h>
#include <Inventor/nodes/SoRotationXYZ.h>
#include <Inventor/nodes/SoMaterial.h>
#include <Inventor/nodes/SoCone.h>
#include <Inventor/SbLinear.h>
#include <Inventor/fields/SoField.h>

#include "gplaut04.h"

#define FONT_SIZE 24

//////////////////////////////////////////////////////////////////////
//
SoSeparator *
drawCoords(int where, float pos[], SbVec3f axisColors[],  float height)
//
//////////////////////////////////////////////////////////////////////
{
    float xGo=0, yGo=0, zGo = 0;

    SoSeparator *myAxis= new SoSeparator;
    SoSeparator *xGrp = new SoSeparator;
    SoSeparator *yGrp = new SoSeparator;
    SoSeparator *zGrp = new SoSeparator;

    SoSeparator *redAxis = createAxis(axisColors[0][0], axisColors[0][1], axisColors[0][2], height, "X");
    SoSeparator *blueAxis = createAxis(axisColors[1][0], axisColors[1][1],axisColors[1][2], height, "Y");
    SoSeparator *greenAxis = createAxis(axisColors[2][0], axisColors[2][1],axisColors[2][2], height, "Z");

    SoTransform  *xXform = new SoTransform;
    xXform->scaleFactor.setValue(1, 1, 1);
    xXform->rotation.setValue(SbVec3f(0.0, 0.0, 1.0),-M_PI_2);
    if(where == 2)
        xXform->translation.setValue(1, 0, 0);
    else
        xXform->translation.setValue(xGo, 0, 0);

    SoTransform  *zXform = new SoTransform;
    zXform->scaleFactor.setValue(1.0, 1.0, 1.0);
    if(where == 2)
        zXform->translation.setValue(0, 0, 1);
    else
        zXform->translation.setValue(0, 0, zGo);
    zXform->rotation.setValue(SbVec3f(1.0, 0.0, 0.0),M_PI_2);

    SoTransform  *yXform = new SoTransform;
    yXform->scaleFactor.setValue(1.0, 1.0, 1.0);
    if(where == 2)
        yXform->translation.setValue(0, 1, 0);
    else
        yXform->translation.setValue(0, yGo, 0);

// to draw the ticker of the X upward, we still need to
// rotate X for 180 degree.
    SoRotationXYZ *rotX = new SoRotationXYZ;
    rotX->axis = SoRotationXYZ::X;
    rotX->angle = M_PI;

    xGrp->addChild(rotX);
    xGrp->addChild(xXform);
    xGrp->addChild(redAxis);

    yGrp->addChild(yXform);
    yGrp->addChild(blueAxis);

    zGrp->addChild(zXform);
    zGrp->addChild(greenAxis);

    SoSeparator *axs = new SoSeparator;
    axs->addChild(xGrp);
    axs->addChild(yGrp);
    axs->addChild(zGrp);

    SoTransform *axsXform = new SoTransform;
    axsXform->translation.setValue(pos);
    myAxis->addChild(axsXform);
    myAxis->addChild(axs);

    return myAxis;
}

SoSeparator * createAxis(float red, float green, float blue, float height, const char * label)
{
    SoSeparator *axisSep = new SoSeparator;
    SoMaterial  *axisMtl = new SoMaterial;

//define materil
    axisMtl->ambientColor.setValue(1.0,1.0,1.0);
    axisMtl->diffuseColor.setValue(red, green, blue);
    axisMtl->specularColor.setValue(1.0,1.0,1.0);
    axisMtl->shininess = 0.9;
    axisMtl->transparency = 0.0;

//define a cylindar for the axis
    SoCylinder *cyl = new SoCylinder;
    cyl->radius = height/350.0;
    cyl->height = 1.1 * height;

//define arrow
    SoSeparator *arrSep = new SoSeparator;
    SoCone *arrow = new SoCone;
    arrow->bottomRadius = height/150.0;
    arrow->height = 5.0*height/100.0;
    SoTransform *arrXform = new SoTransform;
    float pst = (cyl->height.getValue())/2.0;
    arrXform->translation.setValue(0.0, pst, 0.0);
    arrSep->addChild(arrXform);
    arrSep->addChild(arrow);

// define font for the axis
    SoFont *axFont = new SoFont;
    axFont->name.setValue("Helvetica");
    axFont->size.setValue(FONT_SIZE);
    SoText2 *axName = new SoText2;
    axName->string.setValue(label);
    arrSep->addChild(axFont);
    arrSep->addChild(axName);

//add child to axisSep
    axisSep->addChild(axisMtl);
    axisSep->addChild(cyl);
    axisSep->addChild(arrSep);
    for(int i=0; i<=10; i++)
        axisSep->addChild(drawTicker((float)(i/10.0-0.5)*height, height/100.0));

    return axisSep;
}


SoSeparator * drawTicker(float pos, float height)
{
// define ticker
    SoSeparator *tkSep = new SoSeparator;
    SoCylinder *ticker = new SoCylinder;
    ticker->radius = height/10.0;
    ticker->height = height;

//define transform for the ticker
    SoTransform *tkXform = new SoTransform;
    tkXform->translation.setValue(0.00, pos, 0.00);
    tkXform->rotation.setValue(SbVec3f(0.0,0.0,1.0),M_PI_2);
    tkSep->addChild(tkXform);
    tkSep->addChild(ticker);
    return  tkSep;
}
