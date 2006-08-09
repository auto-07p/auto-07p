#include <math.h>
#include <Inventor/So.h>
#include <Inventor/nodes/SoTexture2.h>

////////////////////////////////////////////////////////////////////////
//
//  the mass here for the smaller primary must less than or equal to 0.0;
//
SoSeparator *
createPrimary(double mass, double pos, float scale, char *txtureFileName)
//
////////////////////////////////////////////////////////////////////////
{
    float radius = pow(fabs(mass)/((4.0/3.0)*M_PI),1.0/3.0);
    radius = radius*scale/5.0;
    radius = (radius < 0.01) ? 0.01 : radius;

// create a root
    SoSeparator *primSep = new SoSeparator;

// mapping a texture
    SoTexture2 *primTxture = new SoTexture2;
    primTxture->filename.setValue(txtureFileName);
    primSep->addChild(primTxture);

// define the prim transform
    SoTransform *primXform = new SoTransform;
    primXform->translation.setValue(pos, 0.0, 0.0);
    primXform->rotation.setValue(SbVec3f(1, 0.0, 0.0),M_PI_2);
    primSep->addChild(primXform);

//define the prim
    SoSphere *prim = new SoSphere;
    prim->radius = radius;
    primSep->addChild(prim);

    return primSep;
}
