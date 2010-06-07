#include <algorithm>
#include <math.h>
#include <Inventor/So.h>
#include <Inventor/nodes/SoTexture2.h>
#include "r3bplaut04.h"

#define CR3BP 0
#define INERTIAL_B  1
#define INERTIAL_S  2
#define INERTIAL_E  3

static void
lagpoint (double mu, double LP [7][2], double eps)
{
/**************************************************************************************
 * Eusebius Doedel & Volodymyr Romanov                             July 3, 2003        *
 ***************************************************************************************
 * Calculation of Lagrange points L1, L2, L3, L4, L5 for two-body  system with the     *
 * mass ratio mu=m2/(m1+m2).                                                           *
 *                                                                                     *
 * Input parameters:                                                                   *
 *                   mu - mass ratio;                                                  *
 *                   eps - precision.                                                  *
 *                                                                                     *
 * Output parameters:                                                                  *
 *                   LP [5],[2] - array, containing coordinates of                     *
 *                                Lagrange points as:                                  *
 *                   LP[0][0] - x coordinate of the bigger planet (-mu)                *
 *                   LP[0][1] - y coordinate of the bigger planet (0)                  *
 *                   LP[1][0] - x coordinate of the smaller planet (1-mu)              *
 *                   LP[1][1] - y coordinate of the smaller planet (0)                 *
 *                   LP[2][0] - x coordinate of L1                                     *
 *                   LP[2][1] - y coordinate of L1  (0)                                *
 *                   LP[3][0] - x coordinate of L2                                     *
 *                   LP[3][1] - y coordinate of L3  (0)                                *
 *                                         ..........                                  *
 *                   LP[6][0] - x coordinate of L5                                     *
 *                   LP[6][1] - y coordinate of L5  (!=0)                              *
 ***************************************************************************************
 */
    double x0, x1, y0;
    double r1, r2;
    double f, d;
    double eps1;

    if (mu < 0)
    {
        printf ("Function lagpoint error: Your mass ratio mu could not be negative\n");
        return;
    }

    if (mu > 0.5)
    {
        printf ("Function lagpoint error: Your mass ratio mu must be less or equal than 0.5\n");
        return;
    }

    eps1 = mu;
    if ((eps > eps1) && (mu > 0))
    {
        printf ("Function lagpoint message: Your accuracy is insufficient\n");
        printf ("Automatic correction of the accuracy was made: eps =%E\n", eps1);
        eps = eps1;
    }
/*Calculation of coordinates of the planet --------------------------------*/
    x0 = -mu;
    y0 = 0;
    LP[0][0] = x0;
    LP[0][1] = y0;
    x0 = 1 - mu;
    y0 = 0;
    LP[1][0] = x0;
    LP[1][1] = y0;

/*L4 and L5 calculation ---------------------------------------------------*/
    x0 = 0.5 - mu;
    y0 = sqrt(3.0)/2.0;
    LP[5][0] = x0;
    LP[5][1] = y0;
    LP[6][0] = x0;
    LP[6][1] = - y0;

/*L3 calculation ----------------------------------------------------------*/
    x0 = - 1-mu;
    y0 = 0;
    do
    {
        x1 = x0;
        r1 = (x0 + mu);
        r2 = (x0 - 1 + mu);
        f = (x0*r1*r1 + (1 - mu))*r2*r2 + mu*r1*r1;
        d = (r1 + 2*x0)*r1*r2*r2 + 2*x0*r1*r1*r2 + 2*(1 - mu)*r2 + 2*mu*r1;
        x0 = x0 - f/d;
    }
    while (fabs(x0-x1) > eps);
    LP[4][0] = x0;
    LP[4][1] = y0;

/*L2 calculation ----------------------------------------------------------*/
    if (mu >= eps)
    {
        x0 = 1 + mu;
        y0 = 0;
        do
        {
            x1 = x0;
            r1 = (x0 + mu);
            r2 = (x0 - 1 + mu);
            f = (x0*r1*r1 - (1 - mu))*r2*r2 - mu*r1*r1;
            d = (r1 + 2*x0)*r1*r2*r2 + 2*x0*r1*r1*r2 - 2*(1 - mu)*r2 - 2*mu*r1;
            x0 = x0 - f/d;
        }
        while (fabs(x0-x1) > eps);
    }
    else
    {
        x0 = 1;
        y0 = 0;
    }
    LP[3][0] = x0;
    LP[3][1] = y0;

/*L1 calculation -----------------------------------------------------------*/
    if (mu >= eps)
    {
        x0 = 1 - mu;
        y0 = 0;
        do
        {
            x1 = x0;
            r1 = (x0 + mu);
            r2 = (x0 - 1 + mu);
            f = (x0*r1*r1 - (1 - mu))*r2*r2 + mu*r1*r1;
            d = (r1 + 2*x0)*r1*r2*r2 + 2*x0*r1*r1*r2 - 2*(1 - mu)*r2 + 2*mu*r1;
            x0 = x0 - f/d;
        }
        while (fabs(x0-x1) > eps);
    }
    else
    {
        x0 = 1;
        y0 = 0;
    }
    LP[2][0] = x0;
    LP[2][1] = y0;
    return;
}


///////////////////////////////////////////////////////////////////////////
//
//  By Chenghai Zhang
//
SoSeparator *
createLibrationPoint(float mu, float dis, float scale, int whichCoordSystem)
//
///////////////////////////////////////////////////////////////////////////
{

// create a root
    SoSeparator *libSep = new SoSeparator;

// create the lib object
    SoGroup * libGrp = new SoGroup;

    static bool blOpenStar = FALSE;
    static bool blUseStar  = FALSE;
    static bool blLibBlink = FALSE;

// Read object data from a file
    if(blUseStar)
    {
        SoInput mySceneInput;
        if (mySceneInput.openFile("widgets/star.iv"))
        {
            SoSeparator *starObject = SoDB::readAll(&mySceneInput);
            if (starObject == NULL)
            {
                blOpenStar=FALSE;
            }
            else
            {
                mySceneInput.closeFile();
                libGrp->addChild(starObject);
            }
        }
        else
        {
            blOpenStar = FALSE;
        }
    }

    if(!blUseStar || !blOpenStar )
    {
        SoCube *lib = new SoCube;
        lib->height= 1.0;
        lib->width = 1.0;
        lib->depth = 1.0;
        libGrp->addChild(lib);
    }

//define silver materil for the lib
    SoMaterial *libMtl = new SoMaterial;
    libMtl->ambientColor.setValue(1.0,1.0,1.0);
    libMtl->diffuseColor.setValue(1.0,1.0,1.0);
    libMtl->specularColor.setValue(0.5,0.5,0.5);
    libMtl->shininess = 0.9;
    libMtl->transparency = 0.0;
    libSep->addChild(libMtl);

    double lp[7][2] = {}, eps;
    eps = 1.e-5;
    lagpoint (mu, lp, eps);

    float libPtMax[3], libPtMin[3];
    libPtMax[0]=libPtMax[1]=libPtMax[2]=libPtMin[0]=libPtMin[1]=libPtMin[2]=0;

// decide the size of the libpoints
    float x[5],y[5],z[5];
    for(int libPtId=1; libPtId<6; libPtId++)
    {
        x[libPtId-1] = lp[libPtId+1][0];
        y[libPtId-1] = lp[libPtId+1][1];
        z[libPtId-1] = 0;

// define the lib transform
        float xOffset = 0;
        if(whichCoordSystem == INERTIAL_B)
            xOffset = 0;
        else if(whichCoordSystem == INERTIAL_S)
            xOffset = mu;
        else if(whichCoordSystem == INERTIAL_E)
            xOffset = mu-1;

        x[libPtId-1] += xOffset;

        libPtMax[0] = (libPtMax[0]>x[libPtId-1]) ? libPtMax[0]  : x[libPtId-1];
        libPtMax[1] = (libPtMax[1]>y[libPtId-1]) ? libPtMax[1]  : y[libPtId-1];
        libPtMax[2] = (libPtMax[2]>z[libPtId-1]) ? libPtMax[2]  : z[libPtId-1];
        libPtMin[0] = (libPtMin[0]<x[libPtId-1]) ? libPtMin[0]  : x[libPtId-1];
        libPtMin[1] = (libPtMin[1]<y[libPtId-1]) ? libPtMin[1]  : y[libPtId-1];
        libPtMin[2] = (libPtMin[2]<z[libPtId-1]) ? libPtMin[2]  : z[libPtId-1];
    }

    dis = std::max(std::max(dis,libPtMax[0]-libPtMin[0]),
                   std::max(libPtMax[1]-libPtMin[1],libPtMax[2]-libPtMin[2]));

// draw the lib points.
    float sf = scale*dis/100.0;
    for(int libPtId=1; libPtId<6; libPtId++)
    {
        SoSeparator *aLibPt = new SoSeparator;
        SoTransform *libXform = new SoTransform;

        libXform->translation.setValue(x[libPtId-1], y[libPtId-1], z[libPtId-1]);
        libXform->scaleFactor.setValue(sf, sf, sf);
        libXform->rotation.setValue(SbVec3f(1.0, 0.0, 0.0), M_PI_2);
        aLibPt->addChild(libXform);

        if(blLibBlink)
        {
            SoBlinker *libBlk = new SoBlinker;
            libBlk->addChild(libGrp);
            libBlk->speed = 0.10;
            aLibPt->addChild(libBlk);
        }
        else
        {
            aLibPt->addChild(libGrp);
        }

        libSep->addChild(aLibPt);
    }
    return libSep;
}
