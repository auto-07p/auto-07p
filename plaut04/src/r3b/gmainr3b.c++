#include "gplaut04.h"
#include "../createDisk.h"
#include "../solution.h"

float libPtScaler = 1.0;
float smallPrimRadius=1.0;
float largePrimRadius=1.0;
int   numOfStars = 100;
double mass = 0.0;
bool blMassDependantOption = false;

SoSeparator *createR3BPoints(float nodemin[], float nodemax[])
{
    SoSeparator *result = new SoSeparator;
    float dis = std::max(std::max((nodemax[0]-nodemin[0]),
                                  (nodemax[1]-nodemin[1])),
                         (nodemax[2]-nodemin[2]));

    if(whichType == SOLUTION && whichCoordSystem != ROTATING_F)
    {
        if(options[OPT_REF_PLAN])
        {
            float radius =diskRadius;
            if(whichCoordSystem == INERTIAL_B ) radius = 1-mass;
            SoSeparator *diskSep = createDisk(diskPosition,radius);
            result->addChild(diskSep);
        }
        result->addChild(mySolNode->createInertialFrameScene(dis));
        return result;
    }

    if(whichType == BIFURCATION) dis = 1.0;
    if(options[OPT_LIB_POINTS])
    {
        SoSeparator * libPtsSep = createLibrationPoint(mass, dis, libPtScaler,
                                                       whichCoordSystem);
        result->addChild(libPtsSep);
    }

// create reference DISK
    if(options[OPT_REF_PLAN])
    {
        float position[3];
	SoSeparator *diskSep;
        if(whichType == BIFURCATION)
        {
            diskSep = createDisk(diskPosition,diskRadius);
        }
        else
        {
            position[0]=-mass;
            position[1]=position[2]=0;
	    diskSep = createDisk(position,1.0);
        }
        result->addChild(diskSep);
    }

// create the primaries
    if(options[OPT_PRIMARY])
    {
        double pos1 = 1-mass;
        double pos2 = -mass;
        char *txtureFileName = new char [strlen(autoDir) + 30];
        sprintf(txtureFileName, "%s%s", autoDir, "/plaut04/widgets/large.jpg");
        result->addChild(createPrimary(1-mass, pos2, 0.25*largePrimRadius, txtureFileName));
        sprintf(txtureFileName, "%s%s", autoDir, "/plaut04/widgets/small.jpg");
        result->addChild(createPrimary(mass, pos1, 0.25*smallPrimRadius, txtureFileName));
        delete [] txtureFileName;
    }
    return result;
}

#if 0
////////////////////////////////////////////////////////////////////////
//
//      Set the initial values for the variables: animationLabel, orbitSpeed
//      satSpeed.
//
void
initPara()
//
////////////////////////////////////////////////////////////////////////
{
    if(solHead != NULL)
    {
        solutionp cur = solHead;
        while(cur)
        {
            cur=cur->next;
            delete solHead;
            solHead = cur;
        }
    }
    solHead = NULL;
    animationLabel = MY_ALL;
    orbitSpeed = 1.0;
    satSpeed   = 0.5;
}

//////////////////////////////////////////////////////////////////////////
//
SoSeparator *
createStarryBackground(int total, float diameter)
//
//////////////////////////////////////////////////////////////////////////
{
    static bool starsCreated = false;
    static SoSeparator * myBg = new SoSeparator;
    if(!starsCreated)
    {
        starsCreated = true;
        srand(time(NULL));
        SoSphere * star = new SoSphere;
        star->radius = 0.002*(rand()%5+1);
        float x, y, z;
        for(int i=0; i<total; ++i)
        {
            SoSeparator * aStar = new SoSeparator;
            SoTranslation *starTrans = new SoTranslation;
            x = ((rand()%200)/100.0-1)*diameter;
            y = ((rand()%200)/100.0-1)*diameter;
            z = ((rand()%200)/100.0-1)*diameter;
            starTrans->translation.setValue(x, y, z);
            aStar->addChild(starTrans);
            aStar->addChild(star);
            myBg->addChild(aStar);
        }
    }
    myBg->unrefNoDelete();
    return myBg;
}
#endif
