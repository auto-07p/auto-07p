#include <stdlib.h>
#include <math.h>
#include "gplaut04.h"

extern UserData clientData;

////////////////////////////////////////////////////////////////////////////////////
//
void
computePrimaryPositionInInertialSystem(int whichCenter, float mass, float R, float T, float t,
float bigPosition[], float smallPosition[], float velocity[])
//
////////////////////////////////////////////////////////////////////////////////////
{
    float OMEGA = 2.0*M_PI;
    float ot = OMEGA*t;
    float alpha;
    if( whichCenter == INERTIAL_B)
    {
        alpha = mass;
    }
    else if(whichCenter == INERTIAL_S )
    {
        alpha = 0;
    }
    else
    {
        alpha = 1;
    }
    bigPosition[0] = -alpha*R*cos(ot);
    bigPosition[1] = -alpha*R*sin(ot);
    bigPosition[2] = 0;
    smallPosition[0] = (1-alpha)*R*cos(ot);
    smallPosition[1] = (1-alpha)*R*sin(ot);
    smallPosition[2] = 0;
    float otr = OMEGA*R;
    velocity[0] = otr*sin(ot);
    velocity[1] = otr*cos(ot);
    velocity[2]=0;
}


////////////////////////////////////////////////////////////////////////////////////
//
void
smallPrimaryMovingOrbit(float R, float T, float t,
float position[], float velocity[])
//
////////////////////////////////////////////////////////////////////////////////////
{
    float OMEGA = 2.0*M_PI;
    float ot = OMEGA*t;

    position[0] = R*cos(ot);
    position[1] = R*sin(ot);
    position[2] = 0;
    float otr = OMEGA*R;
    velocity[0] = otr*sin(ot);
    velocity[1] = otr*cos(ot);
    velocity[2]=0;
}


///////////////////////////////////////////////////////////////////////////////
//
//       given the center, xyzCoords, velocity, time, mass ratio and T, R,g
//       calculate the inertial frame position.
//
///////////////////////////////////////////////////////////////////////////////

void satelliteMovingOrbit(int center, float xyzCoords[],
float time, float mu, float R, float TS, float T, float g,
float *rpf, float *vpf )
{
    float OMEGA = 2.0*M_PI;
    time = time*TS;
    float ot = time*OMEGA;

    float havv[3];
    float HavOmr = OMEGA*R*R;
    havv[0] = 0;
    havv[1] = 0;
    havv[2] = HavOmr;

    float ex[3], ey[3], ez[3];
    ex[0] = cos(ot); ex[1]=sin(ot);  ex[2]= 0;
    ey[0] =-sin(ot); ey[1]=cos(ot);  ey[2]= 0;
    ez[0] = 0 ;      ez[1]=0;        ez[2]= 1;

    float ptmp[3];
    float c = -mu;

    if(center == 1)      c = 0;
    else if(center == 2) c = -mu;
    else                 c = 1-mu;

    for(int i=0; i<3; i++)
    {
        ptmp[i] = (xyzCoords[0]-c)*ex[i] + xyzCoords[1]*ey[i]+ xyzCoords[2]*ez[i];
        rpf[i]= R*ptmp[i];
    }
}
