#include "r3bplaut04.h"

extern SolNode mySolNode;

void
normalizeSolData()
{
    int np = mySolNode.numOrbits;

    double avg[3], div[3], con[3];
    div[0] = (mySolNode.max[0]-mySolNode.min[0])/2.0;
    div[1] = (mySolNode.max[1]-mySolNode.min[1])/2.0;
    div[2] = (mySolNode.max[2]-mySolNode.min[2])/2.0;

    if(div[0]/mySolNode.max[0]>1.0e-10)
    {
        div[0] = 1.0/div[0];
        con[0] = div[0]*mySolNode.min[0];
    }
    if(div[1]/mySolNode.max[1]>1.0e-10)
    {
        div[1] = 1.0/div[1];
        con[1] = div[1]*mySolNode.min[1];
    }
    if(div[2]/mySolNode.max[2]>1.0e-10)
    {
        div[2] = 1.0/div[2];
        con[2] = div[2]*mySolNode.min[2];
    }

    long int sump = 0;
    for(int i=0; i<np; i++)
    {
        long int nt = mySolNode.numVerticesEachPeriod[i];
        for(int j=0; j<nt; j++)
        {
            if(div[0]/mySolNode.max[0]>1.0e-10)
                mySolNode.xyzCoords[sump+j][0]=mySolNode.xyzCoords[sump+j][0]*div[0]-con[0]-1.0;
            if(div[1]/mySolNode.max[1]>1.0e-10)
                mySolNode.xyzCoords[sump+j][1]=mySolNode.xyzCoords[sump+j][1]*div[1]-con[1]-1.0;
            if(div[2]/mySolNode.max[2]>1.0e-10)
                mySolNode.xyzCoords[sump+j][2]=mySolNode.xyzCoords[sump+j][2]*div[2]-con[2]-1.0;
        }
        sump += nt;
    }
}
