#include "r3bplaut04.h"

extern BifNode myBifNode;

void
normalizeBifData()
{
    int np = myBifNode.numBranches;
    float avg[3], div[3];
    avg[0] = (myBifNode.max[0]+myBifNode.min[0])/2.0;
    avg[1] = (myBifNode.max[1]+myBifNode.min[1])/2.0;
    avg[2] = (myBifNode.max[2]+myBifNode.min[2])/2.0;
    div[0] = (myBifNode.max[0]-myBifNode.min[0])/2.0;
    div[1] = (myBifNode.max[1]-myBifNode.min[1])/2.0;
    div[2] = (myBifNode.max[2]-myBifNode.min[2])/2.0;
    long int sump = 0;
    for(int i=0; i<np; i++)
    {
        long int nt = myBifNode.numVerticesEachBranch[i];
        for(int j=0; j<nt; j++)
        {
            if( !((myBifNode.max[0]<=1.0  && myBifNode.max[0]>0.5 &&
                myBifNode.min[0]>=-1.0 && myBifNode.min[0]<-0.5 )||
                (div[0]<0.00000001)))
            {
                myBifNode.xyzCoords[sump+j][0]=
                    (myBifNode.xyzCoords[sump+j][0]-avg[0])/div[0];
            }

            if( !((myBifNode.max[1]<=1.0  && myBifNode.max[1]>0.5 &&
                myBifNode.min[1]>=-1.0 && myBifNode.min[1]<-0.5 )||
                (div[0]<0.00000001)))
            {
                myBifNode.xyzCoords[sump+j][1]=
                    (myBifNode.xyzCoords[sump+j][1]-avg[1])/div[1];
            }

            if( !((myBifNode.max[2]<=1.0  && myBifNode.max[2]>0.5 &&
                myBifNode.min[2]>=-1.0 && myBifNode.min[2]<0.1 )||
                (div[2]<0.00000001)))
            {
                myBifNode.xyzCoords[sump+j][2]=
                    (myBifNode.xyzCoords[sump+j][2]-avg[2])/div[2];
            }
        }
        sump += nt;
    }
}
