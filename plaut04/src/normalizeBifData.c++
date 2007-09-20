#include "gplaut04.h"

extern BifNode myBifNode;

/////////////////////////////////////////////////////////////////////////////////
//
void
normalizeBifData()
//
/////////////////////////////////////////////////////////////////////////////////
{
    int np = myBifNode.numBranches;
    float avg[3], div[3];
    for(int k=0; k<3; k++)
    {
        avg[k] = (myBifNode.max[k]+myBifNode.min[k])/2.0;
        div[k] = (myBifNode.max[k]-myBifNode.min[k])/2.0;
    }
    long int sump = 0;
    for(int i=0; i<np; i++)
    {
        long int nt = myBifNode.numVerticesEachBranch[i];
        for(int j=0; j<nt; j++)
        {
            for(int k=0; k<3; k++)
            {
                if( !((myBifNode.max[k]<=1.0  && myBifNode.max[k]>0.5 &&
                    myBifNode.min[k]>=-1.0 && myBifNode.min[k]<-0.5 )||
                    (div[k]<0.00000001)))
                {
                    myBifNode.xyzCoords[sump+j][k]=
                        (myBifNode.xyzCoords[sump+j][k]-avg[k])/div[k];
                }
            }
        }
        sump += nt;
    }
}
