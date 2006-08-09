#include "gplaut04.h"

extern SolNode mySolNode;

void 
normalizeSolData()//SolNode mySolNode)
{
    // To convert the original data to [-1,1];
    int np = mySolNode.numOrbits;

#ifdef DEBUG
    cout <<" Max sol 0 :" <<mySolNode.max[0]<<" Min "<<mySolNode.min[0]<<endl;
    cout <<" Max sol 1 :" <<mySolNode.max[1]<<" Min "<<mySolNode.min[1]<<endl;
    cout <<" Max sol 2 :" <<mySolNode.max[2]<<" Min "<<mySolNode.min[2]<<endl;
#endif
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
        /**
            if( !((mySolNode.max[0]<=1.0  && mySolNode.max[0]>0.5 &&
                 mySolNode.min[0]>=-1.0 && mySolNode.min[0]<-0.5 )||
                (div[0]<0.00000001)))
            {
                mySolNode.xyzCoords[sump+j][0]=
                       (mySolNode.xyzCoords[sump+j][0]-avg[0])/div[0];
            }
                                                                                                               
            if( !((mySolNode.max[1]<=1.0  && mySolNode.max[1]>0.5 &&
                 mySolNode.min[1]>=-1.0 && mySolNode.min[1]<-0.5 )||
                (div[0]<0.00000001)))
            {
                mySolNode.xyzCoords[sump+j][1]=
                    (mySolNode.xyzCoords[sump+j][1]-avg[1])/div[1];
            }
                                                                                                               
            if( !((mySolNode.max[2]<=1.0  && mySolNode.max[2]>0.5 &&
                 mySolNode.min[2]>=-1.0 && mySolNode.min[2]<0.1 )||
                (div[2]<0.00000001)))
            {
                mySolNode.xyzCoords[sump+j][2]=
                    (mySolNode.xyzCoords[sump+j][2]-avg[2])/div[2];
            }
        **/
        }
        sump += nt;
    }
}
