#include "normalizeSolData.h"

#include "gplaut04.h"
#include "solution.h"

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
    double div[3], con[3];
    for(int k=0; k<3; k++)
    {
        con[k] = 0.0;
        div[k] = (mySolNode.max[k]-mySolNode.min[k])/2.0;

        if(div[k]/mySolNode.max[k]>1.0e-10) 
        {
            div[k] = 1.0/div[k];
            con[k] = div[k]*mySolNode.min[k];
        }
    }

    long int sump = 0;
    for(int i=0; i<np; i++)
    {
        long int nt = mySolNode.numVerticesEachPeriod[i];
        for(int j=0; j<nt; j++)
        {
            for(int k=0; k<3; k++)
            {
                if(div[k]/mySolNode.max[k]>1.0e-10)
                mySolNode.xyzCoords[sump+j][k]=
                    mySolNode.xyzCoords[sump+j][k]*div[k]-con[k]-1.0;
            }
        /**
            if( !((mySolNode.max[k]<=1.0  && mySolNode.max[k]>0.5 &&
                 mySolNode.min[k]>=-1.0 && mySolNode.min[k]<-0.5 )||
                (div[k]<0.00000001)))
            {
                mySolNode.xyzCoords[sump+j][k]=
                       (mySolNode.xyzCoords[sump+j][k]-avg[k])/div[k];
            }
        **/
        }
        sump += nt;
    }
}
