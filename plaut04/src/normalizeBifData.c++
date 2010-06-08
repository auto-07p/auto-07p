#include "normalizeBifData.h"

#include "gplaut04.h"
#include "bifurcation.h"

/////////////////////////////////////////////////////////////////////////////////
//
void
normalizeBifData(long int idx, float xyzCoords[3])
//
/////////////////////////////////////////////////////////////////////////////////
{
    for(int k=0; k<3; k++)
    {
        if (myBifNode.varIndices[k] == -1)
	    xyzCoords[k] = 0.0;
	else
	    xyzCoords[k] = clientData.bifData[idx*myBifNode.nar + 
                                              myBifNode.varIndices[k]];
    }
    if(!options[OPT_NORMALIZE_DATA]) return;
    for(int k=0; k<3; k++)
    {
        float avg = (myBifNode.max[k]+myBifNode.min[k])/2.0;
        float div = (myBifNode.max[k]-myBifNode.min[k])/2.0;
        if( !((myBifNode.max[k]<=1.0  && myBifNode.max[k]>0.5 &&
	   myBifNode.min[k]>=-1.0 && myBifNode.min[k]<-0.5 )||
	  (div<0.00000001)))
        {
            xyzCoords[k]=(xyzCoords[k]-avg)/div;
        }
    }
}
