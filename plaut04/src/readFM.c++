#include "readFM.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gplaut04.h"
#include "bifurcation.h"

extern UserData clientData;
//////////////////////////////////////////////////////////////////////////////////
//
int 
readFM(const char* dFileName, const int size)
//
//////////////////////////////////////////////////////////////////////////////////
{
    int state = 0;

    char buffer[256];
    int branch, point, prevpoint, myid;
    int maxndim = clientData.maxndim;
    bool eigenvalues;

    float fl1, fl2;
    FILE * inFile;
    inFile = fopen(dFileName, "r");

    if (!inFile)
    {
        printf("Cannot open the diagnostic file. \n");
        state = 1;
        return state;
    }

    char * next;

    long icounter = 1, branchcounter = 0;
    int rowi = 0;
    prevpoint = -1;
    int branchi = 0;

    while ( (next = fgets(buffer, sizeof(buffer), inFile)) != NULL
        && icounter < size)
    {
        if (strstr(buffer, "Total Time") != NULL)
	{
            /* New branch */
            branchcounter += myBifNode->numVerticesEachBranch(branchi++);
	    icounter = branchcounter + 1;
	}

	eigenvalues = strstr(buffer, "Eigenvalue") != NULL;
        if(eigenvalues || strstr(buffer, "Multiplier") != NULL)
        {
            if(clientData.multipliers == NULL) {
                clientData.multipliers = new float[size*maxndim][2];
                clientData.numFM = new int[size];
	    }
            clientData.numFM[icounter] = 0;
            rowi = 0;
            do
            {
                int ret;
                if(eigenvalues)
                    ret = sscanf(buffer, "%d%d Eigenvalue %d: %e%e",
                           &branch, &point, &myid, &fl1, &fl2);
                else
                    ret = sscanf(buffer, "%d%d Multiplier %d%e%e",
                           &branch, &point, &myid, &fl1, &fl2);
                if(ret != 5) { 
                    if (rowi == 0)
                        --icounter;
                    break;
                }
                if (point == prevpoint && rowi == 0)
                    --icounter;
                prevpoint = point;
                clientData.multipliers[icounter*maxndim+rowi][0] = fl1;
                clientData.multipliers[icounter*maxndim+rowi][1] = fl2;
                rowi++;
            }                 //end inner while
            while( fgets(buffer, sizeof(buffer), inFile) &&
                     (strstr(buffer, "Multiplier") != NULL ||
                      strstr(buffer, "Eigenvalue") != NULL) );
            if(rowi !=0)
                clientData.numFM[icounter] = eigenvalues ? -rowi : rowi;
            ++icounter;
        }
    }                         // end outter while
    fclose(inFile);

    return state;
}


