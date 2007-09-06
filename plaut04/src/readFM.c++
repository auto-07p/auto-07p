#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gplaut04.h"

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
    char dummy[20];
    int branch, point, myid;

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

    long icounter = 2;
    int rowi = 0;

    while ( (next = fgets(buffer, sizeof(buffer), inFile)) != NULL
        && icounter < size-2)
    {
        if(strstr(buffer, "Multipliers:") != NULL ||
           strstr(buffer, "Eigenvalues:") != NULL )
        {
            rowi = 0;
            while( fgets(buffer, sizeof(buffer), inFile) &&
                     (strstr(buffer, "Multiplier") != NULL ||
                      strstr(buffer, "Eigenvalue") != NULL) )
            {
                sscanf(buffer, "%d%d%s%d%e%e",&branch, &point, dummy, &myid, &fl1, &fl2);
                clientData.multipliers[icounter-1][rowi][0] = fl1;
                clientData.multipliers[icounter-1][rowi][1] = fl2;
                rowi++;
                clientData.numFM = (rowi>clientData.numFM ) ? rowi : clientData.numFM;
            }                 //end inner while
            ++icounter;
        }
    }                         // end outter while
    fclose(inFile);

    return state;
}


