#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "r3bplaut04.h"

extern UserData clientData;

/////////////////////////////////////////////////////
//
int readFM(const char* dFileName, const int size)
//
/////////////////////////////////////////////////////
{
    int state = 0;

    char buffer[256];
    char dummy[20];
    int branch, point, myid;
    float floqueMultipliers[1000][6][2];

    float fl1, fl2;
    FILE * inFile;
    inFile = fopen(dFileName, "r");

// Check if there was an error opening the file
    if (!inFile)
    {
        printf("Unable to open the diagnostic file. \n");
        state = 1;
        return state;
    }

    char * next;

    long icounter =2, lCounter=1;

    while ( (next = fgets(buffer, sizeof(buffer), inFile)) != NULL
        && icounter < size-2)
    {
        if(strstr(buffer, "Multipliers:") != NULL)
        {
            for(int i=0; i<6; ++i)
            {
                fscanf(inFile, "%d%d%s%d%e%e",&branch, &point, dummy, &myid, &fl1, &fl2);
                clientData.multipliers[point-1][i][0] = fl1;
                clientData.multipliers[point-1][i][1] = fl2;
            }
            ++icounter;
            fgets(buffer,sizeof(buffer),inFile);
        }
        else
        {
// do nothing, just discard this line.
        }
    }
    fclose(inFile);

    return state;
}
