#include <stdlib.h>
#include <stdio.h>

#include "gplaut04.h"
#define MAX_LINE 256

extern BifNode myBifNode;
extern UserData clientData;

bool
readBifurcation(const char *bFileName, int varIndices[])
{
    char line[MAX_LINE];
    int  branch, sn, type;
    float dummy;

    FILE * inFile;
    inFile = fopen(bFileName, "r");
    if(!inFile)
    {
        printf(" Cannot open input file: %s\n", bFileName);
        return false;
    }

    long int numBranches   = 0;
    long int totalPoints   = 0;
    long int numPtInBranch = 0;
    long int last   = 0;
    int maxColSize  = 0;
    int lb          = 0;
    int lbType      = 0;
    int lbStability = 0;
    int totalLabels = 0;
    int numPtInCurrentInterval = 0;

    while(fgets(line, sizeof(line), inFile) !=NULL)
    {
        sscanf(line,"%d", &branch);
        int ic = 0;
        float data[MAX_LIST];
        if(branch != 0)
        {
            if(last == 0)
            {
                myBifNode.branchID[numBranches] = branch;
                ++numBranches;
                ++numPtInBranch;
            }
            else if (last == branch)
            {
                ++numPtInBranch;
            }
            else
            {
                ++numBranches;
                myBifNode.branchID[numBranches-1] = branch;
                myBifNode.numVerticesEachBranch[numBranches-1] = ++numPtInBranch;
                numPtInBranch = 0;
            }

            char * word = strtok(line," ");
            while(word != NULL)
            {
                data[ic]=atof(word);
                ++ic;
                word=strtok(NULL," ");
            }

            lb     = (int)data[3];
            lbType = (int)data[2];

            if(lb == 0)
            {
                ++numPtInCurrentInterval;
            }
            else
            {
                myBifNode.numVerticesEachLabelInterval[totalLabels] = ++numPtInCurrentInterval;
                numPtInCurrentInterval = 0;
            }

            if((int)data[0] > 0)
            {
                lbStability = (((int)data[1]>0) ? 1 : 2);
            }
            else
            {
                lbStability = (((int)data[1]>0) ? 3 : 4);
            }

            if( lb != 0)
            {
                myBifNode.labels[totalLabels++] = lb;
                clientData.labelIndex[totalLabels][1] = totalPoints;
                clientData.labelIndex[totalLabels][2] = lbType;
                clientData.labelIndex[totalLabels][3] = lbStability;
            }

            maxColSize=(maxColSize>ic) ? maxColSize : ic;
            for(int i=0; i<ic-4; i++)
            {
                dummy = data[i+4];
                clientData.bifData[totalPoints][i] = dummy;
            }
            myBifNode.ptStability[totalPoints] = lbStability;
            ++totalPoints;
        }
        else if(last != 0)
        {
            myBifNode.numVerticesEachBranch[numBranches-1]=numPtInBranch;
            numPtInBranch = 0;
        }
        else
        {
// Do nothing
        }
        last = branch;
    }

    myBifNode.totalLabels = totalLabels;

    if(clientData.totalLabels != totalLabels && clientData.totalLabels != 0)
    {
        printf(" total labels in b-file is: %i, in s-file is: %i\n", totalLabels, clientData.totalLabels);
        printf(" The total number of labels in the bifurcation file is different from\n");
        printf(" the total number of labels in the solution file! CHECK IT.\n");
        exit(1);
    }

// if no s file, program should still work, so set totalLabels here again.
    if(clientData.totalLabels == 0) clientData.totalLabels = totalLabels;

    myBifNode.nar = maxColSize-4;
    myBifNode.numBranches = numBranches;
    myBifNode.totalNumPoints = totalPoints;
                                                  //-1;
    myBifNode.numVerticesEachBranch[numBranches-1]=numPtInBranch;

    fclose(inFile);
    return true;
}


////////////////////////////////////////////////////////////////////
//
bool
parseBifurcation(const char *bFileName)
//
////////////////////////////////////////////////////////////////////
{
    char line[MAX_LINE];
    int  branch, sn, type;
    float dummy;

    FILE * inFile;
    inFile = fopen(bFileName, "r");
    if(!inFile)
    {
        printf(" Cannot open input file named: %s\n", bFileName);
        return false;
    }

    int numBranches = 0;
    long int totalPoints = 0;
    long int numPtInBranch = 0;
    long int last = 0;
    int maxColSize = 0;

    while(fgets(line, sizeof(line), inFile) !=NULL)
    {
        sscanf(line,"%d", &branch);
        int ic = 0;
        float data[100];
        if(branch != 0)
        {
            if(last == 0)
            {
                numBranches++;
                numPtInBranch++;
            }
            else if (last == branch)
            {
                numPtInBranch++;
            }
            else
            {
                myBifNode.numVerticesEachBranch[numBranches]=numPtInBranch;
                numBranches ++;
                numPtInBranch = 0;
            }

            char * word = strtok(line," ");
            while(word != NULL)
            {
                data[ic]=atof(word);
                ++ic;
                word=strtok(NULL," ");
            }
            maxColSize=(maxColSize>ic) ? maxColSize : ic;
            totalPoints++;
        }
        else if(last !=0)
        {
            myBifNode.numVerticesEachBranch[numBranches-1]=numPtInBranch;
            numPtInBranch = 0;
        }
        else
        {
// Do nothing here is just OK.
        }
        last = branch;
    }
    myBifNode.nar = maxColSize-4;
    myBifNode.numBranches = numBranches;
    myBifNode.totalNumPoints = totalPoints;
    myBifNode.numVerticesEachBranch[numBranches-1]=numPtInBranch;

    fclose(inFile);
    return true;
}
