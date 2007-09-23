#include <stdlib.h>
#include <stdio.h>

#include "gplaut04.h"
#define MAX_LINE 1024

extern BifNode myBifNode;
extern UserData clientData;
////////////////////////////////////////////////////////////////////////////////
//
bool
readBifurcation(const char *bFileName, int varIndices[])
//
////////////////////////////////////////////////////////////////////////////////
{
    char line[MAX_LINE];
    int  branch;
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
    long int numPtsInThisBranch = 0;
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
            char * word = strtok(line," ");
            while(word != NULL)
            {
                data[ic]=atof(word);
                ++ic;
                word=strtok(NULL," ");
            }

            if(abs((int)data[1]) == 1)
            {
                myBifNode.branchID[numBranches] = branch;
                if(numBranches>0) {
                    myBifNode.numVerticesEachBranch[numBranches-1] =
                        numPtsInThisBranch;
                }
                ++numBranches;
                numPtsInThisBranch = 0;
            }
            ++numPtsInThisBranch;
#ifndef R3B
            ++totalPoints;
#endif

            // set the stability of the point
            lbType = (int)data[2];
            lb     = (int)data[3]; 
            if((int)data[0] > 0)
            {
                lbStability = (((int)data[1]>0) ? 1 : 2);
            }
            else
            {
                lbStability = (((int)data[1]>0) ? 3 : 4);
            }

            ++numPtInCurrentInterval;
            if(lb != 0)
            {
                myBifNode.numVerticesEachLabelInterval[totalLabels] = numPtInCurrentInterval;
                numPtInCurrentInterval = 0;
#ifdef R3B
                totalLabels++;
#endif
                myBifNode.labels[totalLabels] = lb;
                clientData.labelIndex[totalLabels][1] = totalPoints;
                clientData.labelIndex[totalLabels][2] = lbType;
                clientData.labelIndex[totalLabels][3] = lbStability;
#ifndef R3B
                totalLabels++;
#endif
            }

            maxColSize=(maxColSize>ic) ? maxColSize : ic;
            for(int i=0; i<ic-4; i++)
            {
                dummy = data[i+4];
#ifndef R3B
                clientData.bifData[totalPoints-1][i] = dummy;
#else
                clientData.bifData[totalPoints][i] = dummy;
#endif
            }

#ifdef R3B
            myBifNode.ptStability[totalPoints] = lbStability;
            ++totalPoints;
#else
            myBifNode.ptStability[totalPoints-1] = lbStability;
#endif
        }
    }

    if(numBranches>0)
        myBifNode.numVerticesEachBranch[numBranches-1] = numPtsInThisBranch;
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

    myBifNode.numBranches = numBranches;

#ifdef DEBUG
    cout <<"======================================"<<endl;
    cout <<" myBifNode.nar :         "<<myBifNode.nar<<endl;
    cout <<" myBifNode.numBranches : "<<myBifNode.numBranches<<endl;
    cout <<" myBifNode.totalNumPoints: "<<myBifNode.totalNumPoints<<endl;
    cout <<" ID "<<"  myBifNode.numVerticesEachBranch: "<<endl;
    for(int xi=0; xi<myBifNode.numBranches; xi++)
       cout << xi<<"    "<<myBifNode.numVerticesEachBranch[xi]<<endl;
    for(int xi=0; xi<totalLabels; xi++)
    {
         int xxx = clientData.labelIndex[xi][1];
         cout <<"index"<<xi<<" | Num points in : "<<clientData.labelIndex[xi][1] 
              <<" | labelIndex: "<<myBifNode.labels[xi]<< "  | data: "
             <<clientData.bifData[xxx-1][0]<<" Type: "<<clientData.labelIndex[xi][2]<<endl; 
    }
    cout <<"======================================"<<endl;

    for(int xi=0; xi<220; xi++)
        cout <<" Index: "<<xi<<" "<<clientData.bifData[xi][0]<<"  "
            <<clientData.bifData[xi][1]<<"  "<<clientData.bifData[xi][2]<<endl;
    cout <<"======================================"<<endl;
#endif

#ifdef R3B
    myBifNode.nar = maxColSize-4;
    myBifNode.totalNumPoints = totalPoints;
                                                  //-1;
    myBifNode.numVerticesEachBranch[numBranches-1]=numPtsInThisBranch;
#endif

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
    int  branch;

    FILE * inFile;
    inFile = fopen(bFileName, "r");
    if(!inFile)
    {
#ifndef R3B
        printf(" Cannot open input file: %s\n", bFileName);
#else
        printf(" Cannot open input file named: %s\n", bFileName);
#endif
        return false;
    }

#ifdef R3B
    int numBranches = 0;
#endif
    long int totalPoints = 0;
#ifdef R3B
    long int numPtsInThisBranch = 0;
    long int last = 0;
#endif
    int maxColSize = 0;

    while(fgets(line, sizeof(line), inFile) !=NULL)
    {
        sscanf(line,"%d", &branch);
        int ic = 0;
        float data[100];
        if(branch != 0)
        {

#ifndef R3B
            ++totalPoints;
#else
            if(last == 0)
            {
                numBranches++;
                numPtsInThisBranch++;
            }
            else if (last == branch)
            {
                numPtsInThisBranch++;
            }
            else
            {
                myBifNode.numVerticesEachBranch[numBranches]=numPtsInThisBranch;
                numBranches ++;
                numPtsInThisBranch = 0;
            }
#endif

            char * word = strtok(line," ");
            while(word != NULL)
            {
                data[ic]=atof(word);
                ++ic;
                word=strtok(NULL," ");
            }

            maxColSize=(maxColSize>ic) ? maxColSize : ic;
#ifdef R3B
            totalPoints++;
#endif
        }
#ifdef R3B
        else if(last !=0)
        {
            myBifNode.numVerticesEachBranch[numBranches-1]=numPtsInThisBranch;
            numPtsInThisBranch = 0;
        }
        else
        {
// Do nothing here is just OK.
        }
        last = branch;
#endif
    }
    myBifNode.nar = maxColSize-4;
#ifdef R3B
    myBifNode.numBranches = numBranches;
#endif
    myBifNode.totalNumPoints = totalPoints;
#ifdef R3B
    myBifNode.numVerticesEachBranch[numBranches-1]=numPtsInThisBranch;
#endif

    fclose(inFile);
    return true;
}
