#include <stdlib.h>
#include <stdio.h>

#include "gplaut04.h"
#define MAX_LINE 256

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
    int  branch, sn, type;
    float dummy;

    FILE * inFile;
    inFile = fopen(bFileName, "r");
    if(!inFile)
    {
        printf(" Cannot open input file: %s\n", bFileName);
        return false;
    }

#ifndef R3B
    long int totalBranches   = 0;
#else
    long int numBranches   = 0;
#endif
    long int totalPoints   = 0;
#ifndef R3B
    long int numPtsInThisBranch = 0;
#else
    long int numPtInBranch = 0;
#endif
    long int last   = 0;
#ifndef R3B
    long int curPt  = 0;

#endif
    int maxColSize  = 0;
    int lb          = 0;
    int lbType      = 0;
    int lbStability = 0;
    int totalLabels = 0;
    int numPtInCurrentInterval = 0;

#ifndef R3B
    bool isLastEndPt     = false;

#endif
    while(fgets(line, sizeof(line), inFile) !=NULL)
    {
        sscanf(line,"%d", &branch);
        int ic = 0;
        float data[MAX_LIST];
        if(branch != 0)
        {
#ifndef R3B
            myBifNode.branchID[totalBranches] = branch;
            ++numPtsInThisBranch;
            ++totalPoints;
#else
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
#endif

            char * word = strtok(line," ");
            while(word != NULL)
            {
                data[ic]=atof(word);
                ++ic;
                word=strtok(NULL," ");
            }

#ifndef R3B
            // set the stability of the point
            curPt  = (int)data[1];
            lbType = (int)data[2];
#endif
            lb     = (int)data[3]; 
#ifndef R3B
            if((int)data[0] > 0)
#else
            lbType = (int)data[2];

            if(lb == 0)
#endif
            {
#ifndef R3B
                lbStability = (((int)data[1]>0) ? 1 : 2);
#else
                ++numPtInCurrentInterval;
#endif
            }
            else
            {
#ifndef R3B
                lbStability = (((int)data[1]>0) ? 3 : 4);
#else
                myBifNode.numVerticesEachLabelInterval[totalLabels] = ++numPtInCurrentInterval;
                numPtInCurrentInterval = 0;
#endif
            }
#ifndef R3B
            myBifNode.ptStability[totalPoints-1] = lbStability;
#endif

#ifndef R3B
            // set the num of points in this label interval
            if(lb == 0)
#else
            if((int)data[0] > 0)
#endif
            {
#ifndef R3B
                ++numPtInCurrentInterval;
#else
                lbStability = (((int)data[1]>0) ? 1 : 2);
#endif
            }
            else
            {
#ifndef R3B
                myBifNode.numVerticesEachLabelInterval[totalLabels] = ++numPtInCurrentInterval;
                numPtInCurrentInterval = 0;
#else
                lbStability = (((int)data[1]>0) ? 3 : 4);
            }
#endif

#ifndef R3B
                myBifNode.labels[totalLabels] = lb;
#else
            if( lb != 0)
            {
                myBifNode.labels[totalLabels++] = lb;
#endif
                clientData.labelIndex[totalLabels][1] = totalPoints;
                clientData.labelIndex[totalLabels][2] = lbType;
#ifndef R3B
                clientData.labelIndex[totalLabels++][3] = lbStability;
#else
                clientData.labelIndex[totalLabels][3] = lbStability;
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

#ifndef R3B
            // ---- added for dealing with pp2's first pt.
            if( isLastEndPt && curPt == 2)
            {
                numPtsInThisBranch += myBifNode.numVerticesEachBranch[totalBranches-1];
                --totalBranches;
                isLastEndPt  = false;
#else
            myBifNode.ptStability[totalPoints] = lbStability;
            ++totalPoints;
#endif
            }
#ifndef R3B
            // --- end 

            // change branch
            if( (abs(lbType) % 10) == 9 )
#else
        else if(last != 0)
#endif
            {
#ifndef R3B
                ++totalBranches;
                myBifNode.numVerticesEachBranch[totalBranches-1] = numPtsInThisBranch;
                numPtsInThisBranch = 0;
                isLastEndPt  = true;
#else
            myBifNode.numVerticesEachBranch[numBranches-1]=numPtInBranch;
            numPtInBranch = 0;
#endif
            }
#ifndef R3B
            // ---- added for dealing with pp2's first pt.
#endif
            else
            {
#ifndef R3B
                isLastEndPt  = false;
            }
            // --- end 


#else
// Do nothing
#endif
        }
#ifdef R3B
        last = branch;
#endif
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

#ifndef R3B
    myBifNode.numBranches = totalBranches;

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
#else
    myBifNode.nar = maxColSize-4;
    myBifNode.numBranches = numBranches;
    myBifNode.totalNumPoints = totalPoints;
                                                  //-1;
    myBifNode.numVerticesEachBranch[numBranches-1]=numPtInBranch;
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
    int  branch, sn, type;
    float dummy;

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
    long int numPtInBranch = 0;
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
            myBifNode.numVerticesEachBranch[numBranches-1]=numPtInBranch;
            numPtInBranch = 0;
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
    myBifNode.numVerticesEachBranch[numBranches-1]=numPtInBranch;
#endif

    fclose(inFile);
    return true;
}
