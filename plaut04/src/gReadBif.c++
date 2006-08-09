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

    long int totalBranches   = 0;
    long int totalPoints   = 0;
    long int numPtsInThisBranch = 0;
    long int last   = 0;
    long int curPt  = 0;

    int maxColSize  = 0;
    int lb          = 0;
    int lbType      = 0;
    int lbStability = 0;
    int totalLabels = 0;
    int numPtInCurrentInterval = 0;

    bool isLastEndPt     = false;

    while(fgets(line, sizeof(line), inFile) !=NULL)
    {
        sscanf(line,"%d", &branch);
        int ic = 0;
        float data[MAX_LIST];
        if(branch != 0)
        {
            myBifNode.branchID[totalBranches] = branch;
            ++numPtsInThisBranch;
            ++totalPoints;

            char * word = strtok(line," ");
            while(word != NULL)
            {
                data[ic]=atof(word);
                ++ic;
                word=strtok(NULL," ");
            }

            // set the stability of the point
            curPt  = (int)data[1];
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
            myBifNode.ptStability[totalPoints-1] = lbStability;

            // set the num of points in this label interval
            if(lb == 0)
            {
                ++numPtInCurrentInterval;
            }
            else
            {
                myBifNode.numVerticesEachLabelInterval[totalLabels] = ++numPtInCurrentInterval;
                numPtInCurrentInterval = 0;

                myBifNode.labels[totalLabels] = lb;
                clientData.labelIndex[totalLabels][1] = totalPoints;
                clientData.labelIndex[totalLabels][2] = lbType;
                clientData.labelIndex[totalLabels++][3] = lbStability;
            }

            maxColSize=(maxColSize>ic) ? maxColSize : ic;
            for(int i=0; i<ic-4; i++)
            {
                dummy = data[i+4];
                clientData.bifData[totalPoints-1][i] = dummy;
            }

            // ---- added for dealing with pp2's first pt.
            if( isLastEndPt && curPt == 2)
            {
                numPtsInThisBranch += myBifNode.numVerticesEachBranch[totalBranches-1];
                --totalBranches;
                isLastEndPt  = false;
            }
            // --- end 

            // change branch
            if( abs(abs(lbType)-9)<1.0e-10 ) 
            {
                ++totalBranches;
                myBifNode.numVerticesEachBranch[totalBranches-1] = numPtsInThisBranch;
                numPtsInThisBranch = 0;
                isLastEndPt  = true;
            }
            // ---- added for dealing with pp2's first pt.
            else
            {
                isLastEndPt  = false;
            }
            // --- end 


        }
    }

    myBifNode.totalLabels = totalLabels;

    if(clientData.totalLabels != totalLabels && clientData.totalLabels != 0)
    {
        printf(" total labels in b-file is: %i, in s-file is: %i\n", totalLabels, clientData.totalLabels);
        printf(" The total number of labels in the bifurcation file is different from\n");
        printf(" the total number of labels in the solution file! CHECK IT.\n");
        exit(1);
    }

    if(clientData.totalLabels == 0) clientData.totalLabels = totalLabels;

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
        printf(" Cannot open input file: %s\n", bFileName);
        return false;
    }

    long int totalPoints = 0;
    int maxColSize = 0;

    while(fgets(line, sizeof(line), inFile) !=NULL)
    {
        sscanf(line,"%d", &branch);
        int ic = 0;
        float data[100];
        if(branch != 0)
        {

            ++totalPoints;

            char * word = strtok(line," ");
            while(word != NULL)
            {
                data[ic]=atof(word);
                ++ic;
                word=strtok(NULL," ");
            }

            maxColSize=(maxColSize>ic) ? maxColSize : ic;
        }
    }
    myBifNode.nar = maxColSize-4;
    myBifNode.totalNumPoints = totalPoints;

    fclose(inFile);
    return true;
}
