/* Allows reading in very large files */
#define _LARGEFILE_SOURCE
#define _FILE_OFFSET_BITS 64

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
            char * word = strtok(line," \n");
            while(word != NULL)
            {
                data[ic]=fortranatof(word);
                ++ic;
                word=strtok(NULL," \n");
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
                myBifNode.labels[totalLabels] = lb;
                clientData.labelIndex[totalLabels][1] = totalPoints;
                clientData.labelIndex[totalLabels][2] = lbType;
                clientData.labelIndex[totalLabels][3] = lbStability;
                totalLabels++;
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

    FILE * inFile;
    inFile = fopen(bFileName, "r");
    if(!inFile)
    {
        printf(" Cannot open input file: %s\n", bFileName);
        return false;
    }

    long int totalPoints = 0;
    long int numBranches = 0;
    int maxColSize = 0;

    while(fgets(line, sizeof(line), inFile) !=NULL)
    {
        char *word = line;
        word = strpbrk(word, "-0123456789");
        int ic = 1;
        if(word[0] != '0')
        {
            word = strpbrk(word, " \t\n");
            while(word != NULL)
            {
                word = strpbrk(word, "-0123456789");
                if (word == NULL) break;
                ++ic;
		if (ic == 2 &&
		    ((word[0] == '1' && word[1] == ' ') ||
		     (word[0] == '-' && word[1] == '1' && word[2] == ' ')))
		      numBranches++; 
                word = strpbrk(word, " \t\n");
            }
            if (ic > maxColSize) maxColSize = ic;
            ++totalPoints;
        }
    }
    myBifNode.nar = maxColSize-4;
    myBifNode.totalNumPoints = totalPoints;
    myBifNode.numBranches = numBranches;

    fclose(inFile);
    return true;
}
