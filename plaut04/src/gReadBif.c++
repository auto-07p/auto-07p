/* Allows reading in very large files */
#ifndef _LARGEFILE_SOURCE
#define _LARGEFILE_SOURCE
#endif
#ifndef _FILE_OFFSET_BITS
#define _FILE_OFFSET_BITS 64
#endif

#include <stdlib.h>
#include <stdio.h>

#include "gplaut04.h"

extern BifNode myBifNode;
extern UserData clientData;

/*go to the end of the line*/
static void go_to_eol(FILE *f)
{
    int i;
    do 
    {
        i = fgetc(f);
    } while (i != '\n' && i != EOF );
}

////////////////////////////////////////////////////////////////////////////////
//
bool
readBifurcation(const char *bFileName, int varIndices[])
//
////////////////////////////////////////////////////////////////////////////////
{
    int  branch;
    FILE * inFile;
    inFile = fopen(bFileName, "rt");
    if(!inFile)
    {
        printf(" Cannot open input file: %s\n", bFileName);
        return false;
    }

    long int numBranches   = 0;
    long int totalPoints   = 0;
    long int numPtsInThisBranch = 0;
    int maxColSize  = 0;
    int lb, lbType, lbStability;
    int totalLabels = 0;
    int numPtInCurrentInterval = 0;
    int ndim;
    long int pt;

    while(fscanf(inFile, "%d", &branch) == 1)
    {
        int ic = 0;
        if(branch == 0) {
            if(fscanf(inFile, " NDIM=%d", &ndim) == 1 &&
	       ndim > myBifNode.maxndim)
	        myBifNode.maxndim = ndim;
            go_to_eol(inFile);
	}
	else
        {
            fscanf(inFile, "%ld %d %d", &pt, &lbType, &lb);
            int i = 0;
	    char dummystr[25], c;
            while (fscanf(inFile,"%24s%c", dummystr, &c) == 2) {
                clientData.bifData[totalPoints*myBifNode.nar+(i++)] =
		    fortranatof(dummystr);
		if(c=='\n')break;
            }

            if(abs(pt) == 1)
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
            if(branch > 0)
            {
                lbStability = (pt>0 ? 1 : 2);
            }
            else
            {
                lbStability = (pt>0 ? 3 : 4);
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
    FILE * inFile;
    inFile = fopen(bFileName, "rt");
    if(!inFile)
    {
        printf(" Cannot open input file: %s\n", bFileName);
        return false;
    }

    long int totalPoints = 0;
    long int numBranches = 0;
    int maxColSize = 0;
    int branch;

    while(fscanf(inFile, "%d", &branch) == 1)
    {
        if(branch != 0)
        {
            long int pt;
            fscanf(inFile, "%ld", &pt);
            if (abs(pt) == 1) numBranches++;
	    char c;
            int ic = -1;
            while (fscanf(inFile,"%*s%c", &c) == 1 && c !='\n') ic++;
            if (ic > maxColSize) maxColSize = ic;
            ++totalPoints;
        }
        else
	{
            go_to_eol(inFile);
        }
    }
    myBifNode.nar = maxColSize;
    myBifNode.totalNumPoints = totalPoints;
    myBifNode.numBranches = numBranches;

    fclose(inFile);
    return true;
}
