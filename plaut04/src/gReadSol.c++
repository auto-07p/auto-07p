#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include <ctype.h>
#include <math.h>

#include "gplaut04.h"

#define MAX_LINE_LENGTH 256

extern SolNode mySolNode;
extern float *tv;
extern int whichCoordSystem;
extern UserData clientData;

///////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
solutionp
parseSolution( const char* sFileName, bool & blOpenFile, long int &total, long int &totalNumPoints)
//
///////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
{
    solutionp head = NULL;
    solutionp current = NULL;
    solutionp last = NULL;
    FILE * inFile;
    long int position, i;
    int ibr,ntot,itp,lab,nfpr,isw,ntpl,nar,nrowpr,ntst,ncol,npar1;
    int maxColSize=0;

// Open input file
    if((inFile=fopen(sFileName,"r"))==NULL)
    {
        printf(" Cannot open input file : %s ! \n", sFileName);
        blOpenFile = false;
        mySolNode.numOrbits = total;
        mySolNode.totalNumPoints = totalNumPoints;
        mySolNode.nar = maxColSize;
        return head;
    }
    blOpenFile = true;

    totalNumPoints= 0;   
    position = ftell(inFile);
    fscanf(inFile,"%d %d %d %d %d %d %d %d %d %d %d %d",
        &ibr,&ntot,&itp,&lab,&nfpr,&isw,&ntpl,&nar,
        &nrowpr,&ntst,&ncol,&npar1);
    maxColSize = nar;

    total = 0;
    while(!feof(inFile))
    {
        totalNumPoints += (ntpl != 1) ? ntpl : 2;
        current = new solution;

        if(head == NULL)
            head = current;

        if(last != NULL)
            last->next = current;

        current->position  = position;
        current->nrowpr    = nrowpr;
        current->branch    = ibr;
        current->point     = ntot;
        current->type      = itp;
        current->label     = lab;
        current->new_label = lab;
        current->next = NULL;

        last = current;

        for(i=0;i<nrowpr+1;i++)
            while(fgetc(inFile)!='\n');

        if(ntpl != 0 && ntpl != 1)
        {
            total=total+1;
        }

        position = ftell(inFile);
        fscanf(inFile,"%d %d %d %d %d %d %d %d %d %d %d %d",
            &ibr,&ntot,&itp,&lab,&nfpr,&isw,&ntpl,&nar,
            &nrowpr,&ntst,&ncol,&npar1);
        if(maxColSize < nar) maxColSize = nar;
    }

    fclose(inFile);

    mySolNode.numOrbits = total;
    mySolNode.totalNumPoints = totalNumPoints;
    mySolNode.nar = maxColSize;
    return head;
}


///////////////////////////////////////////////////////////////////////
//
// READ solution (orbits) to the array
//
bool
readSolution(solutionp current, const char* sFileName, int varIndices[])
//
///////////////////////////////////////////////////////////////////////

{
    FILE * inFile;
    int ibr,ntot,itp,lab,nfpr,isw,ntpl,nar,nrowpr,ntst,ncol,npar1;
    long int i, j;
    float dummy;
    char line[MAX_LINE_LENGTH];

    if((inFile = fopen(sFileName,"r")) == NULL)
    {
        printf(" Cannot open input file: %s\n", sFileName);
        return false;
    }

    mySolNode.totalNumPoints = 0;
    for(i=0; i<3; i++)
    {
        mySolNode.max[i]=0.0;
        mySolNode.min[i]=0.0;
    }
    long int counter = 0;
    long int orbitCounter = 0;
    long int row = 0;
    long int branchCounter = 0;
    long int lastBranchID  = -999;
    long int totalNumPointsInEachBranch= 0;
    int lbStability = 0;

    mySolNode.labels[0]=0;
    mySolNode.ntst[0]=0;
    mySolNode.ncol[0]=0;

    while(current != NULL)
    {
        rewind(inFile);
        fseek(inFile,current->position,SEEK_SET);

        fscanf(inFile,"%d %d %d %d %d %d %d %d %d %d %d %d",\
            &ibr,&ntot,&itp,&lab,&nfpr,&isw,&ntpl,&nar,\
            &nrowpr,&ntst,&ncol,&npar1);
        if(lastBranchID == -999) lastBranchID = ibr;
        if(lastBranchID != ibr && lastBranchID != -999)
        {
            mySolNode.numVerticesEachBranch[branchCounter]=totalNumPointsInEachBranch;
            mySolNode.numOrbitsInEachBranch[branchCounter]=orbitCounter;
            mySolNode.branchID[branchCounter++]=lastBranchID;
            totalNumPointsInEachBranch = 0;
            lastBranchID = ibr;
            orbitCounter = 0;
        }
        clientData.labelIndex[counter][0] = row;
        clientData.labelIndex[counter][2] = itp;

        if(ibr > 0)
        {
             lbStability = ((ntot>0) ? 1 : 2);
        }
        else
        {
             lbStability = ((ntot>0) ? 3 : 4);
        }

        clientData.labelIndex[counter][3] = lbStability;

        if( ntpl != 0) 
        {
            mySolNode.numVerticesEachPeriod[counter]=ntpl;
            mySolNode.labels[counter] = lab;
            mySolNode.ntst[counter] = ntst;
            mySolNode.ncol[counter] = ncol;
            orbitCounter++;
        }

        while(fgetc(inFile)!='\n');
        {
            {
                for(i=0; i<ntpl; ++i)
                {
                    ++(mySolNode.totalNumPoints);
                    ++totalNumPointsInEachBranch;
                    for(j=0; j<nar; ++j)
                    {
// read all the data set to the dynamic array.
                        char dummystr[25];
                        fscanf(inFile,"%*^[0123456789.EeDd+-]");
                        fscanf(inFile,"%24[0123456789.EeDd+-]",dummystr);
                        dummy=fortranatof(dummystr);
                        clientData.solData[row][j]=dummy;
                        if(row == 0) clientData.solMax[j] = dummy;
                        else
                        if(clientData.solMax[j] < dummy) clientData.solMax[j] = dummy;
                            if(row == 0) clientData.solMin[j] = dummy;
                        else
                        if(clientData.solMin[j] > dummy) clientData.solMin[j] = dummy;
                    }
                    ++row;
                }

                if(ntst != 0 && ncol !=0 )
                {
                    for(i=0; i<ntpl+2; ++i) fgets(line, sizeof(line), inFile);
                }

                int nLines = (npar1/7 == 0) ? npar1/7 : npar1/7 + 1;
                for(int nzoo = 0; nzoo<nLines; ++nzoo)
                {
                    fgets(line, sizeof(line), inFile);
                    int xCol = (npar1 - nzoo*7) > 7 ? 7 : npar1 - nzoo*7;
                    for(i=0; i<xCol; ++i)
                    {
                        char dummystr[25];
                        fscanf(inFile,"%f",&dummy);
                        fscanf(inFile,"%*^[0123456789.EeDd+-]");
                        fscanf(inFile,"%24[0123456789.EeDd+-]",dummystr);
                        dummy=fortranatof(dummystr);
                        if(dummy != 0)
                        {
                            mySolNode.par[counter][nzoo*7+i] = dummy;
                            if( nzoo*7+i==1 ) mySolNode.mass[counter] = dummy;
                            else if( nzoo*7+i==10 ) clientData.solPeriod[counter]=dummy;
                        }
                    }
                }
            }
        }
        current = current->next;
        if( ntpl != 0) 
            counter++;
    }
    mySolNode.numVerticesEachBranch[branchCounter]=totalNumPointsInEachBranch;
    mySolNode.numOrbitsInEachBranch[branchCounter]=orbitCounter;
    mySolNode.branchID[branchCounter]=ibr;
    mySolNode.numBranches = ++branchCounter;

    clientData.totalLabels = counter;
    mySolNode.totalLabels = counter;
    mySolNode.numOrbits    = counter;


    double parMax, parMin, parMid;

    for(int jv = 0; jv<mySolNode.npar; ++jv)
    {
        long int startBranch = 0;
        long int endBranch = 0;
        for(int iBranch=0; iBranch<mySolNode.numBranches; iBranch++)
        {
            parMax = mySolNode.par[startBranch][mySolNode.parID[jv]];
            parMin = mySolNode.par[startBranch][mySolNode.parID[jv]];
            parMid = mySolNode.par[startBranch][mySolNode.parID[jv]];
            endBranch = startBranch+mySolNode.numOrbitsInEachBranch[iBranch];
            for(int innerLoop = startBranch; innerLoop<endBranch; ++innerLoop)
            {
                if(parMax <mySolNode.par[innerLoop][mySolNode.parID[jv]] )
                    parMax = mySolNode.par[innerLoop][mySolNode.parID[jv]];
                if(parMin >mySolNode.par[innerLoop][mySolNode.parID[jv]] )
                    parMin = mySolNode.par[innerLoop][mySolNode.parID[jv]];
            }
            mySolNode.parMax[iBranch][jv]=parMax;
            mySolNode.parMin[iBranch][jv]=parMin;
            mySolNode.parMid[iBranch][jv]=(parMin+parMax)/2.0;
            startBranch = endBranch;
        }
    }


    if(whichCoordSystem != ROTATING_F)
    {
        float r[3],v[3];
        int center = 0;
        if(whichCoordSystem == INERTIAL_B) center = 0;
        if(whichCoordSystem == INERTIAL_S) center = 1;
        if(whichCoordSystem == INERTIAL_E) center = 2;
        for(int i=0; i<mySolNode.totalNumPoints; i++)
        {
            mySolNode.xyzCoords[i][0]=r[0];
            mySolNode.xyzCoords[i][1]=r[1];
            mySolNode.xyzCoords[i][2]=r[2];
        }
    }
    fclose(inFile);
    return true;
}
