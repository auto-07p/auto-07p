/*
======================================================================
======================================================================
         Utility Program for Relabeling Labeled Solutions
                        in AUTO97 Data Files
======================================================================
======================================================================
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void sfile(int mxlb, int *nlb, int **llb, FILE *in, FILE *out)
{
    int ibr, ntot, itp, lab, nfpr, isw, ntpl, nar, nrowpr, ntst, ncol, npar;
    int i, l;
    char *line, *p;

    l = 0; *nlb = 0;
    while (fscanf(in, "%d%d%d%d%d%d%d%d%d%d%d%d",
		  &ibr, &ntot, &itp, &lab, &nfpr, &isw, &ntpl,
		  &nar, &nrowpr, &ntst, &ncol, &npar) == 12) {
	if(*nlb>=mxlb){
	    mxlb *= 2;
	    *llb = realloc(*llb, mxlb);
	}
	fgetc(in);    
	(*llb)[*nlb]=lab;
	(*nlb)++;
	l++;
	fprintf(out, "%6d%6d%6d%6d%6d%6d%8d%6d%8d%5d%5d%5d\n",
		ibr, ntot, itp, l, nfpr, isw, ntpl,
		nar, nrowpr, ntst, ncol, npar);
	line = malloc(150*nrowpr);
	p = line;
	for (i=0; i<nrowpr; i++) {
	    fgets(p, 150, in);
	    p += strlen(p);
	}
	fwrite(line, p-line, 1, out);
	free(line);
    }
}

static void bfile(int nlb, int *llb, FILE *in, FILE *out)
{
    char ch, line[132];
    int l, lnum, lab;

    l = 0;
    lnum = 0;
    while (fgets(line, 132, in) != NULL) {
	lnum++;
	if(memcmp(line, "   0", 4) != 0 &&
	   memcmp(line + 14, "    0", 5) != 0) {
	    sscanf(line + 14, "%d", &lab);
	    if (l > nlb || lab != llb[l]) {
		printf("%s\n%s%d%s%d\n%s%d\n%s\n%s",
		       " WARNING : The two files have incompatible labels :",
		       "  b-file label ", lab, " at line ", lnum,
		       "  s-file label ", llb[l],
		       " New labels may be assigned incorrectly.",
		       " Continue ? : ");
		ch = getchar();
		if(ch != 'y' && ch != 'Y'){
		    printf("%s\n",
			   "Rewrite discontinued. Recover original files");
		    return;
		}
	    }
	    l++;
	    sprintf(line + 14, "%5d", l);
	    line[19] = ' ';
	}
	do {
	    fputs(line, out);
	} while (strlen(line) == 131 && line[130] != '\n' &&
		 fgets(line, 132, in) != NULL);
    }
}

int main(int argc, char *argv[])
{
    int *llb;
    int nlb, mxlb = 10000;
    const char *inb, *outb, *ins, *outs;
    FILE *in, *out;

    llb = malloc(mxlb * sizeof(*llb));
    if (argc==5) {
	inb = argv[1];
	ins = argv[2];
	outb = argv[3];
	outs = argv[4];
    } else {
	inb = "fort.27";
	ins = "fort.28";
	outb = "fort.37";
	outs = "fort.38";
    }

    in = fopen(ins, "rt");
    out = fopen(outs, "wt");
    sfile(mxlb, &nlb, &llb, in, out);
    fclose(in);
    fclose(out);

    in = fopen(inb, "rt");
    out = fopen(outb, "wt");
    bfile(nlb, llb, in, out);
    fclose(in);
    fclose(out);
    return 0;
}
