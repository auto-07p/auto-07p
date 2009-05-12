#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "tek.h"

/* output filter for tektronics 4010-graphics files to generate postscript
 * files.  This does a simple-minded byte by byte translation to predefined
 * PS routines, contained in a prolog file.
 * Defaults:
 * input:	stdin
 * output:	stdout
 * prolog:	as specified 12 lines below
 */

int main(argc, argv)
	int	argc;
	char	*argv[];
{
#ifndef lint
	static char	sccsid[]=" @(#)t2p.c	1.10 tek2ps (Copyright) M. Fischbein  Commercial reproduction prohibited; non-profit reproduction and distribution encouraged.";
#endif
	char		*pro_fn, *auto_dir;
	FILE		*pro_fp, *infile = stdin;
	int		c, mode=ALPHA, tsizex=CHUGEX, tsizey=CHUGEY, oldmode;
	int		cx=0, cy=YDIM-CHUGEY, leftmargin=0;
	int		used_large=FALSE, used_med=FALSE, used_small=FALSE;
	int		dark_vector, HiX=0, HiY=0, LoY=0, LoX=0, BX=0, BY=0;
	int		gotLoY=FALSE, debug=FALSE, beamon, pr_on_er=FALSE;
	int		ignore_er=FALSE;
	double		scale_factor=1.0;

	auto_dir = getenv("AUTO_DIR");
	pro_fn = malloc( strlen(auto_dir) + strlen("/tek2ps/pstek.pro") + 1);
	strcpy(pro_fn, auto_dir);
	strcat(pro_fn, "/tek2ps/pstek.pro");

	/* first, parse command line */
	while ((c = getopt (argc, argv, "s:p:dei")) != EOF) {
		switch (c) {
		case 'p' :	/* use custom prolog */
			pro_fn = optarg;
			break;

		case 'd' :	/* toggle debugging */
			if (debug) {
				debug = FALSE;
			} else {
				debug = TRUE;
			}
			break;

		case 'e':	/* turn on print-before-erase */
				/* and erase-after-print */
			pr_on_er = TRUE;
			break;

		case 'i':	/* ignore erase */
			ignore_er = TRUE;
			break;

		case 's':	/* set scale option */
			scale_factor = atof(optarg)/100.;
			break;
		default:
			fprintf(stderr, "Usage: %s [-p prologfile] [-de] [input]\n", argv[0]);
			
		}
	}

	/* next, copy the prolog file */
	if( (pro_fp = fopen(pro_fn, "r")) == NULL) {
		fprintf(stderr, "Can't open prolog file %s\n", pro_fn);
		exit(1);
	}
	while ((c = getc(pro_fp)) != EOF) {
		putc((char) c, stdout);
	}
	fclose(pro_fp);

	/* check for named file (loop if more than one) */
	do {	/* should indent here */
	if (optind < argc) {
		if ((infile = fopen( argv[optind], "r")) == NULL) {
			fprintf(stderr, "Can't open input file %s\n", argv[optind]);
			exit(1);
		}
	}
	/* check for scale factor */
	if (scale_factor != 1.0) {	/* I know floating pt equality is
					 * a bad idea, but if the option is not
					 * present I explicity initialize to
					 * 1.0, so the bit pattern should be
					 * identical portably
					 */
		fprintf(stdout,"%f %f scale\n", scale_factor, scale_factor);
	}

	/* Now,  read a byte and figure out what to do about it */
	while ((c = getc(infile)) != EOF) { /* should indent below */
	switch (mode) {
	case ALPHA:
		if ( isgraph((char) c) ) {	/* normal printing char */
			/* put char at current position */
			fprintf(stdout,"(%c) %d %d PR\n", (char) c, cx, cy);
			/* increment current postion, wrt type size */
			if ((cx += tsizex) > XDIM) {
				/* new line or margin */
				if((cy -= tsizey) < 0) {
					cy = YDIM - tsizey;
					leftmargin = leftmargin ? 0 : XDIM/2;
				}
				cx = leftmargin;
			}
		} else { /* isn't normal printing character */
		switch (c) {
		case ( HT ):
		case ( SPACE ):
			if ((cx += tsizex) > XDIM) {
				/* new line or margin */
				if((cy -= tsizey) < 0) {
					cy = YDIM - tsizey;
					leftmargin = leftmargin ? 0 : XDIM/2;
				}
				cx = leftmargin;
			}
			break;

		case ( CR ):
		case ( LF ):
			if ((cy -= tsizey) < 0) {
				cy = YDIM - tsizey;
				leftmargin = leftmargin ? 0 : XDIM/2;
			}
			cx = leftmargin;
			break;

		case ( BS ):
			if ((cx -= tsizex) < 0) {
				cx = XDIM;
			}
			break;

		case ( VT ):
			if ((cy += tsizey) > YDIM) {
				cy = 0;
			}
			break;

		case ( GS ):
			mode = GRAPH;
			dark_vector = TRUE;
			break;

		case ( RS ):
			mode = INCRE;
			break;
		
		case ( FS ):
			mode = PTPLT;
			break;

		case ( US ):	/* put in ALPHA mode, already there */
		case ( BEL ):
		case ( SYN ):	/* padding character, ignore*/
		case ( NUL ):	/* padding character, ignore*/
			break;

		case ( ESC ):
			oldmode = ALPHA;
			mode = LCEMD;
			break;

		default :
			if (debug) fprintf(stderr, "Unknown ALPHA mode character 0x%02x\n", c);
			break;
		}	/* end of switch on non-printing char in ALPHA mode */
		}	/* end of printing vs non-printing char in ALPHA mode */
	break;	/* end of ALPHA mode */

	case PTPLT:
	case GRAPH:
		if ( (char) c > US ) {	/* first, handle vector case */
			if( (char) c < '@') { /* High byte */
				if (gotLoY) {	/* must be HiX */
					HiX = ((char) c & 0x1f) << 7;
				} else {	/* must be HiY */
					HiY = ((char) c & 0x1f) << 7;
				}
			} else if ( (char) c < '`') {	/* Lo X: plot */
				gotLoY = FALSE;
				LoX = (c & 0x1f) << 2;
				/* now actually do a plot */
				if (dark_vector) {
					dark_vector = FALSE;
					cx = HiX + LoX + BX;
					cy = HiY + LoY + BY;
					fprintf(stdout,"%d %d moveto\n",cx, cy);
				} else { /* draw the line */
					cx = HiX + LoX + BX;
					cy = HiY + LoY + BY;
					if (mode == GRAPH)  {
						fprintf(stdout,"%d %d lineto stroke %d %d moveto\n", cx, cy, cx, cy);
					} else {	/* mode == PTPLT */
						fprintf(stdout,"%d %d moveto %d %d 1 0 360 arc\n", cx, cy, cx, cy);
					}
				}
			} else {	/* Lo Y or extra byte */
				if (gotLoY) {	/* previous LoY => extra byte */
					BX = (LoY & 0x0c) >> 2;
					BY = (LoY & 0x30) >> 4;
					LoY = (c & 0x1f) << 2;
					/* gotLoY stays TRUE */
				} else {	/* assume is LoY */
					LoY = (c & 0x1f) << 2;
					gotLoY = TRUE;
				}
			}
		} else /* end of GRAPH mode vector address parsing*/
		/* so, it isn't a vector address */
		switch ( c ) {
		case ( NUL ): /* padding */
		case ( SYN ): /* padding */
		case ( BEL ): /* ignore */
			break;

		case ( LF ):
			cy -= tsizey;
			fprintf(stdout, "%d %d moveto\n", cx, cy);
			break;

		case ( CR ):
			mode = ALPHA;
			leftmargin = 0;
			break;

		case ( RS ):
			mode = INCRE;
			break;

		case ( FS ):
			fprintf(stderr,"special point plot not implemented\n");
			break;

		case ( GS ):
			dark_vector = TRUE;
			break;

		case ( US ):
			mode = ALPHA;
			break;

		case ( ESC ):
			oldmode = GRAPH;
			mode = LCEMD;
			break;

		default :
			if (debug) fprintf(stderr, "Unknown GRAPH mode character 0x%02x\n", c);
			break;
		}	/* end of switch on non-vector char in GRAPH mode */
	break;	/* end of GRAPH mode */

	case INCRE:
		/* could do with bit masking and check for control, */
		/* but this is is simpler. (Let the compiler work). */
		switch ( c ) {
		case ( 32 ):
			fprintf(stdout,"stroke %d %d moveto\n", cx, cy);
			beamon = FALSE;
			break;

		case ( 80 ):
			fprintf(stdout,"%d %d moveto\n", cx, cy);
			beamon = TRUE;
			break;

		case ( 68 ):	/* N */
			if (++cy > YDIM) cy = YDIM;
			if (beamon) fprintf(stdout,"%d %d lineto\n", cx, cy);
			break;
			
		case ( 69 ):	/* NE */
			if (++cy > YDIM) cy = YDIM;
			if (++cx > XDIM) cx = XDIM;
			if (beamon) fprintf(stdout,"%d %d lineto\n", cx, cy);
			break;

		case ( 65 ):	/* E */
			if (++cx > XDIM) cx = XDIM;
			if (beamon) fprintf(stdout,"%d %d lineto\n", cx, cy);
			break;

		case ( 73 ):	/* SE */
			if (--cy < 0) cy = 0;
			if (++cx > XDIM) cx = XDIM;
			if (beamon) fprintf(stdout,"%d %d lineto\n", cx, cy);
			break;

		case ( 72 ):	/* S */
			if (--cy < 0) cy = 0;
			if (beamon) fprintf(stdout,"%d %d lineto\n", cx, cy);
			break;

		case ( 74 ):	/* SW */
			if (--cy < 0) cy = 0;
			if (--cx < 0) cx = 0;
			if (beamon) fprintf(stdout,"%d %d lineto\n", cx, cy);
			break;

		case ( 66 ):	/* W */
			if (--cx < 0) cx = 0;
			if (beamon) fprintf(stdout,"%d %d lineto\n", cx, cy);
			break;

		case ( 70 ):	/* NW */
			if (++cy > YDIM) cy = YDIM;
			if (--cx < 0) cx = 0;
			if (beamon) fprintf(stdout,"%d %d lineto\n", cx, cy);
			break;

		case ( ESC ):
			if (beamon) fprintf(stdout,"stroke %d %d moveto\n", cx, cy);
			oldmode = INCRE;
			mode = LCEMD;
			break;

		case ( FS ):
			if (beamon) fprintf(stdout,"stroke %d %d moveto\n", cx, cy);
			mode = PTPLT;
			break;

		case ( GS ):
			if (beamon) fprintf(stdout,"stroke %d %d moveto\n", cx, cy);
			mode = GRAPH;
			dark_vector = TRUE;
			break;

		case ( RS ):
			break;

		case ( US ):
			if (beamon) fprintf(stdout,"stroke %d %d moveto\n", cx, cy);
			mode = ALPHA;
			break;

		default:
			if (debug) fprintf(stderr,"Unknown incremental mode character 0x%02x\n", c);
			break;
		}	/* end of INCR switch on c */
	break;	/* end of INCREmental mode */

	case LCEMD:
		if ((c > 95) && (c < 117)) {
			/* set Z axis */
			if ((char) c & 0x08) {
				fprintf(stdout,"DZ\n");
			} else {
				fprintf(stdout,"FZ\n");
			}
			/* set vector type */
			switch ((char) c & 0x07) {
			case 0:	/* normal vectors */
				fprintf(stdout,"NV\n");
				break;

			case 1:	/* dotted vectors */
				fprintf(stdout,"DV\n");
				break;

			case 2:	/* dot-dash vectors */
				fprintf(stdout,"DDV\n");
				break;

			case 3:	/* short-dash vectors */
				fprintf(stdout,"SDV\n");
				break;

			case 4:	/* long-dash vectors */
				fprintf(stdout,"LDV\n");
				break;

			default:	/* error */
				if (debug) fprintf(stderr,"Unknown beam selector 0x%02x\n", (char) c);
				break;
			}
		} else {
			switch ( c ) {
			case ( FF ):	/* erase */
				if (pr_on_er) {
					fprintf(stdout,"showpage\nNP\n");
					if (scale_factor != 1.0) {
						fprintf(stdout,"%f %f scale\n", scale_factor, scale_factor);
					}
				}
				
				if (!ignore_er) {
					fprintf(stdout, "%d DP\n", tsizey);
					cx = 0; cy = YDIM - tsizey;
				}
				break;

			case ( '8' ):
				/* default size */
				fprintf(stdout, "FntH setfont\n");
				tsizex = CHUGEX; tsizey = CHUGEY;
				break;

			case ( '9' ):
				if (! used_large ) {
					used_large = TRUE;
					fprintf(stdout,"DFntL\n");
				}
				fprintf(stdout, "FntL setfont\n");
				tsizex = CLARGEX; tsizey = CLARGEY;
				break;

			case ( ':' ):
				tsizex = CMEDX; tsizey = CMEDY;
				if (! used_med ) {
					used_med = TRUE;
					fprintf(stdout,"DFntM\n");
				}
				fprintf(stdout, "FntM setfont\n");
				break;

			case ( ';' ):
				tsizex = CSMALLX; tsizey = CSMALLY;
				if (! used_small ) {
					used_small = TRUE;
					fprintf(stdout,"DFntS\n");
				}
				fprintf(stdout, "FntS setfont\n");
				break;

			case ( BS ):
				if ((cx -= tsizex) < 0) {
					cx = XDIM;
				}
				break;

			case ( HT ):
			case ( SPACE ):
				if ((cx += tsizex) > XDIM) {
					/* new line or margin */
					if((cy -= tsizey) < 0) {
						cy = YDIM - tsizey;
						leftmargin = leftmargin ? 0 : XDIM/2;
					}
					cx = leftmargin;
				}
				break;

			case ( VT ):
				if ((cy += tsizey) > YDIM) {
					cy = 0;
				}
				break;

			case ( GS ):
				mode = GRAPH;
				dark_vector = TRUE;
				break;

			case ( LF ):
			case ( CR ):
			case ( DEL ):
			case ( NUL ):
			case ( ESC ):
			case ( BEL ):
			case ( SYN ):
				ungetc( (char) ESC, infile);
				break;

			case ( ETB ):	/* make copy: print & start new page */
				if (pr_on_er) {
					fprintf(stdout, "showpage\nNP\n");
					if (scale_factor != 1.0) {
						fprintf(stdout,"%f %f scale\n", scale_factor, scale_factor);
					}
					cx = 0; cy = YDIM - tsizey;
				} else {
					fprintf(stdout, "copypage\n");
				}
				break;

			case ( SO ):
			case ( SI ):
				if (debug) fprintf(stderr, "No alternate character set implemented\n");
				break;

			case ( CAN ):
			case ( SUB ):
			case ( ENQ ):
				if (debug) fprintf(stderr, "GIN and BYPASS  modes not implemented\n");
				break;

			case ( '?' ):
				ungetc( (char) DEL, infile);
				break;

			default :
				if (debug) fprintf(stderr, "Unknown LCE mode character 0x%02x ignored\n", c);
				break;
			}	/* end of LCE mode switch */
		}
	mode = oldmode;
	break;

	default:
		if (debug) fprintf(stderr, "Unknown major mode %d\n", mode);
		break;
	} /* end of mode switch */
	} /* end of main input loop */
	fprintf(stdout,"showpage\n");
	} while ( ++optind < argc );
	return 0;
}

