#ifndef _GUIPAR_H
#define _GUIPAR_H

#ifndef	_NO_PROTO
#if	!(defined(__STDC__) && __STDC__) \
     && !defined(__cplusplus)		\
     && !defined(c_plusplus)		\
     && !defined(FUNCPROTO)		\
     && !defined(XTFUNCPROTO)		\
     && !defined(XMFUNCPROTO)	
#define	_NO_PROTO
#endif	/* __STDC__ */
#endif	/* _NO_PROTO */

#ifdef	__cplusplus
extern	"C"	{	/* Begin scope of extern "C" */
#endif


#include <Xm/Xm.h>


typedef struct _ClientData {
  
 Widget widget;
 int    data;

} ClientData;

/*   Global Parameter   :   GP<name> */

typedef struct _GlobalPar {

Boolean	exit;
Boolean	quit;
Boolean fileIn;
Boolean question;
int	numFileName;
int     activeFile;
int     numParameter;
int     numDemo;  
int     nthl,nthu,nuzr,nicp; 
char    nthlStr[WX_TEXTLENGTH],nthuStr[WX_TEXTLENGTH],nuzrStr[WX_TEXTLENGTH];
double  thlValue[2*MAX_NTHL];
char    thlStrValue[2*MAX_NTHL][WX_TEXTLENGTH];
Boolean thlToggle[MAX_NTHL];
double  thuValue[2*MAX_NTHU];
char    thuStrValue[2*MAX_NTHU][WX_TEXTLENGTH];
Boolean thuToggle[MAX_NTHU];
double  uzrValue[2*MAX_NUZR];
char    uzrStrValue[2*MAX_NUZR][WX_TEXTLENGTH];
Boolean uzrToggle[MAX_NUZR];
double  icpValue[20*MAX_NICP];
char    icpStrValue[20*MAX_NICP][WX_TEXTLENGTH];
Boolean icpToggle[MAX_NICP];
double  *parValue;
char    **parStrValue;
XmString *parXmLabel;
XmString *demoXmItems;
char    fileDir[WX_FNLEN];
char    filePattern[WX_FNLEN];
char    fileName[WX_FNLEN];
char    demoFileName[WX_FNLEN];
Boolean file_saved;	
char	xedit[WX_FNLEN];
char	emacs[WX_FNLEN];
char    command[WX_FNLEN];
int     autoPid;
int     restart;
int     nicpFlag;
char    q[10];
char    autoDir[50];

} GlobalPar;

extern GlobalPar GP;

#ifdef	__cplusplus
}	/* Close scope of extern "C" */
#endif 

#endif /* _GUIPAR_H */

/* DON'T ADD ANY STUFF AFTER THIS LINE */
