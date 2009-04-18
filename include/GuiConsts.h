#ifndef _GUICONSTS_H
#define _GUICONSTS_H

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

#define  WX_BUTTONS             30   
#define  WX_DEBUG	        1   
#define  WX_TEXTLENGTH          20  
#define  DE_TEXTLENGTH          10
#define  WX_FNLEN               100  
#define  XEDIT                  "xedit"
#define  XEDIT_OPTION           1
#define  EMACS                  "emacs"
#define  EMACS_OPTION           2
#define  AUTO_DIR               "AUTO_DIR"
#define  MAX_NICP               10             /* has to be an even number */
#define  MAX_NUZR               10             /* has to be an even number */
#define  MAX_NTHL               10             /* has to be an even number */
#define  MAX_NTHU               10             /* has to be an even number */
#define  NONE			0
#define  DEFAULT_RFILE          "c.aut"        /* "c.default" */
#define  CURRENT_RFILE		"c.current"
#define  DEFAULT_FILE           "aut.f90"      /* "default.f90" */
#define  DEFAULT_NAME           "aut.f90"
#define  TEXT_WIDTH             72
#define  EMPTY                  ""
#define  MAX_FILESIZE           50000
#define  MIN_FILESIZE           10000
#define  PRINT_FILE		"lp < "

/*  don't modify anything after this line */


#define  MENU_HELP		200
#define  MENU_EXIT		201
#define  MENU_OPEN		202
#define  MENU_NEW		203
#define  MENU_CLOSE		204
#define  MENU_SAVE		205
#define  MENU_SAVE_AS		206
#define  MENU_PRINT		207
#define  MENU_CUT		208
#define  MENU_COPY		209
#define  MENU_PASTE		210
#define  MENU_CLEAR		211
#define  DIALOG_FSELECT		300
#define  DIALOG_CWARNING	301
#define  DIALOG_XWARNING	302
#define  DIALOG_NEW		303
#define  DIALOG_SAVE		304
#define  DIALOG_HELP		305
#define  DIALOG_PRINT		306



#ifdef	__cplusplus
}	/* Close scope of extern "C" */
#endif 

#endif /* _GUICONSTS_H */

/* DON'T ADD ANY STUFF AFTER THIS LINE */
