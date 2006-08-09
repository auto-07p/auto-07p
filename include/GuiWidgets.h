#ifndef _GUIWIDGETS_H
#define _GUIWIDGETS_H

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


/*   Global Widget   :   GW<widgetName> */

typedef struct _GlobalWidget {

  Widget help;                /* message box widget */
  Widget helpList;
  Widget demoList;
  Widget demo_list;
  Widget textDALG;
  Widget textField;
  Widget *parButton;
  Widget *probButton;
  Widget *disButton;
  Widget *tolButton;
  Widget *stepButton;
  Widget *limButton;
  Widget *conButton;
  Widget *runButton;
  Widget *outButton;

  Widget text;			/* multi-line text widget		*/
  Widget cut_button;		/* clipboard cut button 		*/
  Widget copy_button;		/* clipboard copy button 		*/
  Widget paste_button;		/* clipboard paste button 		*/
  Widget clear_button;		/* clipboard clear button 		*/
  Widget open_dialog;		/* file selection dialog 		*/
  Widget new_dialog;		/* file name prompt dialog 		*/
  Widget close_warning;		/* special internal selection dialog	*/
  Widget exit_warning;		/* special internal selection dialog	*/
  Widget save_dialog;		/* save as prompt dialog	 	*/
  Widget print_warning;		/* warning dialog		 	*/

  Widget mainWindow;

  Widget thlButton[2*MAX_NTHL];
  Widget thlTg[MAX_NTHL];
  Widget thuButton[2*MAX_NTHU];
  Widget thuTg[MAX_NTHU];
  Widget uzrButton[2*MAX_NUZR];
  Widget uzrTg[MAX_NUZR];
  Widget icpButton[2*MAX_NICP];
  Widget icpTg[MAX_NICP];
  
  Widget popThl,popThu,popIcp,popUzr;

  Widget copyFrom,copyTo;
  Widget moveFrom,moveTo;
  Widget appendFrom,appendTo;
  

} GlobalWidget;

extern GlobalWidget GW;


#ifdef	__cplusplus
}	/* Close scope of extern "C" */
#endif 

#endif /* _GUIWIDGETS_H */

/* DON'T ADD ANY STUFF AFTER THIS LINE */
