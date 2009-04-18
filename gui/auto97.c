#include "GuiMotif.h"
#include "GuiConsts.h"
#include "GuiWidgets.h"
#include "GuiPar.h"
#include "GuiFuncs.h"
#include "GuiGlobal.h"


/*****************************************************************/
#if 0                /* begin of all headers */

#ifndef	_GUIMOTIF_H
#define	_GUIMOTIF_H


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

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <ctype.h>
#include <assert.h>
#include <time.h>
#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <X11/Intrinsic.h>
#include <X11/Xatom.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/CutPaste.h>
#include <Xm/DialogS.h>
#include <Xm/FileSB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/MainW.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/SeparatoG.h>
#include <Xm/Separator.h>
#include <Xm/SelectioB.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleBG.h>

#ifdef	__cplusplus
}	/* Close scope of extern "C" */
#endif 

#endif /* _GUIMOTIF_H */

/***	DON'T ADD ANYTHING AFTER THIS #endif	***/

/************************************************************************/

#ifndef _GUIDEFS_H
#define _GUIDEFS_H

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
#define  DEFAULT_RFILE          "c.aut"
#define  CURRENT_RFILE		"c.current"
#define  DEFAULT_FILE           "aut.f90"
#define  DEFAULT_NAME           "aut.f90"
#define  TEXT_WIDTH             60
#define  EMPTY                  ""
#define  MAX_FILESIZE           20000
#define  PRINT_FILE		"lp < "
/*
typedef struct _Vector {

int    i;
double value;

} Vector;
*/

typedef struct _ClientData {
  
 Widget widget;
 int    data;

} ClientData;


/*   Global Widget   :   GW<widgetName> */

typedef struct _GlobalWidget {

  Widget help;                /* message box widget */
  Widget helpList;
  Widget demoList;
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
  Widget thuButton[2*MAX_NTHU];
  Widget uzrButton[2*MAX_NUZR];
  Widget icpButton[2*MAX_NICP];
  
  Widget popThl,popThu,popIcp,popUzr;

  Widget copyFrom,copyTo;
  Widget moveFrom,moveTo;
  Widget appendFrom,appendTo;
  

} GlobalWidget;

extern GlobalWidget GW;

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
double  thuValue[2*MAX_NTHU];
char    thuStrValue[2*MAX_NTHU][WX_TEXTLENGTH];
double  uzrValue[2*MAX_NUZR];
char    uzrStrValue[2*MAX_NUZR][WX_TEXTLENGTH];
double  icpValue[2*MAX_NICP];
char    icpStrValue[2*MAX_NICP][WX_TEXTLENGTH];
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

} GlobalPar;

extern GlobalPar GP;

#ifdef	__cplusplus
}	/* Close scope of extern "C" */
#endif 

#endif /* _GUIDEFS_H */

/* DON'T ADD ANY STUFF AFTER THIS LINE */

/************************************************************************/

#ifndef	_GUIFUNCS_H
#define	_GUIFUNCS_H

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

/*
#include "WxDefs.h"
*/

#ifdef	_NO_PROTO

extern Widget AppendData();
extern void   AppendDataCB();
extern void   clallCB();
extern void   CleanCB();
extern void   CloseFile();
extern void   clselCB();
extern void   ConfigInit();
extern char   *copyenv();
extern void   CopyFileToClipboard();
extern Widget CopyData();
extern void   CopyDataCB();
extern int    countAny();
extern void   CreateActionMenu();
extern void   CreateAppendMenu();
extern void   CreateBox();
extern void   CreateBoxCon();
extern void   CreateBoxDis();
extern void   CreateBoxLim();
extern void   CreateBoxOut();
extern void   CreateBoxProb();
extern void   CreateBoxRun();
extern void   CreateBoxStep();
extern void   CreateBoxTol();
extern void   CreateBrowseRfileMenu();
extern void   CreateFilesMenu();
extern XImage *CreateDefaultImage();
extern void   CreateDefineMenu();
extern void   CreateDemoMenu();
extern void   CreateEditMenu(); 
extern void   CreateFileMenu(); 
extern void   CreateFileMenuDef(); 
extern Widget CreateFileSelectionDialog();
extern Widget CreateFormCon();
extern Widget CreateFormDefault();
extern Widget CreateFormDis();
extern Widget CreateFormLim();
extern Widget CreateFormOut();
extern Widget CreateFormProb();
extern Widget CreateFormRun();
extern Widget CreateFormStep();
extern Widget CreateFormTol();
extern void   CreateHelpMenu();
extern Widget CreateLayout();
extern void   CreateLoadRfileMenu(); 
extern void   CreateMenuBar();
extern void   CreateMiscMenu();
extern Widget CreateNewFileDialog();
extern void   CreatePlotMenu();
extern void   CreateRunMenu();
extern Widget CreateQuestionDialog();
extern void   CreateSaveMenu();
extern Widget CreateScrolledDemoList();
extern Widget CreateScrolledHelpList();
extern void   CreateSettingMenu();
extern Widget CreateSpWarningDialog();
extern void   CreateText();
extern void   CreateWriteMenu();
extern void   DefDriverCB();
extern void   DeleteCB();
extern void   DeletePrimarySelection();
extern void   DemoRunCB();
extern void   DemoBrowseCB();
extern void   DemoHelpCB();
extern void   DialogAcceptCB();
extern void   DialogApplyCB();
extern void   DialogCancelCB();
extern void   EntCancelCB();
extern void   EntOkCB();
extern void   ExitCB();
extern void   FileChangedCB();
extern int    GetAutoPid();
extern char   *GetDirName();
extern void   GetJobName();
extern char   **getstr();
extern void   HelpCB();
extern void   ManualCB();
extern void   MenuCB();
extern Widget MoveData();
extern void   MoveDataCB();
extern void   NewOutputCB();
extern Boolean noting();
extern void   OkFormConCB();
extern void   OkFormDefCB();
extern void   OkFormDisCB();
extern void   OkFormLimCB();
extern void   OkFormOutCB();
extern void   OkFormProbCB();
extern void   OkFormRunCB();
extern void   OkFormStepCB();
extern void   OkFormTolCB();
extern Boolean OpenFile();
extern void   OutputCB();
extern void   PasteItemFromClipboard();
extern void   PopdownCB();
extern void   PopupCB();
extern void   PopupCB1();
extern void   PopupCon();
extern void   PopupDis();
extern Widget PopupIcp();
extern Widget PopupIid();
extern Widget PopupIlp();
extern Widget PopupIps();
extern Widget PopupIsp();
extern Widget PopupIsw();
extern Widget PopupJac();
extern void   PopupLim();
extern Widget PopupNcol();
extern void   PopupOut();
extern void   PopupProb();
extern void   PopupRun();
extern void   PopupStep();
extern void   PopupTemplate();
extern void   PopupTemplate1();
extern void   PopupTemplate2();
extern void   PopupTemplate3();
extern void   PopupTemplate4();
extern Widget PopupThl();
extern Widget PopupThu();
extern void   PopupTol();
extern Widget PopupUzr();
extern void   PostItCB();
extern void   PostItCB1();
extern void   PrintCB();
extern char*  ProgramName();
extern void   ReadDefCB();
extern Widget ReadFile();
extern void   ReadFileCB();
extern int    ReadRfile();
extern void   RestartCB();
extern void   RunAllDemoCB();
extern void   RunCB();
extern void   RunDemo();
extern Boolean SaveFile();
extern void   SaveParScreen();
extern void   SelOkCB();
extern void   SetParCB();
extern void   SetParScreen();
extern void   StopCB();
extern void   TekCB();
extern void   TmpCB();
extern void   UpdateTime();
extern void   VtCB();
extern void   Wprintf();
extern void   WriteRfile();

#else

extern Widget AppendData(Widget);
extern void   AppendDataCB(Widget,XtPointer,XtPointer);
extern void   clallCB(Widget,XtPointer,XtPointer);
extern void   CleanCB(Widget,XtPointer,XtPointer);
extern void   CloseFile();
extern void   clselCB(Widget,XtPointer,XtPointer);
extern void   ConfigInit(Widget);
extern char   *copyenv(char *);
extern void   CopyFileToClipboard(Time);
extern Widget CopyData(Widget);
extern void   CopyDataCB(Widget,XtPointer,XtPointer);
extern int    countAny(int);
extern void   CreateActionMenu(Widget);
extern void   CreateAppendMenu(Widget);
extern void   CreateBrowseRfileMenu(Widget);
extern void   CreateBox(Widget);
extern void   CreateBoxCon(Widget);
extern void   CreateBoxDis(Widget);
extern void   CreateBoxLim(Widget);
extern void   CreateBoxOut(Widget);
extern void   CreateBoxProb(Widget);
extern void   CreateBoxRun(Widget);
extern void   CreateBoxStep(Widget);
extern void   CreateBoxTol(Widget);
extern void   CreateFilesMenu(Widget);
extern XImage *CreateDefaultImage(char*, int, int);
extern void   CreateDefineMenu(Widget);
extern void   CreateDemoMenu(Widget);
extern void   CreateEditMenu(Widget); 
extern void   CreateFileMenu(Widget); 
extern void   CreateFileMenuDef(Widget); 
extern Widget CreateFileSelectionDialog(Widget,int);
extern Widget CreateFormCon(Widget);
extern Widget CreateFormDefault(Widget);
extern Widget CreateFormDis(Widget);
extern Widget CreateFormLim(Widget);
extern Widget CreateFormOut(Widget);
extern Widget CreateFormProb(Widget);
extern Widget CreateFormRun(Widget);
extern Widget CreateFormStep(Widget);
extern Widget CreateFormTol(Widget);
extern void   CreateHelpMenu(Widget);
extern Widget CreateLayout(Widget);
extern void   CreateLoadRfileMenu(Widget); 
extern void   CreateMenuBar(Widget);
extern void   CreateMiscMenu(Widget);
extern Widget CreateNewFileDialog(Widget,int);
extern void   CreatePlotMenu(Widget);
extern void   CreateRunMenu(Widget);
extern Widget CreateQuestionDialog(Widget);
extern void   CreateSaveMenu(Widget);
extern Widget CreateScrolledDemoList(Widget);
extern Widget CreateScrolledHelpList(Widget);
extern void   CreateSettingMenu(Widget);
extern Widget CreateSpWarningDialog(Widget,String,String,String,ArgList,int);
extern void   CreateText(Widget);
extern void   CreateWriteMenu(Widget);
extern void   DefDriverCB(Widget,XtPointer,XtPointer);
extern void   DeleteCB(Widget,XtPointer,XtPointer);
extern void   DeletePrimarySelection(Time);
extern void   DemoRunCB(Widget,XtPointer,XtPointer);
extern void   DemoBrowseCB(Widget,XtPointer,XtPointer);
extern void   DemoHelpCB(Widget,XtPointer,XtPointer);
extern void   DialogAcceptCB(Widget,XtPointer,XtPointer);
extern void   DialogApplyCB(Widget,XtPointer,XtPointer);
extern void   DialogCancelCB(Widget,XtPointer,XtPointer);
extern void   EntCancelCB(Widget,XtPointer,XtPointer);
extern void   EntOkCB(Widget,XtPointer,XtPointer);
extern void   ExitCB(Widget,XtPointer,XtPointer);
extern void   FileChangedCB(Widget,XtPointer,XtPointer);
extern int    GetAutoPid(char *);
extern char   *GetDirName();
extern void   GetJobName(char *,char *);
extern char   **getstr(int,int);
extern void   HelpCB(Widget ,XtPointer, XtPointer);
extern void   ManualCB(Widget,XtPointer,XtPointer);
extern void   MenuCB(Widget,XtPointer,XtPointer);
extern Widget MoveData(Widget);
extern void   MoveDataCB(Widget,XtPointer,XtPointer);
extern void   NewOutputCB(Widget,XtPointer,XtPointer);
extern Boolean noting(char *);
extern void   OkFormConCB(Widget,XtPointer,XtPointer);
extern void   OkFormDefCB(Widget,XtPointer,XtPointer);
extern void   OkFormDisCB(Widget,XtPointer,XtPointer);
extern void   OkFormLimCB(Widget,XtPointer,XtPointer);
extern void   OkFormOutCB(Widget,XtPointer,XtPointer);
extern void   OkFormProbCB(Widget,XtPointer,XtPointer);
extern void   OkFormRunCB(Widget,XtPointer,XtPointer);
extern void   OkFormStepCB(Widget,XtPointer,XtPointer);
extern void   OkFormTolCB(Widget,XtPointer,XtPointer);
extern Boolean OpenFile(char *);
extern void   OutputCB(Widget,XtPointer,XtPointer);
extern void   PasteItemFromClipboard();
extern void   PopdownCB(Widget,XtPointer,XtPointer);
extern void   PopupCB(Widget,XtPointer,XtPointer);
extern void   PopupCB1(Widget,XtPointer,XtPointer);
extern void   PopupCon(Widget);
extern void   PopupDis(Widget);
extern Widget PopupIcp(Widget);
extern Widget PopupIid(Widget);
extern Widget PopupIlp(Widget);
extern Widget PopupIps(Widget);
extern Widget PopupIsp(Widget);
extern Widget PopupIsw(Widget);
extern Widget PopupJac(Widget);
extern void   PopupLim(Widget);
extern Widget PopupNcol(Widget);
extern void   PopupOut(Widget);
extern void   PopupProb(Widget);
extern void   PopupRun(Widget);
extern void   PopupStep(Widget);
extern void   PopupTemplate(Widget,Widget*,Widget*,Widget*,
                            Widget*,Widget*,int);
extern void   PopupTemplate1(Widget,Widget*,Widget*,Widget*,Widget*,Widget*);
extern void   PopupTemplate2(Widget,Widget*);
extern void   PopupTemplate3(Widget,Widget*,Widget*,Widget*,Widget*,Widget*);
extern void   PopupTemplate4(Widget,Widget*,Widget*,Widget*);
extern Widget PopupThl(Widget);
extern Widget PopupThu(Widget);
extern void   PopupTol(Widget);
extern Widget PopupUzr(Widget);
extern void   PostItCB(Widget,XtPointer,XEvent *,Boolean *);
extern void   PostItCB1(Widget,XtPointer,XEvent *,Boolean *);
extern void   PrintCB(Widget,XtPointer,XtPointer);
extern char*  ProgramName(char *);
extern void   ReadDefCB(Widget,XtPointer,XtPointer);
extern Widget ReadFile(Widget);
extern void   ReadFileCB(Widget,XtPointer,XtPointer);
extern int    ReadRfile(char *);
extern void   RestartCB(Widget,XtPointer,XtPointer);
extern void   RunAllDemoCB(Widget,XtPointer,XtPointer);
extern void   RunCB(Widget,XtPointer,XtPointer);
extern void   RunDemo(char *);
extern Boolean SaveFile();
extern void   SaveParScreen(int);
extern void   SelOkCB(Widget,XtPointer,XtPointer);
extern void   SetParCB(Widget w,XtPointer,XtPointer);
extern void   SetParScreen();
extern void   StopCB(Widget,XtPointer,XtPointer);
extern void   TekCB(Widget,XtPointer,XtPointer);
extern void   TmpCB(Widget,XtPointer,XtPointer);
extern void   UpdateTime(XtPointer, XtIntervalId *);
extern void   VtCB(Widget,XtPointer,XtPointer);
#if __STDC__
extern void   Wprintf(Widget,...);
#else
extern void   Wprintf(...);
#endif
extern void   WriteRfile(char *);

#endif	/* _NO_PROTO */

#ifdef	__cplusplus
}	/* Close scope of extern "C" */
#endif 

#endif  /* _GUIFUNCS_H */

/***	DON'T ADD ANYTHING AFTER THIS #endif	***/


/************************************************************************/

#endif         /*  end of all headers */

/*
***********************************************************************
**
**			Main.c
**                           
************************************************************************
*/

#ifdef _NO_PROTO
main(argc,argv)
int	argc;
char	*argv[];
#else
main(int argc,char **argv)
#endif

{
  XtAppContext	applContext;
  Widget	topLevel,mainWindow;



/*  topLevel = XtVaAppInitialize(&applContext,"XAuto",NULL,0,
			       &argc,argv,NULL,NULL);
*/

  topLevel = XtInitialize(argv[0],"Auto",NULL,0,&argc,argv);

  mainWindow = XmCreateMainWindow(topLevel,"MainWindow",NULL,0);
  XtVaSetValues(mainWindow,XmNshadowThickness,0,NULL);
  XtManageChild(mainWindow);
  ConfigInit(mainWindow);
  CreateMenuBar(mainWindow);
  GW.mainWindow=CreateLayout(mainWindow);
  CreateText(GW.mainWindow);
  XtManageChild(GW.text);
  XmAddTabGroup(GW.text);
  XtSetSensitive(GW.text, False);
  XtRealizeWidget(topLevel);
/*
  XtAppMainLoop(applContext);
*/
  XtMainLoop();
}



#ifdef _NO_PROTO
Widget  CreateLayout(parent)
Widget		parent;
#else
Widget  CreateLayout(Widget parent)
#endif

{
  Widget rtrn,form,rwcl,line,frame,frame1,button,pane,wgt,line1; 
  Widget form2,frame2,rwcl2,clock;

  form = XmCreateForm(parent,"Form",NULL,0);
  XtManageChild(form);


  pane = XmCreateRowColumn(form,"RowColumn",NULL,0);
  XtManageChild(pane);

  XtVaSetValues(pane,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_NONE,
		XmNbottomOffset,10,
/*
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,4,
*/
		NULL);



  form2=XmCreateForm(pane,"fm",NULL,0);
  XtManageChild(form2);
  clock = XmCreateLabel(form2,"Face",NULL,0);
  XtManageChild(clock);
  XtVaSetValues(clock,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_NONE,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,5,
		XmNbottomOffset,5, 
		XmNleftOffset,15,
		XmNrightOffset,10,
		NULL);
  UpdateTime(clock,NULL);


  wgt = XmCreateForm(pane,"Form",NULL,0);
  XtManageChild(wgt);

  frame = XmCreateFrame(wgt,"Frame",NULL,0);
  XtManageChild(frame);
  XtVaSetValues(frame,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_NONE,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,5, 
/*		XmNbottomOffset,5,*/
		XmNleftOffset,10,
		XmNrightOffset,4,
		NULL);

  rwcl  = XmCreateRowColumn(frame,"RowColumn",NULL,0);
  XtManageChild(rwcl);

  button = XmCreateLabel(rwcl,"DefineConsts",NULL,0);
  XtManageChild(button);
  line   = XmCreateSeparator(rwcl,"Separator",NULL,0);
  XtManageChild(line);

/*
  line1 = CreateFormDefault(parent);
*/
  button = XmCreatePushButton(rwcl,"Problem",NULL,0);
  XtManageChild(button);
/*
  XtVaSetValues(button,XmNmnemonic,'P',
		XmNtranslations,XtParseTranslationTable(transTable),
		XmNaccelerator,"Shift<Key>P:",NULL);
*/
  line = CreateFormProb(parent);  
  XtAddCallback(button,XmNactivateCallback,PopupCB,line);
  PopupProb(button);


  button = XmCreatePushButton(rwcl,"Discretize",NULL,0);
  XtManageChild(button);
/*
  XtVaSetValues(button,XmNmnemonic,'D',
		XmNtranslations,XtParseTranslationTable(transTable),
		XmNaccelerator,"Shift<Key>D:",NULL);
*/
  line = CreateFormDis(parent);
  XtAddCallback(button,XmNactivateCallback,PopupCB,line);
  PopupDis(button);

  button = XmCreatePushButton(rwcl,"Tolerances",NULL,0);
  XtManageChild(button);
/*
  XtVaSetValues(button,XmNmnemonic,'T',
		XmNtranslations,XtParseTranslationTable(transTable),
		XmNaccelerator,"Shift<Key>T:",NULL);
*/
  line = CreateFormTol(parent);
  XtAddCallback(button,XmNactivateCallback,PopupCB,line);
  PopupTol(button);

  button = XmCreatePushButton(rwcl,"Step Size",NULL,0);
  XtManageChild(button);
/*
  XtVaSetValues(button,XmNmnemonic,'S',
		XmNtranslations,XtParseTranslationTable(transTable),
		XmNaccelerator,"Shift<Key>S:",NULL);
*/
  line = CreateFormStep(parent);
  XtAddCallback(button,XmNactivateCallback,PopupCB,line);
  PopupStep(button);

  button = XmCreatePushButton(rwcl,"Limits",NULL,0);
  XtManageChild(button);
/*
  XtVaSetValues(button,XmNmnemonic,'L',
		XmNtranslations,XtParseTranslationTable(transTable),
		XmNaccelerator,"Shift<Key>L:",NULL);
*/
  line = CreateFormLim(parent);
  XtAddCallback(button,XmNactivateCallback,PopupCB,line);
  PopupLim(button);

  button = XmCreatePushButton(rwcl,"Parameters",NULL,0);
  XtManageChild(button);
/*
  XtVaSetValues(button,XmNmnemonic,'C',
		XmNtranslations,XtParseTranslationTable(transTable),
		XmNaccelerator,"Shift<Key>C:",NULL);
*/
  line = CreateFormCon(parent);
  XtAddCallback(button,XmNactivateCallback,PopupCB,line);
  PopupCon(button);

  button = XmCreatePushButton(rwcl,"Computation",NULL,0);
  XtManageChild(button);
/*
  XtVaSetValues(button,XmNmnemonic,'R',
		XmNtranslations,XtParseTranslationTable(transTable),
		XmNaccelerator,"Shift<Key>R:",NULL);
*/
  line = CreateFormRun(parent);
  XtAddCallback(button,XmNactivateCallback,PopupCB,line);
  PopupRun(button);

  button = XmCreatePushButton(rwcl,"Output",NULL,0);
  XtManageChild(button);
/*
  XtVaSetValues(button,XmNmnemonic,'O',
		XmNtranslations,XtParseTranslationTable(transTable),
		XmNaccelerator,"Shift<Key>O:",NULL);
*/
  line = CreateFormOut(parent);
  XtAddCallback(button,XmNactivateCallback,PopupCB,line);
  PopupOut(button);


/*
  XtVaSetValues(button,XmNmnemonic,'A',
		XmNtranslations,XtParseTranslationTable(transTable),
		XmNaccelerator,"Shift<Key>A:",NULL);
  line1 = CreateFormDefault(parent);
*/


  frame1 = XmCreateFrame(wgt,"Frame",NULL,0);
  XtManageChild(frame1);
  XtVaSetValues(frame1,
		XmNtopAttachment,XmATTACH_WIDGET,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
                XmNtopWidget,frame,
		XmNtopOffset,10,
/*		XmNbottomOffset,10, */
		XmNleftOffset,10,
		XmNrightOffset,4,
		NULL);

  rwcl = XmCreateRowColumn(frame1,"RowColumn",NULL,0);
  XtManageChild(rwcl);

/*

  button = XmCreatePushButton(rwcl,"Run",NULL,0);
  XtManageChild(button);
  XtAddCallback(button,XmNactivateCallback,RunCB,1);

  button = XmCreatePushButton(rwcl,"Reset",NULL,0);
  XtManageChild(button);
  XtAddCallback(button,XmNactivateCallback,RunCB,2);

  button = XmCreatePushButton(rwcl,"Exit",NULL,0);
  XtManageChild(button); 
  wgt = CreateQuestionDialog(parent);
  XtAddCallback(button,XmNactivateCallback,PopupCB,wgt);

*/

  button = XmCreateLabel(rwcl,"LoadConsts",NULL,0);
  XtManageChild(button);

  button = XmCreateSeparator(rwcl,"Separator",NULL,0);
  XtManageChild(button);

  line1 = ReadFile(parent);
  button = XmCreatePushButton(rwcl,"Previous",NULL,0);
  XtManageChild(button);
  XtAddCallback(button,XmNactivateCallback,PopupCB,line1);    


  button = XmCreatePushButton(rwcl,"Default",NULL,0);
  XtManageChild(button);
  XtAddCallback(button,XmNactivateCallback,ReadDefCB,NULL);


  form2 = XmCreateForm(pane,"Fm",NULL,0);
  XtManageChild(form2);
  frame2=XmCreateFrame(form2,"Fr",NULL,0);
  XtManageChild(frame2);  
  XtVaSetValues(frame2,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_NONE,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,7,     
		XmNbottomOffset,5,
		XmNleftOffset,10,
		XmNrightOffset,4,
		NULL);
  rwcl2=XmCreateRowColumn(frame2,"rm",NULL,0);
  XtManageChild(rwcl2);
  XtVaSetValues(rwcl2,XmNorientation,XmHORIZONTAL,
		XmNpacking,XmPACK_COLUMN,
		XmNnumColumns,1,NULL);
/*
  button = XmCreatePushButton(rwcl2,"Run",NULL,0);
  XtManageChild(button);
  XtAddCallback(button,XmNactivateCallback,RunCB,1);

  button = XmCreatePushButton(rwcl2,"Reset",NULL,0);
  XtManageChild(button);
  XtAddCallback(button,XmNactivateCallback,RunCB,2);
*/
  button = XmCreatePushButton(rwcl2,"Stop ",NULL,0);
  XtManageChild(button);
  XtAddCallback(button,XmNactivateCallback,StopCB,NULL);
  button = XmCreatePushButton(rwcl2,"Exit ",NULL,0);
  XtManageChild(button);
  wgt = CreateQuestionDialog(parent);
  XtAddCallback(button,XmNactivateCallback,PopupCB,wgt);


  line   = XmCreateSeparator(form,"Separator",NULL,0);
  XtManageChild(line);
  XtVaSetValues(line,XmNorientation,XmVERTICAL,NULL);

  XtVaSetValues(line,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_WIDGET,
		XmNrightAttachment,XmATTACH_NONE,
                XmNleftWidget,pane,
		XmNleftOffset,1,
		XmNrightOffset,1,
		NULL);

  rwcl = XmCreateForm(form,"RowColumn",NULL,0);


  XtManageChild(rwcl);

  XtVaSetValues(rwcl,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_WIDGET,
		XmNrightAttachment,XmATTACH_FORM,
                XmNleftWidget,line,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,4,
		XmNrightOffset,10,
		NULL);

  frame = XmCreateFrame(rwcl,"Frame",NULL,0);
  XtManageChild(frame);
  XtVaSetValues(frame,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,5,
		XmNbottomOffset,5,
		XmNleftOffset,5,
		XmNrightOffset,5,
		NULL);

  rtrn = XmCreateMainWindow(frame,"Form",NULL,0);

  XtManageChild(rtrn);


  return rtrn;
}

#ifdef _NO_PROTO
void CreateText(parent)
Widget		parent;
#else
void CreateText(Widget parent)
#endif

/* cdromd -a; eject; */

{
  Arg		al[10];		/*  arg list		*/
  register int	ac;		/*  arg count		*/
  register int	i;		/*  counter		*/
  XFontStruct * newfont;
  Widget v,h;

  /* create text widget */
	
  ac = 0;
  XtSetArg (al[ac], XmNrows, 24);  ac++;
  /* XtSetArg (al[ac], XmNcolumns, 80);  ac++;   */
  XtSetArg (al[ac], XmNcolumns, TEXT_WIDTH);  ac++;
  XtSetArg (al[ac], XmNresizeWidth, False);  ac++;
  XtSetArg (al[ac], XmNresizeHeight, False);  ac++;
  XtSetArg (al[ac], XmNscrollVertical, True);  ac++;
  XtSetArg (al[ac], XmNscrollHorizontal, True);  ac++;
  XtSetArg (al[ac], XmNeditMode, XmMULTI_LINE_EDIT);  ac++;
  XtSetArg (al[ac], XmNscrollingPolicy, XmAUTOMATIC);  ac++;
  XtSetArg (al[ac], XmNblinkRate,0);  ac++;
  
  GW.text = XmCreateScrolledText (parent, "text", al, ac);

/*
  XtVaSetValues(GW.text,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);


  XtVaGetValues(GW.text,XmNverticalScrollBar,&v,
		        XmNhorizontalScrollBar,&h,NULL);
  XtVaSetValues(v,XmNwidth,10,NULL);
  XtVaSetValues(h,XmNwidth,10,NULL);
*/
  /* add value changed callback */
	
  XtAddCallback (GW.text, XmNmodifyVerifyCallback, FileChangedCB, NULL);

}



/*
***********************************************************************
**
**			ConfigInit.c
**                           
************************************************************************
*/

#ifdef _NO_PROTO
void	ConfigInit(parent)
Widget	parent;
#else
void	ConfigInit(Widget parent)
#endif

{

  int i,k1,k2;
  Widget wgt,txtField;
  char demo[10];

  printf("\n");

  strcpy(GP.autoDir,copyenv(AUTO_DIR));
  GP.nicpFlag = 0;
  GP.restart = 0;
  GP.file_saved=True;
  GP.autoPid = -1;
  strcpy(GP.fileName,EMPTY);
  strcpy(GP.demoFileName,EMPTY);
  strcpy(GP.xedit,XEDIT);
  strcpy(GP.emacs,EMACS);

  GP.numDemo = sizeof(demoItems) / sizeof(demoItems[0]);  
  GP.numParameter = sizeof(parLabel) / sizeof(parLabel[0]);  
  GW.parButton = (Widget*) malloc(GP.numParameter*sizeof(Widget));
  GW.probButton = (Widget*) malloc(4*sizeof(Widget));
  GW.disButton = (Widget*) malloc(3*sizeof(Widget));
  GW.tolButton = (Widget*) malloc(6*sizeof(Widget));
  GW.stepButton = (Widget*) malloc(6*sizeof(Widget));
  GW.limButton = (Widget*) malloc(5*sizeof(Widget));
  GW.conButton = (Widget*) malloc(2*sizeof(Widget));
  GW.runButton = (Widget*) malloc(6*sizeof(Widget));
  GW.outButton = (Widget*) malloc(4*sizeof(Widget));
  GP.parValue = (double*) malloc(GP.numParameter*sizeof(double));
  GP.parXmLabel = (XmString*) malloc(GP.numParameter*sizeof(XmString));
  GP.demoXmItems = (XmString*) malloc(GP.numDemo*sizeof(XmString));
  GP.parStrValue = getstr(GP.numParameter,WX_TEXTLENGTH);

  GP.nuzr = 0;
  GP.nthl = 0;
  GP.nthu = 0;

  for(i = 0; i < GP.numParameter; i++)	{
    GP.parValue[i] = NONE;
    GP.parXmLabel[i] = XmStringCreateSimple(parLabel[i]);
    strcpy(GP.parStrValue[i],"");
  }

/*
  sprintf(GP.parStrValue[PAR_NUZR],"%d",GP.nuzr);    
  sprintf(GP.parStrValue[PAR_NTHL],"%d",GP.nthl);    
  sprintf(GP.parStrValue[PAR_NTHU],"%d",GP.nthu);    
*/

  for(i = 0; i < MAX_NTHL; i++){
    k1=2*i;
    k2=k1+1;
    GP.thlToggle[i] = False;
    GP.thlValue[k1]=NONE;
    GP.thlValue[k2]=NONE;
    strcpy(GP.thlStrValue[k1],"");
    strcpy(GP.thlStrValue[k2],"");
  }

  for(i = 0; i < MAX_NTHU; i++){
    k1=2*i;
    k2=k1+1;
    GP.thuToggle[i] = False;
    GP.thuValue[k1]=NONE;
    GP.thuValue[k2]=NONE;
    strcpy(GP.thuStrValue[k1],"");
    strcpy(GP.thuStrValue[k2],"");
  }

  for(i = 0; i < MAX_NUZR; i++){
    k1=2*i;
    k2=k1+1;
    GP.uzrToggle[i] = False;
    GP.uzrValue[k1]=NONE;
    GP.uzrValue[k2]=NONE;
    strcpy(GP.uzrStrValue[k1],"");
    strcpy(GP.uzrStrValue[k2],"");
  }

  for(i = 0; i < MAX_NICP; i++){
    GP.icpToggle[i] = False;
    GP.icpValue[i]=NONE;
    strcpy(GP.icpStrValue[i],"");
/*
    k1=2*i;
    k2=k1+1;
    GP.icpValue[k1]=NONE;
    GP.icpValue[k2]=NONE;
    strcpy(GP.icpStrValue[k1],"");
    strcpy(GP.icpStrValue[k2],"");
*/
  }


  for(i = 0; i < GP.numDemo; i++)  {

    k1 = strlen(demoItems[i]);
    strncpy(demo,demoItems[i],k1-4);
    demo[k1-4] = '\0';

    GP.demoXmItems[i] = XmStringCreateLtoR(demo,XmSTRING_DEFAULT_CHARSET);
/*
    GP.demoXmItems[i] = XmStringCreateLtoR(demoItems[i],XmSTRING_DEFAULT_CHARSET);
*/
  }

  GW.demoList = CreateScrolledDemoList(parent);
  GW.helpList = CreateScrolledHelpList(parent);
  GW.demo_list = CreateDemoList(parent);

  GW.textDALG = XmCreateFormDialog(parent,"Message",NULL,0);

  PopupTemplate2(GW.textDALG,&txtField);  

  GW.textField = XmCreateScrolledText(txtField,"Browser",NULL,0);
  XtManageChild(GW.textField);
  XtVaSetValues(GW.textField,
		XmNblinkRate,0,
		XmNeditable,False,
		XmNeditMode,XmMULTI_LINE_EDIT,
		XmNrows,24,
		XmNcolumns,TEXT_WIDTH,
                XmNresizeWidth, False,
		XmNresizeHeight, False,
		XmNscrollVertical, True,
		XmNscrollHorizontal, True,
		XmNscrollingPolicy, XmAUTOMATIC,
		NULL);   

}

/*
***********************************************************************
**
**			CreateMenuBar.c
**                           
************************************************************************
*/

#ifdef _NO_PROTO
void	CreateMenuBar(parent)
Widget	parent;
#else
void	CreateMenuBar(Widget parent)
#endif

{
  Widget  menuBar;

  menuBar = XmCreateMenuBar(parent,"MenuBar",NULL,0);
  XtManageChild(menuBar);
  
  CreateFileMenuDef(menuBar);

/*
  CreateFileMenu(menuBar);
*/

  CreateEditMenu(menuBar);
  CreateWriteMenu(menuBar);

/*
  CreateSettingMenu(menuBar);
  CreateActionMenu(menuBar);
*/

  CreateDefineMenu(menuBar);

  CreateRunMenu(menuBar);

/*
  CreateLoadRfileMenu(menuBar);
*/

/*
  CreateBrowseRfileMenu(menuBar);
*/

  CreateSaveMenu(menuBar);
  CreateAppendMenu(menuBar);
  CreatePlotMenu(menuBar);
  CreateFilesMenu(menuBar);
  CreateDemoMenu(menuBar);
  CreateMiscMenu(menuBar);      /*MiscMenu*/
  CreateHelpMenu(menuBar);

}


/*
***********************************************************************
**
**			CreateFileMenuDef.c
**                           
************************************************************************
*/

#ifdef _NO_PROTO
void	CreateFileMenuDef(menuBar)
Widget	menuBar;
#else
void	CreateFileMenuDef(Widget menuBar)
#endif

{
  Widget                sept;
  Widget		cascade;	/*  CascadeButton		*/
  Widget		menuPane;	/*  RowColumn	 		*/
  Widget		button;		/*  PushButton			*/


  menuPane = XmCreatePulldownMenu(menuBar,"EqnPane",NULL,0);
/*
  XtManageChild(menuPane);
*/
  cascade = XmCreateCascadeButton(menuBar,"Equations",NULL,0);
  XtVaSetValues(cascade,XmNsubMenuId,menuPane,XmNmnemonic,'q',NULL);
  XtManageChild(cascade);

  GW.open_dialog = XmCreateFileSelectionDialog(menuPane,"Selection", NULL, 0);
  XtAddCallback(GW.open_dialog,XmNokCallback,DialogAcceptCB,(XtPointer)DIALOG_FSELECT);
  XtAddCallback(GW.open_dialog,XmNcancelCallback,DialogCancelCB,(XtPointer)DIALOG_FSELECT);

  button = XmSelectionBoxGetChild(GW.open_dialog,XmDIALOG_HELP_BUTTON);
  XtUnmanageChild(button);
		 
  button = XmCreatePushButtonGadget(menuPane, "Old ...",NULL,0);
  XtVaSetValues(button,XmNmnemonic,'O',NULL);
  XtAddCallback(button, XmNactivateCallback, MenuCB, (XtPointer)MENU_OPEN);
  XtManageChild(button);


  GW.new_dialog = XmCreatePromptDialog(menuPane,"new file dialog",NULL,0);
  XtVaSetValues(GW.new_dialog,XmNselectionLabelString,
		XmStringCreateSimple("Enter name"),NULL);
  button = XmSelectionBoxGetChild(GW.new_dialog,XmDIALOG_HELP_BUTTON);
  XtUnmanageChild(button);

  XtAddCallback(GW.new_dialog, XmNokCallback,DialogAcceptCB, (XtPointer)DIALOG_NEW);
  XtAddCallback(GW.new_dialog, XmNcancelCallback,DialogCancelCB, (XtPointer)DIALOG_NEW);

  button = XmCreatePushButtonGadget (menuPane, "New ...",NULL,0);
  XtVaSetValues(button,XmNmnemonic,'N',NULL);
  XtAddCallback(button, XmNactivateCallback, MenuCB, (XtPointer)MENU_NEW);
  XtManageChild(button);

/*
  button = XmCreatePushButtonGadget (menuPane, "Default",NULL,0);
  XtVaSetValues(button,XmNmnemonic,'e',NULL);
  XtAddCallback(button, XmNactivateCallback, DefDriverCB,NULL);
  XtManageChild(button);


  sept = XmCreateSeparatorGadget(menuPane,"sp",NULL,0);
  XtManageChild(sept);
*/

  button = XmCreatePushButtonGadget(menuPane,"Demo ...",NULL,0);
  XtVaSetValues(button,XmNmnemonic,'D',NULL);
  XtManageChild(button);
  XtAddCallback(button,XmNactivateCallback,PopupCB,GW.demo_list);

#if 0
		 
  sept = XmCreateSeparatorGadget(menuPane,"sp",NULL,0);
  XtManageChild(sept);

/*

  ac = 0;
  XtSetArg(al[ac], XmNlabelString,
	   XmStringCreateLtoR("Close", XmSTRING_DEFAULT_CHARSET)); ac++;
  XtSetArg(al[ac], XmNmnemonic, 'C'); ac++;
  button = XmCreatePushButtonGadget (menuPane, "Clear", al, ac);
  XtAddCallback (button, XmNactivateCallback, MenuCB, (XtPointer)MENU_CLOSE);
  XtManageChild (button);

  GW.close_warning = CreateSpWarningDialog(menuPane, "save_warning",
			    "warning_image", "Save Changes?", al, ac);

  XtAddCallback(GW.close_warning,XmNapplyCallback,DialogApplyCB,(XtPointer)DIALOG_CWARNING);
  XtAddCallback(GW.close_warning,XmNokCallback,DialogAcceptCB, (XtPointer)DIALOG_CWARNING);

*/		 
  ac = 0;
  XtSetArg(al[ac], XmNlabelString,XmStringCreateLtoR("Save", XmSTRING_DEFAULT_CHARSET)); 
  ac++;
	   
  XtSetArg(al[ac], XmNmnemonic, 'S'); ac++;
  button = XmCreatePushButtonGadget (menuPane, "Save", al, ac);
  XtAddCallback (button, XmNactivateCallback, MenuCB, (XtPointer)MENU_SAVE);
  XtManageChild (button);


  ac = 0;
  XtSetArg(al[ac], XmNlabelString,
	   XmStringCreateLtoR("Save As ...", XmSTRING_DEFAULT_CHARSET)); ac++;
  XtSetArg(al[ac], XmNmnemonic, 'A'); ac++;
  button = XmCreatePushButtonGadget (menuPane, "Save As ...", al, ac);
  XtAddCallback (button, XmNactivateCallback, MenuCB, (XtPointer)MENU_SAVE_AS);
  XtManageChild (button);

  ac = 0;
  XtSetArg(al[ac], XmNselectionLabelString, XmStringCreateLtoR
	   ("Save As ...", XmSTRING_DEFAULT_CHARSET));  ac++;
  GW.save_dialog = XmCreatePromptDialog(menuPane, "save dialog", al, ac);
  XtAddCallback (GW.save_dialog, XmNokCallback,
		 DialogAcceptCB, (XtPointer)DIALOG_SAVE);

/*

  ac = 0;
  XtSetArg(al[ac], XmNlabelString,
	   XmStringCreateLtoR("Print", XmSTRING_DEFAULT_CHARSET)); ac++;
  XtSetArg(al[ac], XmNmnemonic, 'P'); ac++;
  button = XmCreatePushButtonGadget (menuPane, "Print", al, ac);
  XtAddCallback (button, XmNactivateCallback, MenuCB, (XtPointer)MENU_PRINT);
  XtManageChild (button);

  ac = 0;
  XtSetArg(al[ac], XmNselectionLabelString, XmStringCreateLtoR
	   ("Save file before printing?", XmSTRING_DEFAULT_CHARSET));  ac++;

  GW.print_warning = CreateSpWarningDialog(menuPane, "print_warning",
		  "warning_image", "Save file before printing?", al, ac);
  XtAddCallback (GW.print_warning, XmNokCallback,
		 DialogAcceptCB, (XtPointer)DIALOG_PRINT);
*/

  sept = XmCreateSeparatorGadget(menuPane,"sp",NULL,0);
  XtManageChild(sept);

  ac = 0;
  XtSetArg(al[ac], XmNlabelString,
	   XmStringCreateLtoR("Clear", XmSTRING_DEFAULT_CHARSET)); ac++;
  XtSetArg(al[ac], XmNmnemonic, 'C'); ac++;
  button = XmCreatePushButtonGadget (menuPane, "Clear", al, ac);
  XtAddCallback (button, XmNactivateCallback, MenuCB, (XtPointer)MENU_CLOSE);
  XtManageChild (button);

  GW.close_warning = CreateSpWarningDialog(menuPane, "save_warning",
			    "warning_image", "Save Changes?", al, ac);

  XtAddCallback(GW.close_warning,XmNapplyCallback,DialogApplyCB,(XtPointer)DIALOG_CWARNING);
  XtAddCallback(GW.close_warning,XmNokCallback,DialogAcceptCB, (XtPointer)DIALOG_CWARNING);

  sept = XmCreateSeparatorGadget(menuPane,"sp",NULL,0);
  XtManageChild(sept);

  ac = 0;
  XtSetArg(al[ac], XmNlabelString,
	   XmStringCreateLtoR("Exit", XmSTRING_DEFAULT_CHARSET)); ac++;
  XtSetArg(al[ac], XmNmnemonic, 'E'); ac++;
  XtSetArg(al[ac], XmNacceleratorText,
	   XmStringCreateLtoR("Escape", XmSTRING_DEFAULT_CHARSET)); ac++;
  XtSetArg(al[ac], XmNaccelerator, "<Key>Escape:"); ac++;
  button = XmCreatePushButtonGadget (menuPane, "Exit", al, ac);
  XtAddCallback (button, XmNactivateCallback, MenuCB, (XtPointer)MENU_EXIT);
  XtManageChild (button);

  GW.exit_warning = CreateSpWarningDialog(menuPane, "exit warning",
	            "warning_image", "Save Changes?", al, ac);

  XtAddCallback (GW.exit_warning, XmNapplyCallback,
		 DialogApplyCB, (XtPointer)DIALOG_XWARNING);
  XtAddCallback (GW.exit_warning, XmNokCallback,
		 DialogAcceptCB, (XtPointer)DIALOG_XWARNING);

#endif


}


#ifdef _NO_PROTO
void	DefDriverCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void	DefDriverCB(Widget w,XtPointer userData, XtPointer callbackArg)
#endif

{
  char command[100];

  strcpy(GP.fileName,DEFAULT_NAME);

/*      
  autoDir = copyenv(AUTO_DIR);
*/

  strcpy(command,GP.autoDir);
/*  strcat(command,"/gui/default.f");   */
  strcat(command,"/gui/");
  strcat(command,DEFAULT_FILE);


  if (!OpenFile(command)) {
    
    XtSetSensitive(GW.text, True);
    XtSetSensitive(GW.cut_button, True);
    XtSetSensitive(GW.copy_button, True);
    XtSetSensitive(GW.clear_button, True);

  }
  
  ReadDefCB(w,userData,callbackArg); 

  printf("Loading default Auto-Equation and Auto-Constants files to buffer ... done\n");

}



/*
***********************************************************************
**
**			CreateWriteMenu.c
**                           
************************************************************************
*/

#ifdef _NO_PROTO
void	CreateWriteMenu(menuBar)
Widget	menuBar;
#else
void	CreateWriteMenu(Widget menuBar)
#endif

{
  Widget		cascade;	/*  CascadeButton		*/
  Widget		menuPane;	/*  RowColumn	 		*/
  Widget		button,bn;      /*  PushButton			*/


  menuPane = XmCreatePulldownMenu(menuBar, "WritePane",NULL,0);
/*
  XtManageChild(menuPane);
*/

  cascade = XmCreateCascadeButton(menuBar,"Write",NULL,0);
  XtManageChild(cascade);
  XtVaSetValues(cascade,XmNsubMenuId,menuPane,XmNmnemonic,'W',NULL);

  button = XmCreatePushButtonGadget(menuPane, "Write", NULL,0);
  XtManageChild(button);
  XtVaSetValues(button,XmNmnemonic,'W',NULL);
  XtAddCallback(button, XmNactivateCallback, MenuCB, (XtPointer)MENU_SAVE);

  GW.save_dialog = XmCreatePromptDialog(menuPane, "save dialog",NULL,0);
  button = XmSelectionBoxGetChild(GW.save_dialog,XmDIALOG_HELP_BUTTON);
  XtUnmanageChild(button);
  XtVaSetValues(GW.save_dialog,XmNselectionLabelString,
		XmStringCreateSimple("Write as"),NULL,0);

  XtAddCallback(GW.save_dialog,XmNokCallback,DialogAcceptCB, (XtPointer)DIALOG_SAVE);

  bn = XmCreatePushButtonGadget(menuPane, "Write As ...",NULL,0);
  XtManageChild(bn);
  XtVaSetValues(bn,XmNmnemonic,'A',NULL);
  XtAddCallback(bn, XmNactivateCallback, MenuCB, (XtPointer)MENU_SAVE_AS);

}


/*
***********************************************************************
**
**			CreateEditMenu.c
**                           
************************************************************************
*/

#ifdef _NO_PROTO
void	CreateEditMenu(menuBar)
Widget	menuBar;
#else
void	CreateEditMenu(Widget menuBar)
#endif

{
  Widget		cascade;	/*  CascadeButton		*/
  Widget		menuPane;	/*  RowColumn	 		*/
  Widget		button;		/*  PushButton			*/

#if 0

  Arg		al[10];			/*  arg list			*/
  register int	ac;			/*  arg count			*/

#endif


  /*	Create "Options" PulldownMenu.	*/
	
  menuPane = XmCreatePulldownMenu(menuBar, "EditPane",NULL,0);
/*
  XtManageChild(menuPane);
*/

/*
  ac = 0;
  XtSetArg (al[ac], XmNsubMenuId, menuPane);  ac++;
  XtSetArg(al[ac], XmNlabelString,
	   XmStringCreateLtoR("Edit", XmSTRING_DEFAULT_CHARSET)); ac++;
  XtSetArg(al[ac], XmNmnemonic, 'E'); ac++;
*/

  cascade= XmCreateCascadeButton(menuBar, "Edit",NULL,0);
  XtVaSetValues(cascade,XmNsubMenuId,menuPane,XmNmnemonic,'E',NULL);
  XtManageChild(cascade);

/*
  ac = 0;
  XtSetArg(al[ac], XmNlabelString,
	   XmStringCreateLtoR("Cut", XmSTRING_DEFAULT_CHARSET)); ac++;
  XtSetArg(al[ac], XmNmnemonic, 't'); ac++;
  XtSetArg(al[ac], XmNacceleratorText,
	   XmStringCreateLtoR("Delete", XmSTRING_DEFAULT_CHARSET)); ac++;

  XtSetArg(al[ac], XmNaccelerator, "<Key>Delete:"); ac++;
*/
  
  GW.cut_button = XmCreatePushButtonGadget(menuPane, "Cut", NULL,0);
  XtAddCallback(GW.cut_button, XmNactivateCallback, MenuCB, (XtPointer)MENU_CUT);
  XtVaSetValues(GW.cut_button,XmNmnemonic,'C',NULL);
  XtManageChild(GW.cut_button);
  XtSetSensitive(GW.cut_button, False);

/*
  ac = 0;
  XtSetArg(al[ac], XmNlabelString,
	   XmStringCreateLtoR("Copy", XmSTRING_DEFAULT_CHARSET)); ac++;
  XtSetArg(al[ac], XmNmnemonic, 'C'); ac++;
  XtSetArg(al[ac], XmNacceleratorText,
	   XmStringCreateLtoR("F2", XmSTRING_DEFAULT_CHARSET)); ac++;

  XtSetArg(al[ac], XmNaccelerator, "<Key>F2:"); ac++;
*/

  GW.copy_button = XmCreatePushButtonGadget(menuPane, "Copy", NULL,0);
  XtAddCallback(GW.copy_button, XmNactivateCallback, MenuCB, (XtPointer)MENU_COPY);
  XtManageChild(GW.copy_button);
  XtVaSetValues(GW.copy_button,XmNmnemonic,'o',NULL);
  XtSetSensitive(GW.copy_button, False);

/*
  ac = 0;
  XtSetArg(al[ac], XmNlabelString,
	   XmStringCreateLtoR("Paste", XmSTRING_DEFAULT_CHARSET)); ac++;
  XtSetArg(al[ac], XmNmnemonic, 'P'); ac++;
  XtSetArg(al[ac], XmNacceleratorText,
	   XmStringCreateLtoR("Insert", XmSTRING_DEFAULT_CHARSET)); ac++;

  XtSetArg(al[ac], XmNaccelerator, "<Key>Insert:"); ac++;
*/


  GW.paste_button = XmCreatePushButtonGadget(menuPane, "Paste",NULL,0); 
  XtAddCallback(GW.paste_button, XmNactivateCallback, MenuCB, (XtPointer)MENU_PASTE);
  XtManageChild(GW.paste_button);
  XtVaSetValues(GW.paste_button,XmNmnemonic,'P',NULL);
  XtSetSensitive(GW.paste_button, False);


#if 0

  ac = 0;
  XtSetArg(al[ac], XmNlabelString,
	   XmStringCreateLtoR("Clear", XmSTRING_DEFAULT_CHARSET)); ac++;
  XtSetArg(al[ac], XmNmnemonic, 'e'); ac++;
	
  /* pseudo accelerator - Text already handles
     this action appropriately */
	
  XtSetArg(al[ac], XmNacceleratorText,
	   XmStringCreateLtoR("Del", XmSTRING_DEFAULT_CHARSET)); ac++;

  GW.clear_button = XmCreatePushButtonGadget (menuPane, "Clear", al, ac);
  XtAddCallback (GW.clear_button, XmNactivateCallback, MenuCB, (XtPointer)MENU_CLEAR);
/*
  XtManageChild (GW.clear_button);
*/
  XtSetSensitive(GW.clear_button, False);

#endif

}

#ifdef _NO_PROTO
 XImage *CreateDefaultImage(bits, width, height)
char *bits;
int   width, height;
#else
 XImage *CreateDefaultImage(char *bits, int width, int height)
#endif

{
  XImage *image;

  image = (XImage *) XtMalloc (sizeof (XImage));
  image->width = width;
  image->height = height;
  image->data = bits;
  image->depth = 1;
  image->xoffset = 0;
  image->format = XYBitmap;
  image->byte_order = LSBFirst;
  image->bitmap_unit = 8;
  image->bitmap_bit_order = LSBFirst;
  image->bitmap_pad = 8;
  image->bytes_per_line = (width+7)/8;

  return (image);
}

/*
**		 Open the present file.  Returns true if file 
** 		 exists and open is sucessful.
*/

#ifdef _NO_PROTO
Boolean OpenFile(file_name)
char    *file_name;
#else
Boolean OpenFile(char *file_name)
#endif

{

  struct stat statbuf;		/* Information on a file. */
  int file_length,n;		/* Length of file. 	  */
  char *file_string;		/* Contents of file. 	  */
  FILE *fp = NULL;		/* Pointer to open file   */
   

/*

  CloseFile();
*/

  if((fp = fopen(file_name, "r")) == NULL) {
    return (False);
  } 

  if(stat(file_name, &statbuf) == 0)
    file_length = statbuf.st_size;
  else
    file_length = MAX_FILESIZE; 

  n = 2*file_length+MIN_FILESIZE;
  file_string = (char *) XtMalloc(n);

  fread(file_string, sizeof(char), file_length, fp);
  file_string[file_length] = '\0';
  XmTextSetString(GW.text, file_string);

  GP.file_saved = True; 
  XtSetSensitive(GW.text, True);
  XtSetSensitive(GW.cut_button, True);
  XtSetSensitive(GW.copy_button, True);


  if(file_string != NULL) XtFree(file_string);

  if (fp != NULL)    fclose(fp);
  return(True);


#if 0

  struct stat statbuf;		/* Information on a file. */
  int file_length;		/* Length of file. 	  */
  char *file_string;		/* Contents of file. 	  */
  FILE *fp = NULL;		/* Pointer to open file   */
   
  if((fp = fopen(file_name, "r+")) == NULL)
    if((fp = fopen(file_name, "r")) != NULL) {
      /* fprintf(stderr, "Warning: unable to open the file\n"); */
      CloseFile();
      return (False);
    } 
    else {
      return(False);
    }

  if(stat(file_name, &statbuf) == 0)
    file_length = statbuf.st_size;
  else
    file_length = 20000; /* arbitrary file length */

  /* read the file string */

  file_string = (char *) XtMalloc(file_length);
  fread(file_string, sizeof(char), file_length, fp);

  /* close up the file */

  if(fclose(fp) != NULL) fprintf(stderr, "Warning: unable to close file.\n");

  /* added the file string to the text widget */

  XmTextSetString(GW.text, file_string);

  GP.file_saved = True; /* intialize to True */
	 
  /* make appropriate item sensitive */

  XtSetSensitive(GW.text, True);
  XtSetSensitive(GW.cut_button, True);
  XtSetSensitive(GW.copy_button, True);
/*
  XtSetSensitive(GW.clear_button, True);
*/

  if(file_string != NULL) XtFree(file_string);
  return(True);

#endif

}


/*
**		Save the present file.
*/

 Boolean SaveFile()

{
  char *file_string = NULL;	   /* Contents of file.		      */
  FILE *fp;		       	   
  char name[20],rname[20];
  int i=0;

  if((fp = fopen(ProgramName(GP.fileName), "w")) == NULL) {
    fprintf(stderr, "Warning: unable to write file %s\n",ProgramName(GP.fileName));
    return(False);;
  }

  file_string = (char *)XmTextGetString(GW.text);


/*
    original line

  fwrite(file_string, sizeof(char), strlen(file_string) + 1, fp); 

*/

  fwrite(file_string, sizeof(char), strlen(file_string), fp);
  GP.file_saved = True;

  strcpy(name,ProgramName(GP.fileName));
  while(name[i] != '.')
    ++i;
  name[i]='\0';

  strcpy(rname,"c.");
  strcat(rname,name);

  /* WriteRfile(CURRENT_RFILE); */

  WriteRfile(rname); 


  if (file_string != NULL) {
    XtFree(file_string); 
  }

  if (fp != NULL)    fclose(fp);

  printf("Saving %s ... done\n",ProgramName(GP.fileName));
  printf("Saving %s ... done\n",rname);

  return(True);



#if 0

  char *file_string = NULL;	   /* Contents of file.		      */
  FILE *tfp;		       	   /* Pointer to open temporary file. */
  char	namebuf[BUFSIZ]; 	   /* for "system" call below */
  int		status;
  char *tempname = (char *)XtMalloc(100); /* Temporary file name.       */



/*  strcpy(tempname, mktemp("/tmp/xmeditXXXXXX"));*/

  strcpy(tempname, mktemp("/tmp/XAutoXXXXXX"));
    
  if((tfp = fopen(tempname, "w")) == NULL) {
    fprintf(stderr, "Warning: unable to open temp file, text not saved.\n");
    return(False);;
  }

  /* get the text string */

  file_string = (char *)XmTextGetString(GW.text);

  /* write to a temp file */

  fwrite(file_string, sizeof(char), strlen(file_string) + 1, tfp);

  /* flush and close the temp file */

  if (fflush(tfp) != NULL) fprintf(stderr,"Warning: unable to flush file.\n");
  if (fclose(tfp) != NULL) fprintf(stderr,"Warning: unable to close file.\n");

  if (file_string != NULL) {
    XtFree(file_string); /* free the text string */
  }

  /* move the tempname to the saved file, but do it independent
     of filesystem boundaries */

  sprintf (namebuf, "cp %s \.\/%s\0", tempname, ProgramName(GP.fileName));
  status = system(namebuf);
  unlink (tempname);
  if (status == 0) {

  } 
  else {
    fprintf(stderr, "Warning: unable to save file.\n");
    XtFree(tempname);
    return(False);
  }
           
    XtFree(tempname);
    return(True);

#endif

}


/*
**              Close the present file.
*/

 void CloseFile()

{

  XmTextSetString(GW.text, "");
  XtSetSensitive(GW.text, False);


#if 0

  /* zero out the text string in the text widget.
     caution: is causes a value changed callack. */

  XmTextSetString(GW.text, "");

  GP.file_saved = True; /* reinitialize file_saved flag */

  /* free the file name */

  if( strcmp(GP.fileName,EMPTY) != 0 ) {
    /* XtFree(GP.file_name); */
    /* GP.file_name = NULL; */
      strcpy(GP.fileName,EMPTY);
  }

  /* set text to insensitive */

  XtSetSensitive(GW.text, False);

#endif

}


/*
**		Copy the present file to the clipboard.
*/

#ifdef _NO_PROTO
void CopyFileToClipboard(time)
Time time;
#else
void CopyFileToClipboard(Time time)
#endif

{
  char *selected_string = XmTextGetSelection (GW.text);  /* text selection    */
  long item_id = 0;			      	      /* clipboard item id */
  long data_id = 0;				      /* clipboard data id */
  int status = 0;				      /* clipboard status  */
  XmString clip_label;

  /* using the clipboard facilities, copy the selected text to the clipboard */

  if(selected_string != NULL) {
    clip_label = XmStringCreateLtoR ("XM_EDITOR", XmSTRING_DEFAULT_CHARSET);

    /* start copy to clipboard, and continue till
       a sucessful start copy is made */

    status = 0;
    while(status != ClipboardSuccess)
      status = XmClipboardStartCopy(XtDisplay(GW.text), XtWindow(GW.text),
				     clip_label, time,
				     GW.text, NULL, &item_id);

    /* move the data to the clipboard, and
       continue till a sucessful copy is made */

    status = 0;
    while(status != ClipboardSuccess)
      status = XmClipboardCopy (XtDisplay(GW.text), XtWindow(GW.text),
				item_id, "STRING", selected_string,
				(long)strlen(selected_string)+1, 0,
				&data_id);

    /* end the copy to the clipboard and continue till
       a sucessful end copy is made */

    status = 0;
    while(status != ClipboardSuccess)
      status = XmClipboardEndCopy(XtDisplay(GW.text),XtWindow(GW.text),item_id);

    /* allow pasting when an item is sucessfully copied to the clipboard */

    XtSetSensitive(GW.paste_button, True);

  }

}	


/*
**		Deletes the primary selection.
*/

#ifdef _NO_PROTO
void DeletePrimarySelection(time)
Time time;
#else
void DeletePrimarySelection(Time time)
#endif

{

  XClientMessageEvent cm;

  /* send a client message to the text widget
     to delete the current selection */


  XmTextCut(GW.text,time);


#if 0

  cm.type = ClientMessage;
  cm.display = XtDisplay(GW.text);
  cm.message_type = XmInternAtom(XtDisplay(GW.text), "KILL_SELECTION", FALSE);
  cm.window = XtWindow(GW.text);
  cm.format = 32;
  cm.data.l[0] = XA_PRIMARY;
  XSendEvent(XtDisplay(GW.text), cm.window, TRUE, NoEventMask, &cm);

#endif

}

/*
**		paste item from the clipboard to the current cursor location
*/

void PasteItemFromClipboard()

{
  /* retrieve the current data from the clipboard
     and paste it at the current cursor position */

  char * selected_string = XmTextGetSelection (GW.text);
					      /* containts of selection  */
  int status = 0;			      /* clipboard status	 */
  char *buffer;			      	      /* temporary text buffer 	 */
  unsigned long length;			      /* length of buffer     	 */
  unsigned long outlength = 0;	      	      /* length of bytes copied	 */
  long private_id = 0;			      /* id of item on clipboard */
  XmTextPosition cursorPos;		      /* text cursor position 	 */
  register int ac;			      /* arg count  	      	 */
  Arg al[10];				      /* arg list	      	 */

  /* find the length of the paste item, continue till the length is found */

  while (status != ClipboardSuccess) {
    status = XmClipboardInquireLength(XtDisplay(GW.text), XtWindow(GW.text),
				      "STRING", &length);
    if (status == ClipboardNoData) {
      length = 0;
      break;
    }
   }
			
  if (length == 0) {
    fprintf(stderr, "Warning: paste failed, no items to paste.\n");	
    return;
  }

  /* malloc to necessary space */

  buffer = XtMalloc(length);

  status = XmClipboardRetrieve (XtDisplay(GW.text), XtWindow(GW.text), "STRING",
				buffer, length, &outlength, &private_id);
			
  /* Dialogs need to be added to indicate errors in pasting */
  
  if (status != ClipboardSuccess) {
    fprintf(stderr, "Warning: paste failed, status = %d\n", status);	
    return;
  }

  /* get cursor position for pasting */

  XtSetArg(al[0], XmNcursorPosition, &cursorPos);
  XtGetValues(GW.text, al, 1);
			
  /* add new text */

  XmTextReplace(GW.text, cursorPos, cursorPos, buffer);
}

/*
**		Process callback from Text.
*/

#ifdef _NO_PROTO
void 	FileChangedCB (w, client_data, call_data) 
Widget		w;		/*  widget id		*/
XtPointer		client_data;	/*  data from application   */
XtPointer		call_data;	/*  data from widget class  */
#else
void 	FileChangedCB
(
Widget		w,		/*  widget id		*/
XtPointer		client_data,	/*  data from application   */
XtPointer		call_data	/*  data from widget class  */
)
#endif

{
  /* set the GP.file_saved flag to indicate that the
     file has been modified and the user should be
     notified before exiting. */

  GP.file_saved = False;
}

/*
**		Process callback from PushButtons in PulldownMenus.
*/

#ifdef	_NO_PROTO
void 	MenuCB (w, client_data, call_data) 
Widget		w;		/*  widget id		*/
XtPointer		client_data;	/*  data from application   */
XtPointer		call_data;	/*  data from widget class  */
#else
void 	MenuCB
(
Widget		w,		/*  widget id		*/
XtPointer		client_data,	/*  data from application   */
XtPointer		call_data	/*  data from widget class  */
)
#endif

{	
  register int ac;		/* arg count		    */
  Arg al[10];			/* arg list		    */
  /* char *command; */	       	/* command used in printing */
  Widget exitFile;
  XmAnyCallbackStruct * cb;
  char progname[100];

  switch ((size_t)client_data) {

  case MENU_OPEN:

    /* display the file selection dialog */

    XtManageChild(GW.open_dialog);
    break;

  case MENU_NEW:

    /* display the prompt dialog */

    XtManageChild(GW.new_dialog);
    break;

  case MENU_CLOSE:

    /* the present file has not been saved since
       the last modification */

    if (!GP.file_saved) 		/* display the 'save' message dialog */
      XtManageChild (GW.close_warning);
    else
      CloseFile();
    break;

  case MENU_SAVE:

    /* open a temp file for writing */

    SaveFile();
    break;
    
  case MENU_SAVE_AS:
	
    /* Display the 'save as' dialog with the
       present filename displayed in it. */


/*
    
    strcpy(progname,GetDirName());

    strcat(progname,"/");
    strcat(progname,ProgramName(GP.fileName));

    ac = 0;
    while(progname[ac] != '.' && ac < 99) 
      ++ac;
    if(progname[ac] = '.')
      progname[ac] = '\0';
    else
      progname[0] = '\0';
*/

    strcpy(progname,ProgramName(GP.fileName));
    ac=0;
    while(progname[ac] != '.' && progname[ac] != '\0')
     ++ac;
    if(progname[ac] = '.')
      progname[ac] = '\0';

    ac = 0;
    /*    XtSetArg(al[ac], XmNtextString,XmStringCreateSimple(GP.fileName));*/
    XtSetArg(al[ac], XmNtextString,XmStringCreateSimple(progname));
    ac++;
    XtSetValues(GW.save_dialog, al, ac);
    XtManageChild(GW.save_dialog);
    break;

  case MENU_PRINT:

#if 0
	
    if (!GP.file_saved) {
      XtManageChild(GW.print_warning);
    }
    else if( strcmp(GP.fileName,EMPTY) != 0) {

      /* malloc space for the command name. 
	 Note: command = size of the filename +
	 "lp " + null terminator */
      
      /* command = (char *)malloc((strlen(GP.file_name) + 4)*sizeof(char));*/
      /* sprintf(command, "lp < %s &", GP.file_name); */
      strcpy(GP.command,"lp < ");
      strcat(GP.command,GP.fileName);
      strcat(GP.command," &");
      if (system(GP.command) != NULL)
	fprintf(stderr, "print failed");
      /* XtFree(command); */
    }
    else {
      printf("No equation was load\n");
    }
#endif

    break;

  case MENU_EXIT:
	
    /* exit if there is no files open */
    if (!GP.file_saved) 		      /* display the 'save' message dialog */
      XtManageChild (GW.exit_warning);
    else {
	
      /* close up file pointers and descriptors */
	
      CloseFile();

      /* exit this program */

      exitFile = CreateQuestionDialog(GW.mainWindow);
      XtManageChild(exitFile);
/*
      exit (0);
*/
    }
    break;

  case MENU_CUT:

    {
	
      /* needed to get the event time */
      
      cb =(XmAnyCallbackStruct *) call_data;

      /* call routine to copy selection to clipboard */
      
      CopyFileToClipboard(cb->event->xbutton.time);
      
      /* call routine to delete primary selection */


      DeletePrimarySelection(cb->event->xbutton.time);


     }
    break;

  case MENU_COPY:
    {
	
      /* needed to get the event time */

      cb =(XmAnyCallbackStruct *) call_data;

      /* call routine to copy selection to clipboard */

      CopyFileToClipboard(cb->event->xbutton.time);

    }
    break;

  case MENU_PASTE:

    /* call the routine that pastes the
       text at the cursor position */
	
    PasteItemFromClipboard();
    break;

  case MENU_CLEAR:
	
    /* call routine to delete primary selection */
    /*added by Randy to get it to compile*/
    DeletePrimarySelection(0);
    /*************************************/
    break;

  case MENU_HELP:
	
    /* no help at this time */
    break;

  default:
	
    /* unknown client_data was recieved and
       there is no setup to handle this */
	
    fprintf(stderr, "Warning: in menu callback\n");
    break;
  }

}


/*
**		Process callback from Dialog apply actions.
*/

#ifdef	_NO_PROTO
 void DialogApplyCB (w, client_data, call_data) 
Widget		w;		/*  widget id		    */
XtPointer		client_data;	/*  data from application   */
XtPointer		call_data;	/*  data from widget class  */
#else
 void DialogApplyCB
(
Widget		w,		/*  widget id		    */
XtPointer		client_data,	/*  data from application   */
XtPointer		call_data	/*  data from widget class  */
)
#endif

{
  /* char *command; */			/* command used in printing */

  switch ((size_t)client_data)	{
    
  case DIALOG_PRINT:

    if( strcmp(GP.fileName,EMPTY) != 0) {
	
      /* malloc space for the command name. 
	 Note: command = size of the filename +
	 "lp " + null terminator */
	/*	
      command = XtMalloc(strlen(GP.file_name) + 4);
      sprintf(command, "lp < %s\0", GP.fileName); */
      strcpy(GP.command,"lp < ");
      strcat(GP.command,GP.fileName);
      strcat(GP.command," &");
      if (system(GP.command) != 0)
	fprintf(stderr, "print failed");
      /* XtFree(command); */
    }

  case DIALOG_CWARNING:

    CloseFile();
    GP.file_saved = True; /* reset the default */
    break;

  case DIALOG_XWARNING:
	
    CloseFile();
    exit (1);
    break;		

  default:
    
    /* unknown client_data was recieved and
       there is no setup to handle this */
	
    fprintf (stderr, "Warning: in apply callback\n");
    break;

  }
}

/*
**		Process callback from Dialog cancel actions.
*/

#ifdef 	_NO_PROTO
 void DialogCancelCB (w, client_data, call_data) 
Widget		w;		/*  widget id		    */
XtPointer		client_data;	/*  data from application   */
XtPointer		call_data;	/*  data from widget class  */
#else
 void DialogCancelCB
(
Widget		w,		/*  widget id		    */
XtPointer		client_data,	/*  data from application   */
XtPointer		call_data	/*  data from widget class  */
)
#endif

{
  switch ((size_t)client_data){
	
  case DIALOG_FSELECT:
	
    /* popdown the file selection box */
    
    XtUnmanageChild (GW.open_dialog);
    break;

  case DIALOG_CWARNING:
  case DIALOG_XWARNING:
  case DIALOG_NEW:
  case DIALOG_PRINT:
  case DIALOG_SAVE:
  case DIALOG_HELP:
	
    /* no action is necessary at this time */
    break;
    
  default:
	
    /* a unknown client_data was recieved and
       there is no setup to handle this */
	
    fprintf (stderr, "Warning: in cancel callback\n");
    break;
  }
}


/*
**		Process callback from Dialog actions.
*/

#ifdef	_NO_PROTO
 void DialogAcceptCB (w, client_data, call_data) 
Widget		w;		/*  widget id		    */
XtPointer		client_data;	/*  data from application   */
XtPointer		call_data;	/*  data from widget class  */
#else
 void DialogAcceptCB 
(
Widget		w,		/*  widget id		    */
XtPointer		client_data,	/*  data from application   */
XtPointer		call_data	/*  data from widget class  */
)
#endif

{
  /* char *command; */		/* command used in printing */
  /* char cmd[100]; */
  char name[10],*autoDir;
  char *fileName,c1[100],c2[100];
  FILE *fp;
  
  switch ((size_t)client_data){
    
  case DIALOG_FSELECT:
	
    /* open the file and read it into the text widget */

    if( strcmp(GP.fileName,EMPTY) != 0) {
      /* XtFree(GP.file_name); 
      GP.file_name = NULL; */
      strcpy(GP.fileName,EMPTY);
    }
    {
      XmFileSelectionBoxCallbackStruct *fcb =
	(XmFileSelectionBoxCallbackStruct *) call_data;

      /* get the filename from the file selection box */
	
      XmStringGetLtoR(fcb->value, XmSTRING_DEFAULT_CHARSET, &fileName);
      XmStringGetLtoR(fcb->dir, XmSTRING_DEFAULT_CHARSET, &autoDir);

      strcpy(GP.fileName,fileName);

      /* Open file, print error if it does not exist. */
	
      if (!OpenFile(GP.fileName)) {
	fprintf(stderr, "\nWarning: unable to open the file\n");
	XtUnmanageChild(GW.open_dialog);
	break;
      }

      printf("\nLoading %s to buffer ... done\n",ProgramName(GP.fileName));
 
      /* load rfile if exist */

      GetJobName(name,ProgramName(GP.fileName));
      
      strcpy(c1,autoDir);
      strcat(c1,"c.");
      strcat(c1, name);

      fp = fopen(c1,"r");
      if(fp != NULL  && ReadRfile(c1) == 0 ) {
	SetParScreen();
	printf("Loading %s to buffer ... done\n",ProgramName(c1));
      }
      else
	printf("%s is not available\n",ProgramName(c1));

      if (fp != NULL)    fclose(fp);
      /* popdown the file selection box */
            
      XtUnmanageChild(GW.open_dialog);
    }
    break;

  case DIALOG_NEW:
	
    /* open the file and read it into the text widget */
	
    if (strcmp(GP.fileName,EMPTY) != 0) {
      /*
      XtFree(GP.file_name);
      GP.file_name = NULL; */
      strcpy(GP.fileName,EMPTY);
    }
    {
      XmSelectionBoxCallbackStruct *scb =
	(XmSelectionBoxCallbackStruct *) call_data;
      
      /* get the filename string from the file
	 name prompt box */
		
      XmStringGetLtoR(scb->value, XmSTRING_DEFAULT_CHARSET, &fileName);
      
      strcpy(GP.fileName,fileName);
      strcat(GP.fileName,".f90");

      if( strcmp(fileName,EMPTY) == 0 )
	strcpy(GP.fileName,DEFAULT_NAME);
      
      /* load the default.c */

/*
      autoDir = copyenv(AUTO_DIR);
*/      
      strcpy(c2,GP.autoDir);
/*
      strcat(c2,"/gui/default.f");
*/
      strcat(c2,"/gui/");
      strcat(c2,DEFAULT_FILE);

      if (!OpenFile(c2)) {

	/* make appropriate item sensitive */
   			   	
	XtSetSensitive(GW.text, True);
	XtSetSensitive(GW.cut_button, True);
	XtSetSensitive(GW.copy_button, True);
	XtSetSensitive(GW.clear_button, True);
      }
      
      if(strcmp(fileName,EMPTY) == 0)
	printf("Loading %s to buffer ... done\n",DEFAULT_FILE);
      else
	printf("Loading %s to buffer ... done\n",GP.fileName);

      ReadDefCB(w, client_data, call_data); 

      printf("Loading default Auto-Constants to buffer ... done\n");

      /* popdown the file selection box */

      XtUnmanageChild(GW.new_dialog);
    }
    break;
    
  case DIALOG_CWARNING:
	
    /* save the file */
	
    if (SaveFile()) {
      CloseFile(); /* close the file */
    } else
      fprintf(stderr,"Warning: unable to save file, file not closed");
    break;
    
  case DIALOG_XWARNING:
	
    /* save the file */
	
    if (SaveFile()) {
      CloseFile(); /* close the file */
      exit(0);
    } else
      fprintf(stderr,
	      "Warning: unable to save file, exit aborted");
    break;

  case DIALOG_SAVE:
	
    {
      XmSelectionBoxCallbackStruct *scb =
	(XmSelectionBoxCallbackStruct *) call_data;
      
      /* get the filename string from the file
	 selection box */
		
      XmStringGetLtoR(scb->value, XmSTRING_DEFAULT_CHARSET, &fileName);

      strcpy(GP.fileName,fileName);
     

      strcat(GP.fileName,".f90");

 
      SaveFile();
      
      XtUnmanageChild(GW.save_dialog);
    }
    break;
    
  case DIALOG_PRINT:

    /* save the file */

    if (SaveFile()) {
      if ( strcmp(GP.fileName,EMPTY) != 0) {
	
	/* malloc space for the command name. 
	   Note: command = size of the filename +
	   "lp " + null terminator */
        /*
	command = XtMalloc(strlen(GP.file_name) + 4);
	sprintf(command, "lp %s\0", GP.file_name);*/
	strcpy(GP.command,"lp < ");
	strcat(GP.command,GP.fileName);
	strcat(GP.command," &");
	if (system(GP.command) != 0)
	  fprintf(stderr, "print failed");
	/* XtFree(command);*/
      }
    } else
      fprintf(stderr, 
	      "Warning: unable to save file, file not printed");
    break;
    
  case DIALOG_HELP:
	
    /* no help at this time */
	
    break;
    
  default:
	
    /* unknown callback type */
	
    fprintf (stderr, "Warning: in accept callback\n");
    break;
  }
}


/*
**		Create special 4 button message box out of a
**	Selection box.
*/

#ifdef _NO_PROTO
 Widget CreateSpWarningDialog 
(parent, name, image_string, message,arglist, argcount)
					  
Widget		parent;
String		name;
String		image_string;
String		message;
ArgList		arglist;
int		argcount;
#else
 Widget CreateSpWarningDialog 
(					  
Widget		parent,
String		name,
String		image_string,
String		message,
ArgList		arglist,
int		argcount
)
#endif

{
  Widget        warning_dialog;	/*  special warning selection box */
  Widget       	work_area;	/*  rowcolumn for pixmap and text */
  Widget       	pixmap_label;	/*  pixmap label 		  */
  Widget       	text_label;	/*  text label 			  */
  Widget       	apply_button;	/*  apply button		  */
  Widget       	ok_button;	/*  ok button			  */
  Widget        kid[5];         /*  buttons		          */
  Pixel		foreground;	/*  dialog foreground		  */
  Pixel		background;	/*  dialog background		  */
  Pixmap       	pixmap;		/*  dialog pixmap		  */
  register int  i;              /*  kid index			  */
  Arg           al[10];         /*  arg list		          */
  register int  ac;             /*  arg count		          */


  warning_dialog = XmCreatePromptDialog(parent, name, arglist, argcount);

  ac = 0;
  XtSetArg(al[ac], XmNorientation, XmHORIZONTAL); ac++;
  work_area = XmCreateRowColumn(warning_dialog, "workarea", al, ac);
  XtManageChild(work_area);

  ac = 0;
  XtSetArg(al[ac], XmNforeground, &foreground); ac++;
  XtSetArg(al[ac], XmNbackground, &background); ac++;
  XtGetValues(warning_dialog, al, ac);

  ac = 0;
  XtSetArg(al[ac], XmNlabelType, XmPIXMAP); ac++;
  pixmap = XmGetPixmap(XtScreen(warning_dialog), image_string,
		       foreground, background);
  XtSetArg(al[ac], XmNlabelPixmap, pixmap); ac++;
  pixmap_label = XmCreateLabelGadget(work_area, "pixmap_label", al, ac);
  XtManageChild(pixmap_label);

  ac = 0;
  XtSetArg(al[ac], XmNlabelString,
	   XmStringCreateLtoR(message, XmSTRING_DEFAULT_CHARSET)); ac++;
  text_label = XmCreateLabelGadget(work_area, "text_label", al, ac);
  XtManageChild(text_label);

  apply_button = XmSelectionBoxGetChild (warning_dialog,
					 XmDIALOG_APPLY_BUTTON);
	
  ac = 0;
  XtSetArg(al[ac], XmNlabelString,
	   XmStringCreateLtoR("Discard", XmSTRING_DEFAULT_CHARSET)); ac++;
  XtSetValues(apply_button, al, ac);
  XtManageChild(apply_button);

  ok_button = XmSelectionBoxGetChild (warning_dialog,
				      XmDIALOG_OK_BUTTON);
  ac = 0;
  XtSetArg(al[ac], XmNlabelString,
	   XmStringCreateLtoR("Save", XmSTRING_DEFAULT_CHARSET)); ac++;
  XtSetValues(ok_button, al, ac);

	
  /*      Unmanage unneeded children.	*/
        
  i = 0;
  kid[i++] = XmSelectionBoxGetChild (warning_dialog, XmDIALOG_TEXT);
   kid[i++] = XmSelectionBoxGetChild (warning_dialog,
				     XmDIALOG_SELECTION_LABEL);
  XtUnmanageChildren (kid, i);

  return(warning_dialog);
}


/*
***********************************************************************
**
**			CreateFileMenu.c
**                           
************************************************************************
*/

#ifdef _NO_PROTO
void	CreateFileMenu(menuBar)
Widget	menuBar;
#else
void	CreateFileMenu(Widget menuBar)
#endif

{
  Widget filePDPN,fileCSCD,openPUSH,exitPUSH,lineSEPT;
  Widget openFile,exitFile,newPUSH,newFile;

  filePDPN = XmCreatePulldownMenu(menuBar,"FilePullDown",NULL,0);
  fileCSCD = XmCreateCascadeButton(menuBar,"File",NULL,0);
  XtVaSetValues(fileCSCD,XmNsubMenuId,filePDPN,XmNmnemonic,'F',NULL);
  XtManageChild(fileCSCD);

/*
  XtManageChild(filePDPN);
*/

  newPUSH = XmCreatePushButtonGadget(filePDPN,"New",NULL,0); 
  XtVaSetValues(newPUSH,XmNmnemonic,'N',NULL);
  XtManageChild(newPUSH);

  newFile = CreateNewFileDialog(menuBar,XEDIT_OPTION);
  XtAddCallback(newPUSH,XmNactivateCallback,PopupCB,newFile);  

  openPUSH = XmCreatePushButtonGadget(filePDPN,"Open",NULL,0); 
  XtVaSetValues(openPUSH,XmNmnemonic,'O',NULL);
  XtManageChild(openPUSH);

  openFile = CreateFileSelectionDialog(menuBar,XEDIT_OPTION);
  XtAddCallback(openPUSH,XmNactivateCallback,PopupCB,openFile);

  lineSEPT = XmCreateSeparatorGadget(filePDPN,"Separator",NULL,0);
  XtManageChild(lineSEPT);

  exitPUSH = XmCreatePushButtonGadget(filePDPN,"Exit",NULL,0);  
  XtVaSetValues(exitPUSH,XmNmnemonic,'E',NULL);
  XtManageChild(exitPUSH);
 
  exitFile = CreateQuestionDialog(menuBar);
  XtAddCallback(exitPUSH,XmNactivateCallback,PopupCB,exitFile);

}


#ifdef _NO_PROTO
Widget	CreateFileSelectionDialog(parent,editor)
Widget	parent;
int     editor;
#else
Widget	CreateFileSelectionDialog(Widget parent, int editor)
#endif

{
  Widget rtrn;
  static ClientData *clientData;

  rtrn=XmCreateFileSelectionDialog(parent,"Selection",NULL,0);


  clientData = (ClientData*) malloc(sizeof(ClientData));
  clientData->widget = rtrn;
  clientData->data   = editor;

  XtAddCallback(rtrn,XmNokCallback,SelOkCB,clientData);
  XtAddCallback(rtrn,XmNcancelCallback,PopdownCB,rtrn);
  XtAddCallback(rtrn,XmNhelpCallback,PopdownCB,rtrn);  

/*
  free(clientData);
*/

  return rtrn;
}


#ifdef _NO_PROTO
void	SelOkCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void	SelOkCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  char *filename,*dir,name[100];
  Widget wgt,help;
  XmString s1,s2;
  XmFileSelectionBoxCallbackStruct *cbs;
  ClientData *clientData;
  int editor;
  

  clientData = (ClientData*) userData; 
  wgt        = clientData->widget;
  editor     = clientData->data;
  XtUnmanageChild(wgt);

  cbs = (XmFileSelectionBoxCallbackStruct *) callbackArg;
  XmStringGetLtoR(cbs->value,XmSTRING_DEFAULT_CHARSET,&filename);
  XmStringGetLtoR(cbs->dir,XmSTRING_DEFAULT_CHARSET,&dir);

  if(fopen(filename,"r") != NULL) {
   /*
    strcpy(GP.fileName,filename);
    */
    if(editor==XEDIT_OPTION) {
      	
      strcpy(name,GP.xedit);
    }
    else if(editor==EMACS_OPTION) {
      	
      strcpy(name,GP.emacs);
    }
    else
      printf("Editor is not available\n");

    strcat(name," ");
    /*
    strcat(GP.command,GP.fileName);
    */
    strcat(name,filename);
    /*
    GP.fileName[0]='\0';
    */
    strcat(name," &");
    system(name);

  }
  else { 
    while(*dir++)
      ++filename;
    strcpy(name,"File \"");
    strcat(name,filename);
    strcat(name,"\" ");
    strcat(name,"does not exist!");
    s1=XmStringCreateLtoR(name,
			XmSTRING_DEFAULT_CHARSET);
    s2=XmStringCreateLtoR("Quit",XmSTRING_DEFAULT_CHARSET);
    wgt = XmCreateMessageDialog(w,"Message",NULL,0);
    help=XmMessageBoxGetChild(wgt,XmDIALOG_HELP_BUTTON);
    XtUnmanageChild(help);
    help=XmMessageBoxGetChild(wgt,XmDIALOG_CANCEL_BUTTON);
    XtUnmanageChild(help);
    XtVaSetValues(wgt,XmNmessageString,s1,XmNokLabelString,s2,NULL);
    XmStringFree(s1);
    XmStringFree(s2);
    XtManageChild(wgt);
  }

}


#ifdef _NO_PROTO
void	PostItCB(w,popup,event,b)
Widget	w,popup;
XButtonEvent *event;
Boolean *b;
#else
void	PostItCB(Widget w,XtPointer popup,XEvent * event, Boolean *b)
#endif


{

  if(((XButtonEvent *)event)->button != Button3)
    return;

  XmMenuPosition(popup,(XButtonEvent *)event);
  XtManageChild(popup);

}



#ifdef _NO_PROTO
void	PostItCB1(w,popup,event,b)
Widget	w,popup;
XButtonEvent *event;
Boolean *b;
#else
void	PostItCB1(Widget w,XtPointer popup,XEvent * event, Boolean *b)
#endif


{

  if(((XButtonEvent *)event)->button != Button1)
    return;

  XmMenuPosition(popup,(XButtonEvent *)event);
  XtManageChild(popup);

}


#ifdef _NO_PROTO
void	PopupCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void	PopupCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
     Widget wgt;

     wgt = (Widget) userData;
     XtManageChild(wgt);
}


#ifdef _NO_PROTO
void	PopupCB1(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void	PopupCB1(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
     ClientData *tmp;
     char name[10];
     int i=0,who;
     
     tmp = (ClientData *) userData;

     if(strcmp(GP.fileName,EMPTY) != 0) {
       strcpy(name,ProgramName(GP.fileName));
       while(name[i] != '.')
	 ++i;
       name[i] = '\0';
       if(tmp->data == 1)
	 XtVaSetValues(GW.copyFrom,XmNvalue,name,NULL);
       else if(tmp->data == 2)
	 XtVaSetValues(GW.moveFrom,XmNvalue,name,NULL);
       else if(tmp->data == 3)
	 XtVaSetValues(GW.appendFrom,XmNvalue,name,NULL);
       else
	 printf("Error : in PopupCB1\n");
     }

     XtManageChild(tmp->widget);
}



#ifdef _NO_PROTO
void	PopdownCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void	PopdownCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
     Widget wgt;

     wgt = (Widget) userData;
     XtUnmanageChild(wgt);
}


#ifdef _NO_PROTO
Widget	CreateNewFileDialog(parent,editor)
Widget	parent;
int     editor;
#else
Widget	CreateNewFileDialog(Widget parent,int editor)
#endif

{
  Widget 	rtrn,wgt;
  XmString      s1,s2,s3;


  rtrn = XmCreatePromptDialog(parent,"Prompt",NULL,0);

  s1=XmStringCreateLtoR("Enter the New File Name",XmSTRING_DEFAULT_CHARSET);  
  s2=XmStringCreateLtoR("Reset",XmSTRING_DEFAULT_CHARSET);
  s3=XmStringCreateLtoR("Quit",XmSTRING_DEFAULT_CHARSET);

  wgt=XmSelectionBoxGetChild(rtrn,XmDIALOG_APPLY_BUTTON);
  XtManageChild(wgt);
  wgt=XmSelectionBoxGetChild(rtrn,XmDIALOG_HELP_BUTTON);
  XtUnmanageChild(wgt);

  XtVaSetValues(rtrn,XmNselectionLabelString,s1,
               XmNapplyLabelString,s2,XmNcancelLabelString,s3,
               NULL);

  XmStringFree(s1);
  XmStringFree(s2);
  XmStringFree(s3);

  XtAddCallback(rtrn,XmNokCallback,EntOkCB,(XtPointer)(size_t)editor);
  XtAddCallback(rtrn,XmNapplyCallback,EntCancelCB,rtrn);  
  XtAddCallback(rtrn,XmNcancelCallback,EntCancelCB,rtrn);  

  return rtrn;
}

#ifdef _NO_PROTO
void	EntOkCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void	EntOkCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
   Widget wgt,help;
   char *filename,name[100];
   XmString s1,s2;
   XmSelectionBoxCallbackStruct *cbs;
   int editor;

   editor = (size_t) userData;
   cbs = (XmSelectionBoxCallbackStruct *) callbackArg;
   XmStringGetLtoR(cbs->value,XmSTRING_DEFAULT_CHARSET,&filename);
 
   if(XmStringEmpty(cbs->value) || noting(filename)) {

     strcpy(name,"No file Entered !");
     s1=XmStringCreateLtoR(name,
			   XmSTRING_DEFAULT_CHARSET);
     s2=XmStringCreateLtoR("Quit",XmSTRING_DEFAULT_CHARSET);
     wgt = XmCreateMessageDialog(w,"Message",NULL,0);
     help=XmMessageBoxGetChild(wgt,XmDIALOG_HELP_BUTTON);
     XtUnmanageChild(help);
     help=XmMessageBoxGetChild(wgt,XmDIALOG_CANCEL_BUTTON);
     XtUnmanageChild(help);
     XtVaSetValues(wgt,XmNmessageString,s1,XmNokLabelString,s2,NULL);
     XmStringFree(s1);
     XmStringFree(s2);
     XtManageChild(wgt);

   }

   else {
     while(*filename == ' ' || *filename == '\t')
       ++filename;
     if(fopen(filename,"r") == NULL)	{
       /*
       strcpy(GP.fileName,filename);
        */
       if(editor==XEDIT_OPTION) {
	 	
	 strcpy(name,GP.xedit);
       }
       else if(editor==EMACS_OPTION) {
	 	
	 strcpy(name,GP.emacs);
       }
       else
	printf("Editor is not available\n");

       strcat(name," ");
       strcat(name,filename);
       GP.fileName[0]='\0';
       strcat(name," &");
       system(name);
       XtVaSetValues(w,XmNtextString,"",NULL);
     }

     else {

       XtVaSetValues(w,XmNtextString,"",NULL);
       strcpy(name,"File \"");
       strcat(name,filename);
       strcat(name,"\" ");
       strcat(name,"exist!");
       s1=XmStringCreateLtoR(name,
			     XmSTRING_DEFAULT_CHARSET);
       s2=XmStringCreateLtoR("Quit",XmSTRING_DEFAULT_CHARSET);
       wgt = XmCreateMessageDialog(w,"Message",NULL,0);
       help=XmMessageBoxGetChild(wgt,XmDIALOG_HELP_BUTTON);
       XtUnmanageChild(help);
       help=XmMessageBoxGetChild(wgt,XmDIALOG_CANCEL_BUTTON);
       XtUnmanageChild(help);
       XtVaSetValues(wgt,XmNmessageString,s1,XmNokLabelString,s2,NULL);
       XmStringFree(s1);
       XmStringFree(s2);
       XtManageChild(wgt);

     }
   }
}


#ifdef _NO_PROTO
Boolean	noting(s)
char *s;
#else
Boolean	noting(char *s)
#endif

{
   while(*s == ' ') ++s;

   if(*s == '\0') 
     return True;
   else
     return False;
}

#ifdef _NO_PROTO
void	EntCancelCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void	EntCancelCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  XtVaSetValues(w,XmNtextString,"",NULL);
}

#ifdef _NO_PROTO
Widget	CreateQuestionDialog(parent)
Widget	parent;
#else
Widget	CreateQuestionDialog(Widget parent)
#endif

{
  Widget help,rtrn;
  XmString s1,s2,s3;

  rtrn=XmCreateQuestionDialog(parent,"Question",NULL,0);
  XtAddCallback(rtrn,XmNokCallback,ExitCB,NULL);

  s1=XmStringCreateLtoR("Do you really want to exit?",
			XmSTRING_DEFAULT_CHARSET);
  s2=XmStringCreateLtoR("Yes",XmSTRING_DEFAULT_CHARSET);
  s3=XmStringCreateLtoR("No",XmSTRING_DEFAULT_CHARSET);

  help=XmMessageBoxGetChild(rtrn,XmDIALOG_HELP_BUTTON);
  XtUnmanageChild(help);

  XtVaSetValues(rtrn,XmNmessageString,s1,XmNokLabelString,s2,
		XmNcancelLabelString,s3,NULL);

  XmStringFree(s1);
  XmStringFree(s2);
  XmStringFree(s3);

  return rtrn;
}


#ifdef _NO_PROTO
void	ExitCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void	ExitCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  exit(0);
}

/*
***********************************************************************
**
**			CreateSettingMenu.c
**                           
************************************************************************
*/


#ifdef _NO_PROTO
void	CreateSettingMenu(menuBar)
Widget  menuBar;	
#else
void	CreateSettingMenu(Widget menuBar)
#endif

{
  Widget setPDPN,setCSCD,lineSEPT;
  Widget probPDPN,probCSCD,probFORM,probPUSH;
  Widget fullPUSH,fullFORM; 
  Widget accuPDPN,accuCSCD,accuFORM;
  Widget tolPDPN,tolCSCD,tolPUSH,tolFORM;
  Widget limPDPN,limCSCD,limPUSH,limFORM;
  Widget stepPDPN,stepCSCD,stepPUSH,stepFORM;
  Widget crPDPN,crCSCD,conPDPN,conCSCD,conPUSH,conFORM;
  Widget runPDPN,runCSCD,runPUSH,runFORM;
  Widget outPDPN,outCSCD,outPUSH,outFORM;
 
  Widget ndim,nbc,nint,jac;
  Widget epsl,epsu,epss,itmx,nwtn,itnw;
  Widget nmx,rl0,rl1,a0,a1;
  Widget ds,dsmin,dsmax,iads;
  Widget nicp,icp,ilp,isp,isw,mxbf,irs,ips;
  Widget npr,iid,iplt,nthl,nthu,nuzr,thl,thu,par;

  Widget prompt;

  Widget disPDPN,disCSCD,disPUSH,disFORM;
  Widget ntst,ncol,iad;



  prompt = XmCreatePromptDialog(menuBar,"Prompt",NULL,0);

  setPDPN = XmCreatePulldownMenu(menuBar,"SetPullDown",NULL,0);

/*
  XtManageChild(setPDPN);
*/

  setCSCD = XmCreateCascadeButton(menuBar,"Set",NULL,0);
  XtVaSetValues(setCSCD,XmNsubMenuId,setPDPN,XmNmnemonic,'S',NULL);
  XtManageChild(setCSCD);



  probPDPN = XmCreatePulldownMenu(setPDPN,"ProbPullDown",NULL,0);
  probCSCD = XmCreateCascadeButton(setPDPN,"Problem",NULL,0);
  XtVaSetValues(probCSCD,XmNsubMenuId,probPDPN,XmNmnemonic,'P',NULL);
  XtManageChild(probCSCD);
  
  ndim  = XmCreatePushButtonGadget(probPDPN,"NDIM",NULL,0);
  XtVaSetValues(ndim,XmNmnemonic,'N',NULL);
  XtManageChild(ndim);
  XtAddCallback(ndim,XmNactivateCallback,PopupCB,prompt);

  nbc  = XmCreatePushButtonGadget(probPDPN,"NBC",NULL,0);
  XtVaSetValues(nbc,XmNmnemonic,'B',NULL);
  XtManageChild(nbc);
  XtAddCallback(nbc,XmNactivateCallback,PopupCB,prompt);

  nint  = XmCreatePushButtonGadget(probPDPN,"NINT",NULL,0);
  XtVaSetValues(nint,XmNmnemonic,'I',NULL);
  XtManageChild(nint);
  XtAddCallback(nint,XmNactivateCallback,PopupCB,prompt);

  jac  = XmCreatePushButtonGadget(probPDPN,"JAC",NULL,0);
  XtVaSetValues(jac,XmNmnemonic,'J',NULL);
  XtManageChild(jac);
  XtAddCallback(jac,XmNactivateCallback,PopupCB,prompt);

  lineSEPT = XmCreateSeparatorGadget(probPDPN,"Separator",NULL,0);
  XtManageChild(lineSEPT);

  probFORM = CreateFormProb(menuBar);
  probPUSH=XmCreatePushButtonGadget(probPDPN,"All Above",NULL,0);
  XtVaSetValues(probPUSH,XmNmnemonic,'P',NULL);
  XtManageChild(probPUSH);  
  XtAddCallback(probPUSH,XmNactivateCallback,PopupCB,probFORM);




  accuPDPN = XmCreatePulldownMenu(setPDPN,"AccuPullDown",NULL,0);
  accuCSCD = XmCreateCascadeButton(setPDPN,"Accuracy",NULL,0);
  XtVaSetValues(accuCSCD,XmNsubMenuId,accuPDPN,XmNmnemonic,'A',NULL);
  XtManageChild(accuCSCD);



  disPDPN = XmCreatePulldownMenu(accuPDPN,"DisPullDown",NULL,0);
  disCSCD = XmCreateCascadeButton(accuPDPN,"Discretization",NULL,0);
  XtVaSetValues(disCSCD,XmNsubMenuId,disPDPN,XmNmnemonic,'D',NULL);
  XtManageChild(disCSCD);

   
  ntst   = XmCreatePushButtonGadget(disPDPN,"NTST",NULL,0);
  XtVaSetValues(ntst,XmNmnemonic,'N',NULL);
  XtManageChild(ntst);
  XtAddCallback(ntst,XmNactivateCallback,PopupCB,prompt);

  ncol   = XmCreatePushButtonGadget(disPDPN,"NCOL",NULL,0);
  XtVaSetValues(ncol,XmNmnemonic,'C',NULL);
  XtManageChild(ncol);
  XtAddCallback(ncol,XmNactivateCallback,PopupCB,prompt);

  iad   = XmCreatePushButtonGadget(disPDPN,"IAD",NULL,0);
  XtVaSetValues(iad,XmNmnemonic,'I',NULL);
  XtManageChild(iad);
  XtAddCallback(iad,XmNactivateCallback,PopupCB,prompt);


  lineSEPT = XmCreateSeparatorGadget(disPDPN,"Separator",NULL,0);
  XtManageChild(lineSEPT);

  disFORM = CreateFormDis(menuBar);
  disPUSH=XmCreatePushButtonGadget(disPDPN,"All Above",NULL,0);
  XtVaSetValues(disPUSH,XmNmnemonic,'A',NULL);
  XtManageChild(disPUSH);  
  XtAddCallback(disPUSH,XmNactivateCallback,PopupCB,disFORM);


  tolPDPN = XmCreatePulldownMenu(accuPDPN,"TolPullDown",NULL,0);
  tolCSCD = XmCreateCascadeButton(accuPDPN,"Tolerance",NULL,0);
  XtVaSetValues(tolCSCD,XmNsubMenuId,tolPDPN,XmNmnemonic,'T',NULL);
  XtManageChild(tolCSCD);
   
  epsl   = XmCreatePushButtonGadget(tolPDPN,"EPSL",NULL,0);
  XtVaSetValues(epsl,XmNmnemonic,'E',NULL);
  XtManageChild(epsl);
  XtAddCallback(epsl,XmNactivateCallback,PopupCB,prompt);

  epsu   = XmCreatePushButtonGadget(tolPDPN,"EPSU",NULL,0);
  XtVaSetValues(epsu,XmNmnemonic,'P',NULL);
  XtManageChild(epsu);
  XtAddCallback(epsu,XmNactivateCallback,PopupCB,prompt);

  epss   = XmCreatePushButtonGadget(tolPDPN,"EPSS",NULL,0);
  XtVaSetValues(epss,XmNmnemonic,'S',NULL);
  XtManageChild(epss);
  XtAddCallback(epss,XmNactivateCallback,PopupCB,prompt);

  itmx   = XmCreatePushButtonGadget(tolPDPN,"ITMX",NULL,0);
  XtVaSetValues(itmx,XmNmnemonic,'I',NULL);
  XtManageChild(itmx);
  XtAddCallback(itmx,XmNactivateCallback,PopupCB,prompt);

  nwtn   = XmCreatePushButtonGadget(tolPDPN,"NWTN",NULL,0);
  XtVaSetValues(nwtn,XmNmnemonic,'N',NULL);
  XtManageChild(nwtn);
  XtAddCallback(nwtn,XmNactivateCallback,PopupCB,prompt);

  itnw   = XmCreatePushButtonGadget(tolPDPN,"ITNW",NULL,0);
  XtVaSetValues(itnw,XmNmnemonic,'T',NULL);
  XtManageChild(itnw);
  XtAddCallback(itnw,XmNactivateCallback,PopupCB,prompt);


  lineSEPT = XmCreateSeparatorGadget(tolPDPN,"Separator",NULL,0);
  XtManageChild(lineSEPT);


  tolFORM = CreateFormTol(menuBar);
  tolPUSH=XmCreatePushButtonGadget(tolPDPN,"All Above",NULL,0);
  XtVaSetValues(tolPUSH,XmNmnemonic,'A',NULL);
  XtManageChild(tolPUSH);  
  XtAddCallback(tolPUSH,XmNactivateCallback,PopupCB,tolFORM);
    


  stepPDPN = XmCreatePulldownMenu(accuPDPN,"StepPullDown",NULL,0);
  stepCSCD = XmCreateCascadeButton(accuPDPN,"Step Size",NULL,0);
  XtVaSetValues(stepCSCD,XmNsubMenuId,stepPDPN,XmNmnemonic,'S',NULL);
  XtManageChild(stepCSCD);

  ds   = XmCreatePushButtonGadget(stepPDPN,"DS",NULL,0);
  XtVaSetValues(ds,XmNmnemonic,'S',NULL);
  XtManageChild(ds);
  XtAddCallback(ds,XmNactivateCallback,PopupCB,prompt);

  dsmin   = XmCreatePushButtonGadget(stepPDPN,"DSMIN",NULL,0);
  XtVaSetValues(dsmin,XmNmnemonic,'M',NULL);
  XtManageChild(dsmin);
  XtAddCallback(dsmin,XmNactivateCallback,PopupCB,prompt);

  dsmax   = XmCreatePushButtonGadget(stepPDPN,"DSMAX",NULL,0);
  XtVaSetValues(dsmax,XmNmnemonic,'X',NULL);
  XtManageChild(dsmax);
  XtAddCallback(dsmax,XmNactivateCallback,PopupCB,prompt);

  iads   = XmCreatePushButtonGadget(stepPDPN,"IADS",NULL,0);
  XtVaSetValues(iads,XmNmnemonic,'I',NULL);
  XtManageChild(iads);
  XtAddCallback(iads,XmNactivateCallback,PopupCB,prompt);


  thl   = XmCreatePushButtonGadget(stepPDPN,"THL",NULL,0);
  XtVaSetValues(thl,XmNmnemonic,'L',NULL);
  XtManageChild(thl);
  XtAddCallback(thl,XmNactivateCallback,PopupCB,prompt);


  thu   = XmCreatePushButtonGadget(stepPDPN,"THU",NULL,0);
  XtVaSetValues(thu,XmNmnemonic,'U',NULL);
  XtManageChild(thu);
  XtAddCallback(thu,XmNactivateCallback,PopupCB,prompt);


  lineSEPT = XmCreateSeparatorGadget(stepPDPN,"Separator",NULL,0);
  XtManageChild(lineSEPT);


  stepFORM = CreateFormStep(menuBar);
  stepPUSH=XmCreatePushButtonGadget(stepPDPN,"All Above",NULL,0);
  XtVaSetValues(stepPUSH,XmNmnemonic,'A',NULL);
  XtManageChild(stepPUSH);  
  XtAddCallback(stepPUSH,XmNactivateCallback,PopupCB,stepFORM);



  limPDPN = XmCreatePulldownMenu(accuPDPN,"LimPullDown",NULL,0);
  limCSCD = XmCreateCascadeButton(accuPDPN,"Limits",NULL,0);
  XtVaSetValues(limCSCD,XmNsubMenuId,limPDPN,XmNmnemonic,'L',NULL);
  XtManageChild(limCSCD);
  

  nmx   = XmCreatePushButtonGadget(limPDPN,"NMX",NULL,0);
  XtVaSetValues(nmx,XmNmnemonic,'N',NULL);
  XtManageChild(nmx);
  XtAddCallback(nmx,XmNactivateCallback,PopupCB,prompt);

  rl0   = XmCreatePushButtonGadget(limPDPN,"RL0",NULL,0);
  XtVaSetValues(rl0,XmNmnemonic,'L',NULL);
  XtManageChild(rl0);
  XtAddCallback(rl0,XmNactivateCallback,PopupCB,prompt);

  rl1   = XmCreatePushButtonGadget(limPDPN,"RL1",NULL,0);
  XtVaSetValues(rl1,XmNmnemonic,'R',NULL);
  XtManageChild(rl1);
  XtAddCallback(rl1,XmNactivateCallback,PopupCB,prompt);

  a0   = XmCreatePushButtonGadget(limPDPN,"A0",NULL,0);
  XtVaSetValues(a0,XmNmnemonic,'0',NULL);
  XtManageChild(a0);
  XtAddCallback(a0,XmNactivateCallback,PopupCB,prompt);

  a1   = XmCreatePushButtonGadget(limPDPN,"A1",NULL,0);
  XtVaSetValues(a1,XmNmnemonic,'A',NULL);
  XtManageChild(a1);
  XtAddCallback(a1,XmNactivateCallback,PopupCB,prompt);

  lineSEPT = XmCreateSeparatorGadget(limPDPN,"Separator",NULL,0);
  XtManageChild(lineSEPT);

  limFORM=CreateFormLim(menuBar);
  limPUSH=XmCreatePushButtonGadget(limPDPN,"All Above",NULL,0);
  XtVaSetValues(limPUSH,XmNmnemonic,'A',NULL);
  XtManageChild(limPUSH);  
  XtAddCallback(limPUSH,XmNactivateCallback,PopupCB,limFORM);



  crPDPN = XmCreatePulldownMenu(setPDPN,"CrPullDown",NULL,0);
  crCSCD = XmCreateCascadeButton(setPDPN,"Cont+Run",NULL,0);
  XtVaSetValues(crCSCD,XmNsubMenuId,crPDPN,XmNmnemonic,'C',NULL);
  XtManageChild(crCSCD);

    
  conPDPN = XmCreatePulldownMenu(crPDPN,"ConPullDown",NULL,0);
  conCSCD = XmCreateCascadeButton(crPDPN,"Continuation",NULL,0);
  XtVaSetValues(conCSCD,XmNsubMenuId,conPDPN,XmNmnemonic,'C',NULL);
  XtManageChild(conCSCD);


  nicp   = XmCreatePushButtonGadget(conPDPN,"NICP",NULL,0);
  XtVaSetValues(nicp,XmNmnemonic,'N',NULL);
  XtManageChild(nicp);
  XtAddCallback(nicp,XmNactivateCallback,PopupCB,prompt);
  
  icp   = XmCreatePushButtonGadget(conPDPN,"ICP",NULL,0);
  XtVaSetValues(icp,XmNmnemonic,'I',NULL);
  XtManageChild(icp);
  XtAddCallback(icp,XmNactivateCallback,PopupCB,prompt);

  lineSEPT = XmCreateSeparatorGadget(conPDPN,"Separator",NULL,0);
  XtManageChild(lineSEPT);


  conFORM = CreateFormCon(menuBar);
  conPUSH=XmCreatePushButtonGadget(conPDPN,"All Above",NULL,0);
  XtVaSetValues(conPUSH,XmNmnemonic,'A',NULL);
  XtManageChild(conPUSH);  
  XtAddCallback(conPUSH,XmNactivateCallback,PopupCB,conFORM);

  runPDPN = XmCreatePulldownMenu(crPDPN,"RunPullDown",NULL,0);
  runCSCD = XmCreateCascadeButton(crPDPN,"Run",NULL,0);
  XtVaSetValues(runCSCD,XmNsubMenuId,runPDPN,XmNmnemonic,'R',NULL);
  XtManageChild(runCSCD);

  
  ilp   = XmCreatePushButtonGadget(runPDPN,"ILP",NULL,0);
  XtVaSetValues(ilp,XmNmnemonic,'I',NULL);
  XtManageChild(ilp);
  XtAddCallback(ilp,XmNactivateCallback,PopupCB,prompt);

  isp   = XmCreatePushButtonGadget(runPDPN,"ISP",NULL,0);
  XtVaSetValues(isp,XmNmnemonic,'S',NULL);
  XtManageChild(isp);
  XtAddCallback(isp,XmNactivateCallback,PopupCB,prompt);
  
  isw   = XmCreatePushButtonGadget(runPDPN,"ISW",NULL,0);
  XtVaSetValues(isw,XmNmnemonic,'W',NULL);
  XtManageChild(isw);
  XtAddCallback(isw,XmNactivateCallback,PopupCB,prompt);
  
  mxbf   = XmCreatePushButtonGadget(runPDPN,"MXBF",NULL,0);
  XtVaSetValues(mxbf,XmNmnemonic,'M',NULL);
  XtManageChild(mxbf);
  XtAddCallback(mxbf,XmNactivateCallback,PopupCB,prompt);
  
  irs   = XmCreatePushButtonGadget(runPDPN,"IRS",NULL,0);
  XtVaSetValues(irs,XmNmnemonic,'R',NULL);
  XtManageChild(irs);
  XtAddCallback(irs,XmNactivateCallback,PopupCB,prompt);
  
  ips   = XmCreatePushButtonGadget(runPDPN,"IPS",NULL,0);
  XtVaSetValues(ips,XmNmnemonic,'P',NULL);
  XtManageChild(ips);
  XtAddCallback(ips,XmNactivateCallback,PopupCB,prompt);

  lineSEPT = XmCreateSeparatorGadget(runPDPN,"Separator",NULL,0);
  XtManageChild(lineSEPT);

  runFORM=CreateFormRun(menuBar);
  runPUSH=XmCreatePushButtonGadget(runPDPN,"All Above",NULL,0);
  XtVaSetValues(runPUSH,XmNmnemonic,'A',NULL);
  XtManageChild(runPUSH);  
  XtAddCallback(runPUSH,XmNactivateCallback,PopupCB,runFORM);



  outPDPN = XmCreatePulldownMenu(setPDPN,"OutPullDown",NULL,0);
  outCSCD = XmCreateCascadeButton(setPDPN,"Output",NULL,0);
  XtVaSetValues(outCSCD,XmNsubMenuId,outPDPN,XmNmnemonic,'O',NULL);
  XtManageChild(outCSCD);

  npr   = XmCreatePushButtonGadget(outPDPN,"NPR",NULL,0);
  XtVaSetValues(npr,XmNmnemonic,'N',NULL);
  XtManageChild(npr);
  XtAddCallback(npr,XmNactivateCallback,PopupCB,prompt);

  iid   = XmCreatePushButtonGadget(outPDPN,"IID",NULL,0);
  XtVaSetValues(iid,XmNmnemonic,'D',NULL);
  XtManageChild(iid);
  XtAddCallback(iid,XmNactivateCallback,PopupCB,prompt);

  iplt   = XmCreatePushButtonGadget(outPDPN,"IPLT",NULL,0);
  XtVaSetValues(iplt,XmNmnemonic,'I',NULL);
  XtManageChild(iplt);
  XtAddCallback(iplt,XmNactivateCallback,PopupCB,prompt);

/*
  nthl   = XmCreatePushButtonGadget(outPDPN,"NTHL",NULL,0);
  XtVaSetValues(nthl,XmNmnemonic,'T',NULL);
  XtManageChild(nthl);
  XtAddCallback(nthl,XmNactivateCallback,PopupCB,prompt);

  nthu   = XmCreatePushButtonGadget(outPDPN,"NTHU",NULL,0);
  XtVaSetValues(nthu,XmNmnemonic,'H',NULL);
  XtManageChild(nthu);
  XtAddCallback(nthu,XmNactivateCallback,PopupCB,prompt);
*/
  nuzr   = XmCreatePushButtonGadget(outPDPN,"UZR",NULL,0);
  XtVaSetValues(nuzr,XmNmnemonic,'U',NULL);
  XtManageChild(nuzr);
  XtAddCallback(nuzr,XmNactivateCallback,PopupCB,prompt);

/*
  thl   = XmCreatePushButtonGadget(outPDPN,"THL",NULL,0);
  XtVaSetValues(thl,XmNmnemonic,'L',NULL);
  XtManageChild(thl);
  XtAddCallback(thl,XmNactivateCallback,PopupCB,prompt);

  thu   = XmCreatePushButtonGadget(outPDPN,"THU",NULL,0);
  XtVaSetValues(thu,XmNmnemonic,'U',NULL);
  XtManageChild(thu);
  XtAddCallback(thu,XmNactivateCallback,PopupCB,prompt);


  par   = XmCreatePushButtonGadget(outPDPN,"PAR",NULL,0);
  XtVaSetValues(par,XmNmnemonic,'P',NULL);
  XtManageChild(par);
  XtAddCallback(par,XmNactivateCallback,PopupCB,prompt);

*/

  lineSEPT = XmCreateSeparatorGadget(outPDPN,"Separator",NULL,0);
  XtManageChild(lineSEPT);

  outFORM=CreateFormOut(menuBar);
  outPUSH=XmCreatePushButtonGadget(outPDPN,"All Above",NULL,0);
  XtVaSetValues(outPUSH,XmNmnemonic,'A',NULL);
  XtManageChild(outPUSH);  
  XtAddCallback(outPUSH,XmNactivateCallback,PopupCB,outFORM);


  lineSEPT    = XmCreateSeparatorGadget(setPDPN,"Separator ",NULL,0);
  XtManageChild(lineSEPT);



  fullPUSH = XmCreatePushButtonGadget(setPDPN,"Full Panel",NULL,0);
  XtVaSetValues(fullPUSH,XmNmnemonic,'F',NULL);
  XtManageChild(fullPUSH);

  fullFORM = CreateFormDefault(menuBar);
  XtAddCallback(fullPUSH,XmNactivateCallback,PopupCB,fullFORM);

}


#ifdef _NO_PROTO
Widget	CreateFormProb(parent)
Widget	parent;
#else
Widget	CreateFormProb(Widget parent)
#endif

{
  Widget rtrn,rwcl,ok,apply,cancel,help;
  static ClientData userData1,userData2;


/*
  rtrn   = XmCreateFormDialog(parent,"DialogShell",NULL,0);
*/


  rtrn   = XmCreateFormDialog(parent,"DialogShell",NULL,0);
  PopupTemplate1(rtrn,&rwcl,&ok,&apply,&cancel,&help);
  CreateBoxProb(rwcl);

  userData1.widget=rtrn;
  userData1.data=1;

  userData2.widget=rtrn;
  userData2.data=0;

  XtAddCallback(ok,XmNactivateCallback,OkFormProbCB,&userData1);
  XtAddCallback(apply,XmNactivateCallback,OkFormProbCB,&userData2);
  XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);
  XtAddCallback(help,XmNactivateCallback,PopupCB,GW.helpList);

#if 0

  rtrn = XmCreateMessageDialog(parent,"Message",NULL,0);
  XtAddCallback(rtrn,XmNokCallback,OkFormProbCB,NULL);
  XtAddCallback(rtrn,XmNhelpCallback,PopupCB,GW.helpList);


  rwcl = XmCreateRowColumn(rtrn,"RowColumn",NULL,0);
  XtManageChild(rwcl);


  CreateBoxProb(rwcl);


  sept = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
  XtManageChild(sept);
  form = XmCreateForm(rtrn,"Form",NULL,0);
  XtManageChild(form);
  apply = XmCreatePushButtonGadget(form,"Apply",NULL,0);
  XtManageChild(apply);
  reset = XmCreatePushButtonGadget(form,"Reset",NULL,0);
  XtManageChild(reset);
  quit  = XmCreatePushButtonGadget(form,"Quit",NULL,0);
  XtManageChild(quit);
  XtAddCallback(quit,XmNactivateCallback,PopdownCB,rtrn);

  XtVaSetValues(rwcl,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNbottomWidget,sept,
		NULL);

  XtVaSetValues(sept,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,5,
		XmNbottomOffset,5,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNbottomWidget,form,
		NULL);

  XtVaSetValues(form,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		NULL);

  XtVaSetValues(apply,XmNleftAttachment,XmATTACH_FORM,NULL);
  XtVaSetValues(apply,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
                XmNleftOffset,5,
		NULL);

  XtVaSetValues(reset,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_WIDGET,
		XmNleftWidget,apply,
		XmNrightAttachment,XmATTACH_WIDGET,
		XmNrightWidget,quit,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
		XmNleftOffset,5,
		XmNrightOffset,5,
		NULL);

  XtVaSetValues(quit,XmNrightAttachment,XmATTACH_FORM,NULL);
  XtVaSetValues(quit,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
                XmNrightOffset,5,
		NULL);

#endif

  return rtrn;
}



#ifdef _NO_PROTO
void	CreateBoxProb(parent)
Widget	parent;
#else
void	CreateBoxProb(Widget parent)
#endif

{
   Widget rwcl,label,frame;
   Widget popJac;

   XtVaSetValues(parent,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,XmNnumColumns,2,NULL);

   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[0],NULL,0);
   XtManageChild(label);
   GW.probButton[0]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.probButton[0]);
   XtVaSetValues(GW.probButton[0],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[PAR_NDIM],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[1],NULL,0);
   XtManageChild(label);
   GW.probButton[1]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.probButton[1]);
   XtVaSetValues(GW.probButton[1],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[PAR_NBC],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[2],NULL,0);
   XtManageChild(label);
   GW.probButton[2]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.probButton[2]);
   XtVaSetValues(GW.probButton[2],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[PAR_NINT],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[3],NULL,0);
   XtManageChild(label);
   GW.probButton[3]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.probButton[3]);
   XtVaSetValues(GW.probButton[3],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[PAR_JAC],
		 NULL);

   XtVaSetValues(GW.probButton[3],XmNeditable,False,NULL);
   popJac=PopupJac(GW.probButton[3]);
   XtAddEventHandler(GW.probButton[3],ButtonPressMask,False,PostItCB,popJac);

 }




#ifdef _NO_PROTO
void   OkFormProbCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   OkFormProbCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  String value;
  ClientData  *tmp;

#if 0

  SaveParScreen(1);
  SaveParScreen(2);
  SaveParScreen(3);
  SaveParScreen(4);
  SetParScreen();

  tmp = (ClientData*) userData;
  if(tmp->data==1)
    XtUnmanageChild(tmp->widget);


#endif
  
  tmp = (ClientData*) userData; 

  XtVaGetValues(GW.probButton[0],XmNvalue,&value,NULL);
  GP.parValue[PAR_NDIM] = atof(value);
  sprintf(GP.parStrValue[PAR_NDIM],"%d",(int) GP.parValue[PAR_NDIM]);
  XtVaSetValues(GW.parButton[PAR_NDIM],XmNvalue,GP.parStrValue[PAR_NDIM],NULL);
  
  XtVaGetValues(GW.probButton[1],XmNvalue,&value,NULL);
  GP.parValue[PAR_NBC] = atof(value);
  sprintf(GP.parStrValue[PAR_NBC],"%d",(int)GP.parValue[PAR_NBC]);
  XtVaSetValues(GW.parButton[PAR_NBC],XmNvalue,GP.parStrValue[PAR_NBC],NULL);
  
  XtVaGetValues(GW.probButton[2],XmNvalue,&value,NULL);
  GP.parValue[PAR_NINT] = atof(value);
  sprintf(GP.parStrValue[PAR_NINT],"%d",(int)GP.parValue[PAR_NINT]);
  XtVaSetValues(GW.parButton[PAR_NINT],XmNvalue,GP.parStrValue[PAR_NINT],NULL);
  
  XtVaGetValues(GW.probButton[3],XmNvalue,&value,NULL);
  GP.parValue[PAR_JAC] = atof(value);
  sprintf(GP.parStrValue[PAR_JAC],"%d",(int)GP.parValue[PAR_JAC]);
  XtVaSetValues(GW.parButton[PAR_JAC],XmNvalue,GP.parStrValue[PAR_JAC],NULL);
  

  if(tmp->data==1)
    XtUnmanageChild(tmp->widget);



}


#ifdef _NO_PROTO
Widget	CreateFormDis(parent)
Widget	parent;
#else
Widget	CreateFormDis(Widget parent)
#endif

{

  Widget rtrn,rwcl,ok,apply,cancel,help;
  static ClientData userData1,userData2;

  rtrn   = XmCreateFormDialog(parent,"DialogShell",NULL,0);
  PopupTemplate(rtrn,&rwcl,&ok,&apply,&cancel,&help,25);
  CreateBoxDis(rwcl);


  userData1.widget=rtrn;
  userData1.data=1;
  userData2.widget=rtrn;
  userData2.data=0;
  
  XtAddCallback(ok,XmNactivateCallback,OkFormDisCB,&userData1);
  XtAddCallback(apply,XmNactivateCallback,OkFormDisCB,&userData2);
  XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);
  XtAddCallback(help,XmNactivateCallback,PopupCB,GW.helpList);

#if 0

  Widget rtrn,rwcl,sept,form,apply,reset,quit,wgt,frame;

/*
  rtrn   = XmCreateFormDialog(parent,"DialogShell",NULL,0);
*/

  rtrn = XmCreateMessageDialog(parent,"Message",NULL,0);

  XtAddCallback(rtrn,XmNokCallback,OkFormDisCB,NULL);
  XtAddCallback(rtrn,XmNhelpCallback,PopupCB,GW.helpList);

 
  rwcl = XmCreateRowColumn(rtrn,"RowColumn",NULL,0);
  XtManageChild(rwcl);

  CreateBoxDis(rwcl);


  sept = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
  XtManageChild(sept);
  form = XmCreateForm(rtrn,"Form",NULL,0);
  XtManageChild(form);
  apply = XmCreatePushButtonGadget(form,"Apply",NULL,0);
  XtManageChild(apply);
  reset = XmCreatePushButtonGadget(form,"Reset",NULL,0);
  XtManageChild(reset);
  quit  = XmCreatePushButtonGadget(form,"Quit",NULL,0);
  XtManageChild(quit);
  XtAddCallback(quit,XmNactivateCallback,PopdownCB,rtrn);

  XtVaSetValues(rwcl,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNbottomWidget,sept,
		NULL);

  XtVaSetValues(sept,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,5,
		XmNbottomOffset,5,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNbottomWidget,form,
		NULL);

  XtVaSetValues(form,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		NULL);

  XtVaSetValues(apply,XmNleftAttachment,XmATTACH_FORM,NULL);
  XtVaSetValues(apply,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
                XmNleftOffset,5,
		NULL);

  XtVaSetValues(reset,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_WIDGET,
		XmNleftWidget,apply,
		XmNrightAttachment,XmATTACH_WIDGET,
		XmNrightWidget,quit,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
		XmNleftOffset,5,
		XmNrightOffset,5,
		NULL);

  XtVaSetValues(quit,XmNrightAttachment,XmATTACH_FORM,NULL);
  XtVaSetValues(quit,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
                XmNrightOffset,5,
		NULL);

#endif

  return rtrn;
}



#ifdef _NO_PROTO
void	CreateBoxDis(parent)
Widget	parent;
#else
void	CreateBoxDis(Widget parent)
#endif

{
   Widget rwcl,label,frame,popNcol;

   XtVaSetValues(parent,XmNorientation,XmHORIZONTAL,XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,1,NULL);

   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[4],NULL,0);
   XtManageChild(label);
   GW.disButton[0]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.disButton[0]);
   XtVaSetValues(GW.disButton[0],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[PAR_NTST],
		 NULL);

   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[5],NULL,0);
   XtManageChild(label);
   GW.disButton[1]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.disButton[1]);
   XtVaSetValues(GW.disButton[1],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[PAR_NCOL],
		 NULL);

   XtVaSetValues(GW.disButton[1],XmNeditable,False,NULL);
   popNcol=PopupNcol(GW.disButton[1]);
   XtAddEventHandler(GW.disButton[1],ButtonPressMask,False,PostItCB,popNcol);

   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[6],NULL,0);
   XtManageChild(label);
   GW.disButton[2]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.disButton[2]);
   XtVaSetValues(GW.disButton[2],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[PAR_IAD],
		 NULL);

 }




#ifdef _NO_PROTO
void   OkFormDisCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   OkFormDisCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  String value;
  ClientData *tmp;

#if 0

  SaveParScreen(1);
  SaveParScreen(2);
  SaveParScreen(3);
  SaveParScreen(4);
  SetParScreen();

  tmp = (ClientData*) userData;
  if(tmp->data==1)
    XtUnmanageChild(tmp->widget);

#endif

  tmp = (ClientData*) userData;

  XtVaGetValues(GW.disButton[0],XmNvalue,&value,NULL);
  GP.parValue[PAR_NTST] = atof(value);
  sprintf(GP.parStrValue[PAR_NTST],"%d",(int)GP.parValue[PAR_NTST]);
  XtVaSetValues(GW.parButton[PAR_NTST],XmNvalue,GP.parStrValue[PAR_NTST],NULL);
  
  XtVaGetValues(GW.disButton[1],XmNvalue,&value,NULL);
  GP.parValue[PAR_NCOL] = atof(value);
  sprintf(GP.parStrValue[PAR_NCOL],"%d",(int)GP.parValue[PAR_NCOL]);
  XtVaSetValues(GW.parButton[PAR_NCOL],XmNvalue,GP.parStrValue[PAR_NCOL],NULL);
  
  XtVaGetValues(GW.disButton[2],XmNvalue,&value,NULL);
  GP.parValue[PAR_IAD] = atof(value);
  sprintf(GP.parStrValue[PAR_IAD],"%d",(int)GP.parValue[PAR_IAD]);
  XtVaSetValues(GW.parButton[PAR_IAD],XmNvalue,GP.parStrValue[PAR_IAD],NULL);

  if(tmp->data==1)
    XtUnmanageChild(tmp->widget);


}



#ifdef _NO_PROTO
Widget	CreateFormTol(parent)
Widget	parent;
#else
Widget	CreateFormTol(Widget parent)
#endif

{

  Widget rtrn,rwcl,ok,apply,cancel,help;
  static ClientData userData1,userData2;

  rtrn   = XmCreateFormDialog(parent,"DialogShell",NULL,0);
  PopupTemplate(rtrn,&rwcl,&ok,&apply,&cancel,&help,25);
  CreateBoxTol(rwcl);

  userData1.widget=rtrn;
  userData1.data=1;
  userData2.widget=rtrn;
  userData2.data=0;

  XtAddCallback(ok,XmNactivateCallback,OkFormTolCB,&userData1);
  XtAddCallback(apply,XmNactivateCallback,OkFormTolCB,&userData2);
  XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);
  XtAddCallback(help,XmNactivateCallback,PopupCB,GW.helpList);

#if 0

  Widget rtrn,rwcl,sept,form,apply,reset,quit,wgt,frame;

/*
  rtrn   = XmCreateFormDialog(parent,"DialogShell",NULL,0);
*/

  rtrn = XmCreateMessageDialog(parent,"Message",NULL,0);
  XtAddCallback(rtrn,XmNokCallback,OkFormTolCB,NULL);
  XtAddCallback(rtrn,XmNhelpCallback,PopupCB,GW.helpList);
 
  rwcl = XmCreateRowColumn(rtrn,"RowColumn",NULL,0);
  XtManageChild(rwcl);

  CreateBoxTol(rwcl);


  sept = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
  XtManageChild(sept);
  form = XmCreateForm(rtrn,"Form",NULL,0);
  XtManageChild(form);
  apply = XmCreatePushButtonGadget(form,"Apply",NULL,0);
  XtManageChild(apply);
  reset = XmCreatePushButtonGadget(form,"Reset",NULL,0);
  XtManageChild(reset);
  quit  = XmCreatePushButtonGadget(form,"Quit",NULL,0);
  XtManageChild(quit);
  XtAddCallback(quit,XmNactivateCallback,PopdownCB,rtrn);

  XtVaSetValues(rwcl,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNbottomWidget,sept,
		NULL);

  XtVaSetValues(sept,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,5,
		XmNbottomOffset,5,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNbottomWidget,form,
		NULL);

  XtVaSetValues(form,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		NULL);

  XtVaSetValues(apply,XmNleftAttachment,XmATTACH_FORM,NULL);
  XtVaSetValues(apply,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
                XmNleftOffset,5,
		NULL);

  XtVaSetValues(reset,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_WIDGET,
		XmNleftWidget,apply,
		XmNrightAttachment,XmATTACH_WIDGET,
		XmNrightWidget,quit,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
		XmNleftOffset,5,
		XmNrightOffset,5,
		NULL);

  XtVaSetValues(quit,XmNrightAttachment,XmATTACH_FORM,NULL);
  XtVaSetValues(quit,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
                XmNrightOffset,5,
		NULL);

#endif

  return rtrn;
}



#ifdef _NO_PROTO
void	CreateBoxTol(parent)
Widget	parent;
#else
void	CreateBoxTol(Widget parent)
#endif

{
   Widget rwcl,label,frame;

   XtVaSetValues(parent,XmNpacking,XmPACK_COLUMN,XmNorientation,XmHORIZONTAL,
		 XmNnumColumns,2,NULL);

   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[7],NULL,0);
   XtManageChild(label);
   GW.tolButton[0]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.tolButton[0]);
   XtVaSetValues(GW.tolButton[0],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[7],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[8],NULL,0);
   XtManageChild(label);
   GW.tolButton[1]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.tolButton[1]);
   XtVaSetValues(GW.tolButton[1],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[8],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[9],NULL,0);
   XtManageChild(label);
   GW.tolButton[2]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.tolButton[2]);
   XtVaSetValues(GW.tolButton[2],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[9],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[10],NULL,0);
   XtManageChild(label);
   GW.tolButton[3]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.tolButton[3]);
   XtVaSetValues(GW.tolButton[3],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[10],
		 NULL);

   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[11],NULL,0);
   XtManageChild(label);
   GW.tolButton[4]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.tolButton[4]);
   XtVaSetValues(GW.tolButton[4],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[11],
		 NULL);

   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[12],NULL,0);
   XtManageChild(label);
   GW.tolButton[5]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.tolButton[5]);
   XtVaSetValues(GW.tolButton[5],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[12],
		 NULL);
 }




#ifdef _NO_PROTO
void   OkFormTolCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   OkFormTolCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  String value;
  ClientData *tmp;

#if 0

  SaveParScreen(1);
  SaveParScreen(2);
  SaveParScreen(3);
  SaveParScreen(4);
  SetParScreen();

  tmp = (ClientData*) userData;
  if(tmp->data==1)
    XtUnmanageChild(tmp->widget);

#endif

  tmp = (ClientData *) userData;

  XtVaGetValues(GW.tolButton[0],XmNvalue,&value,NULL);
  GP.parValue[PAR_EPSL] = atof(value);
  sprintf(GP.parStrValue[PAR_EPSL],"%lg",GP.parValue[PAR_EPSL]);
  XtVaSetValues(GW.parButton[PAR_EPSL],XmNvalue,GP.parStrValue[PAR_EPSL],NULL);
  
  XtVaGetValues(GW.tolButton[1],XmNvalue,&value,NULL);
  GP.parValue[PAR_EPSU] = atof(value);
  sprintf(GP.parStrValue[PAR_EPSU],"%lg",GP.parValue[PAR_EPSU]);
  XtVaSetValues(GW.parButton[PAR_EPSU],XmNvalue,GP.parStrValue[PAR_EPSU],NULL);
  
  XtVaGetValues(GW.tolButton[2],XmNvalue,&value,NULL);
  GP.parValue[PAR_EPSS] = atof(value);
  sprintf(GP.parStrValue[PAR_EPSS],"%lg",GP.parValue[PAR_EPSS]);
  XtVaSetValues(GW.parButton[PAR_EPSS],XmNvalue,GP.parStrValue[PAR_EPSS],NULL);
  
  XtVaGetValues(GW.tolButton[3],XmNvalue,&value,NULL);
  GP.parValue[PAR_ITMX] = atof(value);
  sprintf(GP.parStrValue[PAR_ITMX],"%d",(int)GP.parValue[PAR_ITMX]);
  XtVaSetValues(GW.parButton[PAR_ITMX],XmNvalue,GP.parStrValue[PAR_ITMX],NULL);
  
  XtVaGetValues(GW.tolButton[4],XmNvalue,&value,NULL);
  GP.parValue[PAR_NWTN] = atof(value);
  sprintf(GP.parStrValue[PAR_NWTN],"%d",(int)GP.parValue[PAR_NWTN]);
  XtVaSetValues(GW.parButton[PAR_NWTN],XmNvalue,GP.parStrValue[PAR_NWTN],NULL);
  
  XtVaGetValues(GW.tolButton[5],XmNvalue,&value,NULL);
  GP.parValue[PAR_ITNW] = atof(value);
  sprintf(GP.parStrValue[PAR_ITNW],"%d",(int)GP.parValue[PAR_ITNW]);
  XtVaSetValues(GW.parButton[PAR_ITNW],XmNvalue,GP.parStrValue[PAR_ITNW],NULL);
  
  if(tmp->data==1)
    XtUnmanageChild(tmp->widget);


}


#ifdef _NO_PROTO
Widget	CreateFormStep(parent)
Widget	parent;
#else
Widget	CreateFormStep(Widget parent)
#endif

{


  Widget rtrn,rwcl,ok,apply,cancel,help;
  static ClientData userData1,userData2;

  rtrn   = XmCreateFormDialog(parent,"DialogShell",NULL,0);
  PopupTemplate(rtrn,&rwcl,&ok,&apply,&cancel,&help,25);
  CreateBoxStep(rwcl);

  userData1.widget=rtrn;
  userData1.data=1;
  userData2.widget=rtrn;
  userData2.data=0;

  XtAddCallback(ok,XmNactivateCallback,OkFormStepCB,&userData1);
  XtAddCallback(apply,XmNactivateCallback,OkFormStepCB,&userData2);
  XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);
  XtAddCallback(help,XmNactivateCallback,PopupCB,GW.helpList);

#if 0

  Widget rtrn,rwcl,sept,form,apply,reset,quit,wgt,frame;

/*
  rtrn   = XmCreateFormDialog(parent,"DialogShell",NULL,0);
*/

  rtrn = XmCreateMessageDialog(parent,"Message",NULL,0);
  XtAddCallback(rtrn,XmNokCallback,OkFormStepCB,NULL);
  XtAddCallback(rtrn,XmNhelpCallback,PopupCB,GW.helpList);
 
  rwcl = XmCreateRowColumn(rtrn,"RowColumn",NULL,0);
  XtManageChild(rwcl);

  CreateBoxStep(rwcl);


  sept = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
  XtManageChild(sept);
  form = XmCreateForm(rtrn,"Form",NULL,0);
  XtManageChild(form);
  apply = XmCreatePushButtonGadget(form,"Apply",NULL,0);
  XtManageChild(apply);
  reset = XmCreatePushButtonGadget(form,"Reset",NULL,0);
  XtManageChild(reset);
  quit  = XmCreatePushButtonGadget(form,"Quit",NULL,0);
  XtManageChild(quit);
  XtAddCallback(quit,XmNactivateCallback,PopdownCB,rtrn);

  XtVaSetValues(rwcl,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNbottomWidget,sept,
		NULL);

  XtVaSetValues(sept,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,5,
		XmNbottomOffset,5,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNbottomWidget,form,
		NULL);

  XtVaSetValues(form,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		NULL);

  XtVaSetValues(apply,XmNleftAttachment,XmATTACH_FORM,NULL);
  XtVaSetValues(apply,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
                XmNleftOffset,5,
		NULL);

  XtVaSetValues(reset,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_WIDGET,
		XmNleftWidget,apply,
		XmNrightAttachment,XmATTACH_WIDGET,
		XmNrightWidget,quit,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
		XmNleftOffset,5,
		XmNrightOffset,5,
		NULL);

  XtVaSetValues(quit,XmNrightAttachment,XmATTACH_FORM,NULL);
  XtVaSetValues(quit,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
                XmNrightOffset,5,
		NULL);

#endif

  return rtrn;
}



#ifdef _NO_PROTO
void	CreateBoxStep(parent)
Widget	parent;
#else
void	CreateBoxStep(Widget parent)
#endif

{
   Widget rwcl,label,frame;
   /*   Widget popThl,popThu; */

   XtVaSetValues(parent,XmNpacking,XmPACK_COLUMN,XmNorientation,XmHORIZONTAL,
		 XmNnumColumns,2,NULL);

   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[13],NULL,0);
   XtManageChild(label);
   GW.stepButton[0]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.stepButton[0]);
   XtVaSetValues(GW.stepButton[0],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[13],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[14],NULL,0);
   XtManageChild(label);
   GW.stepButton[1]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.stepButton[1]);
   XtVaSetValues(GW.stepButton[1],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[14],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[15],NULL,0);
   XtManageChild(label);
   GW.stepButton[2]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.stepButton[2]);
   XtVaSetValues(GW.stepButton[2],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[15],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[16],NULL,0);
   XtManageChild(label);
   GW.stepButton[3]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.stepButton[3]);
   XtVaSetValues(GW.stepButton[3],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[16],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[34],NULL,0);
   XtManageChild(label);
   GW.stepButton[4]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.stepButton[4]);
   XtVaSetValues(GW.stepButton[4],
		 XmNblinkRate,0,
		 XmNeditable,False,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[PAR_NTHL],
		 NULL);
/*
   popThl=PopupThl(GW.stepButton[4]);
*/
   XtAddEventHandler(GW.stepButton[4],ButtonPressMask,False,PostItCB1,GW.popThl);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[35],NULL,0);
   XtManageChild(label);
   GW.stepButton[5]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.stepButton[5]);
   XtVaSetValues(GW.stepButton[5],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[PAR_NTHU],
		 NULL);
/*
   popThu=PopupThu(GW.stepButton[5]);
*/
   XtAddEventHandler(GW.stepButton[5],ButtonPressMask,False,PostItCB1,GW.popThu);




}

#ifdef _NO_PROTO
void   OkFormStepCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   OkFormStepCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  String value;
  ClientData *tmp;
  int i,k1,k2;

#if 0

  SaveParScreen(1);
  SaveParScreen(2);
  SaveParScreen(3);
  SaveParScreen(4);
  SetParScreen();

  tmp = (ClientData*) userData;
  if(tmp->data==1)
    XtUnmanageChild(tmp->widget);

#endif

  tmp = (ClientData*) userData;

  XtVaGetValues(GW.stepButton[0],XmNvalue,&value,NULL);
  GP.parValue[PAR_DS] = atof(value);
  sprintf(GP.parStrValue[PAR_DS],"%lg",GP.parValue[PAR_DS]);
  XtVaSetValues(GW.parButton[PAR_DS],XmNvalue,GP.parStrValue[PAR_DS],NULL);
  
  XtVaGetValues(GW.stepButton[1],XmNvalue,&value,NULL);
  GP.parValue[PAR_DSMIN] = atof(value);
  sprintf(GP.parStrValue[PAR_DSMIN],"%lg",GP.parValue[PAR_DSMIN]);
  XtVaSetValues(GW.parButton[PAR_DSMIN],XmNvalue,GP.parStrValue[PAR_DSMIN],NULL);
  
  XtVaGetValues(GW.stepButton[2],XmNvalue,&value,NULL);
  GP.parValue[PAR_DSMAX] = atof(value);
  sprintf(GP.parStrValue[PAR_DSMAX],"%lg",GP.parValue[PAR_DSMAX]);
  XtVaSetValues(GW.parButton[PAR_DSMAX],XmNvalue,GP.parStrValue[PAR_DSMAX],NULL);
  
  XtVaGetValues(GW.stepButton[3],XmNvalue,&value,NULL);
  GP.parValue[PAR_IADS] = atof(value);
  sprintf(GP.parStrValue[PAR_IADS],"%d",(int)GP.parValue[PAR_IADS]);
  XtVaSetValues(GW.parButton[PAR_IADS],XmNvalue,GP.parStrValue[PAR_IADS],NULL);
  
/*

  XtVaGetValues(GW.stepButton[4],XmNvalue,&value,NULL);
  GP.parValue[PAR_NTHL] = atof(value);
  sprintf(GP.parStrValue[PAR_NTHL],"%lg",GP.parValue[PAR_NTHL]);
  XtVaSetValues(GW.parButton[PAR_NTHL],XmNvalue,GP.parStrValue[PAR_NTHL],NULL);
  
  XtVaGetValues(GW.stepButton[5],XmNvalue,&value,NULL);
  GP.parValue[PAR_NTHU] = atof(value);
  sprintf(GP.parStrValue[PAR_NTHU],"%lg",GP.parValue[PAR_NTHU]);
  XtVaSetValues(GW.parButton[PAR_NTHU],XmNvalue,GP.parStrValue[PAR_NTHU],NULL);


  
  GP.nthl = CountAny(2);
  GP.nthu = CountAny(3);


  for(i = 0; i < GP.nthl; i++) {	
    k1=2*i;
    k2=k1+1;
    XtVaGetValues(GW.thlButton[k2],XmNvalue,&value,NULL);
    GP.thlValue[k2] = atof(value);    
    sprintf(GP.thlStrValue[k2],"%lg",GP.thlValue[k2]);
  }
  

  for(i = 0; i < GP.nthu; i++) {	
    k1=2*i;
    k2=k1+1;
    XtVaGetValues(GW.thuButton[k2],XmNvalue,&value,NULL);
    GP.thuValue[k2] = atof(value);    
    sprintf(GP.thuStrValue[k2],"%lg",GP.thuValue[k2]);
  }
  
*/

  if(tmp->data==1)
    XtUnmanageChild(tmp->widget);


  
}

#ifdef _NO_PROTO
Widget	CreateFormLim(parent)
Widget	parent;
#else
Widget	CreateFormLim(Widget parent)
#endif

{


  Widget rtrn,rwcl,ok,apply,cancel,help;
  static ClientData userData1,userData2;

  rtrn   = XmCreateFormDialog(parent,"DialogShell",NULL,0);
  PopupTemplate(rtrn,&rwcl,&ok,&apply,&cancel,&help,25);
  CreateBoxLim(rwcl);


  userData1.widget=rtrn;
  userData1.data=1;
  userData2.widget=rtrn;
  userData2.data=0;


  XtAddCallback(ok,XmNactivateCallback,OkFormLimCB,&userData1);
  XtAddCallback(apply,XmNactivateCallback,OkFormLimCB,&userData2);
  XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);
  XtAddCallback(help,XmNactivateCallback,PopupCB,GW.helpList);

#if 0

  Widget rtrn,rwcl,sept,form,apply,reset,quit,wgt,frame;

/*
  rtrn   = XmCreateFormDialog(parent,"DialogShell",NULL,0);
*/

  rtrn = XmCreateMessageDialog(parent,"Message",NULL,0);
  XtAddCallback(rtrn,XmNokCallback,OkFormLimCB,NULL);  
  XtAddCallback(rtrn,XmNhelpCallback,PopupCB,GW.helpList);
 
  rwcl = XmCreateRowColumn(rtrn,"RowColumn",NULL,0);
  XtManageChild(rwcl);

  CreateBoxLim(rwcl);


  sept = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
  XtManageChild(sept);
  form = XmCreateForm(rtrn,"Form",NULL,0);
  XtManageChild(form);
  apply = XmCreatePushButtonGadget(form,"Apply",NULL,0);
  XtManageChild(apply);
  reset = XmCreatePushButtonGadget(form,"Reset",NULL,0);
  XtManageChild(reset);
  quit  = XmCreatePushButtonGadget(form,"Quit",NULL,0);
  XtManageChild(quit);
  XtAddCallback(quit,XmNactivateCallback,PopdownCB,rtrn);

  XtVaSetValues(rwcl,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNbottomWidget,sept,
		NULL);

  XtVaSetValues(sept,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,5,
		XmNbottomOffset,5,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNbottomWidget,form,
		NULL);

  XtVaSetValues(form,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		NULL);

  XtVaSetValues(apply,XmNleftAttachment,XmATTACH_FORM,NULL);
  XtVaSetValues(apply,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
                XmNleftOffset,5,
		NULL);

  XtVaSetValues(reset,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_WIDGET,
		XmNleftWidget,apply,
		XmNrightAttachment,XmATTACH_WIDGET,
		XmNrightWidget,quit,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
		XmNleftOffset,5,
		XmNrightOffset,5,
		NULL);

  XtVaSetValues(quit,XmNrightAttachment,XmATTACH_FORM,NULL);
  XtVaSetValues(quit,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
                XmNrightOffset,5,
		NULL);

#endif

  return rtrn;
}



#ifdef _NO_PROTO
void	CreateBoxLim(parent)
Widget	parent;
#else
void	CreateBoxLim(Widget parent)
#endif

{
   Widget rwcl,label,frame;

   XtVaSetValues(parent,XmNpacking,XmPACK_COLUMN,XmNorientation,XmHORIZONTAL,
		 XmNnumColumns,2,NULL);

   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[17],NULL,0);
   XtManageChild(label);
   GW.limButton[0]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.limButton[0]);
   XtVaSetValues(GW.limButton[0],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[17],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[18],NULL,0);
   XtManageChild(label);
   GW.limButton[1]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.limButton[1]);
   XtVaSetValues(GW.limButton[1],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[18],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[19],NULL,0);
   XtManageChild(label);
   GW.limButton[2]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.limButton[2]);
   XtVaSetValues(GW.limButton[2],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[19],
		 NULL);

   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[20],NULL,0);
   XtManageChild(label);
   GW.limButton[3]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.limButton[3]);
   XtVaSetValues(GW.limButton[3],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[20],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[21],NULL,0);
   XtManageChild(label);
   GW.limButton[4]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.limButton[4]);
   XtVaSetValues(GW.limButton[4],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[21],
		 NULL);


 }

#ifdef _NO_PROTO
void   OkFormLimCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   OkFormLimCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  String value;
  ClientData *tmp;

#if 0

  SaveParScreen(1);
  SaveParScreen(2);
  SaveParScreen(3);
  SaveParScreen(4);
  SetParScreen();

  tmp = (ClientData*) userData;
  if(tmp->data==1)
    XtUnmanageChild(tmp->widget);

#endif
 
  tmp = (ClientData *) userData;

  XtVaGetValues(GW.limButton[0],XmNvalue,&value,NULL);
  GP.parValue[PAR_NMX] = atof(value);
  sprintf(GP.parStrValue[PAR_NMX],"%d",(int)GP.parValue[PAR_NMX]);
  XtVaSetValues(GW.parButton[PAR_NMX],XmNvalue,GP.parStrValue[PAR_NMX],NULL);
  
  XtVaGetValues(GW.limButton[1],XmNvalue,&value,NULL);
  GP.parValue[PAR_RL0] = atof(value);
  sprintf(GP.parStrValue[PAR_RL0],"%lg",GP.parValue[PAR_RL0]);
  XtVaSetValues(GW.parButton[PAR_RL0],XmNvalue,GP.parStrValue[PAR_RL0],NULL);
  
  XtVaGetValues(GW.limButton[2],XmNvalue,&value,NULL);
  GP.parValue[PAR_RL1] = atof(value);
  sprintf(GP.parStrValue[PAR_RL1],"%lg",GP.parValue[PAR_RL1]);
  XtVaSetValues(GW.parButton[PAR_RL1],XmNvalue,GP.parStrValue[PAR_RL1],NULL);
  
  XtVaGetValues(GW.limButton[3],XmNvalue,&value,NULL);
  GP.parValue[PAR_A0] = atof(value);
  sprintf(GP.parStrValue[PAR_A0],"%lg",GP.parValue[PAR_A0]);
  XtVaSetValues(GW.parButton[PAR_A0],XmNvalue,GP.parStrValue[PAR_A0],NULL);
  
  XtVaGetValues(GW.limButton[4],XmNvalue,&value,NULL);
  GP.parValue[PAR_A1] = atof(value);
  sprintf(GP.parStrValue[PAR_A1],"%lg",GP.parValue[PAR_A1]);
  XtVaSetValues(GW.parButton[PAR_A1],XmNvalue,GP.parStrValue[PAR_A1],NULL);

  if(tmp->data == 1)
    XtUnmanageChild(tmp->widget);


}

#ifdef _NO_PROTO
Widget	CreateFormCon(parent)
Widget	parent;
#else
Widget	CreateFormCon(Widget parent)
#endif

{

  Widget rtrn,rwcl,ok,apply,cancel,help;
  static ClientData userData1,userData2;

  rtrn   = XmCreateFormDialog(parent,"DialogShell",NULL,0);
  PopupTemplate1(rtrn,&rwcl,&ok,&apply,&cancel,&help);
  CreateBoxCon(rwcl);

  userData1.widget = rtrn;
  userData1.data = 1;
  userData2.widget = rtrn;
  userData2.data = 0;

  XtAddCallback(ok,XmNactivateCallback,OkFormConCB,&userData1);
  XtAddCallback(apply,XmNactivateCallback,OkFormConCB,&userData2);
  XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);
  XtAddCallback(help,XmNactivateCallback,PopupCB,GW.helpList);

#if 0

  Widget rtrn,rwcl,sept,form,apply,reset,quit,wgt,frame;

/*
  rtrn   = XmCreateFormDialog(parent,"DialogShell",NULL,0);
*/

  rtrn = XmCreateMessageDialog(parent,"Message",NULL,0);
  XtAddCallback(rtrn,XmNokCallback,OkFormConCB,NULL);
  XtAddCallback(rtrn,XmNhelpCallback,PopupCB,GW.helpList);
 
  rwcl = XmCreateRowColumn(rtrn,"RowColumn",NULL,0);
  XtManageChild(rwcl);

  CreateBoxCon(rwcl);


  sept = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
  XtManageChild(sept);
  form = XmCreateForm(rtrn,"Form",NULL,0);
  XtManageChild(form);
  apply = XmCreatePushButtonGadget(form,"Apply",NULL,0);
  XtManageChild(apply);
  reset = XmCreatePushButtonGadget(form,"Reset",NULL,0);
  XtManageChild(reset);
  quit  = XmCreatePushButtonGadget(form,"Quit",NULL,0);
  XtManageChild(quit);
  XtAddCallback(quit,XmNactivateCallback,PopdownCB,rtrn);

  XtVaSetValues(rwcl,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNbottomWidget,sept,
		NULL);

  XtVaSetValues(sept,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,5,
		XmNbottomOffset,5,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNbottomWidget,form,
		NULL);

  XtVaSetValues(form,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		NULL);

  XtVaSetValues(apply,XmNleftAttachment,XmATTACH_FORM,NULL);
  XtVaSetValues(apply,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
                XmNleftOffset,5,
		NULL);

  XtVaSetValues(reset,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_WIDGET,
		XmNleftWidget,apply,
		XmNrightAttachment,XmATTACH_WIDGET,
		XmNrightWidget,quit,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
		XmNleftOffset,5,
		XmNrightOffset,5,
		NULL);

  XtVaSetValues(quit,XmNrightAttachment,XmATTACH_FORM,NULL);
  XtVaSetValues(quit,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
                XmNrightOffset,5,
		NULL);
#endif

  return rtrn;
}



#ifdef _NO_PROTO
void	CreateBoxCon(parent)
Widget	parent;
#else
void	CreateBoxCon(Widget parent)
#endif

{
   Widget rwcl,label,frame;

   XtVaSetValues(parent,XmNpacking,XmPACK_COLUMN,XmNorientation,XmHORIZONTAL,
		 XmNnumColumns,1,NULL);

   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[22],NULL,0);
   XtManageChild(label);
   GW.conButton[0]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.conButton[0]);
   XtVaSetValues(GW.conButton[0],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[22],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[23],NULL,0);
   XtManageChild(label);
   GW.conButton[1]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.conButton[1]);
   XtVaSetValues(GW.conButton[1],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[23],
		 NULL);

   XtVaSetValues(GW.conButton[1],XmNeditable,False,NULL);
   XtAddEventHandler(GW.conButton[1],ButtonPressMask,False,PostItCB1,GW.popIcp);

 }


#ifdef _NO_PROTO
void   OkFormConCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   OkFormConCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  String value;
  ClientData *tmp;
  int i,k1,k2;

#if 0

  SaveParScreen(1);
  SaveParScreen(2);
  SaveParScreen(3);
  SaveParScreen(4);
  SetParScreen();

  tmp = (ClientData*) userData;
  if(tmp->data==1)
    XtUnmanageChild(tmp->widget);

#endif

  tmp = (ClientData*) userData;

  XtVaGetValues(GW.conButton[0],XmNvalue,&value,NULL);
  GP.parValue[PAR_NICP] = atof(value);
  sprintf(GP.parStrValue[PAR_NICP],"%d",(int)GP.parValue[PAR_NICP]);
  XtVaSetValues(GW.parButton[PAR_NICP],XmNvalue,GP.parStrValue[PAR_NICP],NULL);
  
/*

  XtVaGetValues(GW.conButton[1],XmNvalue,&value,NULL);
  GP.parValue[PAR_ICP] = atof(value);
  sprintf(GP.parStrValue[PAR_ICP],"%d",(int)GP.parValue[PAR_ICP]);
  XtVaSetValues(GW.parButton[PAR_ICP],XmNvalue,GP.parStrValue[PAR_ICP],NULL);
  
  
  for(i = 0; i < (int)GP.parValue[PAR_NICP]; i++) {	
    k1=2*i;
    k2=k1+1;
    XtVaGetValues(GW.icpButton[k2],XmNvalue,&value,NULL);
    GP.icpValue[k2] = atof(value);    
    sprintf(GP.icpStrValue[k2],"%d",(int)GP.icpValue[k2]);
  }

*/
  

  if(tmp->data==1)
    XtUnmanageChild(tmp->widget);


}

#ifdef _NO_PROTO
Widget	CreateFormRun(parent)
Widget	parent;
#else
Widget	CreateFormRun(Widget parent)
#endif

{

  Widget rtrn,rwcl,ok,apply,cancel,help;
  static ClientData userData1,userData2;

  rtrn   = XmCreateFormDialog(parent,"DialogShell",NULL,0);
  PopupTemplate(rtrn,&rwcl,&ok,&apply,&cancel,&help,25);
  CreateBoxRun(rwcl);

  userData1.widget=rtrn;
  userData1.data=1;
  userData2.widget=rtrn;
  userData2.data=0;


  XtAddCallback(ok,XmNactivateCallback,OkFormRunCB,&userData1);
  XtAddCallback(apply,XmNactivateCallback,OkFormRunCB,&userData2);
  XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);
  XtAddCallback(help,XmNactivateCallback,PopupCB,GW.helpList);

#if 0

  Widget rtrn,rwcl,sept,form,apply,reset,quit,wgt,frame;

/*
  rtrn   = XmCreateFormDialog(parent,"DialogShell",NULL,0);
*/

  rtrn = XmCreateMessageDialog(parent,"Message",NULL,0);
  XtAddCallback(rtrn,XmNokCallback,OkFormRunCB,NULL);
  XtAddCallback(rtrn,XmNhelpCallback,PopupCB,GW.helpList);
 
  rwcl = XmCreateRowColumn(rtrn,"RowColumn",NULL,0);
  XtManageChild(rwcl);

  CreateBoxRun(rwcl);


  sept = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
  XtManageChild(sept);
  form = XmCreateForm(rtrn,"Form",NULL,0);
  XtManageChild(form);
  apply = XmCreatePushButtonGadget(form,"Apply",NULL,0);
  XtManageChild(apply);
  reset = XmCreatePushButtonGadget(form,"Reset",NULL,0);
  XtManageChild(reset);
  quit  = XmCreatePushButtonGadget(form,"Quit",NULL,0);
  XtManageChild(quit);
  XtAddCallback(quit,XmNactivateCallback,PopdownCB,rtrn);

  XtVaSetValues(rwcl,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNbottomWidget,sept,
		NULL);

  XtVaSetValues(sept,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,5,
		XmNbottomOffset,5,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNbottomWidget,form,
		NULL);

  XtVaSetValues(form,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		NULL);

  XtVaSetValues(apply,XmNleftAttachment,XmATTACH_FORM,NULL);
  XtVaSetValues(apply,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
                XmNleftOffset,5,
		NULL);

  XtVaSetValues(reset,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_WIDGET,
		XmNleftWidget,apply,
		XmNrightAttachment,XmATTACH_WIDGET,
		XmNrightWidget,quit,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
		XmNleftOffset,5,
		XmNrightOffset,5,
		NULL);

  XtVaSetValues(quit,XmNrightAttachment,XmATTACH_FORM,NULL);
  XtVaSetValues(quit,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
                XmNrightOffset,5,
		NULL);

#endif

  return rtrn;
}



#ifdef _NO_PROTO
void	CreateBoxRun(parent)
Widget	parent;
#else
void	CreateBoxRun(Widget parent)
#endif

{
   Widget rwcl,label,frame;
   Widget popIlp,popIsp,popIsw,popIps;


   XtVaSetValues(parent,XmNpacking,XmPACK_COLUMN,XmNorientation,XmHORIZONTAL,
		 XmNnumColumns,2,NULL);

   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[24],NULL,0);
   XtManageChild(label);
   GW.runButton[0]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.runButton[0]);
   XtVaSetValues(GW.runButton[0],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[24],
		 NULL);

   XtVaSetValues(GW.runButton[0],XmNeditable,False,NULL);
   popIlp=PopupIlp(GW.runButton[0]);
   XtAddEventHandler(GW.runButton[0],ButtonPressMask,False,PostItCB,popIlp);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   


   label = XmCreateLabelGadget(rwcl,parLabel[25],NULL,0);
   XtManageChild(label);
   GW.runButton[1]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.runButton[1]);
   XtVaSetValues(GW.runButton[1],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[25],
		 NULL);


   XtVaSetValues(GW.runButton[1],XmNeditable,False,NULL);
   popIsp=PopupIsp(GW.runButton[1]);
   XtAddEventHandler(GW.runButton[1],ButtonPressMask,False,PostItCB,popIsp);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[26],NULL,0);
   XtManageChild(label);
   GW.runButton[2]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.runButton[2]);
   XtVaSetValues(GW.runButton[2],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[26],
		 NULL);

   XtVaSetValues(GW.runButton[2],XmNeditable,False,NULL);
   popIsw=PopupIsw(GW.runButton[2]);
   XtAddEventHandler(GW.runButton[2],ButtonPressMask,False,PostItCB,popIsw);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[27],NULL,0);
   XtManageChild(label);
   GW.runButton[3]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.runButton[3]);
   XtVaSetValues(GW.runButton[3],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[27],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[28],NULL,0);
   XtManageChild(label);
   GW.runButton[4]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.runButton[4]);
   XtVaSetValues(GW.runButton[4],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[28],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[29],NULL,0);
   XtManageChild(label);
   GW.runButton[5]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.runButton[5]);
   XtVaSetValues(GW.runButton[5],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[29],
		 NULL);


   XtVaSetValues(GW.runButton[5],XmNeditable,False,NULL);
   popIps=PopupIps(GW.runButton[5]);
   XtAddEventHandler(GW.runButton[5],ButtonPressMask,False,PostItCB,popIps);

 }


#ifdef _NO_PROTO
void   OkFormRunCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   OkFormRunCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  String value;
  ClientData *tmp;

#if 0

  SaveParScreen(1);
  SaveParScreen(2);
  SaveParScreen(3);
  SaveParScreen(4);
  SetParScreen();

  tmp = (ClientData*) userData;
  if(tmp->data==1)
    XtUnmanageChild(tmp->widget);


#endif


  tmp = (ClientData*) userData;

  XtVaGetValues(GW.runButton[0],XmNvalue,&value,NULL);
  GP.parValue[PAR_ILP] = atof(value);
  sprintf(GP.parStrValue[PAR_ILP],"%d",(int)GP.parValue[PAR_ILP]);
  XtVaSetValues(GW.parButton[PAR_ILP],XmNvalue,GP.parStrValue[PAR_ILP],NULL);
  
  XtVaGetValues(GW.runButton[1],XmNvalue,&value,NULL);
  GP.parValue[PAR_ISP] = atof(value);
  sprintf(GP.parStrValue[PAR_ISP],"%d",(int)GP.parValue[PAR_ISP]);
  XtVaSetValues(GW.parButton[PAR_ISP],XmNvalue,GP.parStrValue[PAR_ISP],NULL);
  
  XtVaGetValues(GW.runButton[2],XmNvalue,&value,NULL);
  GP.parValue[PAR_ISW] = atof(value);
  sprintf(GP.parStrValue[PAR_ISW],"%d",(int)GP.parValue[PAR_ISW]);
  XtVaSetValues(GW.parButton[PAR_ISW],XmNvalue,GP.parStrValue[PAR_ISW],NULL);
  
  XtVaGetValues(GW.runButton[3],XmNvalue,&value,NULL);
  GP.parValue[PAR_MXBF] = atof(value);
  sprintf(GP.parStrValue[PAR_MXBF],"%d",(int)GP.parValue[PAR_MXBF]);
  XtVaSetValues(GW.parButton[PAR_MXBF],XmNvalue,GP.parStrValue[PAR_MXBF],NULL);
  
  XtVaGetValues(GW.runButton[4],XmNvalue,&value,NULL);
  GP.parValue[PAR_IRS] = atof(value);
  sprintf(GP.parStrValue[PAR_IRS],"%d",(int)GP.parValue[PAR_IRS]);
  XtVaSetValues(GW.parButton[PAR_IRS],XmNvalue,GP.parStrValue[PAR_IRS],NULL);
  
  XtVaGetValues(GW.runButton[5],XmNvalue,&value,NULL);
  GP.parValue[PAR_IPS] = atof(value);
  sprintf(GP.parStrValue[PAR_IPS],"%d",(int)GP.parValue[PAR_IPS]);
  XtVaSetValues(GW.parButton[PAR_IPS],XmNvalue,GP.parStrValue[PAR_IPS],NULL);

  if(tmp->data==1)
    XtUnmanageChild(tmp->widget);


}


#ifdef _NO_PROTO
Widget	CreateFormOut(parent)
Widget	parent;
#else
Widget	CreateFormOut(Widget parent)
#endif

{


  Widget rtrn,rwcl,ok,apply,cancel,help;
  static ClientData userData1,userData2;

  rtrn   = XmCreateFormDialog(parent,"DialogShell",NULL,0);
  PopupTemplate1(rtrn,&rwcl,&ok,&apply,&cancel,&help);
  CreateBoxOut(rwcl);

  userData1.widget=rtrn;
  userData1.data=1;
  userData2.widget=rtrn;
  userData2.data=0;


  XtAddCallback(ok,XmNactivateCallback,OkFormOutCB,&userData1);
  XtAddCallback(apply,XmNactivateCallback,OkFormOutCB,&userData2);
  XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);
  XtAddCallback(help,XmNactivateCallback,PopupCB,GW.helpList);

#if 0

  Widget rtrn,rwcl,sept,form,apply,reset,quit,wgt,frame;

/*
  rtrn   = XmCreateFormDialog(parent,"DialogShell",NULL,0);
*/

  rtrn = XmCreateMessageDialog(parent,"Message",NULL,0);
  XtAddCallback(rtrn,XmNokCallback,OkFormOutCB,NULL);
  XtAddCallback(rtrn,XmNhelpCallback,PopupCB,GW.helpList);

  rwcl = XmCreateRowColumn(rtrn,"RowColumn",NULL,0);
  XtManageChild(rwcl);

  CreateBoxOut(rwcl);


  sept = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
  XtManageChild(sept);
  form = XmCreateForm(rtrn,"Form",NULL,0);
  XtManageChild(form);
  apply = XmCreatePushButtonGadget(form,"Apply",NULL,0);
  XtManageChild(apply);
  reset = XmCreatePushButtonGadget(form,"Reset",NULL,0);
  XtManageChild(reset);
  quit  = XmCreatePushButtonGadget(form,"Quit",NULL,0);
  XtManageChild(quit);
  XtAddCallback(quit,XmNactivateCallback,PopdownCB,rtrn);

  XtVaSetValues(rwcl,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNbottomWidget,sept,
		NULL);

  XtVaSetValues(sept,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,5,
		XmNbottomOffset,5,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNbottomWidget,form,
		NULL);

  XtVaSetValues(form,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		NULL);

  XtVaSetValues(apply,XmNleftAttachment,XmATTACH_FORM,NULL);
  XtVaSetValues(apply,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
                XmNleftOffset,5,
		NULL);

  XtVaSetValues(reset,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_WIDGET,
		XmNleftWidget,apply,
		XmNrightAttachment,XmATTACH_WIDGET,
		XmNrightWidget,quit,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
		XmNleftOffset,5,
		XmNrightOffset,5,
		NULL);

  XtVaSetValues(quit,XmNrightAttachment,XmATTACH_FORM,NULL);
  XtVaSetValues(quit,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
                XmNrightOffset,5,
		NULL);

#endif

  return rtrn;
}



#ifdef _NO_PROTO
void	CreateBoxOut(parent)
Widget	parent;
#else
void	CreateBoxOut(Widget parent)
#endif

{
   Widget rwcl,label,frame;
   Widget popIid;


   XtVaSetValues(parent,XmNpacking,XmPACK_COLUMN,XmNorientation,XmHORIZONTAL,
		 XmNnumColumns,2,NULL);

   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[30],NULL,0);
   XtManageChild(label);
   GW.outButton[0]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.outButton[0]);
   XtVaSetValues(GW.outButton[0],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[30],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[31],NULL,0);
   XtManageChild(label);
   GW.outButton[1]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.outButton[1]);
   XtVaSetValues(GW.outButton[1],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[31],
		 NULL);

   XtVaSetValues(GW.outButton[1],XmNeditable,False,NULL);
   popIid=PopupIid(GW.outButton[1]);
   XtAddEventHandler(GW.outButton[1],ButtonPressMask,False,PostItCB,popIid);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[32],NULL,0);
   XtManageChild(label);
   GW.outButton[2]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.outButton[2]);
   XtVaSetValues(GW.outButton[2],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[32],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[33],NULL,0);
   XtManageChild(label);
   GW.outButton[3]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.outButton[3]);
   XtVaSetValues(GW.outButton[3],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[PAR_NUZR],
		 NULL);

   XtVaSetValues(GW.outButton[3],XmNeditable,False,NULL);
   XtAddEventHandler(GW.outButton[3],ButtonPressMask,False,PostItCB1,GW.popUzr);


/*

   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[33],NULL,0);
   XtManageChild(label);
   GW.outButton[4]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.outButton[4]);
   XtVaSetValues(GW.outButton[4],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[33],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[34],NULL,0);
   XtManageChild(label);
   GW.outButton[5]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.outButton[5]);
   XtVaSetValues(GW.outButton[5],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[34],
		 NULL);

   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[36],NULL,0);
   XtManageChild(label);
   GW.outButton[6]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.outButton[6]);
   XtVaSetValues(GW.outButton[6],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[36],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[37],NULL,0);
   XtManageChild(label);
   GW.outButton[7]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.outButton[7]);
   XtVaSetValues(GW.outButton[7],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[37],
		 NULL);


   frame = XmCreateFrame(parent,"Frame",NULL,0);
   XtManageChild(frame);
   rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);
   
   label = XmCreateLabelGadget(rwcl,parLabel[38],NULL,0);
   XtManageChild(label);
   GW.outButton[8]=XmCreateTextField(rwcl,"Button",NULL,0);
   XtManageChild(GW.outButton[8]);
   XtVaSetValues(GW.outButton[8],
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH,
		 XmNmaxLength,WX_TEXTLENGTH,
		 XmNvalue,GP.parStrValue[38],
		 NULL);
*/

 }


#ifdef _NO_PROTO
void   OkFormOutCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   OkFormOutCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  String value;
  ClientData *tmp;
  int i,k1,k2;

#if 0

  SaveParScreen(1);
  SaveParScreen(2);
  SaveParScreen(3);
  SaveParScreen(4);
  SetParScreen();

  tmp = (ClientData*) userData;
  if(tmp->data==1)
    XtUnmanageChild(tmp->widget);


#endif

  tmp=(ClientData*) userData;

  XtVaGetValues(GW.outButton[0],XmNvalue,&value,NULL);
  GP.parValue[PAR_NPR] = atof(value);
  sprintf(GP.parStrValue[PAR_NPR],"%d",(int)GP.parValue[PAR_NPR]);
  XtVaSetValues(GW.parButton[PAR_NPR],XmNvalue,GP.parStrValue[PAR_NPR],NULL);
  
  XtVaGetValues(GW.outButton[1],XmNvalue,&value,NULL);
  GP.parValue[PAR_IID] = atof(value);
  sprintf(GP.parStrValue[PAR_IID],"%d",(int)GP.parValue[PAR_IID]);
  XtVaSetValues(GW.parButton[PAR_IID],XmNvalue,GP.parStrValue[PAR_IID],NULL);
  
  XtVaGetValues(GW.outButton[2],XmNvalue,&value,NULL);
  GP.parValue[PAR_IPLT] = atof(value);
  sprintf(GP.parStrValue[PAR_IPLT],"%d",(int)GP.parValue[PAR_IPLT]);
  XtVaSetValues(GW.parButton[PAR_IPLT],XmNvalue,GP.parStrValue[PAR_IPLT],NULL);
  
/*

  XtVaGetValues(GW.outButton[3],XmNvalue,&value,NULL);
  GP.parValue[PAR_NUZR] = atof(value);
  sprintf(GP.parStrValue[PAR_NUZR],"%lg",GP.parValue[PAR_NUZR]);
  XtVaSetValues(GW.parButton[PAR_NUZR],XmNvalue,GP.parStrValue[PAR_NUZR],NULL);

  
  GP.nuzr = CountAny(1);

  for(i = 0; i < GP.nuzr; i++) {	
    k1=2*i;
    k2=k1+1;
    XtVaGetValues(GW.uzrButton[k2],XmNvalue,&value,NULL);
    GP.uzrValue[k2] = atof(value);    
    sprintf(GP.uzrStrValue[k2],"%lg",GP.uzrValue[k2]);
  }
  
*/

  
  if(tmp->data==1)
    XtUnmanageChild(tmp->widget);

/*
  XtVaGetValues(GW.outButton[4],XmNvalue,&value,NULL);
  GP.parValue[33] = atof(value);
  sprintf(GP.parStrValue[33],"%lg",GP.parValue[33]);
  XtVaSetValues(GW.parButton[33],XmNvalue,GP.parStrValue[33],NULL);
  
  XtVaGetValues(GW.outButton[5],XmNvalue,&value,NULL);
  GP.parValue[34] = atof(value);
  sprintf(GP.parStrValue[34],"%lg",GP.parValue[34]);
  XtVaSetValues(GW.parButton[34],XmNvalue,GP.parStrValue[34],NULL);
  
  
  XtVaGetValues(GW.outButton[6],XmNvalue,&value,NULL);
  GP.parValue[36] = atof(value);
  sprintf(GP.parStrValue[36],"%lg",GP.parValue[36]);
  XtVaSetValues(GW.parButton[36],XmNvalue,GP.parStrValue[36],NULL);
  
  
  XtVaGetValues(GW.outButton[7],XmNvalue,&value,NULL);
  GP.parValue[37] = atof(value);
  sprintf(GP.parStrValue[37],"%lg",GP.parValue[37]);
  XtVaSetValues(GW.parButton[37],XmNvalue,GP.parStrValue[37],NULL);
  
  
  XtVaGetValues(GW.outButton[8],XmNvalue,&value,NULL);
  GP.parValue[38] = atof(value);
  sprintf(GP.parStrValue[38],"%lg",GP.parValue[38]);
  XtVaSetValues(GW.parButton[38],XmNvalue,GP.parStrValue[38],NULL);
*/



}

#ifdef _NO_PROTO
Widget	CreateFormDefault(parent)
Widget	parent;
#else
Widget	CreateFormDefault(Widget parent)
#endif

{
  Widget rtrn,rwcl,sept,form,ok,apply,cancel,help,wgt,frame;
  static ClientData userData1,userData2;


/*
  rtrn = XmCreateMessageDialog(parent,"Message",NULL,0);
*/


  rtrn   = XmCreateFormDialog(parent,"DialogShell",NULL,0);
  PopupTemplate(rtrn,&rwcl,&ok,&apply,&cancel,&help,28);
  CreateBox(rwcl);

  userData1.widget=rtrn;
  userData1.data=1;
  userData2.widget=rtrn;
  userData2.data=0;


  XtAddCallback(ok,XmNactivateCallback,OkFormDefCB,&userData1);
  XtAddCallback(apply,XmNactivateCallback,OkFormDefCB,&userData2);
  XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);
  XtAddCallback(help,XmNactivateCallback,PopupCB,GW.helpList);


#if 0


  sept = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
  XtManageChild(sept);

/*
  form = XmCreateForm(rtrn,"Form",NULL,0);
*/

  form = XmCreateRowColumn(rtrn,"Form",NULL,0);
  XtManageChild(form);
  XtVaSetValues(form,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,1,NULL);


  ok = XmCreatePushButtonGadget(form,"Ok",NULL,0);
  XtManageChild(ok);
  reset = XmCreatePushButtonGadget(form,"Reset",NULL,0);
  XtManageChild(reset);
  cancel  = XmCreatePushButtonGadget(form,"Cancel",NULL,0);
  XtManageChild(cancel);
  help  = XmCreatePushButtonGadget(form,"Help",NULL,0);
  XtManageChild(help);

  XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);
  XtAddCallback(help,XmNactivateCallback,PopupCB,GW.helpList);


  XtVaSetValues(rwcl,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNbottomWidget,sept,
		NULL);

  XtVaSetValues(sept,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,5,
		XmNbottomOffset,5,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNbottomWidget,form,
		NULL);

  XtVaSetValues(form,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		NULL);



  XtVaSetValues(ok,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
                XmNleftOffset,5,
		NULL);

  XtVaSetValues(reset,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNrightPosition,50,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
		XmNrightOffset,-50,
		NULL);

  XtVaSetValues(cancel,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftPosition,50,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
		XmNleftOffset,50,
		NULL);


  XtVaSetValues(help,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
/*		XmNtopOffset,5,				*/
		XmNbottomOffset,5,
                XmNrightOffset,5,
		NULL);

#endif

  return rtrn;
}



#ifdef _NO_PROTO
void	CreateBox(parent)
Widget	parent;
#else
void	CreateBox(Widget parent)
#endif

{
   int i;
   Widget rwcl,label,frame;
   Widget popJac,popNcol,popIlp,popIsp,popIsw,popIps,popIid;
   /*   Widget popThl,popThu,popUzr,popIcp;  */

   XtVaSetValues(parent,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,6,NULL);

   for(i = 0; i < GP.numParameter; i++)	{
         frame = XmCreateFrame(parent,"Frame",NULL,0);
	 XtManageChild(frame);
         rwcl = XmCreateRowColumn(frame,"RowColumn",NULL,0);
	 XtManageChild(rwcl);
	 XtVaSetValues(rwcl,XmNpacking,XmPACK_TIGHT,NULL);

	 label = XmCreateLabelGadget(rwcl,parLabel[i],NULL,0);
	 XtManageChild(label);
/*
	 label = XmCreateTextField(rwcl,parLabel[i],NULL,0);
	 XtManageChild(label);
*/
         GW.parButton[i]=XmCreateTextField(rwcl,"Button",NULL,0);
	 XtManageChild(GW.parButton[i]);
	 XtVaSetValues(GW.parButton[i],
		       XmNblinkRate,0,
		       XmNcolumns,DE_TEXTLENGTH-2,
		       XmNmaxLength,WX_TEXTLENGTH,
		       XmNvalue,GP.parStrValue[i],
		       NULL);

	 switch(i) {
	 case PAR_NDIM:
	 case PAR_NBC:
	 case PAR_NINT:
	 case PAR_NTST:
	 case PAR_IAD:
	 case PAR_EPSL:
	 case PAR_EPSU:
	 case PAR_EPSS:
	 case PAR_NWTN:
	 case PAR_ITNW:
	 case PAR_DS:
	 case PAR_DSMIN:
	 case PAR_DSMAX:
	 case PAR_IADS:
	 case PAR_NMX:
	 case PAR_RL0:
	 case PAR_RL1:
	 case PAR_A0:
	 case PAR_A1:
	 case PAR_NICP:
	 case PAR_MXBF:
	 case PAR_IRS:
	 case PAR_NPR:
	 case PAR_IPLT:

	   break;

	 case PAR_JAC:

           XtVaSetValues(GW.parButton[i],XmNeditable,False,NULL);
	   popJac=PopupJac(GW.parButton[i]);
	   XtAddEventHandler(GW.parButton[i],ButtonPressMask,False,PostItCB,popJac);
	   break;

	 case PAR_NCOL:

           XtVaSetValues(GW.parButton[i],XmNeditable,False,NULL);
	   popNcol=PopupNcol(GW.parButton[i]);
	   XtAddEventHandler(GW.parButton[i],ButtonPressMask,False,PostItCB,popNcol);
	   break;

	 case PAR_ICP:

           XtVaSetValues(GW.parButton[i],XmNeditable,False,NULL);
	   GW.popIcp=PopupIcp(GW.parButton[i]);
	   XtAddEventHandler(GW.parButton[i],ButtonPressMask,False,PostItCB1,GW.popIcp);
	   break;

	 case PAR_ILP:

           XtVaSetValues(GW.parButton[i],XmNeditable,False,NULL);
	   popIlp=PopupIlp(GW.parButton[i]);
	   XtAddEventHandler(GW.parButton[i],ButtonPressMask,False,PostItCB,popIlp);
	   break;

	 case PAR_ISP:

           XtVaSetValues(GW.parButton[i],XmNeditable,False,NULL);
	   popIsp=PopupIsp(GW.parButton[i]);
	   XtAddEventHandler(GW.parButton[i],ButtonPressMask,False,PostItCB,popIsp);
	   break;

	 case PAR_ISW:

           XtVaSetValues(GW.parButton[i],XmNeditable,False,NULL);
	   popIsw=PopupIsw(GW.parButton[i]);
	   XtAddEventHandler(GW.parButton[i],ButtonPressMask,False,PostItCB,popIsw);
	   break;

	 case PAR_IPS:

           XtVaSetValues(GW.parButton[i],XmNeditable,False,NULL);
	   popIps=PopupIps(GW.parButton[i]);
	   XtAddEventHandler(GW.parButton[i],ButtonPressMask,False,PostItCB,popIps);
	   break;

	 case PAR_IID:

           XtVaSetValues(GW.parButton[i],XmNeditable,False,NULL);
	   popIid=PopupIid(GW.parButton[i]);
	   XtAddEventHandler(GW.parButton[i],ButtonPressMask,False,PostItCB,popIid);
	   break;

	 case PAR_NUZR:

           XtVaSetValues(GW.parButton[i],XmNeditable,False,NULL);
	   GW.popUzr=PopupUzr(GW.parButton[i]);
	   XtAddEventHandler(GW.parButton[i],ButtonPressMask,False,PostItCB1,GW.popUzr);
	   break;

	 case PAR_NTHL:

           XtVaSetValues(GW.parButton[i],XmNeditable,False,NULL);
	   GW.popThl=PopupThl(GW.parButton[i]);
	   XtAddEventHandler(GW.parButton[i],ButtonPressMask,False,PostItCB1,GW.popThl);
	   break;

	 case PAR_NTHU:

           XtVaSetValues(GW.parButton[i],XmNeditable,False,NULL);
	   GW.popThu=PopupThu(GW.parButton[i]);
	   XtAddEventHandler(GW.parButton[i],ButtonPressMask,False,PostItCB1,GW.popThu);
	   break;

	 default:
	   break;
	 }

   }
 }


#ifdef _NO_PROTO
void	OkFormDefCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void	OkFormDefCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  String value;
  int i,k1,k2;
  ClientData *tmp;

#if 0

  SaveParScreen(1);
  SaveParScreen(2);
  SaveParScreen(3);
  SaveParScreen(4);
  SetParScreen();

  tmp = (ClientData*) userData;
  if(tmp->data==1)
    XtUnmanageChild(tmp->widget);



#endif

  tmp = (ClientData*) userData;

  for(i=0; i<GP.numParameter; i++) {
    XtVaGetValues(GW.parButton[i],XmNvalue,&value,NULL);
    GP.parValue[i] = atof(value);

	 switch(i) {
	 case PAR_NDIM:
	 case PAR_NBC:
	 case PAR_NINT:
	 case PAR_JAC:
	 case PAR_NTST:
	 case PAR_NCOL:
	 case PAR_IAD:
	 case PAR_NWTN:
	 case PAR_ITNW:
	 case PAR_IADS:
	 case PAR_NMX:
	 case PAR_NICP:
	 case PAR_ICP:
	 case PAR_ILP:
	 case PAR_ISP:
	 case PAR_ISW:
	 case PAR_MXBF:
	 case PAR_IRS:
	 case PAR_IPS:
	 case PAR_NPR:
	 case PAR_IID:
	 case PAR_IPLT:
	 case PAR_NUZR:
	 case PAR_NTHL:
	 case PAR_NTHU:

	   sprintf(GP.parStrValue[i],"%d",(int)GP.parValue[i]);
	   break;

	 case PAR_EPSL:
	 case PAR_EPSU:
	 case PAR_EPSS:
	 case PAR_DS:
	 case PAR_DSMIN:
	 case PAR_DSMAX:
	 case PAR_RL0:
	 case PAR_RL1:
	 case PAR_A0:
	 case PAR_A1:
	   sprintf(GP.parStrValue[i],"%lg",GP.parValue[i]);
	   break;

	 default:
	   break;
	 }
  }


    /*   update problem settings */

    XtVaSetValues(GW.probButton[0],XmNvalue,GP.parStrValue[PAR_NDIM],NULL);
    XtVaSetValues(GW.probButton[1],XmNvalue,GP.parStrValue[PAR_NBC],NULL);
    XtVaSetValues(GW.probButton[2],XmNvalue,GP.parStrValue[PAR_NINT],NULL);
    XtVaSetValues(GW.probButton[3],XmNvalue,GP.parStrValue[PAR_JAC],NULL);

    /*   update discretization settings */

    XtVaSetValues(GW.disButton[0],XmNvalue,GP.parStrValue[PAR_NTST],NULL);
    XtVaSetValues(GW.disButton[1],XmNvalue,GP.parStrValue[PAR_NCOL],NULL);
    XtVaSetValues(GW.disButton[2],XmNvalue,GP.parStrValue[PAR_IAD],NULL);


    /* update tolerance settings */

    XtVaSetValues(GW.tolButton[0],XmNvalue,GP.parStrValue[PAR_EPSL],NULL);
    XtVaSetValues(GW.tolButton[1],XmNvalue,GP.parStrValue[PAR_EPSU],NULL);
    XtVaSetValues(GW.tolButton[2],XmNvalue,GP.parStrValue[PAR_EPSS],NULL);
    XtVaSetValues(GW.tolButton[3],XmNvalue,GP.parStrValue[PAR_ITMX],NULL);
    XtVaSetValues(GW.tolButton[4],XmNvalue,GP.parStrValue[PAR_NWTN],NULL);
    XtVaSetValues(GW.tolButton[5],XmNvalue,GP.parStrValue[PAR_ITNW],NULL);

 
   /* update step size settings */

    XtVaSetValues(GW.stepButton[0],XmNvalue,GP.parStrValue[PAR_DS],NULL);
    XtVaSetValues(GW.stepButton[1],XmNvalue,GP.parStrValue[PAR_DSMIN],NULL);
    XtVaSetValues(GW.stepButton[2],XmNvalue,GP.parStrValue[PAR_DSMAX],NULL);
    XtVaSetValues(GW.stepButton[3],XmNvalue,GP.parStrValue[PAR_IADS],NULL);
    XtVaSetValues(GW.stepButton[4],XmNvalue,GP.parStrValue[PAR_NTHL],NULL);
    XtVaSetValues(GW.stepButton[5],XmNvalue,GP.parStrValue[PAR_NTHU],NULL);

    /* update limit settings */

    XtVaSetValues(GW.limButton[0],XmNvalue,GP.parStrValue[PAR_NMX],NULL);
    XtVaSetValues(GW.limButton[1],XmNvalue,GP.parStrValue[PAR_RL0],NULL);
    XtVaSetValues(GW.limButton[2],XmNvalue,GP.parStrValue[PAR_RL1],NULL);
    XtVaSetValues(GW.limButton[3],XmNvalue,GP.parStrValue[PAR_A0],NULL);
    XtVaSetValues(GW.limButton[4],XmNvalue,GP.parStrValue[PAR_A1],NULL);


    /* update continuation  setting */

    XtVaSetValues(GW.conButton[0],XmNvalue,GP.parStrValue[PAR_NICP],NULL);
    XtVaSetValues(GW.conButton[1],XmNvalue,GP.parStrValue[PAR_ICP],NULL);


    /* update run setting */


    XtVaSetValues(GW.runButton[0],XmNvalue,GP.parStrValue[PAR_ILP],NULL);
    XtVaSetValues(GW.runButton[1],XmNvalue,GP.parStrValue[PAR_ISP],NULL);
    XtVaSetValues(GW.runButton[2],XmNvalue,GP.parStrValue[PAR_ISW],NULL);
    XtVaSetValues(GW.runButton[3],XmNvalue,GP.parStrValue[PAR_MXBF],NULL);
    XtVaSetValues(GW.runButton[4],XmNvalue,GP.parStrValue[PAR_IRS],NULL);
    XtVaSetValues(GW.runButton[5],XmNvalue,GP.parStrValue[PAR_IPS],NULL);

    /* update output settings */

    XtVaSetValues(GW.outButton[0],XmNvalue,GP.parStrValue[PAR_NPR],NULL);
    XtVaSetValues(GW.outButton[1],XmNvalue,GP.parStrValue[PAR_IID],NULL);
    XtVaSetValues(GW.outButton[2],XmNvalue,GP.parStrValue[PAR_IPLT],NULL);
    XtVaSetValues(GW.outButton[3],XmNvalue,GP.parStrValue[PAR_NUZR],NULL);

  if(tmp->data==1)
    XtUnmanageChild(tmp->widget);


}



/*
***********************************************************************
**
**			CreateActionMenu.c
**                           
************************************************************************
*/


#ifdef _NO_PROTO
void	CreateActionMenu(menuBar)
Widget	menuBar;
#else
void	CreateActionMenu(Widget menuBar)
#endif

{
  Widget actionPDPN,actionCSCD,compPUSH,runPUSH,stopPUSH,lineSEPT;

  actionPDPN = XmCreatePulldownMenu(menuBar,"ActionPullDown",NULL,0);
  actionCSCD = XmCreateCascadeButton(menuBar,"Action",NULL,0);
  XtVaSetValues(actionCSCD,XmNsubMenuId,actionPDPN,XmNmnemonic,'A',NULL);
  XtManageChild(actionCSCD);
/*
  XtManageChild(actionPDPN);
*/
  compPUSH = XmCreatePushButtonGadget(actionPDPN,"Compile",NULL,0);
  XtVaSetValues(compPUSH,XmNmnemonic,'C',NULL);
  XtManageChild(compPUSH);  
  XtAddCallback(compPUSH,XmNactivateCallback,RunCB,NULL);

  runPUSH = XmCreatePushButtonGadget(actionPDPN,"Run",NULL,0);
  XtVaSetValues(runPUSH,XmNmnemonic,'R',NULL);
  XtManageChild(runPUSH);  
  XtAddCallback(runPUSH,XmNactivateCallback,RunCB,NULL);

  lineSEPT = XmCreateSeparatorGadget(actionPDPN,"Separator",NULL,0);
  XtManageChild(lineSEPT);  

  stopPUSH = XmCreatePushButtonGadget(actionPDPN,"Stop",NULL,0);
  XtVaSetValues(stopPUSH,XmNmnemonic,'S',NULL);
  XtManageChild(stopPUSH);  
  XtAddCallback(stopPUSH,XmNactivateCallback,StopCB,NULL);


}


#ifdef	_NO_PROTO
void	CleanCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void	CleanCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
    if(strcmp(GP.fileName,EMPTY) != 0)
      system("make clean &");
    else
      printf("No equation was loaded\n");
}


#ifdef	_NO_PROTO
void	RunCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void	RunCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
     char name[20],env[20],command[200];
     int  i=0,what;
     FILE *fp;

     what = (size_t) userData;
     
     if( strcmp(GP.fileName,EMPTY) != 0 ) {

       if(what == 1) {

	 env[0] = '\0';	
	 fp = fopen("Makefile","r");
	 if(fp == NULL)
	   system("cp $AUTO_DIR/gui/auto.makefile ./Makefile");
/*	 if (fp != NULL)    fclose(fp); */

	 strcpy(name,ProgramName(GP.fileName));
	 while(name[i] != '.') {
	   env[i] = name[i];
	   ++i;
	 }
	 env[i] = '\0';

         strcpy(name,"s.");
	 strcat(name,env);
	 fp = fopen(name,"r");
	 if (fp != NULL) {
	   strcpy(command,"cp ");
	   strcat(command,name);
	   strcat(command," fort.3");
	   system(command);
	 }
	 if (fp != NULL)    fclose(fp);

	 strcpy(name,"c.");
	 strcat(name,env);

	 /* WriteRfile(CURRENT_RFILE); */

	  WriteRfile(name); 

	 
	 if(i==0) {
	   if(GP.parValue[PAR_IRS] > 0.0) {
	     if(GP.restart == 0)
	       sprintf(command,"make restart PROGRAMNAME= RESTARTNAME= &");
	     else {
	       sprintf(command,"make restart PROGRAMNAME= RESTARTNAME=%s &",GP.q);
	       GP.restart = 0; 
	     }
	   }
	   else {
	     if(GP.restart == 0) {
	       sprintf(command,"make PROGRAMNAME= RESTARTNAME= &");
	     }
	     else {
	       sprintf(command,"make PROGRAMNAME= RESTARTNAME=%s &",GP.q);
	       GP.restart = 0;
	     }
	   }

	   system(command);
	 }
	 else {
	   if(GP.parValue[PAR_IRS] > 0.0)	   {
	     if(GP.restart == 0) {
	       sprintf(command,"make restart PROGRAMNAME=%s RESTARTNAME=%s &",env,env);
	     }
	     else {
	       sprintf(command,"make restart PROGRAMNAME=%s RESTARTNAME=%s &",env,GP.q);
	       GP.restart = 0;
	     }
	   }
	   else {
	     if(GP.restart == 0) {
	       sprintf(command,"make PROGRAMNAME=%s RESTARTNAME=%s &",env,env);
	     }
	     else {
	       sprintf(command,"make PROGRAMNAME=%s RESTARTNAME=%s &",env,GP.q);	       
	       GP.restart = 0;
	     }
	   }
	   system(command);
	 }
	 

/*   
	 if(i==0) {
	   putenv("PROGRAMNAME=");
	 }
	 else {
	   	
	   strcpy(GP.command,"PROGRAMNAME=");
	   strcat(GP.command,env);
	   putenv(GP.command);	  

	 }

	 if (GP.parValue[PAR_IRS] > 0.0)
	   system("make restart &");
	 else
	   system("make &");
*/

       }

     }
     else
       printf("No equation was loaded\n");

}



#ifdef	_NO_PROTO
void	StopCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void	StopCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  char jobName[10];

  if(strcmp(GP.fileName,EMPTY) == 0)
    printf("No running program to be killed\n");

  else { 

    GetJobName(jobName,ProgramName(GP.fileName));
    if(strcmp(jobName,EMPTY) != 0)
      GP.autoPid = GetAutoPid(jobName);    
    
    if(GP.autoPid == -1) 
      printf("No running program to be killed\n");
    else {
      kill(GP.autoPid,SIGKILL);
      printf("The running program has been killed\n");
    }
  }

}	

/*
***********************************************************************
**
**			CreateSaveMenu.c
**                           
************************************************************************
*/

#ifdef _NO_PROTO
void	CreateSaveMenu(menuBar)
Widget	menuBar;
#else
void	CreateSaveMenu(Widget menuBar)
#endif

{
  Widget savePDPN,saveCSCD,savPUSH,sasPUSH,prompt1,button;

  savePDPN = XmCreatePulldownMenu(menuBar,"SavePullDown",NULL,0);
/*
  XtManageChild(savePDPN);
*/
  saveCSCD = XmCreateCascadeButton(menuBar,"Save",NULL,0);
  XtManageChild(saveCSCD);
  XtVaSetValues(saveCSCD,XmNsubMenuId,savePDPN,XmNmnemonic,'v',NULL);


  savPUSH = XmCreatePushButtonGadget(savePDPN,"Save",NULL,0);
  XtVaSetValues(savPUSH,XmNmnemonic,'S',NULL);
  XtManageChild(savPUSH);
  XtAddCallback(savPUSH,XmNactivateCallback,OutputCB,(XtPointer *)1);


  prompt1 = XmCreatePromptDialog(menuBar,"Prompt",NULL,0);
  XtAddCallback(prompt1,XmNokCallback,NewOutputCB,(XtPointer *)1);

  XtVaSetValues(prompt1,XmNselectionLabelString,
		XmStringCreateSimple("Enter Name"),NULL);
  button = XmSelectionBoxGetChild(prompt1,XmDIALOG_HELP_BUTTON);
  XtUnmanageChild(button);

  sasPUSH = XmCreatePushButtonGadget(savePDPN,"Save as ...",NULL,0);
  XtVaSetValues(sasPUSH,XmNmnemonic,'a',NULL);
  XtManageChild(sasPUSH);
  XtAddCallback(sasPUSH,XmNactivateCallback,PopupCB,prompt1);

}


/*
***********************************************************************
**
**			CreatePlotMenu.c
**                           
************************************************************************
*/

#ifdef _NO_PROTO
void	CreatePlotMenu(menuBar)
Widget	menuBar;
#else
void	CreatePlotMenu(Widget menuBar)
#endif

{
  Widget plotPDPN,plotCSCD,defPUSH,otherPUSH,prompt,button;

  plotPDPN = XmCreatePulldownMenu(menuBar,"PlotPullDown",NULL,0);
/*
  XtManageChild(plotPDPN);
*/
  plotCSCD = XmCreateCascadeButton(menuBar,"Plot",NULL,0);
  XtManageChild(plotCSCD);
  XtVaSetValues(plotCSCD,XmNsubMenuId,plotPDPN,XmNmnemonic,'P',NULL);
 
  defPUSH = XmCreatePushButtonGadget(plotPDPN,"Plot",NULL,0);
  XtVaSetValues(defPUSH,XmNmnemonic,'P',NULL);
  XtManageChild(defPUSH);
  XtAddCallback(defPUSH,XmNactivateCallback,TekCB,(XtPointer *)1);


  prompt = XmCreatePromptDialog(menuBar,"Prompt",NULL,0);
  XtAddCallback(prompt,XmNokCallback,TekCB,(XtPointer)2);
  XtVaSetValues(prompt,XmNselectionLabelString,
		XmStringCreateSimple("Enter Name"),NULL);
  button = XmSelectionBoxGetChild(prompt,XmDIALOG_HELP_BUTTON);
  XtUnmanageChild(button);

  otherPUSH = XmCreatePushButtonGadget(plotPDPN,"Name ...",NULL,0);
  XtVaSetValues(otherPUSH,XmNmnemonic,'N',NULL);
  XtManageChild(otherPUSH);
  XtAddCallback(otherPUSH,XmNactivateCallback,PopupCB,prompt);

}


/*
***********************************************************************
**
**			CreateAppendMenu.c
**                           
************************************************************************
*/

#ifdef _NO_PROTO
void	CreateAppendMenu(menuBar)
Widget	menuBar;
#else
void	CreateAppendMenu(Widget menuBar)
#endif

{
  Widget appendPDPN,appendCSCD,appPUSH,aptPUSH,prompt2,button,bn;

  appendPDPN = XmCreatePulldownMenu(menuBar,"AppendPullDown",NULL,0);
/*
  XtManageChild(appendPDPN);
*/
  appendCSCD = XmCreateCascadeButton(menuBar,"Append",NULL,0);
  XtManageChild(appendCSCD);
  XtVaSetValues(appendCSCD,XmNsubMenuId,appendPDPN,XmNmnemonic,'A',NULL);
 
  appPUSH = XmCreatePushButtonGadget(appendPDPN,"Append",NULL,0);
  XtVaSetValues(appPUSH,XmNmnemonic,'A',NULL);
  XtManageChild(appPUSH);
  XtAddCallback(appPUSH,XmNactivateCallback,OutputCB,(XtPointer)2);

 
  prompt2 = XmCreatePromptDialog(menuBar,"Prompt",NULL,0);
  XtAddCallback(prompt2,XmNokCallback,NewOutputCB,(XtPointer)2);

  XtVaSetValues(prompt2,XmNselectionLabelString,
		XmStringCreateSimple("Enter Name"),NULL);
  bn = XmSelectionBoxGetChild(prompt2,XmDIALOG_HELP_BUTTON);
  XtUnmanageChild(bn);

  button = XmSelectionBoxGetChild(prompt2,XmDIALOG_HELP_BUTTON);
  XtUnmanageChild(button);

  aptPUSH = XmCreatePushButtonGadget(appendPDPN,"Append to ...",NULL,0);
  XtVaSetValues(aptPUSH,XmNmnemonic,'t',NULL);
  XtManageChild(aptPUSH);
  XtAddCallback(aptPUSH,XmNactivateCallback,PopupCB,prompt2);


}



#ifdef _NO_PROTO
void   NewOutputCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   NewOutputCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{ 
  int who;
  char *newName,command[100];
  XmSelectionBoxCallbackStruct *cbs;

  cbs = (XmSelectionBoxCallbackStruct *) callbackArg;
  XmStringGetLtoR(cbs->value,XmSTRING_DEFAULT_CHARSET,&newName);

  who = (size_t) userData;

/* 	
  strcpy(command,"NEW_PROGRAMNAME=");
  strcat(command,newName);
  putenv(command);
*/  
  switch(who) {
    
  case 1:                          /* save as */
    
    sprintf(command,"make saveas NEW_PROGRAMNAME=%s &",newName);
    system(command);
/*
    system("make saveas &");
*/
    break;
    
  case 2:                          /* append to */

    sprintf(command,"make appendto NEW_PROGRAMNAME=%s &",newName);
    system(command);    
/*
    system("make appendto &");
*/
    break;
    
  default:
    break;

  }


}

#ifdef _NO_PROTO
void OutputCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void OutputCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
     char name[20],env[20],command[100];
     int  i=0,what;

     what = (size_t) userData;

     if( strcmp(GP.fileName,EMPTY) != 0) {

       env[0] = '\0';
     
       strcpy(name,ProgramName(GP.fileName));
       while(name[i] != '.') {
	 env[i] = name[i];
	 ++i;
       }
       env[i]='\0';

/*
       if(i==0) {
	 putenv("PROGRAMNAME=");
       else {
	 	
	 strcpy(command,"PROGRAMNAME=");
	 strcat(command,env);
	 putenv(command);
       }
*/
       switch(what) {

       case 1:                          /* save */
	 {
	 if(i==0)
	   sprintf(command,"make save PROGRAMNAME= &");
	 else
	   sprintf(command,"make save PROGRAMNAME=%s &",env);
	 system(command);
/*
	 system("make save &");
*/
       }
	 break;

       case 2:                          /* append */
	 {
	 if(i==0)
	   sprintf(command,"make append PROGRAMNAME= &");
	 else
	   sprintf(command,"make append PROGRAMNAME=%s &",env);
	 system(command);
/*
	 system("make append &");
*/
       }
	 break;

       default:
	 break;

       }
     }
     else
       printf("No equation was loaded\n");
}



/*
***********************************************************************
**
**			CreateDemoMenu.c
**                           
************************************************************************
*/


#ifdef _NO_PROTO
void	CreateDemoMenu(menuBar)
Widget	menuBar;
#else
void	CreateDemoMenu(Widget menuBar)
#endif

{
  Widget demoPDPN,demoCSCD,selPUSH,clselPUSH,allPUSH,clallPUSH,lineSEPT;

  demoPDPN = XmCreatePulldownMenu(menuBar,"DemoPullDown",NULL,0);
/*
  XtManageChild(demoPDPN);
*/
  demoCSCD = XmCreateCascadeButton(menuBar,"Demos",NULL,0);
  XtManageChild(demoCSCD);
  XtVaSetValues(demoCSCD,XmNsubMenuId,demoPDPN,XmNmnemonic,'o',NULL);
 
  selPUSH = XmCreatePushButtonGadget(demoPDPN,"Select ...",NULL,0);
  XtVaSetValues(selPUSH,XmNmnemonic,'S',NULL);
  XtManageChild(selPUSH);
  XtAddCallback(selPUSH,XmNactivateCallback,PopupCB,GW.demoList);

  clselPUSH = XmCreatePushButtonGadget(demoPDPN,"Reset",NULL,0);
  XtVaSetValues(clselPUSH,XmNmnemonic,'R',NULL);
  XtManageChild(clselPUSH);
  XtAddCallback(clselPUSH,XmNactivateCallback,clselCB,NULL);

/*

  lineSEPT = XmCreateSeparatorGadget(demoPDPN,"Separator",NULL,0);
  XtManageChild(lineSEPT);


  allPUSH = XmCreatePushButtonGadget(demoPDPN,"Run All Demo",NULL,0);
  XtVaSetValues(allPUSH,XmNmnemonic,'A',NULL);
  XtManageChild(allPUSH);
  XtAddCallback(allPUSH,XmNactivateCallback,RunAllDemoCB,NULL);


  clallPUSH = XmCreatePushButtonGadget(demoPDPN,"Reset All Demo",NULL,0);
  XtVaSetValues(clallPUSH,XmNmnemonic,'D',NULL);
  XtManageChild(clallPUSH);
  XtAddCallback(clallPUSH,XmNactivateCallback,clallCB,NULL);

*/

}


#ifdef _NO_PROTO
Widget	CreateScrolledDemoList(parent)
Widget	parent;
#else
Widget	CreateScrolledDemoList(Widget parent)
#endif

{
    Widget rtrn,list;
    XmString listTitle,run,browse;

    run    = XmStringCreateSimple("Run");
    browse = XmStringCreateSimple("Browse");
    listTitle = XmStringCreateSimple("Auto Demos:");

    rtrn = XmCreateSelectionDialog(parent,"Selection",NULL,0);
    list = XmSelectionBoxGetChild(rtrn,XmDIALOG_APPLY_BUTTON);
    XtManageChild(list);

    XtVaSetValues(rtrn,
		  XmNlistLabelString,listTitle,
                  XmNlistItems, GP.demoXmItems,
                  XmNlistItemCount,GP.numDemo,
                  XmNvisibleItemCount,8,
                  XmNokLabelString,run,
                  XmNapplyLabelString,browse,
                  NULL);

    XtAddCallback(rtrn,XmNokCallback,DemoRunCB,NULL);
    XtAddCallback(rtrn,XmNapplyCallback,DemoBrowseCB,NULL);
    XtAddCallback(rtrn,XmNhelpCallback,DemoHelpCB,NULL);


    XmStringFree(run);
    XmStringFree(browse);
    XmStringFree(listTitle);

    return rtrn;

}



#ifdef _NO_PROTO
Widget	CreateDemoList(parent)
Widget	parent;
#else
Widget	CreateDemoList(Widget parent)
#endif

{
    Widget rtrn,list;
    XmString listTitle;  

    listTitle = XmStringCreateSimple("Auto Demos:");

    rtrn = XmCreateSelectionDialog(parent,"Selection",NULL,0);
    list = XmSelectionBoxGetChild(rtrn,XmDIALOG_HELP_BUTTON);
    XtUnmanageChild(list);
    list = XmSelectionBoxGetChild(rtrn,XmDIALOG_APPLY_BUTTON);
    XtUnmanageChild(list);

    XtVaSetValues(rtrn,
		  XmNlistLabelString,listTitle,
                  XmNlistItems, GP.demoXmItems,
                  XmNlistItemCount,GP.numDemo,
                  XmNvisibleItemCount,8,
/*
                  XmNokLabelString,run,
                  XmNapplyLabelString,browse,
*/
                  NULL);

    XtAddCallback(rtrn,XmNokCallback,DemoCopyCB,NULL);

    XmStringFree(listTitle);

    return rtrn;

}


#ifdef _NO_PROTO
void DemoCopyCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void DemoCopyCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
   XmString item;
   XmSelectionBoxCallbackStruct *cbs;
   int i,index = -1, k;
   char cmd[200],demo[100];
   FILE *fp;

   cbs = (XmSelectionBoxCallbackStruct *) callbackArg;
   item=cbs->value;

   for(i=0; i<GP.numDemo; i++)  {
     if( XmStringCompare(item,GP.demoXmItems[i]) ) {
       index = i;
       k = strlen(demoItems[index]);
       strncpy(GP.demoFileName,demoItems[index],k-4);
       GP.demoFileName[k-4] = '\0';
       strcpy(GP.fileName,demoItems[index]);

       strcpy(cmd,"cp ");
       strcpy(demo,GP.autoDir);
       strcat(demo,"/demos/");
       strcat(demo,GP.demoFileName);
       strcat(cmd,demo);
       strcat(cmd,"/*");
       strcat(cmd,GP.demoFileName);
       strcat(cmd,"*  . &");
       system(cmd);
	
       strcpy(cmd,demo);
       strcat(cmd,"/");
       strcat(cmd,GP.fileName);

      if (!OpenFile(cmd)) {
	fprintf(stderr, "\nWarning: unable to open the file\n");
	break;
      }
      printf("\nLoading %s to buffer ... done\n",demoItems[index]);
 
      /* load rfile  */

      strcpy(cmd,demo);
      strcat(cmd,"/c.");
      strcat(cmd,GP.demoFileName);

      fp = fopen(cmd,"r");
      if(fp != NULL  && ReadRfile(cmd) == 0 ) {
	SetParScreen();
	printf("Loading c.%s to buffer ... done\n",GP.demoFileName);
      }
      else
	printf("c.%s is not available\n",GP.demoFileName);

      if (fp != NULL)    fclose(fp);

       break;
     }
   }
   if(index == -1)
     printf("\nNo item was selected\n");
}

#ifdef _NO_PROTO
void DemoRunCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void DemoRunCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
   XmString item;
   XmSelectionBoxCallbackStruct *cbs;
   int i,index = -1,k;

   cbs = (XmSelectionBoxCallbackStruct *) callbackArg;
   item=cbs->value;

   for(i=0; i<GP.numDemo; i++)  {
     if( XmStringCompare(item,GP.demoXmItems[i]) ) {
       index = i;
       k = strlen(demoItems[index]);
       strncpy(GP.demoFileName,demoItems[index],k-4);
       GP.demoFileName[k-4] = '\0';
       RunDemo(GP.demoFileName);
       break;
     }
   }
   if(index == -1)
     printf("\nNo item was selected\n");

}

#ifdef _NO_PROTO
void DemoBrowseCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void DemoBrowseCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{

   XmString item;
   int i,k,index,fileLen;
   FILE *fp=NULL;
   struct stat statbuf;
   char *fileStr,file[100],name[10];
   
   index = -1;

   XtVaGetValues(w,XmNtextString,&item,NULL);
   for(i=0; i<GP.numDemo; i++)  {
     if( XmStringCompare(item,GP.demoXmItems[i]) ) {
       index = i;
       k = strlen(demoItems[index]);
       strncpy(name,demoItems[index],k-4); 
       name[k-2] = '\0';
       strcpy(file,GP.autoDir);
       strcat(file,"/demos/");
       strcat(file,name);
       strcat(file,"/");
       strcat(file,demoItems[index]);
       fp=fopen(file,"r");
       if(stat(file,&statbuf) == 0)
	 fileLen=statbuf.st_size;
       else
	 fileLen=50000;   /* arbitrary file length */

       fileStr= (char *) XtMalloc(fileLen);
       fread(fileStr,sizeof(char),fileLen,fp);
       XmTextSetString(GW.textField,fileStr);
       XtFree(fileStr);
       XtManageChild(GW.textDALG);
       XtUnmanageChild(GW.demoList);

       break;
     }
   }
   if(index == -1)
     printf("No item was selected\n");

}

#ifdef _NO_PROTO
void DemoHelpCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void DemoHelpCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
   XmString item;
   int i,index = -1;

   XtVaGetValues(w,XmNtextString,&item,NULL);
   for(i=0; i<GP.numDemo; i++)  {
     if( XmStringCompare(item,GP.demoXmItems[i]) ) {
       index = i;

       XmTextSetString(GW.textField,demoHelp[index]);
       XtManageChild(GW.textDALG);

       break;
     }
   }
   if(index == -1)
     printf("No item was selected\n");

}


/*
***********************************************************************
**
**			CreateDefineMenu.c
**                           
************************************************************************
*/


#ifdef _NO_PROTO
void	CreateDefineMenu(menuBar)
Widget	menuBar;
#else
void	CreateDefineMenu(Widget menuBar)
#endif

{
  Widget defineCSCD,define;

  define = CreateFormDefault(menuBar);
  defineCSCD = XmCreateCascadeButton(menuBar,"Define",NULL,0);
  XtManageChild(defineCSCD);
  XtVaSetValues(defineCSCD,XmNmnemonic,'D',NULL);   
  XtAddCallback(defineCSCD,XmNactivateCallback,PopupCB,define);

}

#if 0

/*
***********************************************************************
**
**			CreateLoadRfileMenu.c
**                           
************************************************************************
*/


#ifdef _NO_PROTO
void	CreateLoadRfileMenu(menuBar)
Widget	menuBar;
#else
void	CreateLoadRfileMenu(Widget menuBar)
#endif

{
  Widget miscPDPN,miscCSCD;
  Widget readPUSH,readFile;

  miscPDPN = XmCreatePulldownMenu(menuBar,"MiscPullDown",NULL,0);

  miscCSCD = XmCreateCascadeButton(menuBar,"Load",NULL,0);
  XtManageChild(miscCSCD);
  XtVaSetValues(miscCSCD,XmNsubMenuId,miscPDPN,XmNmnemonic,'L',NULL);
   
  readFile = ReadFile(menuBar);
  readPUSH = XmCreatePushButtonGadget(miscPDPN,"New Rfile",NULL,0);
  XtVaSetValues(readPUSH,XmNmnemonic,'N',NULL);
  XtManageChild(readPUSH);
  XtAddCallback(readPUSH,XmNactivateCallback,PopupCB,readFile);    


  readPUSH = XmCreatePushButtonGadget(miscPDPN,"Default Rfile",NULL,0);
  XtVaSetValues(readPUSH,XmNmnemonic,'D',NULL);
  XtManageChild(readPUSH);
  XtAddCallback(readPUSH,XmNactivateCallback,ReadDefCB,NULL);


}


#endif



/*
***********************************************************************
**
**			CreateRunMenu.c
**                           
************************************************************************
*/


#ifdef _NO_PROTO
void	CreateRunMenu(menuBar)
Widget	menuBar;
#else
void	CreateRunMenu(Widget menuBar)
#endif

{
  Widget runCSCD;
/*
  Widget runPDPN,runCSCD;
  Widget runPUSH;
*/
/*
  runPDPN = XmCreatePulldownMenu(menuBar,"RunPullDown",NULL,0);
*/
  runCSCD = XmCreateCascadeButton(menuBar,"Run",NULL,0);
  XtManageChild(runCSCD);
/*
  XtVaSetValues(runCSCD,XmNsubMenuId,runPDPN,XmNmnemonic,'R',NULL);
*/
  XtVaSetValues(runCSCD,XmNmnemonic,'R',NULL);
  XtAddCallback(runCSCD,XmNactivateCallback,RunCB,(XtPointer *)1);

/*
  runPUSH = XmCreatePushButtonGadget(runPDPN,"Run",NULL,0);
  XtVaSetValues(runPUSH,XmNmnemonic,'R',NULL);
  XtManageChild(runPUSH);
  XtAddCallback(runPUSH,XmNactivateCallback,RunCB,(XtPointer *)1);
*/

}


/*
***********************************************************************
**
**			CreateFilesMenu.c
**                           
************************************************************************
*/


#ifdef _NO_PROTO
void	CreateFilesMenu(menuBar)
Widget	menuBar;
#else
void	CreateFilesMenu(Widget menuBar)
#endif

{
  Widget filePDPN,fileCSCD,reset,delete;
  static ClientData userData1,userData2,userData3;
  Widget bn1,bn2,bn3,bn4,bn5,bn6,bn7,bn8;
  static Widget copy,move,append;




  reset = XmCreatePromptDialog(menuBar,"pm",NULL,0);
  XtAddCallback(reset,XmNokCallback,RestartCB,NULL);

  XtVaSetValues(reset,XmNselectionLabelString,
		XmStringCreateSimple("Enter name"),NULL);
  bn1 = XmSelectionBoxGetChild(reset,XmDIALOG_HELP_BUTTON);
  XtUnmanageChild(bn1);



  copy = CopyData(menuBar);
  userData1.widget = copy;
  userData1.data = 1;


  move = MoveData(menuBar);
  userData2.widget = move;
  userData2.data = 2;


  append = AppendData(menuBar);
  userData3.widget = append;
  userData3.data = 3;



  delete = XmCreatePromptDialog(menuBar,"pm",NULL,0);
  XtAddCallback(delete,XmNokCallback,DeleteCB,NULL);
  XtVaSetValues(delete,XmNselectionLabelString,
		XmStringCreateSimple("Enter name"),NULL);
  bn5 = XmSelectionBoxGetChild(delete,XmDIALOG_HELP_BUTTON);
  XtUnmanageChild(bn5);



  filePDPN = XmCreatePulldownMenu(menuBar,"FilesPullDown",NULL,0);
/*
  XtManageChild(filePDPN);
*/

  fileCSCD = XmCreateCascadeButton(menuBar,"Files",NULL,0);
  XtManageChild(fileCSCD);
  XtVaSetValues(fileCSCD,XmNsubMenuId,filePDPN,XmNmnemonic,'F',NULL);


  bn2 = XmCreatePushButtonGadget(filePDPN,"Restart",NULL,0);
  XtVaSetValues(bn2,XmNmnemonic,'R',NULL);
  XtManageChild(bn2);
  XtAddCallback(bn2,XmNactivateCallback,PopupCB,reset);


  bn3 = XmCreatePushButtonGadget(filePDPN,"Copy",NULL,0);
  XtVaSetValues(bn3,XmNmnemonic,'C',NULL);
  XtManageChild(bn3);
  XtAddCallback(bn3,XmNactivateCallback,PopupCB1,&userData1);


  bn8 = XmCreatePushButtonGadget(filePDPN,"Append",NULL,0);
  XtVaSetValues(bn8,XmNmnemonic,'A',NULL);
  XtManageChild(bn8);
  XtAddCallback(bn8,XmNactivateCallback,PopupCB1,&userData3);


  bn4 = XmCreatePushButtonGadget(filePDPN,"Move",NULL,0);
  XtVaSetValues(bn4,XmNmnemonic,'M',NULL);
  XtManageChild(bn4);
  XtAddCallback(bn4,XmNactivateCallback,PopupCB1,&userData2);



  bn6 = XmCreatePushButtonGadget(filePDPN,"Delete",NULL,0);
  XtVaSetValues(bn6,XmNmnemonic,'D',NULL);
  XtManageChild(bn6);
  XtAddCallback(bn6,XmNactivateCallback,PopupCB,delete);


  bn7 = XmCreatePushButtonGadget(filePDPN,"Clean",NULL,0);
  XtVaSetValues(bn7,XmNmnemonic,'l',NULL);
  XtManageChild(bn7);
  XtAddCallback(bn7,XmNactivateCallback,CleanCB,NULL);

}




#ifdef _NO_PROTO
void	ReadDefCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void	ReadDefCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  char command[100];

  strcpy(command,GP.autoDir);
  strcat(command,"/gui/");
  strcat(command,DEFAULT_RFILE);
  if(ReadRfile(command) == 0) 
    SetParScreen();
}

/*
***********************************************************************
**
**			CreateBrowseRfileMenu.c
**                           
************************************************************************
*/


#ifdef _NO_PROTO
void	CreateBrowseRfileMenu(menuBar)
Widget	menuBar;
#else
void	CreateBrowseRfileMenu(Widget menuBar)
#endif

{
  Widget miscPDPN,miscCSCD;
  Widget readPUSH,readFile;

  miscPDPN = XmCreatePulldownMenu(menuBar,"MiscPullDown",NULL,0);

  miscCSCD = XmCreateCascadeButton(menuBar,"Browse",NULL,0);
  XtManageChild(miscCSCD);
  XtVaSetValues(miscCSCD,XmNsubMenuId,miscPDPN,XmNmnemonic,'B',NULL);
   
  readFile = ReadFile(menuBar);
  readPUSH = XmCreatePushButtonGadget(miscPDPN,"New Rfile",NULL,0);
  XtVaSetValues(readPUSH,XmNmnemonic,'N',NULL);
  XtManageChild(readPUSH);
  XtAddCallback(readPUSH,XmNactivateCallback,PopupCB,readFile);    


  readPUSH = XmCreatePushButtonGadget(miscPDPN,"Default Rfile",NULL,0);
  XtVaSetValues(readPUSH,XmNmnemonic,'D',NULL);
  XtManageChild(readPUSH);

}


/*
***********************************************************************
**
**			CreateMiscMenu.c 
**                           
************************************************************************
*/


#ifdef _NO_PROTO
void	CreateMiscMenu(menuBar)
Widget	menuBar;
#else
void	CreateMiscMenu(Widget menuBar)
#endif

{
  Widget miscPDPN,miscCSCD,filePUSH,tekPUSH,vtPUSH;
  Widget emacsPDPN,emacsCSCD,enewPUSH,eopenPUSH;
  Widget xeditPDPN,xeditCSCD,xnewPUSH,xopenPUSH;
  Widget lineSEPT,newFile,openFile;
  Widget readPUSH,readFile,printPUSH;

  miscPDPN = XmCreatePulldownMenu(menuBar,"MiscPullDown",NULL,0);
/*
  XtManageChild(miscPDPN);
*/
  miscCSCD = XmCreateCascadeButton(menuBar,"Misc",NULL,0);
  XtManageChild(miscCSCD);
  XtVaSetValues(miscCSCD,XmNsubMenuId,miscPDPN,XmNmnemonic,'M',NULL);
 
  
  tekPUSH = XmCreatePushButtonGadget(miscPDPN,"Tek Window ...",NULL,0);
  XtVaSetValues(tekPUSH,XmNmnemonic,'T',NULL);
  XtManageChild(tekPUSH);
  XtAddCallback(tekPUSH,XmNactivateCallback,TekCB,0);


  vtPUSH = XmCreatePushButtonGadget(miscPDPN,"VT102 Window ...",NULL,0);
  XtVaSetValues(vtPUSH,XmNmnemonic,'V',NULL);
  XtManageChild(vtPUSH);
  XtAddCallback(vtPUSH,XmNactivateCallback,VtCB,NULL);



  lineSEPT = XmCreateSeparatorGadget(miscPDPN,"Separator",NULL,0);
  XtManageChild(lineSEPT);

  emacsPDPN = XmCreatePulldownMenu(miscPDPN,"EmacsPullDown",NULL,0);
/*
  XtManageChild(emacsPDPN);
*/
  emacsCSCD = XmCreateCascadeButton(miscPDPN,"Emacs",NULL,0);
  XtManageChild(emacsCSCD);
  XtVaSetValues(emacsCSCD,XmNsubMenuId,emacsPDPN,XmNmnemonic,'E',NULL);


  newFile = CreateNewFileDialog(menuBar,EMACS_OPTION);
  enewPUSH = XmCreatePushButtonGadget(emacsPDPN,"New ...",NULL,0);
  XtVaSetValues(enewPUSH,XmNmnemonic,'N',NULL);
  XtManageChild(enewPUSH);
  XtAddCallback(enewPUSH,XmNactivateCallback,PopupCB,newFile);  


  openFile = CreateFileSelectionDialog(menuBar,EMACS_OPTION);
  eopenPUSH = XmCreatePushButtonGadget(emacsPDPN,"Open ...",NULL,0);
  XtVaSetValues(eopenPUSH,XmNmnemonic,'O',NULL);
  XtManageChild(eopenPUSH);
  XtAddCallback(eopenPUSH,XmNactivateCallback,PopupCB,openFile);

  xeditPDPN = XmCreatePulldownMenu(miscPDPN,"XeditPullDown",NULL,0);
/*
  XtManageChild(xeditPDPN);
*/
  xeditCSCD = XmCreateCascadeButton(miscPDPN,"Xedit",NULL,0);
  XtManageChild(xeditCSCD);
  XtVaSetValues(xeditCSCD,XmNsubMenuId,xeditPDPN,XmNmnemonic,'X',NULL);


  newFile = CreateNewFileDialog(menuBar,XEDIT_OPTION);
  xnewPUSH = XmCreatePushButtonGadget(xeditPDPN,"New ...",NULL,0);
  XtVaSetValues(xnewPUSH,XmNmnemonic,'N',NULL);
  XtManageChild(xnewPUSH);
  XtAddCallback(xnewPUSH,XmNactivateCallback,PopupCB,newFile);  


  openFile = CreateFileSelectionDialog(menuBar,XEDIT_OPTION);
  xopenPUSH = XmCreatePushButtonGadget(xeditPDPN,"Open ...",NULL,0);
  XtVaSetValues(xopenPUSH,XmNmnemonic,'O',NULL);
  XtManageChild(xopenPUSH);
  XtAddCallback(xopenPUSH,XmNactivateCallback,PopupCB,openFile);  

  lineSEPT = XmCreateSeparatorGadget(miscPDPN,"Separator",NULL,0);
  XtManageChild(lineSEPT);

  printPUSH=XmCreatePushButtonGadget(miscPDPN,"Print",NULL,0);
  XtManageChild(printPUSH);
  XtVaSetValues(printPUSH,XmNmnemonic,'P',NULL);
  XtAddCallback(printPUSH,XmNactivateCallback,PrintCB,NULL);


#if 0

  readFile = ReadFile(menuBar);
  readPUSH = XmCreatePushButtonGadget(miscPDPN,"Load New Rfile",NULL,0);
  XtVaSetValues(readPUSH,XmNmnemonic,'N',NULL);
  XtManageChild(readPUSH);
  XtAddCallback(readPUSH,XmNactivateCallback,PopupCB,readFile);    


  readPUSH = XmCreatePushButtonGadget(miscPDPN,"Load Default Rfile",NULL,0);
  XtVaSetValues(readPUSH,XmNmnemonic,'D',NULL);
  XtManageChild(readPUSH);
/*
  XtAddCallback(readPUSH,XmNactivateCallback,PopupCB,readFile);    
*/

#endif

}



#ifdef _NO_PROTO
void   PrintCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   PrintCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  char command[100];

	
  if(strcmp(GP.fileName,EMPTY) != 0) {

    	
    strcpy(command,PRINT_FILE);
    strcat(command,GP.fileName);
    strcat(command," &");
    if (system(command) != 0)
      fprintf(stderr, "print failed");

  }

  else 
    printf("No equation was load\n");
   

}


#ifdef _NO_PROTO
Widget	ReadFile(parent)
Widget	parent;
#else
Widget 	ReadFile(Widget parent)
#endif

{
    Widget rtrn,button;
    /*    XmString s1;*/

    rtrn = XmCreateFileSelectionDialog(parent,"Selection",NULL,0);

    /*
    s1=XmStringCreateSimple("Enter the New Rfile Name");
    XtVaSetValues(rtrn,XmNselectionLabelString,s1,NULL);
    XmStringFree(s1);
    */

    XtAddCallback(rtrn,XmNokCallback,ReadFileCB,NULL);
    XtAddCallback(rtrn,XmNcancelCallback,PopdownCB,rtrn);

    button = XmSelectionBoxGetChild(rtrn,XmDIALOG_HELP_BUTTON);
    XtUnmanageChild(button);

    return rtrn;
}



#ifdef _NO_PROTO
void	ReadFileCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void	ReadFileCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{

  char *filename;
  static char file[50];
  XmFileSelectionBoxCallbackStruct *cbs;


  cbs = (XmFileSelectionBoxCallbackStruct *) callbackArg;
  XmStringGetLtoR(cbs->value,XmSTRING_DEFAULT_CHARSET,&filename);
  strcpy(file,filename);
  if(ReadRfile(file)==0) {
    SetParScreen();
    XtUnmanageChild(w);
  }
  else {
    printf("Warning : unable to read the AUTO-constants file\n");
    XtUnmanageChild(w);
  }

}



#ifdef _NO_PROTO
void	TekCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void	TekCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif


{
  char name[20],env[20],*newName,command[100]; 
  int  i=0,what;
  XmSelectionBoxCallbackStruct *cbs;

  
  what = (size_t) userData;

  if(what == 1) {	/* plot current */

    if(strcmp(GP.fileName,EMPTY) != 0) {

      env[0] = '\0';
  
      strcpy(name,ProgramName(GP.fileName));
      while(name[i] != '.') {
	env[i] = name[i];
	++i;
      }
      env[i]='\0';

      if(i==0) {
	sprintf(command,"make plot_current PROGRAMNAME= &");
	system(command);
      }
      else {
	sprintf(command,"make plot_current PROGRAMNAME=%s &",env);
	system(command);
      }
/*
      if(i==0)
	putenv("PROGRAMNAME=");
      else {
		
	strcpy(command,"PROGRAMNAME=");
	strcat(command,env);
	putenv(command);
      }
  
      system("make plot_current &");
*/
    }
    else {
      printf("No equation was loaded\n");
    }
  }

  else if(what == 2)	{     /* plot other */

    cbs = (XmSelectionBoxCallbackStruct *) callbackArg;
    XmStringGetLtoR(cbs->value,XmSTRING_DEFAULT_CHARSET,&newName);

    sprintf(command,"make plot_other NEW_PLOTNAME=%s &",newName);
    system(command);

/*    	
    strcpy(command,"NEW_PLOTNAME=");
    strcat(command,newName);
    putenv(command);

    system("make plot_other &");
*/

  }

  else {                      /* Tek Term only */

    system("xterm -bg black -fg white -t &");

  }

}



#ifdef _NO_PROTO
void	VtCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void	VtCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
      system("xterm -bg navy -fg white +t &");
}


/*
***********************************************************************
**
**			CreateHelpMenu.c
**                           
************************************************************************
*/



#ifdef _NO_PROTO
void	CreateHelpMenu(menuBar)
Widget	menuBar;
#else
void	CreateHelpMenu(Widget menuBar)
#endif

{
  Widget helpPDPN,helpCSCD,helpPUSH;
  Widget parPUSH,manPUSH;

  helpPDPN = XmCreatePulldownMenu(menuBar,"HelpPullDown",NULL,0);
/*
  XtManageChild(helpPDPN);
*/
  helpCSCD = XmCreateCascadeButton(menuBar,"Help",NULL,0);
  XtVaSetValues(helpCSCD,XmNsubMenuId,helpPDPN,XmNmnemonic,'H',NULL);
  XtManageChild(helpCSCD);

  XtVaSetValues(menuBar,XmNmenuHelpWidget,helpCSCD,NULL);

  parPUSH = XmCreatePushButtonGadget(helpPDPN,"AUTO-Consts ...",NULL,0);
  XtVaSetValues(parPUSH,XmNmnemonic,'P',NULL);
  XtManageChild(parPUSH); 
/*
  GW.helpList = CreateScrolledHelpList(menuBar);
*/
  XtAddCallback(parPUSH,XmNactivateCallback,PopupCB,GW.helpList);
 
  manPUSH = XmCreatePushButtonGadget(helpPDPN,"User Manual ...",NULL,0);
  XtVaSetValues(manPUSH,XmNmnemonic,'M',NULL);

/*
  XtVaSetValues(manPUSH,XmNacceleratorText,XmStringCreateSimple("F1"),
		XmNaccelerator,"<Key>F1:",NULL);
*/

  XtManageChild(manPUSH); 
  XtAddCallback(manPUSH,XmNactivateCallback,ManualCB,NULL);



}


#ifdef	_NO_PROTO
void	ManualCB(w,userData,callbackArg)
Widget	w;
XtPointer userData,callbackArg;
#else
void	ManualCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  char command[100];

/*
  autoDir=copyenv(AUTO_DIR);
*/  	
  strcpy(command,"ghostview ");
  strcat(command,GP.autoDir);
  strcat(command,"/doc/auto.ps &");
  system(command);

}



#ifdef _NO_PROTO
Widget	CreateScrolledHelpList(parent)
Widget	parent;
#else
Widget	CreateScrolledHelpList(Widget parent)
#endif

{
    Widget rtrn,list;
    XmString listTitle;

    listTitle = XmStringCreateSimple("Help On Auto-Consts:");

    rtrn = XmCreateSelectionDialog(parent,"Selection",NULL,0);
    list = XmSelectionBoxGetChild(rtrn,XmDIALOG_APPLY_BUTTON);
    XtUnmanageChild(list);
    list = XmSelectionBoxGetChild(rtrn,XmDIALOG_HELP_BUTTON);
    XtUnmanageChild(list);

   
    XtVaSetValues(rtrn,
		  XmNlistLabelString,listTitle,
                  XmNlistItems, GP.parXmLabel,
                  XmNlistItemCount,GP.numParameter,
                  XmNvisibleItemCount,8,
                  NULL);

    XtAddCallback(rtrn,XmNokCallback,HelpCB,NULL);


    XmStringFree(listTitle);
/*
    XmStringFree(listItems);
*/

    return rtrn;
}




#ifdef _NO_PROTO
void HelpCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void HelpCB(Widget w, XtPointer userData,XtPointer callbackArg)
#endif

{
   XmString item;
   XmSelectionBoxCallbackStruct *cbs;
   int i,index = -1;
    

   cbs = (XmSelectionBoxCallbackStruct *) callbackArg;
   item=cbs->value;

   for(i=0; i<GP.numParameter; i++)  {
     if( XmStringCompare(item,GP.parXmLabel[i]) ) {
       index = i;
       XmTextSetString(GW.textField,strHelp[index]);
       XtManageChild(GW.textDALG);
       break;
     }
   }
   if(index == -1)
     printf("\nNo item was selected\n");



}

#ifdef _NO_PROTO
char 	**getstr(rh,ch)
int 	rh,ch;
#else
char 	**getstr(int rh,int ch)
#endif

{
	int 	i, r, c;
	char 	**s;

	r = rh+1;
	c = ch+1;
	s = (char **) malloc(r*sizeof(char *) + (r*c)*sizeof(char));
	assert(s!=NULL);
	*s = (char *)(s + r);

	for(i = 1 ; i < r; i++)
		s[i] = s[i-1] + c;

	return s;
}


#ifdef _NO_PROTO
char 	*copyenv(name)
char    *name;
#else
char 	*copyenv(char *name)
#endif

{
     char *s1 = getenv(name);
     char *s2 = s1 ? malloc(strlen(s1) + 1) : NULL;

     return (s2 ? strcpy(s2,s1) : NULL);
}


#ifdef _NO_PROTO
void   RunDemo(fileName)
char   *fileName;
#else
void   RunDemo(char *fileName)
#endif

{
  char command[100];

  strcpy(command,"cd $AUTO_DIR/demos/");
  strcat(command,fileName);
  strcat(command,"; auto ");
  strcat(command,fileName);
  strcat(command,".auto");
  system(command);
}


#ifdef _NO_PROTO
void   clselCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   clselCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{

  char command[50];

  if( strcmp(GP.demoFileName,EMPTY) != 0) {

    strcpy(command,"cd $AUTO_DIR/demos/");
    strcat(command,GP.demoFileName);
    strcat(command,"; auto clean.auto &");
    system(command);
  }
  else
    printf("No cleaning performed\n");
}


#ifdef _NO_PROTO
void   RunAllDemoCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   RunAllDemoCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  system("cd $AUTO_DIR/test; make &");
}


#ifdef _NO_PROTO
void   clallCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   clallCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  system("cd $AUTO_DIR/test; make clean &");
}


#ifdef _NO_PROTO
void   SetParCB(w,userData,callbackArg)
Widget w;
XtPointer userData;
XtPointer callbackArg;
#else
void   SetParCB(Widget w, XtPointer userData, XtPointer callbackArg)
#endif

{
   int whichPar,value;

   whichPar = ((int *)userData)[0];
   value    = ((int *)userData)[1];


   switch (whichPar) {

   case PAR_JAC:

     if(value == 0) {

       GP.parValue[PAR_JAC]=value;
       strcpy(GP.parStrValue[PAR_JAC],"0");
       XtVaSetValues(GW.parButton[PAR_JAC],XmNvalue,GP.parStrValue[PAR_JAC],NULL);
       XtVaSetValues(GW.probButton[3],XmNvalue,GP.parStrValue[PAR_JAC],NULL);

     }
     else if(value == 1) {
       
       GP.parValue[PAR_JAC]=value;
       strcpy(GP.parStrValue[PAR_JAC],"1");
       XtVaSetValues(GW.parButton[PAR_JAC],XmNvalue,GP.parStrValue[PAR_JAC],NULL);
       XtVaSetValues(GW.probButton[3],XmNvalue,GP.parStrValue[PAR_JAC],NULL);
       
     }
     else 
       printf("SetParCB : failure in PAR_JAC\n");
     
    break;
     
   case PAR_NCOL:

     if(value==2) {

       GP.parValue[PAR_NCOL]=value;
       strcpy(GP.parStrValue[PAR_NCOL],"2");
       XtVaSetValues(GW.parButton[PAR_NCOL],XmNvalue,GP.parStrValue[PAR_NCOL],NULL);
       XtVaSetValues(GW.disButton[1],XmNvalue,GP.parStrValue[PAR_NCOL],NULL);
       
     }
     else if(value==3) {

       GP.parValue[PAR_NCOL]=value;
       strcpy(GP.parStrValue[PAR_NCOL],"3");
       XtVaSetValues(GW.parButton[PAR_NCOL],XmNvalue,GP.parStrValue[PAR_NCOL],NULL);
       XtVaSetValues(GW.disButton[1],XmNvalue,GP.parStrValue[PAR_NCOL],NULL);

     }
     else if(value==4) {

       GP.parValue[PAR_NCOL]=value;
       strcpy(GP.parStrValue[PAR_NCOL],"4");
       XtVaSetValues(GW.parButton[PAR_NCOL],XmNvalue,GP.parStrValue[PAR_NCOL],NULL);
       XtVaSetValues(GW.disButton[1],XmNvalue,GP.parStrValue[PAR_NCOL],NULL);

     }
     else if(value==5) {

       GP.parValue[PAR_NCOL]=value;
       strcpy(GP.parStrValue[PAR_NCOL],"5");
       XtVaSetValues(GW.parButton[PAR_NCOL],XmNvalue,GP.parStrValue[PAR_NCOL],NULL);
       XtVaSetValues(GW.disButton[1],XmNvalue,GP.parStrValue[PAR_NCOL],NULL);

     }
     else if(value==6) {

       GP.parValue[PAR_NCOL]=value;
       strcpy(GP.parStrValue[PAR_NCOL],"6");
       XtVaSetValues(GW.parButton[PAR_NCOL],XmNvalue,GP.parStrValue[PAR_NCOL],NULL);
       XtVaSetValues(GW.disButton[1],XmNvalue,GP.parStrValue[PAR_NCOL],NULL);

     }
     else if(value==7) {

       GP.parValue[PAR_NCOL]=value;
       strcpy(GP.parStrValue[PAR_NCOL],"7");
       XtVaSetValues(GW.parButton[PAR_NCOL],XmNvalue,GP.parStrValue[PAR_NCOL],NULL);
       XtVaSetValues(GW.disButton[1],XmNvalue,GP.parStrValue[PAR_NCOL],NULL);

     }
     else
       printf("SetParCB : failure in PAR_NCOL\n");

     break;

   case PAR_ILP:

     if(value==0) {

       GP.parValue[PAR_ILP]=value;
       strcpy(GP.parStrValue[PAR_ILP],"0");
       XtVaSetValues(GW.parButton[PAR_ILP],XmNvalue,GP.parStrValue[PAR_ILP],NULL);
       XtVaSetValues(GW.runButton[0],XmNvalue,GP.parStrValue[PAR_ILP],NULL);

     }
     else if(value==1) {

       GP.parValue[PAR_ILP]=value;
       strcpy(GP.parStrValue[PAR_ILP],"1");
       XtVaSetValues(GW.parButton[PAR_ILP],XmNvalue,GP.parStrValue[PAR_ILP],NULL);
       XtVaSetValues(GW.runButton[0],XmNvalue,GP.parStrValue[PAR_ILP],NULL);

     }
     else
       printf("SetParCB : failure in PAR_ILP\n");

     break;

   case PAR_ISP:

     if(value==0) {

       GP.parValue[PAR_ISP]=value;
       strcpy(GP.parStrValue[PAR_ISP],"0");
       XtVaSetValues(GW.parButton[PAR_ISP],XmNvalue,GP.parStrValue[PAR_ISP],NULL);
       XtVaSetValues(GW.runButton[1],XmNvalue,GP.parStrValue[PAR_ISP],NULL);

     }
     else if(value==1) {

       GP.parValue[PAR_ISP]=value;
       strcpy(GP.parStrValue[PAR_ISP],"1");
       XtVaSetValues(GW.parButton[PAR_ISP],XmNvalue,GP.parStrValue[PAR_ISP],NULL);
       XtVaSetValues(GW.runButton[1],XmNvalue,GP.parStrValue[PAR_ISP],NULL);

     }
     else if(value==2) {

       GP.parValue[PAR_ISP]=value;
       strcpy(GP.parStrValue[PAR_ISP],"2");
       XtVaSetValues(GW.parButton[PAR_ISP],XmNvalue,GP.parStrValue[PAR_ISP],NULL);
       XtVaSetValues(GW.runButton[1],XmNvalue,GP.parStrValue[PAR_ISP],NULL);

     }
     else if(value==3) {

       GP.parValue[PAR_ISP]=value;
       strcpy(GP.parStrValue[PAR_ISP],"3");
       XtVaSetValues(GW.parButton[PAR_ISP],XmNvalue,GP.parStrValue[PAR_ISP],NULL);
       XtVaSetValues(GW.runButton[1],XmNvalue,GP.parStrValue[PAR_ISP],NULL);

     }
     else
       printf("SetParCB : failure in PAR_ISP\n");

     break;

   case PAR_ISW:

     if(value==-1) {

       GP.parValue[PAR_ISW]=value;
       strcpy(GP.parStrValue[PAR_ISW],"-1");;
       XtVaSetValues(GW.parButton[PAR_ISW],XmNvalue,GP.parStrValue[PAR_ISW],NULL);
       XtVaSetValues(GW.runButton[2],XmNvalue,GP.parStrValue[PAR_ISW],NULL);

     }
     else if(value==1) {

       GP.parValue[PAR_ISW]=value;
       strcpy(GP.parStrValue[PAR_ISW],"1");
       XtVaSetValues(GW.parButton[PAR_ISW],XmNvalue,GP.parStrValue[PAR_ISW],NULL);
       XtVaSetValues(GW.runButton[2],XmNvalue,GP.parStrValue[PAR_ISW],NULL);

     }
     else if(value==2) {

       GP.parValue[PAR_ISW]=value;
       strcpy(GP.parStrValue[PAR_ISW],"2");
       XtVaSetValues(GW.parButton[PAR_ISW],XmNvalue,GP.parStrValue[PAR_ISW],NULL);
       XtVaSetValues(GW.runButton[2],XmNvalue,GP.parStrValue[PAR_ISW],NULL);

     }
     else
       printf("SetParCB : failure in PAR_ISW\n");

     break;

   case PAR_IPS:

     if(value==-1) {

       GP.parValue[PAR_IPS]=value;
       strcpy(GP.parStrValue[PAR_IPS],"-1");
       XtVaSetValues(GW.parButton[PAR_IPS],XmNvalue,GP.parStrValue[PAR_IPS],NULL);
       XtVaSetValues(GW.runButton[5],XmNvalue,GP.parStrValue[PAR_IPS],NULL);

     }
     else if(value==0) {

       GP.parValue[PAR_IPS]=value;
       strcpy(GP.parStrValue[PAR_IPS],"0");
       XtVaSetValues(GW.parButton[PAR_IPS],XmNvalue,GP.parStrValue[PAR_IPS],NULL);
       XtVaSetValues(GW.runButton[5],XmNvalue,GP.parStrValue[PAR_IPS],NULL);

     }
     else if(value==1) {

       GP.parValue[PAR_IPS]=value;
       strcpy(GP.parStrValue[PAR_IPS],"1");
       XtVaSetValues(GW.parButton[PAR_IPS],XmNvalue,GP.parStrValue[PAR_IPS],NULL);
       XtVaSetValues(GW.runButton[5],XmNvalue,GP.parStrValue[PAR_IPS],NULL);

     }
     else if(value==2) {

       GP.parValue[PAR_IPS]=value;
       strcpy(GP.parStrValue[PAR_IPS],"2");
       XtVaSetValues(GW.parButton[PAR_IPS],XmNvalue,GP.parStrValue[PAR_IPS],NULL);
       XtVaSetValues(GW.runButton[5],XmNvalue,GP.parStrValue[PAR_IPS],NULL);

     }
     else if(value==3) {

       GP.parValue[PAR_IPS]=value;
       strcpy(GP.parStrValue[PAR_IPS],"3");
       XtVaSetValues(GW.parButton[PAR_IPS],XmNvalue,GP.parStrValue[PAR_IPS],NULL);
       XtVaSetValues(GW.runButton[5],XmNvalue,GP.parStrValue[PAR_IPS],NULL);

     }
     else if(value==4) {

       GP.parValue[PAR_IPS]=value;
       strcpy(GP.parStrValue[PAR_IPS],"4");
       XtVaSetValues(GW.parButton[PAR_IPS],XmNvalue,GP.parStrValue[PAR_IPS],NULL);
       XtVaSetValues(GW.runButton[5],XmNvalue,GP.parStrValue[PAR_IPS],NULL);

     }
     else if(value==5) {

       GP.parValue[PAR_IPS]=value;
       strcpy(GP.parStrValue[PAR_IPS],"5");
       XtVaSetValues(GW.parButton[PAR_IPS],XmNvalue,GP.parStrValue[PAR_IPS],NULL);
       XtVaSetValues(GW.runButton[5],XmNvalue,GP.parStrValue[PAR_IPS],NULL);

     }
     else if(value==6) {

       GP.parValue[PAR_IPS]=value;
       strcpy(GP.parStrValue[PAR_IPS],"6");
       XtVaSetValues(GW.parButton[PAR_IPS],XmNvalue,GP.parStrValue[PAR_IPS],NULL);
       XtVaSetValues(GW.runButton[5],XmNvalue,GP.parStrValue[PAR_IPS],NULL);

     }
     else if(value==7) {

       GP.parValue[PAR_IPS]=value;
       strcpy(GP.parStrValue[PAR_IPS],"7");
       XtVaSetValues(GW.parButton[PAR_IPS],XmNvalue,GP.parStrValue[PAR_IPS],NULL);
       XtVaSetValues(GW.runButton[5],XmNvalue,GP.parStrValue[PAR_IPS],NULL);
     }
     else if(value==8) {

       GP.parValue[PAR_IPS]=value;
       strcpy(GP.parStrValue[PAR_IPS],"8");
       XtVaSetValues(GW.parButton[PAR_IPS],XmNvalue,GP.parStrValue[PAR_IPS],NULL);
       XtVaSetValues(GW.runButton[5],XmNvalue,GP.parStrValue[PAR_IPS],NULL);
     }
     else if(value==11) {

       GP.parValue[PAR_IPS]=value;
       strcpy(GP.parStrValue[PAR_IPS],"11");
       XtVaSetValues(GW.parButton[PAR_IPS],XmNvalue,GP.parStrValue[PAR_IPS],NULL);
       XtVaSetValues(GW.runButton[5],XmNvalue,GP.parStrValue[PAR_IPS],NULL);

     }
     else if(value==12) {

       GP.parValue[PAR_IPS]=value;
       strcpy(GP.parStrValue[PAR_IPS],"12");
       XtVaSetValues(GW.parButton[PAR_IPS],XmNvalue,GP.parStrValue[PAR_IPS],NULL);
       XtVaSetValues(GW.runButton[5],XmNvalue,GP.parStrValue[PAR_IPS],NULL);

     }
     else if(value==13) {

       GP.parValue[PAR_IPS]=value;
       strcpy(GP.parStrValue[PAR_IPS],"13");
       XtVaSetValues(GW.parButton[PAR_IPS],XmNvalue,GP.parStrValue[PAR_IPS],NULL);
       XtVaSetValues(GW.runButton[5],XmNvalue,GP.parStrValue[PAR_IPS],NULL);

     }
     else if(value==14) {

       GP.parValue[PAR_IPS]=value;
       strcpy(GP.parStrValue[PAR_IPS],"14");
       XtVaSetValues(GW.parButton[PAR_IPS],XmNvalue,GP.parStrValue[PAR_IPS],NULL);
       XtVaSetValues(GW.runButton[5],XmNvalue,GP.parStrValue[PAR_IPS],NULL);

     }
     else if(value==15) {

       GP.parValue[PAR_IPS]=value;
       strcpy(GP.parStrValue[PAR_IPS],"15");
       XtVaSetValues(GW.parButton[PAR_IPS],XmNvalue,GP.parStrValue[PAR_IPS],NULL);
       XtVaSetValues(GW.runButton[5],XmNvalue,GP.parStrValue[PAR_IPS],NULL);

     }
     else
       printf("SetParCB : failure in PAR_IPS\n");

     break;

   case PAR_IID:

     if(value==0) {

       GP.parValue[PAR_IID]=value;
       strcpy(GP.parStrValue[PAR_IID],"0");
       XtVaSetValues(GW.parButton[PAR_IID],XmNvalue,GP.parStrValue[PAR_IID],NULL);
       XtVaSetValues(GW.outButton[1],XmNvalue,GP.parStrValue[PAR_IID],NULL);

     }
     else if(value==1) {

       GP.parValue[PAR_IID]=value;
       strcpy(GP.parStrValue[PAR_IID],"1");
       XtVaSetValues(GW.parButton[PAR_IID],XmNvalue,GP.parStrValue[PAR_IID],NULL);
       XtVaSetValues(GW.outButton[1],XmNvalue,GP.parStrValue[PAR_IID],NULL);

     }
     else if(value==2) {

       GP.parValue[PAR_IID]=value;
       strcpy(GP.parStrValue[PAR_IID],"2");
       XtVaSetValues(GW.parButton[PAR_IID],XmNvalue,GP.parStrValue[PAR_IID],NULL);
       XtVaSetValues(GW.outButton[1],XmNvalue,GP.parStrValue[PAR_IID],NULL);

     }
     else if(value==3) {

       GP.parValue[PAR_IID]=value;
       strcpy(GP.parStrValue[PAR_IID],"3");
       XtVaSetValues(GW.parButton[PAR_IID],XmNvalue,GP.parStrValue[PAR_IID],NULL);
       XtVaSetValues(GW.outButton[1],XmNvalue,GP.parStrValue[PAR_IID],NULL);

     }
     else if(value==4) {

       GP.parValue[PAR_IID]=value;
       strcpy(GP.parStrValue[PAR_IID],"4");
       XtVaSetValues(GW.parButton[PAR_IID],XmNvalue,GP.parStrValue[PAR_IID],NULL);
       XtVaSetValues(GW.outButton[1],XmNvalue,GP.parStrValue[PAR_IID],NULL);

     }
     else if(value==5) {

       GP.parValue[PAR_IID]=value;
       strcpy(GP.parStrValue[PAR_IID],"5");
       XtVaSetValues(GW.parButton[PAR_IID],XmNvalue,GP.parStrValue[PAR_IID],NULL);
       XtVaSetValues(GW.outButton[1],XmNvalue,GP.parStrValue[PAR_IID],NULL);

     }

     break;

   default:
     printf("SetParCB : failure.\n");
     break;
     
   }
   
}




#ifdef _NO_PROTO
Widget PopupIid(parent)
Widget parent;
#else
Widget PopupIid(Widget parent)
#endif

{
   Widget rtrn,wgt;
   static int p1[2],p2[2],p3[2],p4[2],p5[2],p6[2];

   p1[0] = PAR_IID;
   p2[0] = PAR_IID;
   p3[0] = PAR_IID;
   p4[0] = PAR_IID;
   p5[0] = PAR_IID;
   p6[0] = PAR_IID;


   rtrn = XmCreatePopupMenu(parent,"Popup",NULL,0);
   
   wgt = XmCreateLabelGadget(rtrn,"IID Menu",NULL,0);
   XtManageChild(wgt);
   wgt = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
   XtManageChild(wgt);

   wgt = XmCreatePushButtonGadget(rtrn,"IID = 0",NULL,0);
   XtManageChild(wgt);
   p1[1]=0;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p1);

   wgt = XmCreatePushButtonGadget(rtrn,"IID = 1",NULL,0);
   XtManageChild(wgt);
   p2[1]=1;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p2);


   wgt = XmCreatePushButtonGadget(rtrn,"IID = 2",NULL,0);
   XtManageChild(wgt);
   p3[1]=2;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p3);


   wgt = XmCreatePushButtonGadget(rtrn,"IID = 3",NULL,0);
   XtManageChild(wgt);
   p4[1]=3;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p4);


   wgt = XmCreatePushButtonGadget(rtrn,"IID = 4",NULL,0);
   XtManageChild(wgt);
   p5[1]=4;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p5);


   wgt = XmCreatePushButtonGadget(rtrn,"IID = 5",NULL,0);
   XtManageChild(wgt);
   p6[1]=5;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p6);



   return rtrn;
}



#ifdef _NO_PROTO
Widget PopupIps(parent)
Widget parent;
#else
Widget PopupIps(Widget parent)
#endif

{
   Widget rtrn,wgt;
   static int p1[2],p2[2],p3[2],p4[2],p5[2],p6[2],p7[2],p8[2];
   static int p9[2],p10[2],p11[2],p12[2],p13[2],p14[2],p15[2];

   p1[0] = PAR_IPS;
   p2[0] = PAR_IPS;
   p3[0] = PAR_IPS;
   p4[0] = PAR_IPS;
   p5[0] = PAR_IPS;
   p6[0] = PAR_IPS;
   p7[0] = PAR_IPS;
   p8[0] = PAR_IPS;
   p9[0] = PAR_IPS;
   p10[0] = PAR_IPS;
   p11[0] = PAR_IPS;
   p12[0] = PAR_IPS;
   p13[0] = PAR_IPS;
   p14[0] = PAR_IPS;
   p15[0] = PAR_IPS;

   rtrn = XmCreatePopupMenu(parent,"Popup",NULL,0);
   
   wgt = XmCreateLabelGadget(rtrn,"IPS Menu",NULL,0);
   XtManageChild(wgt);
   wgt = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
   XtManageChild(wgt);

   wgt = XmCreatePushButtonGadget(rtrn,"IPS = -1",NULL,0);
   XtManageChild(wgt);
   p1[1] = -1;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p1);

   wgt = XmCreatePushButtonGadget(rtrn,"IPS =  0",NULL,0);
   XtManageChild(wgt);
   p2[1]=0;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p2);


   wgt = XmCreatePushButtonGadget(rtrn,"IPS =  1",NULL,0);
   XtManageChild(wgt);
   p3[1]=1;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p3);


   wgt = XmCreatePushButtonGadget(rtrn,"IPS =  2",NULL,0);
   XtManageChild(wgt);
   p4[1]=2;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p4);


   wgt = XmCreatePushButtonGadget(rtrn,"IPS =  3",NULL,0);
   XtManageChild(wgt);
   p5[1]=3;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p5);


   wgt = XmCreatePushButtonGadget(rtrn,"IPS =  4",NULL,0);
   XtManageChild(wgt);
   p6[1]=4;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p6);


   wgt = XmCreatePushButtonGadget(rtrn,"IPS =  5",NULL,0);
   XtManageChild(wgt);
   p7[1]=5;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p7);


   wgt = XmCreatePushButtonGadget(rtrn,"IPS =  6",NULL,0);
   XtManageChild(wgt);
   p8[1]=6;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p8);

   wgt = XmCreatePushButtonGadget(rtrn,"IPS =  7",NULL,0);
   XtManageChild(wgt);
   p13[1]=7;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p13);

   wgt = XmCreatePushButtonGadget(rtrn,"IPS =  8",NULL,0);
   XtManageChild(wgt);
   p14[1]=8;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p14);


   wgt = XmCreatePushButtonGadget(rtrn,"IPS = 11",NULL,0);
   XtManageChild(wgt);
   p9[1]=11;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p9);


   wgt = XmCreatePushButtonGadget(rtrn,"IPS = 12",NULL,0);
   XtManageChild(wgt);
   p10[1]=12;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p10);


   wgt = XmCreatePushButtonGadget(rtrn,"IPS = 13",NULL,0);
   XtManageChild(wgt);
   p11[1]=13;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p11);


   wgt = XmCreatePushButtonGadget(rtrn,"IPS = 14",NULL,0);
   XtManageChild(wgt);
   p12[1]=14;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p12);


   wgt = XmCreatePushButtonGadget(rtrn,"IPS = 15",NULL,0);
   XtManageChild(wgt);
   p15[1]=15;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p15);


   return rtrn;
}

#ifdef _NO_PROTO
Widget PopupIsw(parent)
Widget parent;
#else
Widget PopupIsw(Widget parent)
#endif

{
   Widget rtrn,wgt;
   static int p1[2],p2[2],p3[2];

   p1[0] = PAR_ISW;
   p2[0] = PAR_ISW;
   p3[0] = PAR_ISW;

   rtrn = XmCreatePopupMenu(parent,"Popup",NULL,0);
   
   wgt = XmCreateLabelGadget(rtrn,"ISW Menu",NULL,0);
   XtManageChild(wgt);
   wgt = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
   XtManageChild(wgt);

   wgt = XmCreatePushButtonGadget(rtrn,"ISW = -1",NULL,0);
   XtManageChild(wgt);

   p1[1] = -1;

   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p1);

   wgt = XmCreatePushButtonGadget(rtrn,"ISW =  1",NULL,0);
   XtManageChild(wgt);

   p2[1]=1;

   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p2);


   wgt = XmCreatePushButtonGadget(rtrn,"ISW =  2",NULL,0);
   XtManageChild(wgt);

   p3[1]=2;

   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p3);


   return rtrn;
}

#ifdef _NO_PROTO
Widget PopupIsp(parent)
Widget parent;
#else
Widget PopupIsp(Widget parent)
#endif

{
   Widget rtrn,wgt;
   static int p1[2],p2[2],p3[2],p4[2];

   p1[0] = PAR_ISP;
   p2[0] = PAR_ISP;
   p3[0] = PAR_ISP;
   p4[0] = PAR_ISP;

   rtrn = XmCreatePopupMenu(parent,"Popup",NULL,0);
   
   wgt = XmCreateLabelGadget(rtrn,"ISP Menu",NULL,0);
   XtManageChild(wgt);
   wgt = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
   XtManageChild(wgt);

   wgt = XmCreatePushButtonGadget(rtrn,"ISP = 0",NULL,0);
   XtManageChild(wgt);

   p1[1]=0;

   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p1);

   wgt = XmCreatePushButtonGadget(rtrn,"ISP = 1",NULL,0);
   XtManageChild(wgt);

   p2[1]=1;

   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p2);


   wgt = XmCreatePushButtonGadget(rtrn,"ISP = 2",NULL,0);
   XtManageChild(wgt);

   p3[1]=2;

   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p3);


   wgt = XmCreatePushButtonGadget(rtrn,"ISP = 3",NULL,0);
   XtManageChild(wgt);

   p4[1]=3;

   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p4);


   return rtrn;
}


#ifdef _NO_PROTO
Widget PopupIlp(parent)
Widget parent;
#else
Widget PopupIlp(Widget parent)
#endif

{
   Widget rtrn,wgt;
   static int p1[2],p2[2];

   p1[0] = PAR_ILP;
   p2[0] = PAR_ILP;

   rtrn = XmCreatePopupMenu(parent,"Popup",NULL,0);
   
   wgt = XmCreateLabelGadget(rtrn,"ILP Menu",NULL,0);
   XtManageChild(wgt);
   wgt = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
   XtManageChild(wgt);

   wgt = XmCreatePushButtonGadget(rtrn,"ILP = 0",NULL,0);
   XtManageChild(wgt);

   p1[1]=0;

   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p1);

   wgt = XmCreatePushButtonGadget(rtrn,"ILP = 1",NULL,0);
   XtManageChild(wgt);

   p2[1]=1;

   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p2);


   return rtrn;
}



#ifdef _NO_PROTO
Widget PopupJac(parent)
Widget parent;
#else
Widget PopupJac(Widget parent)
#endif

{
   Widget rtrn,wgt;
   static int p1[2],p2[2];

   p1[0] = PAR_JAC;
   p2[0] = PAR_JAC;

   rtrn = XmCreatePopupMenu(parent,"Popup",NULL,0);
   
   wgt = XmCreateLabelGadget(rtrn,"JAC Menu",NULL,0);
   XtManageChild(wgt);
   wgt = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
   XtManageChild(wgt);

   wgt = XmCreatePushButtonGadget(rtrn,"JAC = 0",NULL,0);
   XtManageChild(wgt);

   p1[1]=0;

   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p1);

   wgt = XmCreatePushButtonGadget(rtrn,"JAC = 1",NULL,0);
   XtManageChild(wgt);

   p2[1]=1;

   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p2);


   return rtrn;
}


#ifdef _NO_PROTO
Widget PopupNcol(parent)
Widget parent;
#else
Widget PopupNcol(Widget parent)
#endif

{
   Widget rtrn,wgt;
   static int p1[2],p2[2],p3[2],p4[2],p5[2],p6[2];

   rtrn = XmCreatePopupMenu(parent,"Popup",NULL,0);

   p1[0] = PAR_NCOL;
   p2[0] = PAR_NCOL;
   p3[0] = PAR_NCOL;
   p4[0] = PAR_NCOL;
   p5[0] = PAR_NCOL;
   p6[0] = PAR_NCOL;
   
   wgt = XmCreateLabelGadget(rtrn,"NCOL Menu",NULL,0);
   XtManageChild(wgt);
   wgt = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
   XtManageChild(wgt);

   wgt = XmCreatePushButtonGadget(rtrn,"NCOL = 2",NULL,0);
   XtManageChild(wgt);
   p1[1] = 2;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p1);

   wgt = XmCreatePushButtonGadget(rtrn,"NCOL = 3",NULL,0);
   XtManageChild(wgt);
   p2[1] = 3;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p2);


   wgt = XmCreatePushButtonGadget(rtrn,"NCOL = 4",NULL,0);
   XtManageChild(wgt);
   p3[1] = 4;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p3);


   wgt = XmCreatePushButtonGadget(rtrn,"NCOL = 5",NULL,0);
   XtManageChild(wgt);
   p4[1] = 5;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p4);


   wgt = XmCreatePushButtonGadget(rtrn,"NCOL = 6",NULL,0);
   XtManageChild(wgt);
   p5[1] = 6;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p5);


   wgt = XmCreatePushButtonGadget(rtrn,"NCOL = 7",NULL,0);
   XtManageChild(wgt);
   p6[1] = 7;
   XtAddCallback(wgt,XmNactivateCallback,SetParCB,p6);


   return rtrn;
}



#ifdef _NO_PROTO
Widget PopupThl(parent)
Widget parent;
#else
Widget PopupThl(Widget parent)
#endif

{
   int i,k1,k2;
   Widget rtrn,wgt,frame,label,rwcl,window,form,ok,apply,cancel,help;
   Widget rc1,rc2,rc11,rc22,t;
   static ClientData userData1,userData2;


   rtrn = XmCreateFormDialog(parent,"Prompt",NULL,0);


   PopupTemplate1(rtrn,&rwcl,&ok,&apply,&cancel,&help);


   XtVaSetValues(rwcl,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,1,NULL);
  


   userData1.widget = rtrn;
   userData1.data = 3;
   userData2.widget = rtrn;
   userData2.data = 7;

   XtAddCallback(ok,XmNactivateCallback,TmpCB,&userData1);
   XtAddCallback(apply,XmNactivateCallback,TmpCB,&userData2);
   XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);
   XtAddCallback(help,XmNactivateCallback,PopupCB,GW.helpList);

   frame = XmCreateFrame(rwcl,"Frame",NULL,0);
   XtManageChild(frame);
   rc1=XmCreateRowColumn(frame,"rc",NULL,0);
   XtManageChild(rc1);

   label = XmCreateLabelGadget(rc1,"  < i,       THL(i) >",NULL,0);
   XtManageChild(label);


   for(i = 0; i < MAX_NTHL; i++)	{
     
     
     k1 = 2*i;
     k2 = k1+1;

     wgt = XmCreateRowColumn(rc1,"RowColumn",NULL,0);
     XtManageChild(wgt);
     XtVaSetValues(wgt,
/*                 XmNpacking,XmPACK_COLUMN, */
		   XmNorientation,XmHORIZONTAL,
                   XmNnumColumns,1,NULL);

     GW.thlButton[k1]=XmCreateTextField(wgt,"Button",NULL,0);
     GW.thlButton[k2]=XmCreateTextField(wgt,"Button",NULL,0);
     GW.thlTg[i]=XmCreateToggleButton(wgt,"",NULL,0);
     XtManageChild(GW.thlTg[i]);
     XtManageChild(GW.thlButton[k1]);
     XtManageChild(GW.thlButton[k2]);
     XtVaSetValues(GW.thlButton[k1],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.thlStrValue[k1],
		   NULL);
     XtVaSetValues(GW.thlButton[k2],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.thlStrValue[k2],
		   NULL);
     
   }




#if 0

   PopupTemplate(rtrn,&rwcl,&ok,&apply,&cancel,&help,27);

   XtVaSetValues(rwcl,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,1,NULL);

   userData1.widget = rtrn;
   userData1.data = 3;
   userData2.widget = rtrn;
   userData2.data = 7;

   XtAddCallback(ok,XmNactivateCallback,TmpCB,&userData1);
   XtAddCallback(apply,XmNactivateCallback,TmpCB,&userData2);
   XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);
   XtAddCallback(help,XmNactivateCallback,PopupCB,GW.helpList);


#if 0

   rtrn = XmCreateFormDialog(parent,"Prompt",NULL,0);

   rwcl = XmCreateRowColumn(rtrn,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,1,NULL);


   wgt = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
   XtManageChild(wgt);

   form = XmCreateForm(rtrn,"Form",NULL,0);
   XtManageChild(form);
   ok = XmCreatePushButtonGadget(form,"  Ok  ",NULL,0);
   XtManageChild(ok);

   cancel = XmCreatePushButtonGadget(form," Cancel ",NULL,0);
   XtManageChild(cancel);
   XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);

   XtVaSetValues(rwcl,
		 XmNtopAttachment,XmATTACH_FORM,
		 XmNbottomAttachment,XmATTACH_POSITION,
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNrightAttachment,XmATTACH_FORM,
		 XmNbottomPosition,90,
		 XmNtopOffset,10,
		 XmNbottomOffset,10,
		 XmNleftOffset,10,
		 XmNrightOffset,10,
		 NULL);
   
   XtVaSetValues(wgt,
		 XmNtopAttachment,XmATTACH_WIDGET,
		 XmNbottomAttachment,XmATTACH_NONE,
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNrightAttachment,XmATTACH_FORM,
		 XmNtopWidget,rwcl,
		 XmNtopOffset,10,
		 XmNbottomOffset,10,
		 XmNleftOffset,10,
		 XmNrightOffset,10,
		 NULL);
   
   
   XtVaSetValues(form,
		 XmNtopAttachment,XmATTACH_WIDGET,
		 XmNbottomAttachment,XmATTACH_FORM,
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNrightAttachment,XmATTACH_FORM,
		 XmNtopWidget,wgt,
		 XmNtopOffset,10,
		 XmNbottomOffset,10,
		 XmNleftOffset,10,
		 XmNrightOffset,10,
		 NULL);

   XtVaSetValues(ok,
/*
		 XmNtopAttachment,XmATTACH_FORM,
		 XmNbottomAttachment,XmATTACH_FORM,
*/
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNrightAttachment,XmATTACH_NONE,
		 XmNtopOffset,10,
		 XmNbottomOffset,10,
		 XmNleftOffset,10,
		 XmNrightOffset,10,
		 NULL);

   XtVaSetValues(cancel,
/*
		 XmNtopAttachment,XmATTACH_FORM,
		 XmNbottomAttachment,XmATTACH_FORM,
*/
		 XmNleftAttachment,XmATTACH_NONE,
		 XmNrightAttachment,XmATTACH_FORM,
		 XmNtopOffset,10,
		 XmNbottomOffset,10,
		 XmNleftOffset,10,
		 XmNrightOffset,10,
		 NULL);

#endif


   rc11=XmCreateRowColumn(rwcl,"rc",NULL,0);
   rc22=XmCreateRowColumn(rwcl,"rc",NULL,0);
   XtManageChild(rc11);
   XtManageChild(rc22);

   frame = XmCreateFrame(rc11,"Frame",NULL,0);
   XtManageChild(frame);
   rc1=XmCreateRowColumn(frame,"rc",NULL,0);
   XtManageChild(rc1);

   frame = XmCreateFrame(rc22,"Frame",NULL,0);
   XtManageChild(frame);
   rc2=XmCreateRowColumn(frame,"rc",NULL,0);
   XtManageChild(rc2);


   XtVaSetValues(rc1,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,(MAX_NTHL/2)+1,NULL);

   wgt = XmCreateFrame(rc1,"fm",NULL,0);
   XtManageChild(wgt);

   t = XmCreateRowColumn(wgt,"rc",NULL,0);
   XtManageChild(t);

   label = XmCreateLabelGadget(t,"Group 1 : <i,THL(i)>",NULL,0);
   XtManageChild(label);
/*
   label = XmCreateSeparatorGadget(rc1,"st",NULL,0);
   XtManageChild(label);
*/

   XtVaSetValues(rc2,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,(MAX_NTHL/2)+1,NULL);

   wgt = XmCreateFrame(rc2,"fm",NULL,0);
   XtManageChild(wgt);


   t = XmCreateRowColumn(wgt,"rc",NULL,0);
   XtManageChild(t);

   label = XmCreateLabelGadget(t,"Group 2 : <i,THL(i)>",NULL,0);
   XtManageChild(label);
/*
   label = XmCreateSeparatorGadget(rc2,"st",NULL,0);
   XtManageChild(label);
*/
      
   for(i = 0; i < MAX_NTHL/2; i++)	{
     
     
     k1 = 2*i;
     k2 = k1+1;
/*     
     frame = XmCreateFrame(rc1,"Frame",NULL,0);
     XtManageChild(frame);
*/
     wgt = XmCreateRowColumn(rc1,"RowColumn",NULL,0);
     XtManageChild(wgt);
     XtVaSetValues(wgt,XmNpacking,XmPACK_COLUMN,
		   XmNorientation,XmHORIZONTAL,
                   XmNnumColumns,1,NULL);

     GW.thlButton[k1]=XmCreateTextField(wgt,"Button",NULL,0);
     GW.thlButton[k2]=XmCreateTextField(wgt,"Button",NULL,0);
     XtManageChild(GW.thlButton[k1]);
     XtManageChild(GW.thlButton[k2]);
     XtVaSetValues(GW.thlButton[k1],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.thlStrValue[k1],
		   NULL);
     XtVaSetValues(GW.thlButton[k2],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.thlStrValue[k2],
		   NULL);
     
   }

   for(i = MAX_NTHL/2; i < MAX_NTHL; i++)	{
     
     
     k1 = 2*i;
     k2 = k1+1;
/*     
     frame = XmCreateFrame(rc2,"Frame",NULL,0);
     XtManageChild(frame);
*/
     wgt = XmCreateRowColumn(rc2,"RowColumn",NULL,0);
     XtManageChild(wgt);
     XtVaSetValues(wgt,XmNpacking,XmPACK_COLUMN,
		   XmNorientation,XmHORIZONTAL,
                   XmNnumColumns,1,NULL);

     GW.thlButton[k1]=XmCreateTextField(wgt,"Button",NULL,0);
     GW.thlButton[k2]=XmCreateTextField(wgt,"Button",NULL,0);
     XtManageChild(GW.thlButton[k1]);
     XtManageChild(GW.thlButton[k2]);
     XtVaSetValues(GW.thlButton[k1],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.thlStrValue[k1],
		   NULL);
     XtVaSetValues(GW.thlButton[k2],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.thlStrValue[k2],
		   NULL);
     
   }

#endif

   return rtrn;

}


#ifdef _NO_PROTO
void   DeleteCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   DeleteCB(Widget w,XtPointer userData, XtPointer callbackArg)
#endif

{

  XmSelectionBoxCallbackStruct *cbs;
  char *name,command[100];

  cbs=(XmSelectionBoxCallbackStruct *) callbackArg;
      
  XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET, &name);

  	
  sprintf(command,"make deletedata DELETEDATA=%s &",name);
  system(command);


#if 0

  XmSelectionBoxCallbackStruct *cbs;
  char *name;

  cbs=(XmSelectionBoxCallbackStruct *) callbackArg;
      
  XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET, &name);

  	
  sprintf(GP.command,"rm -f b\.%s &\0",name);
  system(GP.command);
  printf("Deleting b.%s ... done\n",name);

  	
  sprintf(GP.command,"rm -f s\.%s &\0",name);
  system(GP.command);
  printf("Deleting s.%s ... done\n",name);

  	
  sprintf(GP.command,"rm -f d\.%s &\0",name);
  system(GP.command);
  printf("Deleting d.%s ... done\n",name); 

#endif

}


#ifdef _NO_PROTO
void   RestartCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   RestartCB(Widget w,XtPointer userData, XtPointer callbackArg)
#endif

{

  XmSelectionBoxCallbackStruct *cbs;
  char *name,command[100];

  cbs=(XmSelectionBoxCallbackStruct *) callbackArg;
      
  XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET, &name);


  strcpy(GP.q,name);
  GP.restart = 1;

  	
  sprintf(command,"make restartdata RESTARTDATA=%s &",name);
  system(command);


#if 0

  XmSelectionBoxCallbackStruct *cbs;
  char *name,nm[10];
  int i=0;

  if(strcmp(GP.fileName,EMPTY) != 0) {

    cbs=(XmSelectionBoxCallbackStruct *) callbackArg;
      
    XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET, &name);

    strcpy(nm,ProgramName(GP.fileName));
    while(nm[i] != '.')
      ++i;
    nm[i] = '\0';

/*
    sprintf(GP.command,"cp c\.%s c\.%s &",name,nm);
    system(GP.command);
    printf("Copying c.%s to c.%s ... done\n",name,nm);

    sprintf(GP.command,"cp b\.%s b\.%s &",name,nm);
    system(GP.command);
    printf("Copying b.%s to b.%s ... done\n",name,nm);
*/

    	
    sprintf(GP.command,"cp s\.%s s\.%s &\0",name,nm);
    system(GP.command);
    printf("Copying s.%s to s.%s ... done\n",name,nm);
/*
    sprintf(GP.command,"cp d\.%s d\.%s &",name,nm);
    system(GP.command);
    printf("Copying d.%s to d.%s ... done\n",name,nm);

    printf("Setting restart files as c.%s p.%s s.%s d.%s ... done\n",name,name,name,name);
*/
    printf("Setting restart file as s.%s ... done\n",name);
  }
  else
    printf("No equation was loaded\n");

#endif

}


#ifdef _NO_PROTO
void   CopyDataCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   CopyDataCB(Widget w,XtPointer userData, XtPointer callbackArg)
#endif

{

  String value;
  char from[10],to[10],command[120];
  Widget tmp;

  tmp = (Widget) userData;

  if(strcmp(GP.fileName,EMPTY) != 0) {

    XtVaGetValues(GW.copyFrom,XmNvalue,&value,NULL);
    strcpy(from,value);
    XtVaGetValues(GW.copyTo,XmNvalue,&value,NULL);
    strcpy(to,value);

    	
    sprintf(command,"make copydata COPYFROM=%s COPYTO=%s &",from,to);
    system(command);


  }
  else
    printf("No equation was loaded\n");

  XtUnmanageChild(tmp);

#if 0

  int i=0;
  String value;
  char from[10],to[10];
  Widget tmp;

  tmp = (Widget) userData;

  if(strcmp(GP.fileName,EMPTY) != 0) {

    XtVaGetValues(GW.copyFrom,XmNvalue,&value,NULL);
/*
    if(strcmp(value,"") == 0 )  {
      strcpy(from,ProgramName(GP.fileName));
      while(from[i] != '.')
	++i;
      from[i] = '\0';
    }
    else
*/
    strcpy(from,value);


    XtVaGetValues(GW.copyTo,XmNvalue,&value,NULL);
    strcpy(to,value);

/*
    strcpy(GP.command,"cp c\.");
    strcat(GP.command,from);
    strcat(GP.command," c\.");
    strcat(GP.command,to);
    strcat(GP.command,"\0");
*/

    sprintf(GP.command,"cp c\.%s c\.%s &\0",from,to);
    system(GP.command);
    printf("Copying c.%s to c.%s ... done\n",from,to);

/*
    strcpy(GP.command,"cp b\.");
    strcat(GP.command,from);
    strcat(GP.command," b\.");
    strcat(GP.command,to);
    strcat(GP.command,"\0");
*/
    sprintf(GP.command,"cp b\.%s b\.%s &\0",from,to);
    system(GP.command);
    printf("Copying b.%s to b.%s ... done\n",from,to);

/*
    strcpy(GP.command,"cp s\.");
    strcat(GP.command,from);
    strcat(GP.command," s\.");
    strcat(GP.command,to);
    strcat(GP.command,"\0");
*/
    sprintf(GP.command,"cp s\.%s s\.%s &\0",from,to);
    system(GP.command);
    printf("Copying s.%s to s.%s ... done\n",from,to);

/*
    strcpy(GP.command,"cp d\.");
    strcat(GP.command,from);
    strcat(GP.command," d\.");
    strcat(GP.command,to);
    strcat(GP.command,"\0");
*/
    sprintf(GP.command,"cp d\.%s d\.%s &\0",from,to);
    system(GP.command);
    printf("Copying d.%s to d.%s ... done\n",from,to);

  }
  else
    printf("No equation was loaded\n");


  XtUnmanageChild(tmp);

#endif

}



#ifdef _NO_PROTO
Widget CopyData(parent)
Widget parent;
#else
Widget CopyData(Widget parent)
#endif

{

   Widget frame,label1,label2,form,ok,cancel,rwcl;
   Widget rc,rc2,rc11,rc22,t;
   static Widget rtrn;

   rtrn = XmCreateFormDialog(parent,"CPrompt",NULL,0);
   PopupTemplate4(rtrn,&rwcl,&ok,&cancel);


   XtVaSetValues(rwcl,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,1,NULL);


   XtAddCallback(ok,XmNactivateCallback,CopyDataCB,rtrn);
   XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);

   frame = XmCreateFrame(rwcl,"Frame",NULL,0);
   XtManageChild(frame);
   rc=XmCreateRowColumn(frame,"rc",NULL,0);
   XtManageChild(rc);

   XtVaSetValues(rc,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,1,NULL);
  
   form = XmCreateForm(rc,"fm",NULL,0);
   XtManageChild(form);
   label1 = XmCreateLabelGadget(form,"  Copy",NULL,0);
   XtManageChild(label1);
   GW.copyFrom = XmCreateTextField(form,"label",NULL,0);
   XtManageChild(GW.copyFrom);
   XtVaSetValues(GW.copyFrom,
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH/2,
		 XmNmaxLength,WX_TEXTLENGTH,
		 NULL);


   XtVaSetValues(label1,
		 XmNtopAttachment,XmATTACH_FORM,
		 XmNbottomAttachment,XmATTACH_FORM,
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNrightAttachment,XmATTACH_NONE,
		 XmNtopOffset,10,
		 XmNbottomOffset,10,
		 XmNleftOffset,10,
		 XmNrightOffset,10,
		 NULL);


  XtVaSetValues(GW.copyFrom,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_WIDGET,
		XmNrightAttachment,XmATTACH_FORM,
		XmNleftWidget,label1,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);
  
  


   form = XmCreateForm(rc,"fm",NULL,0);
   XtManageChild(form);
   label2 = XmCreateLabelGadget(form,"    To",NULL,0);
   XtManageChild(label2);
   GW.copyTo = XmCreateTextField(form,"label",NULL,0);
   XtManageChild(GW.copyTo);
   XtVaSetValues(GW.copyTo,
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH/2,
		 XmNmaxLength,WX_TEXTLENGTH,
		 NULL);


  XtVaSetValues(label2,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_NONE,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);
  

  XtVaSetValues(GW.copyTo,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_WIDGET,
		XmNrightAttachment,XmATTACH_FORM,
		XmNleftWidget,label2,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);
  
   return rtrn;

}



#ifdef _NO_PROTO
void   MoveDataCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   MoveDataCB(Widget w,XtPointer userData, XtPointer callbackArg)
#endif

{
  String value;
  char from[10],to[10],command[120];
  Widget tmp;

  tmp = (Widget) userData;

  if(strcmp(GP.fileName,EMPTY) != 0) {

    XtVaGetValues(GW.moveFrom,XmNvalue,&value,NULL);
    strcpy(from,value);
    XtVaGetValues(GW.moveTo,XmNvalue,&value,NULL);
    strcpy(to,value);

    
    sprintf(command,"make movedata MOVEFROM=%s MOVETO=%s &",from,to);
    system(command);
  }
  else
    printf("No equation was loaded\n");

  XtUnmanageChild(tmp);


#if 0

  int i=0;
  String value;
  char from[10],to[10];
  Widget tmp;

  tmp = (Widget) userData;

  if(strcmp(GP.fileName,EMPTY) != 0) {

    XtVaGetValues(GW.moveFrom,XmNvalue,&value,NULL);
/*
    if(strcmp(value,"") == 0 )  {
      strcpy(from,ProgramName(GP.fileName));
      while(from[i] != '.')
	++i;
      from[i] = '\0';
    }
    else
*/
    strcpy(from,value);


    XtVaGetValues(GW.moveTo,XmNvalue,&value,NULL);
    strcpy(to,value);

/*
    strcpy(GP.command,"mv c.");
    strcat(GP.command,from);
    strcat(GP.command," c.");
    strcat(GP.command,to);
    strcat(GP.command,"\0");
*/
    sprintf(GP.command,"mv c\.%s c\.%s &\0",from,to);
    system(GP.command);
    printf("Moving c.%s to c.%s ... done\n",from,to);

/*
    strcpy(GP.command,"mv b.");
    strcat(GP.command,from);
    strcat(GP.command," b.");
    strcat(GP.command,to);
    strcat(GP.command,"\0");
*/
    sprintf(GP.command,"mv b\.%s b\.%s &\0",from,to);
    system(GP.command);
    printf("Moving b.%s to b.%s ... done\n",from,to);

/*
    strcpy(GP.command,"mv s.");
    strcat(GP.command,from);
    strcat(GP.command," s.");
    strcat(GP.command,to);
    strcat(GP.command,"\0");
*/
    sprintf(GP.command,"mv s\.%s s\.%s &\0",from,to);
    system(GP.command);
    printf("Moving s.%s to s.%s ... done\n",from,to);

/*
    strcpy(GP.command,"mv d.");
    strcat(GP.command,from);
    strcat(GP.command," d.");
    strcat(GP.command,to);
    strcat(GP.command,"\0");
*/
    sprintf(GP.command,"mv d\.%s d\.%s &\0",from,to);
    system(GP.command);
    printf("Moving d.%s to d.%s ... done\n",from,to);

  }
  else
    printf("No equation was loaded\n");


  XtUnmanageChild(tmp);

#endif

}




#ifdef _NO_PROTO
Widget MoveData(parent)
Widget parent;
#else
Widget MoveData(Widget parent)
#endif

{

   Widget frame,label1,label2,form,ok,cancel,rwcl;
   Widget rc,rc2,rc11,rc22,t;
   static Widget rtrn;

   rtrn = XmCreateFormDialog(parent,"MPrompt",NULL,0);
   PopupTemplate4(rtrn,&rwcl,&ok,&cancel);


   XtVaSetValues(rwcl,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,1,NULL);


   XtAddCallback(ok,XmNactivateCallback,MoveDataCB,rtrn);
   XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);

   frame = XmCreateFrame(rwcl,"Frame",NULL,0);
   XtManageChild(frame);
   rc=XmCreateRowColumn(frame,"rc",NULL,0);
   XtManageChild(rc);

   XtVaSetValues(rc,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,1,NULL);
  
   form = XmCreateForm(rc,"fm",NULL,0);
   XtManageChild(form);
   label1 = XmCreateLabelGadget(form,"  Move",NULL,0);
   XtManageChild(label1);
   GW.moveFrom = XmCreateTextField(form,"label",NULL,0);
   XtManageChild(GW.moveFrom);
   XtVaSetValues(GW.moveFrom,
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH/2,
		 XmNmaxLength,WX_TEXTLENGTH,
		 NULL);


   XtVaSetValues(label1,
		 XmNtopAttachment,XmATTACH_FORM,
		 XmNbottomAttachment,XmATTACH_FORM,
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNrightAttachment,XmATTACH_NONE,
		 XmNtopOffset,10,
		 XmNbottomOffset,10,
		 XmNleftOffset,10,
		 XmNrightOffset,10,
		 NULL);


  XtVaSetValues(GW.moveFrom,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_WIDGET,
		XmNrightAttachment,XmATTACH_FORM,
		XmNleftWidget,label1,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);
  
  


   form = XmCreateForm(rc,"fm",NULL,0);
   XtManageChild(form);
   label2 = XmCreateLabelGadget(form,"    To",NULL,0);
   XtManageChild(label2);
   GW.moveTo = XmCreateTextField(form,"label",NULL,0);
   XtManageChild(GW.moveTo);
   XtVaSetValues(GW.moveTo,
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH/2,
		 XmNmaxLength,WX_TEXTLENGTH,
		 NULL);


  XtVaSetValues(label2,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_NONE,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);
  

  XtVaSetValues(GW.moveTo,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_WIDGET,
		XmNrightAttachment,XmATTACH_FORM,
		XmNleftWidget,label2,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);
  
   return rtrn;




}





#ifdef _NO_PROTO
void   AppendDataCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   AppendDataCB(Widget w,XtPointer userData, XtPointer callbackArg)
#endif
{

  String value;
  char from[10],to[10],command[120];
  Widget tmp;

  tmp = (Widget) userData;

  if(strcmp(GP.fileName,EMPTY) != 0) {

    XtVaGetValues(GW.appendFrom,XmNvalue,&value,NULL);
    strcpy(from,value);
    XtVaGetValues(GW.appendTo,XmNvalue,&value,NULL);
    strcpy(to,value);

    sprintf(command,"make appenddata APPENDFROM=%s APPENDTO=%s &",from,to);
    system(command);

  }
  else
    printf("No equation was loaded\n");

  XtUnmanageChild(tmp);


#if 0

  int i=0;
  String value;
  char from[10],to[10];
  Widget tmp;

  tmp = (Widget) userData;

  if(strcmp(GP.fileName,EMPTY) != 0) {

    XtVaGetValues(GW.appendFrom,XmNvalue,&value,NULL);
/*
    if(strcmp(value,"") == 0 )  {
      strcpy(from,ProgramName(GP.fileName));
      while(from[i] != '.')
	++i;
      from[i] = '\0';
    }
    else
*/
    strcpy(from,value);


    XtVaGetValues(GW.appendTo,XmNvalue,&value,NULL);
    strcpy(to,value);

/*
    strcpy(GP.command,"cat c.");
    strcat(GP.command,from);
    strcat(GP.command," >> c.");
    strcat(GP.command,to);
    strcat(GP.command,"\0");
*/
    sprintf(GP.command,"cat c\.%s >> c\.%s &\0",from,to);
    system(GP.command);
    printf("Appending c.%s to c.%s ... done\n",from,to);

/*
    strcpy(GP.command,"cat b.");
    strcat(GP.command,from);
    strcat(GP.command," >> b.");
    strcat(GP.command,to);
    strcat(GP.command,"\0");
*/
    sprintf(GP.command,"cat b\.%s >> b\.%s &\0",from,to);
    system(GP.command);
    printf("Appending b.%s to b.%s ... done\n",from,to);

/*
    strcpy(GP.command,"cat s.");
    strcat(GP.command,from);
    strcat(GP.command," >> s.");
    strcat(GP.command,to);
    strcat(GP.command,"\0");
*/
    sprintf(GP.command,"cat s\.%s >> s\.%s &\0",from,to);
    system(GP.command);
    printf("Appending s.%s to s.%s ... done\n",from,to);

/*
    strcpy(GP.command,"cat d.");
    strcat(GP.command,from);
    strcat(GP.command," >> d.");
    strcat(GP.command,to);
    strcat(GP.command,"\0");
*/
    sprintf(GP.command,"cat d\.%s >> d\.%s &\0",from,to);
    system(GP.command);
    printf("Appending d.%s to d.%s ... done\n",from,to);

  }
  else
    printf("No equation was loaded\n");


  XtUnmanageChild(tmp);

#endif

}




#ifdef _NO_PROTO
Widget AppendData(parent)
Widget parent;
#else
Widget AppendData(Widget parent)
#endif

{


   Widget frame,label1,label2,form,ok,cancel,rwcl;
   Widget rc,rc2,rc11,rc22,t;
   static Widget rtrn;

   rtrn = XmCreateFormDialog(parent,"APrompt",NULL,0);
   PopupTemplate4(rtrn,&rwcl,&ok,&cancel);


   XtVaSetValues(rwcl,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,1,NULL);


   XtAddCallback(ok,XmNactivateCallback,AppendDataCB,rtrn);
   XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);

   frame = XmCreateFrame(rwcl,"Frame",NULL,0);
   XtManageChild(frame);
   rc=XmCreateRowColumn(frame,"rc",NULL,0);
   XtManageChild(rc);

   XtVaSetValues(rc,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,1,NULL);
  
   form = XmCreateForm(rc,"fm",NULL,0);
   XtManageChild(form);
   label1 = XmCreateLabelGadget(form,"Append",NULL,0);
   XtManageChild(label1);
   GW.appendFrom = XmCreateTextField(form,"label",NULL,0);
   XtManageChild(GW.appendFrom);
   XtVaSetValues(GW.appendFrom,
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH/2,
		 XmNmaxLength,WX_TEXTLENGTH,
		 NULL);


   XtVaSetValues(label1,
		 XmNtopAttachment,XmATTACH_FORM,
		 XmNbottomAttachment,XmATTACH_FORM,
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNrightAttachment,XmATTACH_NONE,
		 XmNtopOffset,10,
		 XmNbottomOffset,10,
		 XmNleftOffset,10,
		 XmNrightOffset,10,
		 NULL);


  XtVaSetValues(GW.appendFrom,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_WIDGET,
		XmNrightAttachment,XmATTACH_FORM,
		XmNleftWidget,label1,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);
  
  


   form = XmCreateForm(rc,"fm",NULL,0);
   XtManageChild(form);
   label2 = XmCreateLabelGadget(form,"    To",NULL,0);
   XtManageChild(label2);
   GW.appendTo = XmCreateTextField(form,"label",NULL,0);
   XtManageChild(GW.appendTo);
   XtVaSetValues(GW.appendTo,
		 XmNblinkRate,0,
		 XmNcolumns,DE_TEXTLENGTH/2,
		 XmNmaxLength,WX_TEXTLENGTH,
		 NULL);


  XtVaSetValues(label2,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_NONE,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);
  

  XtVaSetValues(GW.appendTo,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_WIDGET,
		XmNrightAttachment,XmATTACH_FORM,
		XmNleftWidget,label2,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);
  
   return rtrn;

}




#ifdef _NO_PROTO
Widget PopupIcp(parent)
Widget parent;
#else
Widget PopupIcp(Widget parent)
#endif

{
   int i,k1,k2;
   Widget rtrn,wgt,frame,label,rwcl,window,form,ok,apply,cancel,help;
   Widget rc1,rc2,rc11,rc22,t;
   static ClientData userData1,userData2;

   rtrn = XmCreateFormDialog(parent,"Prompt",NULL,0);

/*
   PopupTemplate1(rtrn,&rwcl,&ok,&apply,&cancel,&help);
*/
   PopupTemplate3(rtrn,&rwcl,&ok,&apply,&cancel,&help);


   XtVaSetValues(rwcl,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,1,NULL);
  
   userData1.widget = rtrn;
   userData1.data = 1;
   userData2.widget = rtrn;
   userData2.data = 5;

   XtAddCallback(ok,XmNactivateCallback,TmpCB,&userData1);
   XtAddCallback(apply,XmNactivateCallback,TmpCB,&userData2);
   XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);
   XtAddCallback(help,XmNactivateCallback,PopupCB,GW.helpList);

   frame = XmCreateFrame(rwcl,"Frame",NULL,0);
   XtManageChild(frame);
   rc1=XmCreateRowColumn(frame,"rc",NULL,0);
   XtManageChild(rc1);

   label = XmCreateLabelGadget(rc1,"   ICP(i)   ",NULL,0);
   XtManageChild(label);


   for(i = 0; i < MAX_NICP; i++)	{


     wgt = XmCreateRowColumn(rc1,"RowColumn",NULL,0);
     XtManageChild(wgt);
     XtVaSetValues(wgt,
/*                 XmNpacking,XmPACK_COLUMN,*/
		   XmNorientation,XmHORIZONTAL,
                   XmNnumColumns,1,NULL);

     GW.icpButton[i]=XmCreateTextField(wgt,"Button",NULL,0);
     GW.icpTg[i]=XmCreateToggleButton(wgt,"",NULL,0);
     XtManageChild(GW.icpTg[i]);
     XtManageChild(GW.icpButton[i]);
     XtVaSetValues(GW.icpButton[i],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.icpStrValue[i],
		   NULL);
     
/*     
     k1 = 2*i;
     k2 = k1+1;

     wgt = XmCreateRowColumn(rc1,"RowColumn",NULL,0);
     XtManageChild(wgt);
     XtVaSetValues(wgt,XmNpacking,XmPACK_COLUMN,
		   XmNorientation,XmHORIZONTAL,
                   XmNnumColumns,1,NULL);

     GW.icpButton[k1]=XmCreateTextField(wgt,"Button",NULL,0);
     GW.icpButton[k2]=XmCreateTextField(wgt,"Button",NULL,0);
     XtManageChild(GW.icpButton[k1]);
     XtManageChild(GW.icpButton[k2]);
     XtVaSetValues(GW.icpButton[k1],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.icpStrValue[k1],
		   NULL);
     XtVaSetValues(GW.icpButton[k2],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.icpStrValue[k2],
		   NULL);
*/
        
   }


   return rtrn;

}



#ifdef _NO_PROTO
void   TmpCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void   TmpCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  ClientData *tmp;

   
   tmp = (ClientData *) userData;

   SaveParScreen(tmp->data);
   SetParScreen();

   if(tmp->data < 5)
     XtUnmanageChild(tmp->widget);
}

#ifdef _NO_PROTO
void UzrToggleCB(w,userData,callbackArg)
Widget w;
XtPointer userData,callbackArg;
#else
void UzrToggleCB(Widget w,XtPointer userData,XtPointer callbackArg)
#endif

{
  int i;
  XmToggleButtonCallbackStruct *cbs;

  cbs = (XmToggleButtonCallbackStruct *) callbackArg;
  i = (size_t) userData;
  if(cbs->set)
    GP.uzrToggle[i] = True;

}

#ifdef _NO_PROTO
Widget PopupUzr(parent)
Widget parent;
#else
Widget PopupUzr(Widget parent)
#endif

{
   int i,k1,k2;
   Widget rtrn,wgt,frame,label,rwcl,window,form,ok,apply,cancel,help;
   Widget rc1,rc2,rc11,rc22,t;
   static ClientData userData1,userData2;

   rtrn = XmCreateFormDialog(parent,"Prompt",NULL,0);


   PopupTemplate1(rtrn,&rwcl,&ok,&apply,&cancel,&help);


   XtVaSetValues(rwcl,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,1,NULL);
  

   userData1.widget =rtrn;
   userData1.data =2;
   userData2.widget =rtrn;
   userData2.data =6;
   
   XtAddCallback(ok,XmNactivateCallback,TmpCB,&userData1);
   XtAddCallback(apply,XmNactivateCallback,TmpCB,&userData2);
   XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);
   XtAddCallback(help,XmNactivateCallback,PopupCB,GW.helpList);


   frame = XmCreateFrame(rwcl,"Frame",NULL,0);
   XtManageChild(frame);
 
#if 0  

   t = XmCreateRowColumn(frame,"rc",NULL,0);
   XtManageChild(t);

   XtVaSetValues(t,
/*		 XmNpacking,XmPACK_COLUMN,*/
		 XmNorientation,XmHORIZONTAL,
/*		 XmNnumColumns,1,*/
		 NULL);

   rc2=XmCreateRowColumn(t,"rc1",NULL,0);
   XtManageChild(rc2);

   for(i = 0; i < MAX_NUZR; i++) {
     tg = XmCreateToggleButton(rc2,"",NULL,0);
     XtManageChild(tg);
   }

#endif

   rc1=XmCreateRowColumn(frame,"rc",NULL,0);
   XtManageChild(rc1);

   label = XmCreateLabelGadget(rc1,"  < i,       UZR(i) >",NULL,0);
   XtManageChild(label);


   for(i = 0; i < MAX_NUZR; i++)	{
     
     
     k1 = 2*i;
     k2 = k1+1;

     wgt = XmCreateRowColumn(rc1,"RowColumn",NULL,0);
     XtManageChild(wgt);
     XtVaSetValues(wgt,
/*		   XmNpacking,XmPACK_COLUMN, */
		   XmNorientation,XmHORIZONTAL,
                   XmNnumColumns,1,NULL);
     GW.uzrButton[k1]=XmCreateTextField(wgt,"Button",NULL,0);
     GW.uzrButton[k2]=XmCreateTextField(wgt,"Button",NULL,0);

     GW.uzrTg[i]=XmCreateToggleButton(wgt,"",NULL,0);
     XtManageChild(GW.uzrTg[i]);
/*

     XtAddCallback(GW.uzrTg[i],XmNvalueChangedCallback,UzrToggleCB,i);
*/

     XtManageChild(GW.uzrButton[k1]);
     XtManageChild(GW.uzrButton[k2]);
     XtVaSetValues(GW.uzrButton[k1],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.uzrStrValue[k1],
		   NULL);
     XtVaSetValues(GW.uzrButton[k2],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.uzrStrValue[k2],
		   NULL);
     
   }



#if 0

   PopupTemplate(rtrn,&rwcl,&ok,&apply,&cancel,&help,27);

   XtVaSetValues(rwcl,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,1,NULL);

   userData1.widget =rtrn;
   userData1.data =2;
   userData2.widget =rtrn;
   userData2.data =6;
   
   XtAddCallback(ok,XmNactivateCallback,TmpCB,&userData1);
   XtAddCallback(apply,XmNactivateCallback,TmpCB,&userData2);
   XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);
   XtAddCallback(help,XmNactivateCallback,PopupCB,GW.helpList);


#if 0

   rtrn = XmCreateFormDialog(parent,"Prompt",NULL,0);

   rwcl = XmCreateRowColumn(rtrn,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,1,NULL);


   wgt = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
   XtManageChild(wgt);

   form = XmCreateForm(rtrn,"Form",NULL,0);
   XtManageChild(form);
   ok = XmCreatePushButtonGadget(form,"  Ok  ",NULL,0);
   XtManageChild(ok);

   cancel = XmCreatePushButtonGadget(form," Cancel ",NULL,0);
   XtManageChild(cancel);
   XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);

   XtVaSetValues(rwcl,
		 XmNtopAttachment,XmATTACH_FORM,
		 XmNbottomAttachment,XmATTACH_POSITION,
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNrightAttachment,XmATTACH_FORM,
		 XmNbottomPosition,90,
		 XmNtopOffset,10,
		 XmNbottomOffset,10,
		 XmNleftOffset,10,
		 XmNrightOffset,10,
		 NULL);
   
   XtVaSetValues(wgt,
		 XmNtopAttachment,XmATTACH_WIDGET,
		 XmNbottomAttachment,XmATTACH_NONE,
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNrightAttachment,XmATTACH_FORM,
		 XmNtopWidget,rwcl,
		 XmNtopOffset,10,
		 XmNbottomOffset,10,
		 XmNleftOffset,10,
		 XmNrightOffset,10,
		 NULL);
   
   
   XtVaSetValues(form,
		 XmNtopAttachment,XmATTACH_WIDGET,
		 XmNbottomAttachment,XmATTACH_FORM,
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNrightAttachment,XmATTACH_FORM,
		 XmNtopWidget,wgt,
		 XmNtopOffset,10,
		 XmNbottomOffset,10,
		 XmNleftOffset,10,
		 XmNrightOffset,10,
		 NULL);

   XtVaSetValues(ok,
/*
		 XmNtopAttachment,XmATTACH_FORM,
		 XmNbottomAttachment,XmATTACH_FORM,
*/
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNrightAttachment,XmATTACH_NONE,
		 XmNtopOffset,10,
		 XmNbottomOffset,10,
		 XmNleftOffset,10,
		 XmNrightOffset,10,
		 NULL);

   XtVaSetValues(cancel,
/*
		 XmNtopAttachment,XmATTACH_FORM,
		 XmNbottomAttachment,XmATTACH_FORM,
*/
		 XmNleftAttachment,XmATTACH_NONE,
		 XmNrightAttachment,XmATTACH_FORM,
		 XmNtopOffset,10,
		 XmNbottomOffset,10,
		 XmNleftOffset,10,
		 XmNrightOffset,10,
		 NULL);

#endif


   rc11=XmCreateRowColumn(rwcl,"rc",NULL,0);
   rc22=XmCreateRowColumn(rwcl,"rc",NULL,0);
   XtManageChild(rc11);
   XtManageChild(rc22);

   frame = XmCreateFrame(rc11,"Frame",NULL,0);
   XtManageChild(frame);
   rc1=XmCreateRowColumn(frame,"rc",NULL,0);
   XtManageChild(rc1);

   frame = XmCreateFrame(rc22,"Frame",NULL,0);
   XtManageChild(frame);
   rc2=XmCreateRowColumn(frame,"rc",NULL,0);
   XtManageChild(rc2);


   XtVaSetValues(rc1,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,(MAX_NUZR/2)+1,NULL);

   wgt = XmCreateFrame(rc1,"fm",NULL,0);
   XtManageChild(wgt);

   t = XmCreateRowColumn(wgt,"rc",NULL,0);
   XtManageChild(t);

   label = XmCreateLabelGadget(t,"Group 1 : <i,UZR(i)>",NULL,0);
   XtManageChild(label);
/*
   label = XmCreateSeparatorGadget(rc1,"st",NULL,0);
   XtManageChild(label);
*/

   XtVaSetValues(rc2,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,(MAX_NUZR/2)+1,NULL);

   wgt = XmCreateFrame(rc2,"fm",NULL,0);
   XtManageChild(wgt);


   t = XmCreateRowColumn(wgt,"rc",NULL,0);
   XtManageChild(t);


   label = XmCreateLabelGadget(t,"Group 2 : <i,UZR(i)>",NULL,0);
   XtManageChild(label);
/*
   label = XmCreateSeparatorGadget(rc2,"st",NULL,0);
   XtManageChild(label);
*/
      
   for(i = 0; i < MAX_NUZR/2; i++)	{
     
     
     k1 = 2*i;
     k2 = k1+1;
/*     
     frame = XmCreateFrame(rc1,"Frame",NULL,0);
     XtManageChild(frame);
*/
     wgt = XmCreateRowColumn(rc1,"RowColumn",NULL,0);
     XtManageChild(wgt);
     XtVaSetValues(wgt,XmNpacking,XmPACK_COLUMN,
		   XmNorientation,XmHORIZONTAL,
                   XmNnumColumns,1,NULL);

     GW.uzrButton[k1]=XmCreateTextField(wgt,"Button",NULL,0);
     GW.uzrButton[k2]=XmCreateTextField(wgt,"Button",NULL,0);
     XtManageChild(GW.uzrButton[k1]);
     XtManageChild(GW.uzrButton[k2]);
     XtVaSetValues(GW.uzrButton[k1],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.uzrStrValue[k1],
		   NULL);
     XtVaSetValues(GW.uzrButton[k2],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.uzrStrValue[k2],
		   NULL);
     
   }

   for(i = MAX_NUZR/2; i < MAX_NUZR; i++)	{
     
     
     k1 = 2*i;
     k2 = k1+1;
/*     
     frame = XmCreateFrame(rc2,"Frame",NULL,0);
     XtManageChild(frame);
*/
     wgt = XmCreateRowColumn(rc2,"RowColumn",NULL,0);
     XtManageChild(wgt);
     XtVaSetValues(wgt,XmNpacking,XmPACK_COLUMN,
		   XmNorientation,XmHORIZONTAL,
                   XmNnumColumns,1,NULL);

     GW.uzrButton[k1]=XmCreateTextField(wgt,"Button",NULL,0);
     GW.uzrButton[k2]=XmCreateTextField(wgt,"Button",NULL,0);
     XtManageChild(GW.uzrButton[k1]);
     XtManageChild(GW.uzrButton[k2]);
     XtVaSetValues(GW.uzrButton[k1],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.uzrStrValue[k1],
		   NULL);
     XtVaSetValues(GW.uzrButton[k2],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.uzrStrValue[k2],
		   NULL);
     
   }

#endif

   return rtrn;

}


#ifdef _NO_PROTO
int	CountAny(who)
int	who;
#else
int	CountAny(int who)
#endif

{
 int i, k1,k2,num=0;
 char  *value;

 switch(who) {
 
 case 0:                /* icp */

  for(i=0; i<MAX_NICP; i++) {
     XtVaGetValues(GW.icpButton[i],XmNvalue,&value,NULL);
     if(strcmp(value,"")!=0)
       ++num;
/*
     k1=2*i;
     k2=k1+1;
     XtVaGetValues(GW.icpButton[k2],XmNvalue,&value,NULL);
     if(strcmp(value,"")!=0)
       ++num;
*/
   }
 
   break;

 case 1:		/* uzr */

   for(i=0; i<MAX_NUZR; i++) {
     k1=2*i;
     k2=k1+1;
     XtVaGetValues(GW.uzrButton[k2],XmNvalue,&value,NULL);
     if(strcmp(value,"")!=0)
       ++num;
   }
   break;

 case 2:		/* thl */

   for(i=0; i<MAX_NTHL; i++) {
     k1=2*i;
     k2=k1+1;
     XtVaGetValues(GW.thlButton[k2],XmNvalue,&value,NULL);
     if(strcmp(value,"")!=0)
       ++num;
   }
   break;

 case 3:		/* thu */

   for(i=0; i<MAX_NTHU; i++) {
     k1=2*i;
     k2=k1+1;
     XtVaGetValues(GW.thuButton[k2],XmNvalue,&value,NULL);
     if(strcmp(value,"")!=0)
       ++num;
   }
   break;

 default:
   break;
 }

  return num;

}



#ifdef _NO_PROTO
Widget PopupThu(parent)
Widget parent;
#else
Widget PopupThu(Widget parent)
#endif

{
   int i,k1,k2;
   Widget rtrn,wgt,frame,label,rwcl,window,form,ok,apply,cancel,help;
   Widget rc1,rc2,rc11,rc22,t;
   static ClientData userData1,userData2;

   rtrn = XmCreateFormDialog(parent,"Prompt",NULL,0);



   PopupTemplate1(rtrn,&rwcl,&ok,&apply,&cancel,&help);


   XtVaSetValues(rwcl,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,1,NULL);
  


   userData1.widget = rtrn;
   userData1.data = 4;
   userData2.widget = rtrn;
   userData2.data = 8;


   XtAddCallback(ok,XmNactivateCallback,TmpCB,&userData1);
   XtAddCallback(apply,XmNactivateCallback,TmpCB,&userData2);
   XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);
   XtAddCallback(help,XmNactivateCallback,PopupCB,GW.helpList);

   frame = XmCreateFrame(rwcl,"Frame",NULL,0);
   XtManageChild(frame);
   rc1=XmCreateRowColumn(frame,"rc",NULL,0);
   XtManageChild(rc1);

   label = XmCreateLabelGadget(rc1,"  < i,       THU(i) >",NULL,0);
   XtManageChild(label);


   for(i = 0; i < MAX_NTHU; i++)	{
     
     
     k1 = 2*i;
     k2 = k1+1;

     wgt = XmCreateRowColumn(rc1,"RowColumn",NULL,0);
     XtManageChild(wgt);
     XtVaSetValues(wgt,
/*                 XmNpacking,XmPACK_COLUMN, */
		   XmNorientation,XmHORIZONTAL,
                   XmNnumColumns,1,NULL);

     GW.thuButton[k1]=XmCreateTextField(wgt,"Button",NULL,0);
     GW.thuButton[k2]=XmCreateTextField(wgt,"Button",NULL,0);
     GW.thuTg[i]=XmCreateToggleButton(wgt,"",NULL,0);
     XtManageChild(GW.thuTg[i]);
     XtManageChild(GW.thuButton[k1]);
     XtManageChild(GW.thuButton[k2]);
     XtVaSetValues(GW.thuButton[k1],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.thuStrValue[k1],
		   NULL);
     XtVaSetValues(GW.thuButton[k2],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.thuStrValue[k2],
		   NULL);
     
   }



#if 0

   PopupTemplate(rtrn,&rwcl,&ok,&apply,&cancel,&help,27);

   XtVaSetValues(rwcl,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,1,NULL);

   userData1.widget = rtrn;
   userData1.data = 4;
   userData2.widget = rtrn;
   userData2.data = 8;



   XtAddCallback(ok,XmNactivateCallback,TmpCB,&userData1);
   XtAddCallback(apply,XmNactivateCallback,TmpCB,&userData2);
   XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);
   XtAddCallback(help,XmNactivateCallback,PopupCB,GW.helpList);

#if 0

   rwcl = XmCreateRowColumn(rtrn,"RowColumn",NULL,0);
   XtManageChild(rwcl);
   XtVaSetValues(rwcl,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,1,NULL);


   wgt = XmCreateSeparatorGadget(rtrn,"Separator",NULL,0);
   XtManageChild(wgt);

   form = XmCreateForm(rtrn,"Form",NULL,0);
   XtManageChild(form);
   ok = XmCreatePushButtonGadget(form,"  Ok  ",NULL,0);
   XtManageChild(ok);

   cancel = XmCreatePushButtonGadget(form," Cancel ",NULL,0);
   XtManageChild(cancel);
   XtAddCallback(cancel,XmNactivateCallback,PopdownCB,rtrn);

   XtVaSetValues(rwcl,
		 XmNtopAttachment,XmATTACH_FORM,
		 XmNbottomAttachment,XmATTACH_POSITION,
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNrightAttachment,XmATTACH_FORM,
		 XmNbottomPosition,90,
		 XmNtopOffset,10,
		 XmNbottomOffset,10,
		 XmNleftOffset,10,
		 XmNrightOffset,10,
		 NULL);
   
   XtVaSetValues(wgt,
		 XmNtopAttachment,XmATTACH_WIDGET,
		 XmNbottomAttachment,XmATTACH_NONE,
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNrightAttachment,XmATTACH_FORM,
		 XmNtopWidget,rwcl,
		 XmNtopOffset,10,
		 XmNbottomOffset,10,
		 XmNleftOffset,10,
		 XmNrightOffset,10,
		 NULL);
   
   
   XtVaSetValues(form,
		 XmNtopAttachment,XmATTACH_WIDGET,
		 XmNbottomAttachment,XmATTACH_FORM,
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNrightAttachment,XmATTACH_FORM,
		 XmNtopWidget,wgt,
		 XmNtopOffset,10,
		 XmNbottomOffset,10,
		 XmNleftOffset,10,
		 XmNrightOffset,10,
		 NULL);

   XtVaSetValues(ok,
/*
		 XmNtopAttachment,XmATTACH_FORM,
		 XmNbottomAttachment,XmATTACH_FORM,
*/
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNrightAttachment,XmATTACH_NONE,
		 XmNtopOffset,10,
		 XmNbottomOffset,10,
		 XmNleftOffset,10,
		 XmNrightOffset,10,
		 NULL);

   XtVaSetValues(cancel,
/*
		 XmNtopAttachment,XmATTACH_FORM,
		 XmNbottomAttachment,XmATTACH_FORM,
*/
		 XmNleftAttachment,XmATTACH_NONE,
		 XmNrightAttachment,XmATTACH_FORM,
		 XmNtopOffset,10,
		 XmNbottomOffset,10,
		 XmNleftOffset,10,
		 XmNrightOffset,10,
		 NULL);

#endif


   rc11=XmCreateRowColumn(rwcl,"rc",NULL,0);
   rc22=XmCreateRowColumn(rwcl,"rc",NULL,0);
   XtManageChild(rc11);
   XtManageChild(rc22);

   frame = XmCreateFrame(rc11,"Frame",NULL,0);
   XtManageChild(frame);

   rc1=XmCreateRowColumn(frame,"rc",NULL,0);
   XtManageChild(rc1);

   frame = XmCreateFrame(rc22,"Frame",NULL,0);
   XtManageChild(frame);

   rc2=XmCreateRowColumn(frame,"rc",NULL,0);
   XtManageChild(rc2);


   XtVaSetValues(rc1,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,(MAX_NTHU/2)+1,NULL);

   wgt = XmCreateFrame(rc1,"fm",NULL,0);
   XtManageChild(wgt);

   t = XmCreateRowColumn(wgt,"rc",NULL,0);
   XtManageChild(t);

   label = XmCreateLabelGadget(t,"Group 1 : <i,THU(i)>",NULL,0);
   XtManageChild(label);
/*
   label = XmCreateSeparatorGadget(rc1,"st",NULL,0);
   XtManageChild(label);
*/

   XtVaSetValues(rc2,XmNorientation,XmHORIZONTAL,
		 XmNpacking,XmPACK_COLUMN,
		 XmNnumColumns,(MAX_NTHU/2)+1,NULL);

   wgt = XmCreateFrame(rc2,"fm",NULL,0);
   XtManageChild(wgt);

   t = XmCreateRowColumn(wgt,"rc",NULL,0);
   XtManageChild(t);

   label = XmCreateLabelGadget(t,"Group 2 : <i,THU(i)>",NULL,0);
   XtManageChild(label);
/*
   label = XmCreateSeparatorGadget(rc2,"st",NULL,0);
   XtManageChild(label);
*/
      
   for(i = 0; i < MAX_NTHU/2; i++)	{
     
     
     k1 = 2*i;
     k2 = k1+1;
/*     
     frame = XmCreateFrame(rc1,"Frame",NULL,0);
     XtManageChild(frame);
*/
     wgt = XmCreateRowColumn(rc1,"RowColumn",NULL,0);
     XtManageChild(wgt);
     XtVaSetValues(wgt,XmNpacking,XmPACK_COLUMN,
		   XmNorientation,XmHORIZONTAL,
                   XmNnumColumns,1,NULL);

     GW.thuButton[k1]=XmCreateTextField(wgt,"Button",NULL,0);
     GW.thuButton[k2]=XmCreateTextField(wgt,"Button",NULL,0);
     XtManageChild(GW.thuButton[k1]);
     XtManageChild(GW.thuButton[k2]);
     XtVaSetValues(GW.thuButton[k1],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.thuStrValue[k1],
		   NULL);
     XtVaSetValues(GW.thuButton[k2],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.thuStrValue[k2],
		   NULL);
     
   }

   for(i = MAX_NTHU/2; i < MAX_NTHU; i++)	{
     
     
     k1 = 2*i;
     k2 = k1+1;
/*     
     frame = XmCreateFrame(rc2,"Frame",NULL,0);
     XtManageChild(frame);
*/
     wgt = XmCreateRowColumn(rc2,"RowColumn",NULL,0);
     XtManageChild(wgt);
     XtVaSetValues(wgt,XmNpacking,XmPACK_COLUMN,
		   XmNorientation,XmHORIZONTAL,
                   XmNnumColumns,1,NULL);

     GW.thuButton[k1]=XmCreateTextField(wgt,"Button",NULL,0);
     GW.thuButton[k2]=XmCreateTextField(wgt,"Button",NULL,0);
     XtManageChild(GW.thuButton[k1]);
     XtManageChild(GW.thuButton[k2]);
     XtVaSetValues(GW.thuButton[k1],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.thuStrValue[k1],
		   NULL);
     XtVaSetValues(GW.thuButton[k2],
		   XmNblinkRate,0,
		   XmNcolumns,DE_TEXTLENGTH,
		   XmNmaxLength,WX_TEXTLENGTH,
		   XmNvalue,GP.thuStrValue[k2],
		   NULL);
     
   }

#endif

   return rtrn;

}



#ifdef _NO_PROTO
void   PopupProb(parent)
Widget parent;
#else
void  PopupProb(Widget parent)
#endif

{
   Widget rtrn,button;

   rtrn = XmCreatePopupMenu(parent,"Popup",NULL,0);
   XtAddEventHandler(parent,ButtonPressMask,False,PostItCB,rtrn);

   button = XmCreateLabelGadget(rtrn,     "Menu",NULL,0);
   XtManageChild(button);
   button = XmCreateSeparatorGadget(rtrn, "Separator",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "NDIM",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "NBC",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "NINT",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "JAC",NULL,0);
   XtManageChild(button);

}


#ifdef _NO_PROTO
void   PopupDis(parent)
Widget parent;
#else
void  PopupDis(Widget parent)
#endif

{
   Widget rtrn,button;

   rtrn = XmCreatePopupMenu(parent,"Popup",NULL,0);
   XtAddEventHandler(parent,ButtonPressMask,False,PostItCB,rtrn);

   button = XmCreateLabelGadget(rtrn,     "Menu",NULL,0);
   XtManageChild(button);
   button = XmCreateSeparatorGadget(rtrn, "Separator",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "NTST",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "NCOL",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "IAD",NULL,0);
   XtManageChild(button);

}


#ifdef _NO_PROTO
void   PopupTol(parent)
Widget parent;
#else
void  PopupTol(Widget parent)
#endif

{
   Widget rtrn,button;

   rtrn = XmCreatePopupMenu(parent,"Popup",NULL,0);
   XtAddEventHandler(parent,ButtonPressMask,False,PostItCB,rtrn);

   button = XmCreateLabelGadget(rtrn,     "Menu",NULL,0);
   XtManageChild(button);
   button = XmCreateSeparatorGadget(rtrn, "Separator",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "EPSL",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "EPSU",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "EPSS",NULL,0);
   XtManageChild(button);


   button = XmCreateLabelGadget(rtrn,     "ITMX",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "NWTN",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "ITNW",NULL,0);
   XtManageChild(button);

}



#ifdef _NO_PROTO
void   PopupStep(parent)
Widget parent;
#else
void  PopupStep(Widget parent)
#endif

{
   Widget rtrn,button;

   rtrn = XmCreatePopupMenu(parent,"Popup",NULL,0);
   XtAddEventHandler(parent,ButtonPressMask,False,PostItCB,rtrn);

   button = XmCreateLabelGadget(rtrn,     "Menu",NULL,0);
   XtManageChild(button);
   button = XmCreateSeparatorGadget(rtrn, "Separator",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "DS",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "DSMIN",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "DSMAX",NULL,0);
   XtManageChild(button);


   button = XmCreateLabelGadget(rtrn,     "IADS",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "THL",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "THU",NULL,0);
   XtManageChild(button);

}




#ifdef _NO_PROTO
void   PopupLim(parent)
Widget parent;
#else
void  PopupLim(Widget parent)
#endif

{
   Widget rtrn,button;

   rtrn = XmCreatePopupMenu(parent,"Popup",NULL,0);
   XtAddEventHandler(parent,ButtonPressMask,False,PostItCB,rtrn);

   button = XmCreateLabelGadget(rtrn,     "Menu",NULL,0);
   XtManageChild(button);
   button = XmCreateSeparatorGadget(rtrn, "Separator",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "NMX",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "RL0",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "RL1",NULL,0);
   XtManageChild(button);


   button = XmCreateLabelGadget(rtrn,     "A0",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "A1",NULL,0);
   XtManageChild(button);

}



#ifdef _NO_PROTO
void   PopupCon(parent)
Widget parent;
#else
void  PopupCon(Widget parent)
#endif

{
   Widget rtrn,button;

   rtrn = XmCreatePopupMenu(parent,"Popup",NULL,0);
   XtAddEventHandler(parent,ButtonPressMask,False,PostItCB,rtrn);

   button = XmCreateLabelGadget(rtrn,     "Menu",NULL,0);
   XtManageChild(button);
   button = XmCreateSeparatorGadget(rtrn, "Separator",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "NICP",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "ICP",NULL,0);
   XtManageChild(button);


}



#ifdef _NO_PROTO
void   PopupRun(parent)
Widget parent;
#else
void  PopupRun(Widget parent)
#endif

{
   Widget rtrn,button;

   rtrn = XmCreatePopupMenu(parent,"Popup",NULL,0);
   XtAddEventHandler(parent,ButtonPressMask,False,PostItCB,rtrn);

   button = XmCreateLabelGadget(rtrn,     "Menu",NULL,0);
   XtManageChild(button);
   button = XmCreateSeparatorGadget(rtrn, "Separator",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "ILP",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "ISP",NULL,0);
   XtManageChild(button);


   button = XmCreateLabelGadget(rtrn,     "ISW",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "MXBF",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "IRS",NULL,0);
   XtManageChild(button);


   button = XmCreateLabelGadget(rtrn,     "IPS",NULL,0);
   XtManageChild(button);



}




#ifdef _NO_PROTO
void   PopupOut(parent)
Widget parent;
#else
void  PopupOut(Widget parent)
#endif

{
   Widget rtrn,button;

   rtrn = XmCreatePopupMenu(parent,"Popup",NULL,0);
   XtAddEventHandler(parent,ButtonPressMask,False,PostItCB,rtrn);

   button = XmCreateLabelGadget(rtrn,     "Menu",NULL,0);
   XtManageChild(button);
   button = XmCreateSeparatorGadget(rtrn, "Separator",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "NPR",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "IID",NULL,0);
   XtManageChild(button);


   button = XmCreateLabelGadget(rtrn,     "IPLT",NULL,0);
   XtManageChild(button);

   button = XmCreateLabelGadget(rtrn,     "UZR",NULL,0);
   XtManageChild(button);


}



#ifdef _NO_PROTO
void   PopupTemplate(parent,rwcl,ok,apply,cancel,help,position)
Widget parent,*rwcl,*ok,*apply,*cancel,*help;
int    position;
#else
void   PopupTemplate
(Widget parent,Widget *rwcl,
Widget *ok,Widget *apply,Widget *cancel,Widget *help,int position)
#endif

{
  Widget sept,form;


  *rwcl = XmCreateRowColumn(parent,"RowColumn",NULL,0);
  XtManageChild(*rwcl);

  sept= XmCreateSeparatorGadget(parent,"Separator",NULL,0);
  XtManageChild(sept);

  form = XmCreateForm(parent,"Form",NULL,0);
  XtManageChild(form);


  
  XtVaSetValues(*rwcl,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_NONE,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);

  
  XtVaSetValues(sept,
		XmNtopAttachment,XmATTACH_WIDGET,
		XmNbottomAttachment,XmATTACH_NONE,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopWidget,*rwcl,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);

  XtVaSetValues(form,
		XmNtopAttachment,XmATTACH_WIDGET,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopWidget,sept,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);
  

  *ok = XmCreatePushButton(form,"  Ok  ",NULL,0);
  XtManageChild(*ok);

  *apply = XmCreatePushButton(form,"Apply ",NULL,0);
  XtManageChild(*apply);

  *cancel = XmCreatePushButton(form,"Cancel",NULL,0);
  XtManageChild(*cancel);

  
  *help = XmCreatePushButton(form," Help ",NULL,0);
  XtManageChild(*help);

  XtVaSetValues(*ok,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_NONE,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);

  XtVaSetValues(*apply,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_POSITION,
		XmNrightAttachment,XmATTACH_NONE,
		XmNleftPosition,position,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);

  XtVaSetValues(*cancel,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_NONE,
		XmNrightAttachment,XmATTACH_POSITION,
		XmNrightPosition,100-position,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);



  XtVaSetValues(*help,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_NONE,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);


}



#ifdef _NO_PROTO
void   PopupTemplate1(parent,rwcl,ok,apply,cancel,help)
Widget parent,*rwcl,*ok,*apply,*cancel,*help;
#else
void   PopupTemplate1
(Widget parent,Widget *rwcl,Widget *ok,Widget *apply,Widget *cancel,Widget *help)
#endif

{
  Widget sept,form;

  *rwcl = XmCreateRowColumn(parent,"RowColumn",NULL,0);
  XtManageChild(*rwcl);

  sept= XmCreateSeparatorGadget(parent,"Separator",NULL,0);
  XtManageChild(sept);

  form = XmCreateForm(parent,"Form",NULL,0);
  XtManageChild(form);

/*
  XtVaSetValues(form,XmNorientation,XmHORIZONTAL,
		XmNpacking,XmPACK_COLUMN,
		XmNnumColumns,1,NULL);
*/
  
  XtVaSetValues(*rwcl,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_NONE,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);

  
  XtVaSetValues(sept,
		XmNtopAttachment,XmATTACH_WIDGET,
		XmNbottomAttachment,XmATTACH_NONE,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopWidget,*rwcl,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);

  XtVaSetValues(form,
		XmNtopAttachment,XmATTACH_WIDGET,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopWidget,sept,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);
  

  *ok = XmCreatePushButton(form," Ok ",NULL,0);
  XtManageChild(*ok);

  *apply = XmCreatePushButton(form,"Apply",NULL,0);
  XtManageChild(*apply);

  *cancel = XmCreatePushButton(form,"Cancel",NULL,0);
  XtManageChild(*cancel);

  
  *help = XmCreatePushButton(form,"Help",NULL,0);
  XtManageChild(*help);


  XtVaSetValues(*ok,
/*
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
*/
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_NONE,
		XmNtopOffset,10,
		XmNbottomOffset,10,
/*
		XmNleftOffset,10,
		XmNrightOffset,5,
*/
		NULL);

  XtVaSetValues(*apply,
/*
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
*/
		XmNleftAttachment,XmATTACH_POSITION,
		XmNrightAttachment,XmATTACH_NONE,
		XmNleftPosition,23,
		XmNtopOffset,10,
		XmNbottomOffset,10,
/*
		XmNleftOffset,5,
		XmNrightOffset,5,
*/
		NULL);

  XtVaSetValues(*cancel,
/*
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
*/
		XmNleftAttachment,XmATTACH_NONE,
		XmNrightAttachment,XmATTACH_POSITION,
		XmNrightPosition,77,
		XmNtopOffset,10,
		XmNbottomOffset,10,
/*
		XmNleftOffset,5,
		XmNrightOffset,5,
*/
		NULL);



  XtVaSetValues(*help,
/*
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_FORM,
*/
		XmNleftAttachment,XmATTACH_NONE,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,10,
		XmNbottomOffset,10,
/*
		XmNleftOffset,5,
		XmNrightOffset,10,
*/
		NULL);

}




#ifdef _NO_PROTO
void   PopupTemplate3(parent,rwcl,ok,apply,cancel,help)
Widget parent,*rwcl,*ok,*apply,*cancel,*help;
#else
void   PopupTemplate3
(Widget parent,Widget *rwcl,Widget *ok,Widget *apply,Widget *cancel,Widget *help)
#endif

{
  Widget sept,form,rc;

  *rwcl = XmCreateRowColumn(parent,"RowColumn",NULL,0);
  XtManageChild(*rwcl);

  sept= XmCreateSeparatorGadget(parent,"Separator",NULL,0);
  XtManageChild(sept);

  rc = XmCreateRowColumn(parent,"rc",NULL,0);
  XtManageChild(rc);
  
  XtVaSetValues(*rwcl,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_NONE,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);

  
  XtVaSetValues(sept,
		XmNtopAttachment,XmATTACH_WIDGET,
		XmNbottomAttachment,XmATTACH_NONE,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopWidget,*rwcl,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);

  XtVaSetValues(rc,
		XmNtopAttachment,XmATTACH_WIDGET,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopWidget,sept,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);
  

  XtVaSetValues(rc,XmNorientation,XmHORIZONTAL,
		XmNpacking,XmPACK_COLUMN,
		XmNnumColumns,2,NULL);

  *ok = XmCreatePushButton(rc,"  Ok  ",NULL,0);
  XtManageChild(*ok);

  *apply = XmCreatePushButton(rc,"Apply",NULL,0);
  XtManageChild(*apply);

  *cancel = XmCreatePushButton(rc,"Cancel",NULL,0);
  XtManageChild(*cancel);

  *help = XmCreatePushButton(rc," Help ",NULL,0);
  XtManageChild(*help);

}



#ifdef _NO_PROTO
void   PopupTemplate4(parent,rwcl,ok,cancel)
Widget parent,*rwcl,*ok,*cancel;
#else
void   PopupTemplate4(Widget parent,Widget *rwcl,Widget *ok,Widget *cancel)
#endif

{
  Widget sept,form,rc;

  *rwcl = XmCreateRowColumn(parent,"RowColumn",NULL,0);
  XtManageChild(*rwcl);

  sept= XmCreateSeparatorGadget(parent,"Separator",NULL,0);
  XtManageChild(sept);

  form = XmCreateForm(parent,"fm",NULL,0);
  XtManageChild(form);
  
  XtVaSetValues(*rwcl,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_NONE,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);

  
  XtVaSetValues(sept,
		XmNtopAttachment,XmATTACH_WIDGET,
		XmNbottomAttachment,XmATTACH_NONE,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopWidget,*rwcl,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);

  XtVaSetValues(form,
		XmNtopAttachment,XmATTACH_WIDGET,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopWidget,sept,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);
  

  *ok = XmCreatePushButton(form,"  Ok  ",NULL,0);
  XtManageChild(*ok);

  XtVaSetValues(*ok,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_NONE,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		NULL);

  *cancel = XmCreatePushButton(form,"Cancel",NULL,0);
  XtManageChild(*cancel);

  XtVaSetValues(*cancel,
		XmNleftAttachment,XmATTACH_NONE,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNrightOffset,10,
		NULL);

}



#ifdef _NO_PROTO
void   PopupTemplate2(parent,fm)
Widget parent,*fm;
#else
void   PopupTemplate2(Widget parent,Widget *fm)
#endif

{
  Widget sept,form,cancel,help;

  *fm = XmCreateMainWindow(parent,"MainW",NULL,0);
  XtManageChild(*fm);

  sept= XmCreateSeparatorGadget(parent,"Separator",NULL,0);
  XtManageChild(sept);

  form = XmCreateForm(parent,"Form",NULL,0);
  XtManageChild(form);


  XtVaSetValues(*fm,
		XmNtopAttachment,XmATTACH_FORM,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
                XmNbottomWidget,sept,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);

  
  XtVaSetValues(sept,
		XmNtopAttachment,XmATTACH_NONE,
		XmNbottomAttachment,XmATTACH_WIDGET,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNbottomWidget,form,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);

  XtVaSetValues(form,
		XmNtopAttachment,XmATTACH_NONE,
		XmNbottomAttachment,XmATTACH_FORM,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);
  

  cancel = XmCreatePushButton(form,"Cancel",NULL,0);
  XtManageChild(cancel);
  XtAddCallback(cancel,XmNactivateCallback,PopdownCB,parent);

/*  
  help = XmCreatePushButton(form," Help ",NULL,0);
  XtManageChild(help);
  XtAddCallback(help,XmNactivateCallback,PopupCB,GW.demoList);
*/

  XtVaSetValues(cancel,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);
/*

  XtVaSetValues(help,
		XmNleftAttachment,XmATTACH_NONE,
		XmNrightAttachment,XmATTACH_FORM,
		XmNtopOffset,10,
		XmNbottomOffset,10,
		XmNrightOffset,10,
		NULL);
*/
}







#if 0

#include <stdio.h>
#include <stdlib.h>

/*   tmp codes begin */

#define WX_FNLEN	10
#define MAX_NICP	20
#define MAX_NUZR	20
#define MAX_NTHL	20
#define MAX_NTHU	20


enum {

  PAR_NDIM,
  PAR_NBC,                          
  PAR_NINT,
  PAR_JAC,
  PAR_NTST,     
  PAR_NCOL,                         
  PAR_IAD,
  PAR_EPSL,
  PAR_EPSU,
  PAR_EPSS,             
  PAR_ITMX,
  PAR_NWTN,
  PAR_ITNW,
  PAR_DS,
  PAR_DSMIN,
  PAR_DSMAX,
  PAR_IADS,                   
  PAR_NMX,
  PAR_RL0,
  PAR_RL1,
  PAR_A0,
  PAR_A1,
  PAR_NICP,                      
  PAR_ICP,
  PAR_ILP,
  PAR_ISP,
  PAR_ISW,               
  PAR_MXBF,
  PAR_IRS,
  PAR_IPS,
  PAR_NPR,
  PAR_IID,
  PAR_IPLT,                     
  PAR_NUZR,
  PAR_NTHL,
  PAR_NTHU

};

typedef struct _TEMP {

double parValue[36];
char   parStrValue[36][WX_FNLEN];

double icpValue[2*MAX_NICP];
char   icpStrValue[2*MAX_NICP][WX_FNLEN];

double uzrValue[2*MAX_NUZR];
char   uzrStrValue[2*MAX_NUZR][WX_FNLEN];

double thlValue[2*MAX_NTHL];
char   thlStrValue[2*MAX_NTHL][WX_FNLEN];

double thuValue[2*MAX_NTHU];
char   thuStrValue[2*MAX_NTHU][WX_FNLEN];

int    nuzr,nthl,nthu;

} TEMP;


extern int  ReadRfile(char *rfile);
extern void WriteRfile(char *rfile);

extern TEMP GP;

TEMP GP;
  
/*   tmp codes end  */


#endif

#ifdef _NO_PROTO
int    ReadRfile(rfile)
char   *rfile;
#else
int    ReadRfile(char *rfile)
#endif

{

  FILE *fp;
  int  c,k1,k2;
  double tmp;

  if( (fp = fopen(rfile,"r")) == NULL)  {
    printf("Unable to open the file : %s\n",rfile);
    if (fp != NULL)    fclose(fp);    
    return (-1);
  }
  if( fscanf(fp,"%lg%lg%lg%lg",
	 &GP.parValue[PAR_NDIM],
	 &GP.parValue[PAR_IPS],
	 &GP.parValue[PAR_IRS],
	 &GP.parValue[PAR_ILP]) != 4) {
    if (fp != NULL)    fclose(fp);
    return (-1);
  }

  sprintf(GP.parStrValue[PAR_NDIM],"%d",(int) GP.parValue[PAR_NDIM]);  
  sprintf(GP.parStrValue[PAR_IPS ],"%d",(int) GP.parValue[PAR_IPS]);  
  sprintf(GP.parStrValue[PAR_IRS ],"%d",(int) GP.parValue[PAR_IRS]);  
  sprintf(GP.parStrValue[PAR_ILP ],"%d",(int) GP.parValue[PAR_ILP]);  

  while( (c=fgetc(fp)) != '\n' );



  if( fscanf(fp,"%lg", &GP.parValue[PAR_NICP]) != 1) {
    if (fp != NULL)    fclose(fp);
    return (-1);
  }
  sprintf(GP.parStrValue[PAR_NICP],"%d",(int) GP.parValue[PAR_NICP]);  

  if((int) GP.parValue[PAR_NICP] > MAX_NICP) {
    GP.nicpFlag = -1;
    GP.nicp = MAX_NICP;
  }
  else  
    GP.nicp = (int) GP.parValue[PAR_NICP];


  for(c=0; c < (int)GP.parValue[PAR_NICP]; c++) {
    if( fscanf(fp,"%lg",&GP.icpValue[c]) != 1) {
      if (fp != NULL)    fclose(fp);      
      return (-1);
    }
    sprintf(GP.icpStrValue[c],"%d",(int) GP.icpValue[c]);  
  }
  if(( (int) GP.parValue[PAR_NICP]) > 0) {
    GP.parValue[PAR_ICP]=GP.icpValue[0];
    sprintf(GP.parStrValue[PAR_ICP],"%d",(int)GP.parValue[PAR_ICP]);    
  }
  else {
    strcpy(GP.parStrValue[PAR_ICP],"");
  }



  while( (c=fgetc(fp)) != '\n' );


  if( fscanf(fp,"%lg%lg%lg%lg%lg%lg%lg%lg",
	 &GP.parValue[PAR_NTST],
	 &GP.parValue[PAR_NCOL],
	 &GP.parValue[PAR_IAD],
	 &GP.parValue[PAR_ISP],
	 &GP.parValue[PAR_ISW],
	 &GP.parValue[PAR_IPLT],
	 &GP.parValue[PAR_NBC],
	 &GP.parValue[PAR_NINT]) != 8) {
    if (fp != NULL)    fclose(fp);
    return (-1);
  }
  
  sprintf(GP.parStrValue[PAR_NTST],"%d",(int) GP.parValue[PAR_NTST]);  
  sprintf(GP.parStrValue[PAR_NCOL],"%d",(int) GP.parValue[PAR_NCOL]);  
  sprintf(GP.parStrValue[PAR_IAD],"%d",(int) GP.parValue[PAR_IAD]);  
  sprintf(GP.parStrValue[PAR_ISP],"%d",(int) GP.parValue[PAR_ISP]);  
  sprintf(GP.parStrValue[PAR_ISW],"%d",(int) GP.parValue[PAR_ISW]);  
  sprintf(GP.parStrValue[PAR_IPLT],"%d",(int) GP.parValue[PAR_IPLT]);  
  sprintf(GP.parStrValue[PAR_NBC],"%d",(int) GP.parValue[PAR_NBC]);  
  sprintf(GP.parStrValue[PAR_NINT],"%d",(int) GP.parValue[PAR_NINT]);  


  while( (c=fgetc(fp)) != '\n' );


  if(fscanf(fp,"%lg%lg%lg%lg%lg",
	 &GP.parValue[PAR_NMX],
	 &GP.parValue[PAR_RL0],
	 &GP.parValue[PAR_RL1],
	 &GP.parValue[PAR_A0],
	 &GP.parValue[PAR_A1]) != 5) {
    if (fp != NULL)    fclose(fp);
    return (-1);
  }

  sprintf(GP.parStrValue[PAR_NMX],"%d",(int) GP.parValue[PAR_NMX]);  
  sprintf(GP.parStrValue[PAR_RL0],"%lg", GP.parValue[PAR_RL0]);  
  sprintf(GP.parStrValue[PAR_RL1],"%lg", GP.parValue[PAR_RL1]);  
  sprintf(GP.parStrValue[PAR_A0],"%lg", GP.parValue[PAR_A0]);  
  sprintf(GP.parStrValue[PAR_A1],"%lg", GP.parValue[PAR_A1]);  



  while( (c=fgetc(fp)) != '\n' );


  if( fscanf(fp,"%lg%lg%lg%lg%lg%lg%lg",
	 &GP.parValue[PAR_NPR],
	 &GP.parValue[PAR_MXBF],
	 &GP.parValue[PAR_IID],
	 &GP.parValue[PAR_ITMX],
	 &GP.parValue[PAR_ITNW],
	 &GP.parValue[PAR_NWTN],
	 &GP.parValue[PAR_JAC]) != 7) {
    if (fp != NULL)    fclose(fp);
    return (-1);
  }

  sprintf(GP.parStrValue[PAR_NPR],"%d",(int) GP.parValue[PAR_NPR]);  
  sprintf(GP.parStrValue[PAR_MXBF],"%d",(int) GP.parValue[PAR_MXBF]);  
  sprintf(GP.parStrValue[PAR_IID],"%d",(int) GP.parValue[PAR_IID]);  
  sprintf(GP.parStrValue[PAR_ITMX],"%d",(int) GP.parValue[PAR_ITMX]);  
  sprintf(GP.parStrValue[PAR_ITNW],"%d",(int) GP.parValue[PAR_ITNW]);  
  sprintf(GP.parStrValue[PAR_NWTN],"%d",(int) GP.parValue[PAR_NWTN]);  
  sprintf(GP.parStrValue[PAR_JAC],"%d",(int) GP.parValue[PAR_JAC]);  


  while( (c=fgetc(fp)) != '\n' );

  if( fscanf(fp,"%lg%lg%lg",
	 &GP.parValue[PAR_EPSL],
	 &GP.parValue[PAR_EPSU],
	 &GP.parValue[PAR_EPSS]) != 3) {
    if (fp != NULL)    fclose(fp);
    return (-1);
  }

  sprintf(GP.parStrValue[PAR_EPSL],"%lg",GP.parValue[PAR_EPSL]);  
  sprintf(GP.parStrValue[PAR_EPSU],"%lg",GP.parValue[PAR_EPSU]);  
  sprintf(GP.parStrValue[PAR_EPSS],"%lg",GP.parValue[PAR_EPSS]);  


  while( (c=fgetc(fp)) != '\n' );

  if( fscanf(fp,"%lg%lg%lg%lg",
	 &GP.parValue[PAR_DS],
	 &GP.parValue[PAR_DSMIN],
	 &GP.parValue[PAR_DSMAX],
	 &GP.parValue[PAR_IADS]) != 4) {
    if (fp != NULL)    fclose(fp);
    return (-1);
  }


  sprintf(GP.parStrValue[PAR_DS],"%lg",GP.parValue[PAR_DS]);  
  sprintf(GP.parStrValue[PAR_DSMIN],"%lg",GP.parValue[PAR_DSMIN]);  
  sprintf(GP.parStrValue[PAR_DSMAX],"%lg",GP.parValue[PAR_DSMAX]);  
  sprintf(GP.parStrValue[PAR_IADS],"%d",(int) GP.parValue[PAR_IADS]);  


  while( (c=fgetc(fp)) != '\n' );


  if( fscanf(fp,"%lg", &tmp) != 1) {
    if (fp != NULL)    fclose(fp);
    return (-1);
  }

  GP.nthl = (int) tmp;

  while( (c=fgetc(fp)) != '\n' );

  for(c=0; c < GP.nthl; c++) {
    k1=2*c;
    k2=k1+1;
    if( fscanf(fp,"%lg%lg",&GP.thlValue[k1],&GP.thlValue[k2]) != 2) {
      if (fp != NULL)    fclose(fp);
      return (-1);
    }
    /*    while( (c=fgetc(fp)) != '\n' );*/
    sprintf(GP.thlStrValue[k1],"%d",(int) GP.thlValue[k1]);  
    sprintf(GP.thlStrValue[k2],"%lg",      GP.thlValue[k2]);  
  }
  if(GP.nthl > 0) {
    GP.parValue[PAR_NTHL]=GP.thlValue[1];
    sprintf(GP.parStrValue[PAR_NTHL],"%lg",GP.parValue[PAR_NTHL]);    
  }
  else {
    strcpy(GP.parStrValue[PAR_NTHL],"");
  }


  if( fscanf(fp,"%lg", &tmp) != 1) {
    if (fp != NULL)    fclose(fp);
    return (-1);
  }
  GP.nthu = (int) tmp;

  while( (c=fgetc(fp)) != '\n' );

  for(c=0; c < GP.nthu; c++) {
    k1=2*c;
    k2=k1+1;
    if( fscanf(fp,"%lg%lg",&GP.thuValue[k1],&GP.thuValue[k2]) != 2) {
      if (fp != NULL)    fclose(fp);
      return (-1);
    }	
    /*  while( (c=fgetc(fp)) != '\n' ); */
    sprintf(GP.thuStrValue[k1],"%d",(int) GP.thuValue[k1]);  
    sprintf(GP.thuStrValue[k2],"%lg",      GP.thuValue[k2]);  
  }
  if(GP.nthu > 0) {
    GP.parValue[PAR_NTHU]=GP.thuValue[1];
    sprintf(GP.parStrValue[PAR_NTHU],"%lg",GP.parValue[PAR_NTHU]);    
  }
  else {
    strcpy(GP.parStrValue[PAR_NTHU],"");
  }

  if( fscanf(fp,"%lg", &tmp) != 1) {
    if (fp != NULL)    fclose(fp);
    return (-1);
  }
  GP.nuzr = (int) tmp;

  while( (c=fgetc(fp)) != '\n' );

  for(c=0; c < GP.nuzr; c++) {
    k1=2*c;
    k2=k1+1;
    if( fscanf(fp,"%lg%lg",&GP.uzrValue[k1],&GP.uzrValue[k2]) != 2) {
      if (fp != NULL)    fclose(fp);
      return (-1);
    }
    sprintf(GP.uzrStrValue[k1],"%d",(int) GP.uzrValue[k1]);  
    sprintf(GP.uzrStrValue[k2],"%lg",      GP.uzrValue[k2]);  
  }
  if(GP.nuzr > 0) {
    GP.parValue[PAR_NUZR]=GP.uzrValue[1];
    sprintf(GP.parStrValue[PAR_NUZR],"%lg",GP.parValue[PAR_NUZR]);    
  }
  else {
    strcpy(GP.parStrValue[PAR_NUZR],"");
  }

  
  if (fp != NULL)    fclose(fp);  


  for(c=0; c<MAX_NUZR; c++)
    GP.uzrToggle[c] = False;
  for(c=0; c<MAX_NTHU; c++)
    GP.thuToggle[c] = False;
  for(c=0; c<MAX_NTHL; c++)
    GP.thlToggle[c] = False;
  for(c=0; c<MAX_NICP; c++)
    GP.icpToggle[c] = False;


  return 0;
}


#ifdef _NO_PROTO
void   WriteRfile(rfile)
char   *rfile;
#else
void   WriteRfile(char *rfile)
#endif

{
  FILE *fp;
  int c,k1,k2;
  double tmp;

  if( (fp = fopen(rfile,"w")) == NULL) 
    printf("Unable to write file : %s\n",rfile);

  fprintf(fp,"%d %d %d %d",
	 (int)GP.parValue[PAR_NDIM],
	 (int)GP.parValue[PAR_IPS],
	 (int)GP.parValue[PAR_IRS],
	 (int)GP.parValue[PAR_ILP]);

  fprintf(fp,"\t\t\t\tNDIM,IPS,IRS,ILP\n");



  k1=0;
  for(c=0; c < GP.nicp; c++) {
    if(GP.icpToggle[c]) 
      ++k1;
  }

  k2 = (int)GP.nicp - k1;

  fprintf(fp,"%d ", k2);
/*  fprintf(fp,"%d ", (int)GP.parValue[PAR_NICP]);*/

  for(c=0; c < GP.nicp; c++) {
    if(!GP.icpToggle[c])
      fprintf(fp,"%d ",(int)GP.icpValue[c]);
  }

  fprintf(fp,"\t\t\t\tNICP,(ICP(I),I=1,NICP)\n");

  fprintf(fp,"%d %d %d %d %d %d %d %d ",
	 (int)GP.parValue[PAR_NTST],
	 (int)GP.parValue[PAR_NCOL],
	 (int)GP.parValue[PAR_IAD],
	 (int)GP.parValue[PAR_ISP],
	 (int)GP.parValue[PAR_ISW],
	 (int)GP.parValue[PAR_IPLT],
	 (int)GP.parValue[PAR_NBC],
	 (int)GP.parValue[PAR_NINT]);
  
  fprintf(fp,"\t\tNTST,NCOL,IAD,ISP,ISW,IPLT,NBC,NINT\n");

  fprintf(fp,"%d %lg %lg %lg %lg ",
	 (int)GP.parValue[PAR_NMX],
	      GP.parValue[PAR_RL0],
	      GP.parValue[PAR_RL1],
	      GP.parValue[PAR_A0],
	      GP.parValue[PAR_A1]);


  fprintf(fp,"\t\t\tNMX,RL0,RL1,A0,A1\n");

  fprintf(fp,"%d %d %d %d %d %d %d",
	 (int)GP.parValue[PAR_NPR],
	 (int)GP.parValue[PAR_MXBF],
	 (int)GP.parValue[PAR_IID],
	 (int)GP.parValue[PAR_ITMX],
	 (int)GP.parValue[PAR_ITNW],
	 (int)GP.parValue[PAR_NWTN],
	 (int)GP.parValue[PAR_JAC]);

  fprintf(fp,"\t\t\tNPR,MXBF,IID,ITMX,ITNW,NWTN,JAC\n");

  fprintf(fp,"%lg %lg %lg ",
	 GP.parValue[PAR_EPSL],
	 GP.parValue[PAR_EPSU],
	 GP.parValue[PAR_EPSS]);


  fprintf(fp,"\t\tEPSL,EPSU,EPSS\n");

  fprintf(fp,"%lg %lg %lg %d ",
	      GP.parValue[PAR_DS],
	      GP.parValue[PAR_DSMIN],
	      GP.parValue[PAR_DSMAX],
	 (int)GP.parValue[PAR_IADS]);


  fprintf(fp,"\t\tDS,DSMIN,DSMAX,IADS\n");
  

  k1=0;
  for(c=0; c < GP.nthl; c++) {
    if(GP.thlToggle[c]) 
      ++k1;
  }

  k2 = (int)GP.nthl - k1;


  fprintf(fp,"%d ", k2);
  fprintf(fp,"\t\t\t\tNTHL,((I,THL(I)),I=1,NTHL)\n");
  
  for(c=0; c < GP.nthl; c++) {
    k1=2*c;
    k2=k1+1;
    if(!GP.thlToggle[c])
      fprintf(fp,"%d %lg\n",(int)GP.thlValue[k1],GP.thlValue[k2]);

  }


  k1=0;
  for(c=0; c < GP.nthu; c++) {
    if(GP.thuToggle[c]) 
      ++k1;
  }

  k2 = (int)GP.nthu - k1;


  fprintf(fp,"%d ", k2);
  fprintf(fp,"\t\t\t\tNTHU,((I,THU(I)),I=1,NTHU)\n");


  for(c=0; c < GP.nthu; c++) {
    k1=2*c;
    k2=k1+1;
    if(!GP.thuToggle[c])
      fprintf(fp,"%d %lg\n",(int)GP.thuValue[k1],GP.thuValue[k2]);
  }


  k1=0;
  for(c=0; c < GP.nuzr; c++) {
    if(GP.uzrToggle[c]) 
      ++k1;
  }

  k2 = (int)GP.nuzr - k1;

  fprintf(fp,"%d ", k2);
  fprintf(fp,"\t\t\t\tNUZR,((I,UZR(I)),I=1,NUZR)\n");

  for(c=0; c < GP.nuzr; c++) {
    k1=2*c;
    k2=k1+1;
    if(!GP.uzrToggle[c])
      fprintf(fp,"%d %lg\n",(int)GP.uzrValue[k1],GP.uzrValue[k2]);
  }

  if (fp != NULL)    fclose(fp);

}


#ifdef _NO_PROTO
void   SetParScreen()
#else
void   SetParScreen()
#endif

{
  int i,k1,k2;
  Boolean toggle;

  for(i=0; i<MAX_NUZR; i++) {
    if(GP.uzrToggle[i])
      XtVaSetValues(GW.uzrTg[i],XmNset,True,NULL);
    else
      XtVaSetValues(GW.uzrTg[i],XmNset,False,NULL);
  }
  for(i=0; i<MAX_NTHU; i++) {
    if(GP.thuToggle[i])
      XtVaSetValues(GW.thuTg[i],XmNset,True,NULL);
    else
      XtVaSetValues(GW.thuTg[i],XmNset,False,NULL);
  }
  for(i=0; i<MAX_NTHL; i++) {
    if(GP.thlToggle[i])
      XtVaSetValues(GW.thlTg[i],XmNset,True,NULL);
    else
      XtVaSetValues(GW.thlTg[i],XmNset,False,NULL);
  }
  for(i=0; i<MAX_NICP; i++) {
    if(GP.icpToggle[i])
      XtVaSetValues(GW.icpTg[i],XmNset,True,NULL);
    else
      XtVaSetValues(GW.icpTg[i],XmNset,False,NULL);
  }


  k1=0;
  for(i=0; i<GP.nuzr; i++) {
    if(GP.uzrToggle[i])
      ++k1;
  }
  k2=GP.nuzr - k1;
  GP.parValue[PAR_NUZR] = k2;

  sprintf(GP.parStrValue[PAR_NUZR],"%d",k2);

/*  sprintf(GP.parStrValue[PAR_NUZR],"%d",GP.nuzr);*/


  k1=0;
  for(i=0; i<GP.nthl; i++) {
    if(GP.thlToggle[i])
      ++k1;
  }
  k2=GP.nthl - k1;
  GP.parValue[PAR_NTHL] = k2;
  sprintf(GP.parStrValue[PAR_NTHL],"%d",k2);

  k1=0;
  for(i=0; i<GP.nthu; i++) {
    if(GP.thuToggle[i])
      ++k1;
  }
  k2=GP.nthu - k1;

  GP.parValue[PAR_NTHU] = k2;
  sprintf(GP.parStrValue[PAR_NTHU],"%d",k2);


  k1=0;
  for(i=0; i<GP.nicp; i++) {
    if(GP.icpToggle[i])
      ++k1;
  }
  k2=GP.nicp - k1;

  GP.parValue[PAR_NICP] = k2;
  sprintf(GP.parStrValue[PAR_NICP],"%d",k2);



  for(i=0; i<GP.numParameter; i++) {
    XtVaSetValues(GW.parButton[i],XmNvalue,GP.parStrValue[i],NULL);
  }

  for(i=GP.nicp; i<MAX_NICP; i++)
    XtVaSetValues(GW.icpButton[i],XmNvalue,"",NULL);

  for(i=0; i< GP.nicp; i++) 
    XtVaSetValues(GW.icpButton[i],XmNvalue,GP.icpStrValue[i],NULL);

  for(i=GP.nuzr; i<MAX_NUZR; i++)
    XtVaSetValues(GW.uzrButton[i],XmNvalue,"",NULL);

  for(i=0; i<GP.nuzr; i++) {
    k1=2*i;
    k2=k1+1;
    XtVaSetValues(GW.uzrButton[k1],XmNvalue,GP.uzrStrValue[k1],NULL);
    XtVaSetValues(GW.uzrButton[k2],XmNvalue,GP.uzrStrValue[k2],NULL);
  }


  for(i=GP.nthl; i<MAX_NTHL; i++)
    XtVaSetValues(GW.thlButton[i],XmNvalue,"",NULL);

  for(i=0; i<GP.nthl; i++) {
    k1=2*i;
    k2=k1+1;
    XtVaSetValues(GW.thlButton[k1],XmNvalue,GP.thlStrValue[k1],NULL);
    XtVaSetValues(GW.thlButton[k2],XmNvalue,GP.thlStrValue[k2],NULL);
  }

  for(i=GP.nthu; i<MAX_NTHU; i++)
    XtVaSetValues(GW.thuButton[i],XmNvalue,"",NULL);

  for(i=0; i<GP.nthu; i++) {
    k1=2*i;
    k2=k1+1;
    XtVaSetValues(GW.thuButton[k1],XmNvalue,GP.thuStrValue[k1],NULL);
    XtVaSetValues(GW.thuButton[k2],XmNvalue,GP.thuStrValue[k2],NULL);
  }


    /*   update problem settings */


    XtVaSetValues(GW.probButton[0],XmNvalue,GP.parStrValue[0],NULL);
    XtVaSetValues(GW.probButton[1],XmNvalue,GP.parStrValue[1],NULL);
    XtVaSetValues(GW.probButton[2],XmNvalue,GP.parStrValue[2],NULL);
    XtVaSetValues(GW.probButton[3],XmNvalue,GP.parStrValue[3],NULL);

    /*   update discretization settings */

    XtVaSetValues(GW.disButton[0],XmNvalue,GP.parStrValue[4],NULL);
    XtVaSetValues(GW.disButton[1],XmNvalue,GP.parStrValue[5],NULL);
    XtVaSetValues(GW.disButton[2],XmNvalue,GP.parStrValue[6],NULL);


    /* update tolerance settings */

    XtVaSetValues(GW.tolButton[0],XmNvalue,GP.parStrValue[7],NULL);
    XtVaSetValues(GW.tolButton[1],XmNvalue,GP.parStrValue[8],NULL);
    XtVaSetValues(GW.tolButton[2],XmNvalue,GP.parStrValue[9],NULL);
    XtVaSetValues(GW.tolButton[3],XmNvalue,GP.parStrValue[10],NULL);
    XtVaSetValues(GW.tolButton[4],XmNvalue,GP.parStrValue[11],NULL);
    XtVaSetValues(GW.tolButton[5],XmNvalue,GP.parStrValue[12],NULL);

 
   /* update step size settings */

    XtVaSetValues(GW.stepButton[0],XmNvalue,GP.parStrValue[13],NULL);
    XtVaSetValues(GW.stepButton[1],XmNvalue,GP.parStrValue[14],NULL);
    XtVaSetValues(GW.stepButton[2],XmNvalue,GP.parStrValue[15],NULL);
    XtVaSetValues(GW.stepButton[3],XmNvalue,GP.parStrValue[16],NULL);
    XtVaSetValues(GW.stepButton[4],XmNvalue,GP.parStrValue[34],NULL);
    XtVaSetValues(GW.stepButton[5],XmNvalue,GP.parStrValue[35],NULL);


    /* update limit settings */

    XtVaSetValues(GW.limButton[0],XmNvalue,GP.parStrValue[17],NULL);
    XtVaSetValues(GW.limButton[1],XmNvalue,GP.parStrValue[18],NULL);
    XtVaSetValues(GW.limButton[2],XmNvalue,GP.parStrValue[19],NULL);
    XtVaSetValues(GW.limButton[3],XmNvalue,GP.parStrValue[20],NULL);
    XtVaSetValues(GW.limButton[4],XmNvalue,GP.parStrValue[21],NULL);


    /* update continuation  setting */

    XtVaSetValues(GW.conButton[0],XmNvalue,GP.parStrValue[22],NULL);
    XtVaSetValues(GW.conButton[1],XmNvalue,GP.parStrValue[23],NULL);


    /* update run setting */


    XtVaSetValues(GW.runButton[0],XmNvalue,GP.parStrValue[24],NULL);
    XtVaSetValues(GW.runButton[1],XmNvalue,GP.parStrValue[25],NULL);
    XtVaSetValues(GW.runButton[2],XmNvalue,GP.parStrValue[26],NULL);
    XtVaSetValues(GW.runButton[3],XmNvalue,GP.parStrValue[27],NULL);
    XtVaSetValues(GW.runButton[4],XmNvalue,GP.parStrValue[28],NULL);
    XtVaSetValues(GW.runButton[5],XmNvalue,GP.parStrValue[29],NULL);

    /* update output settings */

    XtVaSetValues(GW.outButton[0],XmNvalue,GP.parStrValue[30],NULL);
    XtVaSetValues(GW.outButton[1],XmNvalue,GP.parStrValue[31],NULL);
    XtVaSetValues(GW.outButton[2],XmNvalue,GP.parStrValue[32],NULL);
    XtVaSetValues(GW.outButton[3],XmNvalue,GP.parStrValue[33],NULL);


}


#ifdef _NO_PROTO
void   SaveParScreen(who)
int    who;
#else
void   SaveParScreen(int who)
#endif

{
  

  String value;
  int i,k1,k2;
  Boolean toggle;

  switch (who) {
    
  case 1:
  case 5:

    GP.nicp = CountAny(0);
    if( GP.nicpFlag == 0) {
      
      k1=0;
      for(i=0; i<GP.nicp; i++) {
        XtVaGetValues(GW.icpTg[i],XmNset,&toggle,NULL);
	if(toggle) {
	  GP.icpToggle[i]=True;
	  ++k1;
	}
	else
	  GP.icpToggle[i]=False;
	  
      }

      GP.parValue[PAR_NICP] = GP.nicp - k1;

/*      GP.parValue[PAR_NICP]=GP.nicp; */
      sprintf(GP.parStrValue[PAR_NICP],"%d",(int)GP.parValue[PAR_NICP]);      
    }

    for(i = 0; i < GP.nicp; i++) {	

      XtVaGetValues(GW.icpButton[i],XmNvalue,&value,NULL);
      GP.icpValue[i] = atof(value);    
      sprintf(GP.icpStrValue[i],"%d",(int) GP.icpValue[i]);
      
    }
    if(GP.nicp>0) {

      k1=0;
      while(GP.icpToggle[k1]) 
	++k1;
      GP.parValue[PAR_ICP] = GP.icpValue[k1];


      sprintf(GP.parStrValue[PAR_ICP],"%d",(int)GP.parValue[PAR_ICP]); 
      XtVaSetValues(GW.parButton[PAR_NICP],XmNvalue,GP.parStrValue[PAR_NICP],NULL);
      XtVaSetValues(GW.parButton[PAR_ICP],XmNvalue,GP.parStrValue[PAR_ICP],NULL);
      XtVaSetValues(GW.conButton[0],XmNvalue,GP.parStrValue[PAR_NICP],NULL);
      XtVaSetValues(GW.conButton[1],XmNvalue,GP.parStrValue[PAR_ICP],NULL);            

    }

    break;

  case 2:
  case 6:

    GP.nuzr = CountAny(1);
    for(i = 0; i < GP.nuzr; i++) {	
      k1=2*i;
      k2=k1+1;
      
      XtVaGetValues(GW.uzrButton[k1],XmNvalue,&value,NULL);
      GP.uzrValue[k1] = atof(value);    
      sprintf(GP.uzrStrValue[k1],"%d",(int) GP.uzrValue[k1]);
      
      XtVaGetValues(GW.uzrButton[k2],XmNvalue,&value,NULL);
      GP.uzrValue[k2] = atof(value);    
      sprintf(GP.uzrStrValue[k2],"%lg",GP.uzrValue[k2]);
      
    }
    if(GP.nuzr > 0) {
      
      k1=0;
      for(i=0; i<GP.nuzr; i++) {
        XtVaGetValues(GW.uzrTg[i],XmNset,&toggle,NULL);
	if(toggle) {
	  GP.uzrToggle[i]=True;
	  ++k1;
	}
	else
	  GP.uzrToggle[i]=False;
	  
      }

      GP.parValue[PAR_NUZR] = GP.nuzr - k1;
      sprintf(GP.parStrValue[PAR_NUZR],"%d",(int) GP.parValue[PAR_NUZR]);       
      
      XtVaSetValues(GW.parButton[PAR_NUZR],XmNvalue,GP.parStrValue[PAR_NUZR],NULL);
      XtVaSetValues(GW.outButton[3],XmNvalue,GP.parStrValue[PAR_NUZR],NULL);
    }


    break;

  case 3:
  case 7:


    GP.nthl = CountAny(2);
    for(i = 0; i < GP.nthl; i++) {	
      k1=2*i;
      k2=k1+1;
      
      XtVaGetValues(GW.thlButton[k1],XmNvalue,&value,NULL);
      GP.thlValue[k1] = atof(value);    
      sprintf(GP.thlStrValue[k1],"%d",(int) GP.thlValue[k1]);
      
      XtVaGetValues(GW.thlButton[k2],XmNvalue,&value,NULL);
      GP.thlValue[k2] = atof(value);    
      sprintf(GP.thlStrValue[k2],"%lg",GP.thlValue[k2]);
      
    }
    if(GP.nthl > 0) {

      
      k1=0;
      for(i=0; i<GP.nthl; i++) {
        XtVaGetValues(GW.thlTg[i],XmNset,&toggle,NULL);
	if(toggle) {
	  GP.thlToggle[i]=True;
	  ++k1;
	}
	else
	  GP.thlToggle[i]=False;
	  
      }

      GP.parValue[PAR_NTHL] = GP.nthl - k1;

/*      GP.parValue[PAR_NTHL] = GP.nthl; */
      sprintf(GP.parStrValue[PAR_NTHL],"%d",(int) GP.parValue[PAR_NTHL]);       
       
      XtVaSetValues(GW.parButton[PAR_NTHL],XmNvalue,GP.parStrValue[PAR_NTHL],NULL);
      XtVaSetValues(GW.stepButton[4],XmNvalue,GP.parStrValue[PAR_NTHL],NULL);
    }
   break;
    
  case 4:
  case 8:


    GP.nthu = CountAny(3);
    for(i = 0; i < GP.nthu; i++) {	
      k1=2*i;
      k2=k1+1;
      
      XtVaGetValues(GW.thuButton[k1],XmNvalue,&value,NULL);
      GP.thuValue[k1] = atof(value);    
      sprintf(GP.thuStrValue[k1],"%d",(int) GP.thuValue[k1]);
      
      XtVaGetValues(GW.thuButton[k2],XmNvalue,&value,NULL);
      GP.thuValue[k2] = atof(value);    
      sprintf(GP.thuStrValue[k2],"%lg",GP.thuValue[k2]);
      
    }
    if(GP.nthu>0) {
      
      k1=0;
      for(i=0; i<GP.nthu; i++) {
        XtVaGetValues(GW.thuTg[i],XmNset,&toggle,NULL);
	if(toggle) {
	  GP.thuToggle[i]=True;
	  ++k1;
	}
	else
	  GP.thuToggle[i]=False;
	  
      }

      GP.parValue[PAR_NTHU] = GP.nthu - k1;
/*      GP.parValue[PAR_NTHU] = GP.nthu; */
      sprintf(GP.parStrValue[PAR_NTHU],"%d",(int)GP.parValue[PAR_NTHU]);       
      
      XtVaSetValues(GW.parButton[PAR_NTHU],XmNvalue,GP.parStrValue[PAR_NTHU],NULL);
      XtVaSetValues(GW.stepButton[5],XmNvalue,GP.parStrValue[PAR_NTHU],NULL);
    }

    break;

  default:
    break;
  }

}


#if 0

main()
{
 char *rfile="c.exp.1";
 char *tfile="c.exp";

 ReadRfile(rfile);
 printf("reading ... done\n");
 WriteRfile(tfile);
  
}

#endif


#ifdef _NO_PROTO
char *ProgramName(pgm)
char *pgm;
#else
char *ProgramName(char *pgm)
#endif

{
  static char *progname;

  if (progname=strrchr(pgm, '/'))
    {
      progname++;
    }
  else
    {
      progname = pgm;
    }

  return progname;
}



#ifdef _NO_PROTO
void   GetJobName(jobName,programName)
char *jobName,*programName;
#else
void   GetJobName(char *jobName,char *programName)
#endif

{   
   int i = 0,n;

   n = strlen(programName);

   while(programName[i] != '.') {
     jobName[i] = programName[i];
     ++i;
     if(i > n) {
       i=0;
       break;
     }
   }
   if(i>0)
     jobName[i] = '\0';
   else
     strcpy(jobName,EMPTY);

}



char *GetDirName()
{
  static char dir[100];
  FILE *fp;

  system("pwd > ./XAutoDir &");
  fp=fopen("./XAutoDir","r");
  fscanf(fp,"%s",dir);
  if (fp != NULL)    fclose(fp);
  system("rm -f ./XAutoDir &");

  return dir;
}



#ifdef _NO_PROTO
void UpdateTime(w, id)
Widget          w;
XtIntervalId    *id;
#else
void UpdateTime(XtPointer w, XtIntervalId *id)
#endif

{
    long  tloc,nextSecond,nextMinute;
   
    time(&tloc); 
    Wprintf(w, "%s", ctime(&tloc));
/*    nextSecond = (1- tloc % 1) * 1000; 
    XtAddTimeOut(nextSecond, UpdateTime, w);
*/
    nextMinute = (60- tloc % 60) * 1000; 
    XtAddTimeOut(nextMinute, UpdateTime, w);
}

#if __STDC__
void Wprintf(Widget w,...)
#else
void Wprintf(va_alist)
va_dcl
#endif
{
  va_list   args;
  char      *format,str[100],s[20];  /* DANGER: Fixed buffer size */
  Arg       wargs[1];
  XmString  xmstr;
  int i;

#if __STDC__
  va_start(args,w);
#else
  Widget    w;
  va_start(args);
  w = va_arg(args, Widget);
#endif

  if(!XtIsSubclass(w, xmLabelWidgetClass))
    XtError("Wprintf() requires a Label Widget");
  format = va_arg(args, char *);
  vsprintf(str, format, args);

/*  for(i=10; i<20; i++) */
  for(i=10; i<16; i++)
    s[i-10]=str[i];
  s[i-10]='\0';

  xmstr =  XmStringLtoRCreate(s, XmSTRING_DEFAULT_CHARSET);
  XtSetArg(wargs[0], XmNlabelString, xmstr);
  XtSetValues(w, wargs, 1);     
  va_end(args);
}


#ifdef _NO_PROTO
int GetAutoPid(pgm)
char *pgm;
#else
int GetAutoPid(char *pgm)
#endif

{ 
   int pid;
   FILE *fp;
   char command[100];

   strcpy(command,"ps | awk '/");
   strcat(command,pgm);
   strcat(command,"/ {print $1}' > autopid");
   system(command);
   
   fp = fopen("autopid","r");   
   if(fscanf(fp,"%d",&pid) != 1) {
     if (fp != NULL)    fclose(fp);
     system("rm -f autopid &");
     return -1;
   }
   else {
     if (fp != NULL)    fclose(fp);
     system("rm -f autopid &");
     return pid;
   }
}
