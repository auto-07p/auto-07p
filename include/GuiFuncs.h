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

#ifdef	_NO_PROTO

extern Widget AppendData();
extern void   AppendDataCB();
extern void   clallCB();
extern void   cleanCB();
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
extern Widget CreateDemoList();
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
extern void   DemoCopyCB();
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
extern void   UzrToggleCB();
extern void   VtCB();
extern void   Wprintf();
extern void   WriteRfile();

#else

extern Widget AppendData(Widget);
extern void   AppendDataCB(Widget,XtPointer,XtPointer);
extern void   clallCB(Widget,XtPointer,XtPointer);
extern void   cleanCB(Widget,XtPointer,XtPointer);
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
extern Widget CreateDemoList(Widget);
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
extern void   DemoCopyCB(Widget,XtPointer,XtPointer);
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
extern void   UzrToggleCB(Widget,XtPointer,XtPointer);
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
