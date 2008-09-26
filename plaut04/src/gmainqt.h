#ifndef QT3_SUPPORT
#define QT3_SUPPORT
#endif
#include <qwidget.h>
#include <qspinbox.h>
#include <qcombobox.h>
#include <qtabwidget.h>
#include <qslider.h>
#include <qpushbutton.h>
#include <qlabel.h>
#include <qlayout.h>
#include <qdialog.h>
#include <qradiobutton.h>
#include <qcheckbox.h>
#include <qmainwindow.h>
#include <qgroupbox.h>
#if QT_VERSION >= 0x40000
#include <qmenu.h>
#define QPopupMenu QMenu
#else
#include <qpopupmenu.h>
#endif

#include <Inventor/Qt/SoQt.h>
#include <Inventor/Qt/viewers/SoQtExaminerViewer.h>

class DecSpinBox : public QSpinBox
{
    Q_OBJECT

public:
    DecSpinBox(int minValue, int maxValue, int step, QWidget * parent,
	       const char *name);
#if QT_VERSION >= 0x40000
    QString  textFromValue(int value) const;
    int valueFromText(QString text) const;
#else
    QString  mapValueToText(int value);
    int mapTextToValue(bool* ok);
#endif
};

class LineColorSpinBox : public DecSpinBox
{
    Q_OBJECT

public:
    LineColorSpinBox(int minValue, int maxValue, int step, QWidget * parent,
	          const char *name, int id);

public slots:
    void valueChangedCB(int value);

private:
    int which;
};

class LinePatternComboBox : public QComboBox
{
    Q_OBJECT

public:
    LinePatternComboBox(bool rw, QWidget * parent, const char *name, int id);

public slots:
    void valueChangedCB(int position);

private:
    int which;
};

typedef struct EditMenuItems
{
    QPopupMenu *items;
    int     which;
} EditMenuItems;

class PreferDialog : public QDialog
{
    Q_OBJECT

public:
    PreferDialog(QWidget * parent, const char *name);
    void update();

public slots:
    void closePreferDialogAndGiveUpChange();
    void closePreferDialogAndUpdateScene();
    void savePreferAndUpdateScene();
    void applyPreferDialogChangeAndUpdateScene();
    void graphCoordinateSystemToggledCB(int which);
    void graphTypeWidgetToggledCB(int which);
    void graphStyleWidgetToggledCB(int which);
    void graphCoordWidgetToggledCB(int which);
    void defaultGraphWidgetToggledCB(int bit);

private:
    void createPreferNotebookPages(QTabWidget *notebook);
    void createPreferActionFormControls(QWidget *parent);
    void createPreferDefaultPages(QWidget *parent);
    void createLineAttPages(QWidget *parent);
    void createLineColorAndPatternPrefSheetGuts(QWidget *parent, QGridLayout *layout, const char *name, int id);
    void createColorAndLinePrefSheetHeader(QWidget *parent, QGridLayout *form, int column);
    void createLineAttrPrefSheetParts(QWidget *parent, QGridLayout *form, const char** name);
    void createGraphCoordinateSystemFrameGuts(QGroupBox *frame);
    void createGraphStyleFrameGuts(QGroupBox *frame);
    void createGraphTypeFrameGuts(QGroupBox *frame);
    void createOptionFrameGuts(QGroupBox *frame);
    void createGraphCoordPartsFrameGuts(QGroupBox *frame);
    QRadioButton **coordSysButton, **styleButton, **typeButton,
      **coordButton;
    QCheckBox **widgetButton;
};

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow();
    void setListValue();

public slots:
    void quit();
    void fileMenuPick(int which);
    void typeMenuPick(int which);
    void typeMenuDisplay();
    void styleMenuPick(int which);
    void styleMenuDisplay();
    void coordMenuPick(int which);
    void coordMenuDisplay();
    void optMenuPick(int which);
    void optMenuDisplay();
    void editMenuPick(int which);
    void centerMenuDisplay();
    void showAboutDialog();
    void showHelp();
    void createPreferDialog();
    void xListCallBack(const QString &);
    void yListCallBack(const QString &);
    void zListCallBack(const QString &);
    void lblListCallBack(const QString &);
    void colorMethodSelectionCB(const QString &);
    void numPeriodAnimatedCB(const QString &);
    void orbitSpeedCB(int position);
    void satSpeedCB(int position);
    void lineWidthCB(int position);
    void dimensionToggledCB();
    void createBdBoxCB();

private:
    void buildMenu();
    QPopupMenu *buildFileMenu();
    QPopupMenu *buildTypeMenu();
    QPopupMenu *buildHelpMenu();
    QPopupMenu *buildOptionMenu();
    QPopupMenu *buildStyleMenu();
    QPopupMenu *buildCoordMenu();
    QPopupMenu *buildCenterMenu();

    void getFileName(int);

    QComboBox *xAxisList, *yAxisList, *zAxisList, *labelsList,
              *colorMethodSeletionList;
    QPushButton *dimButton;
    PreferDialog *preferDialog;

    EditMenuItems *optMenuItems;
    SoSeparator *sceneGraph;
#ifdef USE_EXAM_VIEWER
    SoQtExaminerViewer *renderArea;
#else
    SoQtRenderArea *renderArea;
#endif
};
