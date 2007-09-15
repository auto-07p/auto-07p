#include <qwidget.h>
#include <qspinbox.h>
#include <qcombobox.h>

#include <Inventor/Qt/SoQt.h>
#include <Inventor/Qt/viewers/SoQtExaminerViewer.h>

class DecSpinBox : public QSpinBox
{
    Q_OBJECT

public:
    DecSpinBox(int minValue, int maxValue, int step, QWidget * parent,
	       const char *name);
    QString  mapValueToText(int value);
    int mapTextToValue(bool* ok);
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

class Actions : public QWidget
{
    Q_OBJECT

public slots:
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
    void createPreferDialog(void);
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

    void graphCoordinateSystemToggledCB(int which);
    void graphTypeWidgetToggledCB(int which);
    void graphStyleWidgetToggledCB(int which);
    void graphCoordWidgetToggledCB(int which);
    void defaultGraphWidgetToggledCB(int bit);

    void closePreferDialogAndGiveUpChange();
    void closePreferDialogAndUpdateScene();
    void savePreferAndUpdateScene();
    void applyPreferDialogChangeAndUpdateScene();
};
