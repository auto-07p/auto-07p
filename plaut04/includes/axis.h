#ifndef MY_AXIS_H
    #define MY_AXIS_H
#endif
//#define TD_FONT

#include <Inventor/nodes/SoSeparator.h>
#include <Inventor/nodes/SoCylinder.h>
#include <Inventor/nodes/SoCone.h>
#include <Inventor/nodes/SoFont.h>
#include <Inventor/nodes/SoText2.h>
#include <Inventor/nodes/SoText3.h>
#include <Inventor/nodes/SoTransform.h>
#include <Inventor/nodes/SoMaterial.h>


class Axis
{

private:
	int type;               // type == 1, + 
	                        // type == 2, |_
	double maxValue ;
	double minValue ;
	double adjust ;
	int numOfTickers ;
	const char *name;
	double tickers[11];
	SbColor color;
	bool show3D;
               //                     which == 1, X
	int which; // which axis to draw, which == 0, Y
			   //					  which ==-1, Z

    SoSeparator * drawTicker(float pos, float height);
    SoSeparator * drawScale(float pos, float scale);
    SoSeparator * drawAxisName(float pos);
    SoSeparator * drawArrow();

public:
	Axis();
	Axis(int type, double maxValue, double minValue, int numOfTickers, const char* name, int wh, SbColor color);
	Axis(bool td, int type, double maxValue, double minValue, int numOfTickers, const char* name, int wh, SbColor color);
	~Axis();

	SoSeparator *createAxis();	

	void setMaxValue(double mx){ maxValue = mx; }
	void setMinValue(double mi){ minValue = mi; }
	void setNumOfTicker(int it){ numOfTickers = it; }
	void setName(const char* anName){ name = anName; }
	void set(int type, double maxValue, double minValue, int numOfTickers, const char * name, int wh, SbColor color);

};

