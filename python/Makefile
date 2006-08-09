include $(DV)/Make.include

all:
	-mkdir -p variables
	echo $(DV) > variables/DV.$(shell uname)
	-mkdir -p lib/$(DV_ARCH)_$(DV_GRAPHIC_LIB)
	cd src;$(MAKE) DV=$(DV)

clean:
	rm -rf lib
	cd src;$(MAKE) DV=$(DV) clean
	rm -f *.pyc *~ log err
