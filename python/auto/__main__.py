#! /usr/bin/env python

# Some ways to run AUTO:
# a) python auto
# b) import auto
#    auto.auto()
# c) from auto import *

if __name__ == "__main__":
    import sys, os
    autodir = os.path.dirname(os.path.abspath(__file__))
    for autodirectory in autodir, os.path.dirname(autodir):
        if autodirectory not in sys.path:
            sys.path.insert(0, autodirectory)
    from auto import AUTOclui
    AUTOclui.auto()
else:
    from auto.AUTOclui import *
