#! /usr/bin/env python

# Some ways to run AUTO:
# a) python auto
# b) import auto
#    auto.auto()
# c) from auto import *

if __name__ == "__main__":
    import sys, os
    sys.path.insert(0,os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
    from auto import AUTOclui
    AUTOclui.auto()
else:
    from auto.AUTOclui import *
