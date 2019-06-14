#! /usr/bin/env python

class AUTORegressionError(Exception):
    """A regression test has failed"""
    pass

class AUTORuntimeError(Exception):
    """A runtime error has occured"""
    pass
