#! /usr/bin/env python

class AUTORegressionError(StandardError):
    """A regression test has failed"""
    pass

class AUTORuntimeError(StandardError):
    """A runtime error has occured"""
    pass
