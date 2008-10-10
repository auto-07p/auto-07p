#! /usr/bin/env python
import types
import ConfigParser
import os
import array
import string
N = array

# This file contains code from the Python distribution.  As
# per the Python License we include the following:

##  -----------------------------------------------------------------------
##  Copyright 1991-1995 by Stichting Mathematisch Centrum, Amsterdam,
##  The Netherlands.

##                          All Rights Reserved

##  Permission to use, copy, modify, and distribute this software and its
##  documentation for any purpose and without fee is hereby granted,
##  provided that the above copyright notice appear in all copies and that
##  both that copyright notice and this permission notice appear in
##  supporting documentation, and that the names of Stichting Mathematisch
##  Centrum or CWI or Corporation for National Research Initiatives or
##  CNRI not be used in advertising or publicity pertaining to
##  distribution of the software without specific, written prior
##  permission.

##  While CWI is the initial source for this software, a modified version
##  is made available by the Corporation for National Research Initiatives
##  (CNRI) at the Internet address ftp://ftp.python.org.

##  STICHTING MATHEMATISCH CENTRUM AND CNRI DISCLAIM ALL WARRANTIES WITH
##  REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
##  MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL STICHTING MATHEMATISCH
##  CENTRUM OR CNRI BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
##  DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
##  PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
##  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
##  PERFORMANCE OF THIS SOFTWARE.
##  -----------------------------------------------------------------------

# for bool, False, and True, taken from SCons:
# __COPYRIGHT__
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
# KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
# WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#

# Portions of the following are derived from the compat.py file in
# Twisted, under the following copyright:
#
# Copyright (c) 2001-2004 Twisted Matrix Laboratories

# Two routines taken from Tkinter.py for merging dictionaries
def _flatten(tuple):
        res = ()
        for item in tuple:
                if type(item) in (types.TupleType, types.ListType):
                        res = res + _flatten(item)
                elif item is not None:
                        res = res + (item,)
        return res

def cnfmerge(cnfs):
        if type(cnfs) is types.DictionaryType:
                return cnfs
        elif type(cnfs) in (types.NoneType, types.StringType):
                return cnfs
        else:
                cnf = {}
                for c in _flatten(cnfs):
                        try:
                                cnf.update(c)
                        except (AttributeError, TypeError), msg:
                                print "_cnfmerge: fallback due to:", msg
                                for k, v in c.items():
                                        cnf[k] = v
                return cnf


def findBaseClass(inputClass,baseClass):
	try:
		for base in inputClass.__bases__:
			if base == baseClass:
				return 1
			else:
				if findBaseClass(base,baseClass) == 1:
					return 1
		return 0
	# Sometimes inputClass isn't really a class, if so we just return false
	except AttributeError:
		return 0

def getAUTORC(section):
    parser = ConfigParser.ConfigParser()
    parser.add_section(section)
    path = os.path.expandvars("$AUTO_DIR/.autorc")
    if os.path.exists(path):
        parser.read(path)
    path = os.path.expandvars("$HOME/.autorc")
    if os.path.exists(path):
        parser.read(path)
    if os.path.exists("./autorc"):
        parser.read("./autorc")
    elif os.path.exists("./.autorc"):
        parser.read("./.autorc")
    return parser

import __builtin__

try:
    all
except NameError:
    # Pre-2.5 Python has no all() function.
    def all(iterable):
        """
	Returns True if all elements of the iterable are true.
	"""
	for element in iterable:
             if not element:
                return False
	return True
    __builtin__.all = all
    all = all
 	
try:
    any
except NameError:
    # Pre-2.5 Python has no any() function.
    def any(iterable):
        """
        Returns True if any element of the iterable is true.
        """
        for element in iterable:
            if element:
                return True
        return False
    __builtin__.any = any
    any = any
 	

try:
    bool
except NameError:
    # Pre-2.2 Python has no bool() function.
    def bool(value):
        """Demote a value to 0 or 1, depending on its truth value.

        This is not to be confused with types.BooleanType, which is
        way too hard to duplicate in early Python versions to be
        worth the trouble.
        """
        return not not value
    __builtin__.bool = bool
    bool = bool

try:
    dict
except NameError:
    # Pre-2.2 Python has no dict() keyword.
    def dict(seq=[], **kwargs):
        """
        New dictionary initialization.
        """
        d = {}
        for k, v in seq:
            d[k] = v
        d.update(kwargs)
        return d
    __builtin__.dict = dict

try:
    False
except NameError:
    # Pre-2.2 Python has no False keyword.
    __builtin__.False = not 1
    # Assign to False in this module namespace so it shows up in pydoc output.
    False = False

try:
    True
except NameError:
    # Pre-2.2 Python has no True keyword.
    __builtin__.True = not 0
    # Assign to True in this module namespace so it shows up in pydoc output.
    True = True

# 
try: 
    zip 
except NameError: 
    # Pre-2.2 Python has no zip() function. 
    def zip(*lists):
        """
	Emulates the behavior we need from the built-in zip() function
	added in Python 2.2.

	Returns a list of tuples, where each tuple contains the i-th
	element rom each of the argument sequences. The returned
	list is truncated in length to the length of the shortest
	argument sequence.
	"""
	result = []
	for i in xrange(min(map(len, lists))):
	    result.append(tuple(map(lambda l, i=i: l[i], lists)))
	return result
    __builtin__.zip = zip 

try:
    isinstance('', (list, str))
except TypeError:
    #
    # Wrap isinstance() to make it compatible with the version in
    # Python 2.2 and newer.
    #
    _isinstance = isinstance
    from types import *
    d = { str: StringType, list: ListType, tuple: TupleType,
	  dict: DictType, slice: SliceType, float: FloatType,
	  int: IntType }
    def isinstance(obj, type_or_seq):
        if type(type_or_seq) in (type([]),type(())):
            for t in type_or_seq:
                if isinstance(obj, t):
                    return 1
            return 0
	if d.has_key(type_or_seq):
            return type(obj) == d[type_or_seq]
	return _isinstance(obj, type_or_seq)
    __builtin__.isinstance = isinstance

try:
    object
except NameError:
    class object:
        pass
    __builtin__.object = object

# very basic numpy emulation:
def array(l, code=None):
    if isinstance(l, N.ArrayType):
	if code is None:
	    code = l.typecode
	return N.array(code, l.tolist())
    if code is None:
	code = 'd'
    if isinstance(l, list):
        if isinstance(l[0], N.ArrayType):
	    return l
	elif isinstance(l[0], list):
	    a2 = []
	    for a1 in l:
		a2.append(N.array(code, a1))
	    return a2
	return N.array(code, l)
    return N.array(code, [l])

def rank(a):
    if isinstance(a, list):
	return 2
    return 1

def take(a, idx, axis=0):
    b=[]
    if axis == 1:
	for j in a:
	    b.append(take(j, idx))
	return b
    try:
	for i in idx:
	    b.append(a[i])
    except TypeError:
	raise IndexError
    return array(b)

def array2string(a,precision=0):
    return '[ '+string.join(map(str, a),"  ")+']'

def shape(a):
    if isinstance(a, list):
	return (len(a), len(a[0]))
    else:
	return (len(a),)
        
def zeros(dim,code):
    if len(dim) == 1:
	return N.array(code,dim[0]*[0.0])
    a = []
    for i in range(dim[0]):
	a.append(N.array(code,dim[1]*[0.0]))
    return a

def less(a, val):
    cond = []
    for v in a:
        cond.append(v < val)
    return cond

def ravel(a):
    return N.array('d',a)

ArrayType = N.ArrayType

class myreadlines:
    def __init__(self,f):
        self.lines = f.readlines()
	self.lineno = -1
	    
    def __getitem__(self,i):
	self.lineno = self.lineno + 1
        return self.lines[self.lineno]

    def next(self):
        return self.__getitem__(0)

def test():
    a={}
    b={}
    a["one"]   = "blue"
    a["two"]   = "green"
    b["two"]   = "cyan"
    b["three"] = "red"
    b["four"]  = "yellow"
    print cnfmerge((a,b))

if __name__ == "__main__":
    test()
