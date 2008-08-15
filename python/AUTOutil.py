#! /usr/bin/env python
import types
import ConfigParser
import os

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
    if(os.path.exists(os.path.expandvars("$AUTO_DIR/.autorc"))):
        parser.read(os.path.expandvars("$AUTO_DIR/.autorc"))
    if(os.path.exists(os.path.expandvars("$HOME/.autorc"))):
        parser.read(os.path.expandvars("$HOME/.autorc"))
    if(os.path.exists("./autorc")):
        parser.read("./autorc")
    elif(os.path.exists("./.autorc")):
        parser.read("./.autorc")
    return parser

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
