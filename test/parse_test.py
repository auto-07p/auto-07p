#!/usr/bin/env python
#global imports for the whole file
import re,sys,getopt,os
try:
    import cStringIO
except ImportError: # Python 3
    import io as cStringIO
try:
    from urllib import pathname2url
except ImportError: # Python 3
    from urllib.request import pathname2url
import parseB

#A few template strings which will be used later for reports, etc.

no_demo_string="""
Trial file does not have appropriate demo

Errors (if any) reported during compilation:

%s
"""

different_floats="""
======================
trial string:
======================
%s

======================
correct string:
======================
%s

"""

no_matching_float="""
Trial string has a non float where the correct string has a float:
demo:          %s
correct_value: %s
trial value:   %s

"""+different_floats

#All the demos in the AUTO distribution
all_demos=["pp2","exp","int","dd2","opt","lin","bvp","pp3","wav","plp",
           "ab","abc","brc","brf","bru","chu","cir","enz",
           "ext","ezp","frc","fsh","ivp","kar","kpr","lor","lrz",
           "mtn","nag","non","obv","ops","pd1","pd2","pen","phs",
           "ppp","pvl","rev","san","she","spb","stw","tim","tor"]

DEMO_OK=0
DEMO_MINOR_DATA_ERROR=1
DEMO_MAJOR_DATA_ERROR=2
DEMO_FAILED=4

def check_demo(demo,correct_filename,trial_filename,epsilon,abseps):
    # A regular expression for finding floating point numbers
    float_regex=re.compile('[ -][0-9]\.[0-9]+[-+eE][-+0-9][0-9]+',re.S)
    # A regular expression for finding each demo block
    demo_regex=re.compile("Demo %s is started.*Demo %s is done"%(demo,demo),re.S,)
    # A regular expression for finding a header
    header_regex=re.compile('BR',re.S)
    # A regular expression for finding each error block
    error_regex=re.compile("===%s start===.*===%s end==="%(demo,demo),re.S)

    # This is where the report get stored until it gets returned
    report = cStringIO.StringIO()

    # Search for all possible demos in each file
    correct_file=open(correct_filename,"r")
    correct_file=correct_file.read()
    trial_file=open(trial_filename,"r")
    trial_file=trial_file.read()

    # Use the regular expressions to find the appropriate blocks (if any)
    correct_result=demo_regex.search(correct_file)
    if correct_result is None:
        return (DEMO_FAILED,"Correct file does not have appropriate demo")
    else:
        correct_string=correct_result.group()

    trial_result=demo_regex.search(trial_file)
    if trial_result is None:
        error_file = open("%serrors"%trial_filename,"r")
        error_file = error_file.read()
        error_result=error_regex.search(error_file)
        if error_result is None:
            return (DEMO_FAILED,"Demo could not be found in trial file")
        else:
            return (DEMO_FAILED,no_demo_string%error_result.group())
    else:
        trial_string=trial_result.group()

    # Now that we have the corresponding blocks from each file we look at each
    # Floating point number is each and see if they are within epsilon
    # percentage of each other.
    end=0
    offset=1
    correct_item=header_regex.search(correct_string,0)
    trial_item=header_regex.search(trial_string,0)
    maxdiff = 0
    maxratio = 0
    if correct_item is not None and trial_item is not None:
        end=correct_item.end()
        offset=trial_item.end()-end
    while True:
        start=end
        correct_item=float_regex.search(correct_string,start)
        if correct_item is None:
            break
        start=correct_item.start()
        end=correct_item.end()
        trial_item=float_regex.search(trial_string[start+offset-1:end+offset+1])
        correct_data=parseB.AUTOatof(correct_item.group())
        try:
            trial_data=parseB.AUTOatof(trial_item.group())
            if trial_item.start() > 0:
                offset=trial_item.start()+offset-1
        except (ValueError, AttributeError):
            correct_string="%s<B><font color=Red>%s</font></B>%s"%(
                correct_string[:start],correct_string[start:end],
                correct_string[end:])
            trial_string="%s<B><font color=Red>%s</font></B>%s"%(
                trial_string[:start+offset],trial_string[start+offset:end+offset],
                trial_string[end+offset:])
            return (DEMO_MAJOR_DATA_ERROR,
                    no_matching_float%(demo,correct_item.group(),trial_item,
                                       trial_string,correct_string))

        if correct_string[start-7:start-3] == "Time":
            start=end
            correct_item=header_regex.search(correct_string,start)
            trial_item=header_regex.search(trial_string,start+offset)
            if correct_item is not None and trial_item is not None:
                start=correct_item.start()
                end=correct_item.end()
                offset=trial_item.end()-end
        else:
            diff = abs(trial_data-correct_data)
            if trial_data == 0.0:
                ratio1 = abs(correct_data)+epsilon
            else:
                ratio1 = abs(diff/trial_data)

            if correct_data == 0.0:
                ratio2 = abs(trial_data)+epsilon
            else:
                ratio2 = abs(diff/correct_data)
            ratio = max(ratio1, ratio2)

            if diff > abseps and ratio > epsilon:
                maxdiff = max(diff, maxdiff)
                maxratio = max(ratio, maxratio)
                report.write("Files do not match at the %dth character for demo %s\n"%(start,demo))
                report.write("Value of trial is %e\n"%trial_data)
                report.write("Value of correct file is %e\n\n"%correct_data)
                correct_string="%s<I><font color=Blue>%s</font></I>%s"%(
                    correct_string[:start],correct_string[start:end],
                    correct_string[end:])
                trial_string="%s<I><font color=Blue>%s</font></I>%s"%(
                    trial_string[:start+offset],trial_string[start+offset:end+offset],
                    trial_string[end+offset:])
                start += 31
                end += 31
    report_string = report.getvalue()
    if len(report_string)==0:
        return (DEMO_OK,"")
    else:
        return (DEMO_MINOR_DATA_ERROR,report_string+
                different_floats%(trial_string,correct_string),
                maxratio,maxdiff)

#====================== HTMLGen style class and functions ========

class BasicDocument(object):
    def __init__(self):
        self.header = ["""<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">
<HTML>

<!-- This file generated using parse_test.py module. -->
<HEAD>
  <META NAME="GENERATOR" CONTENT="AUTO-07P">
        <TITLE></TITLE> </HEAD>
<BODY>"""]
        self.footer = ["\n</BODY> </HTML>\n"]
        self.lst = []

    def append(self, s):
        self.lst.append(str(s))

    def write(self, s):
        f = open(s,"w")
        f.write("\n".join(self.header+self.lst+self.footer))
        f.close()

class Table(object):
    def __init__(self, name, width="", cell_padding=""):
        header = ['<A NAME="%s"></A>\n'%name,
                '<P><TABLE border=2 cellpadding=%s cellspacing=1 width="%s">\n'%
                (cell_padding, width),
                '<CAPTION align=top><STRONG>%s</STRONG></CAPTION>\n'%name]
        self.header = header
        self.heading = []
        self.body = []
        self.footer = ['</TABLE><P>\n']

    def __str__(self):
        lst = self.header + ['<TR Align=center> ']
        for s in self.heading:
            lst.append('<TH ColSpan=1>%s</TH>'%s)
        lst.append('</TR>\n')
        for line in self.body:
            lst.append('<TR>')
            for cell in line:
                lst.append('<TD Align=left >%s</TD>\n'%cell)
            lst.append('</TR>\n')
        lst.extend(self.footer)
        return "".join(lst)

def H(sz, s):
    return "<H%d>%s</H%d>\n"%(sz,s,sz)

def Image(s, border=0):
    return '<IMG src="%s" alt="%s" border="%d">'%(s,s,border)

def Pre(s):
    return '<PRE>%s</PRE>\n'%(s)

def Href(text="", url=""):
    return '<A HREF="%s">%s</A>'%(url,text)

#====================== main program =============================

def parse(trial_file,epsilon=None, abseps=None, demo=None, html_dir=None,
          correct_file=None, demos=None):
    if isinstance(trial_file, str):
        trial_file = trial_file,
    if epsilon is None:
        epsilon = 0.01
    if abseps is None:
        abseps = 1e-7
    if demo is None:
        demo=""
    if demos is None:
        demos=all_demos
    if html_dir is None:
        html_dir="verification"
    if correct_file is None:
        correct_file = "hyper.all"
    report_list = { DEMO_OK: [],
                    DEMO_MINOR_DATA_ERROR: [],
                    DEMO_MAJOR_DATA_ERROR: [],
                    DEMO_FAILED: [] }

    print ("Comparing against file:  %s"%correct_file)
    print ("Putting results in directory:  %s"%html_dir)

    table = []
    if len(demo) == 0:
        top = BasicDocument()
        top.append(H(1,"Correct file %s"%correct_file))
        ok = Image("green_swatch.png",border=0)
        data_minor_error = Image("yellow_swatch.png",border=0)
        data_major_error = Image("orange_swatch.png",border=0)
        did_not_work = Image("red_swatch.png",border=0)
        dct = { DEMO_MINOR_DATA_ERROR: data_minor_error,
                DEMO_MAJOR_DATA_ERROR: data_major_error,
                DEMO_FAILED: did_not_work }
        for demo in demos:
            table_line = []
            i=0
            for fil in trial_file:
                if i%5 == 0:
                    table_line.append(demo)
                i = i + 1
                demofile = "%s_%s%d.html"%(os.path.basename(fil),
                                           demo.replace(os.sep,"_"),i)
                report = check_demo(demo,correct_file,fil,epsilon,abseps)
                if report[0]==DEMO_MINOR_DATA_ERROR:
                    report_list[report[0]].append((demo,)+report[2:])
                else:
                    report_list[report[0]].append(demo)
                if report[0]==DEMO_OK:
                    table_line.append(ok)
                else:
                    sub_page = BasicDocument()
                    sub_page.append(Pre(report[1]))
                    sub_page.write(os.path.join(html_dir,demofile))
                    table_line.append(Href(text=dct[report[0]],url=demofile))
            table.append(table_line)
        if len(html_dir) != 0:
            html_table =Table("Results of tests",width="30%",cell_padding="0")
            html_table.body = table

            i=0
            headings=[]
            for fil in trial_file:
                if i%5 == 0:
                    headings.append("Demo")
                i = i + 1
                headings.append(os.path.basename(os.path.dirname(fil))+"\n"
                                +os.path.basename(fil))
            html_table.heading = headings
            top.append(html_table)
            top.write("%s/index.html"%html_dir)
        dct = { DEMO_OK: "No errors found in:",
                DEMO_MINOR_DATA_ERROR: "Minor errors found in:",
                DEMO_MAJOR_DATA_ERROR: "Major errors found in:",
                DEMO_FAILED: "The following demos failed:" }
        for i in [DEMO_OK, DEMO_MINOR_DATA_ERROR,
                  DEMO_MAJOR_DATA_ERROR, DEMO_FAILED]:
            if len(report_list[i]) > 0:            
                if i==DEMO_MINOR_DATA_ERROR:
                    print(dct[i])
                    print("          Relative Error Absolute Error")
                    for lst in report_list[i]:
                        print("%-6s: %15.7e%15.7e"%(lst[0],lst[1],lst[2]))
                elif len(report_list[i]) > 0:
                    print(" ".join([dct[i]]+report_list[i]))
        print("Please point your web browser to the following URL "
              "for detailed results:\nfile:%s"%
              pathname2url(os.path.abspath("verification/index.html")))
    else:
        check_demo(demo,correct_file,trial_file,epsilon,abseps)

if __name__ == "__main__":
    opts_list,args = getopt.getopt(sys.argv[1:],"c:t:d:e:h:")
    epsilon = None
    demo = None
    html_dir = None
    correct_file = None
    trial_file = args
    for x in opts_list:
        if x[0]=="-a":
            abseps=float(x[1])
        if x[0]=="-c":
            correct_file=x[1]                         
        if x[0]=="-d":
            demo=x[1]
        if x[0]=="-e":
            epsilon=float(x[1])
        if x[0]=="-h":
            html_dir=x[1]
    parse(trial_file, correct_file, demo, epsilon, html_dir)






