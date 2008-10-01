#!/usr/bin/env auto15x
#======================================================================
#======================================================================
#         Utility Program for Relabeling Labeled Solutions
#                        in AUTO Data Files
#======================================================================
#======================================================================

import parseS,sys

def relabel(n1b,n1s,n2b,n2s):
    s = parseS.parseS(n1s)
    labels = s.getLabels()
    s.uniquelyLabel()
    s.writeFilename(n2s)
    inf = open(n1b, "rb")
    outf = open(n2b, "wb")
    l = 0
    lnum = 0
    lines = inf
    if not hasattr(lines,"next"):
        import AUTOutil
        lines = AUTOutil.myreadlines(inf)
    if hasattr(str,"split"):
        split = str.split
        find = str.find
        lstrip = str.lstrip
    else:
        import string
        split = string.split
        find = string.find
        lstrip = string.lstrip
    for line in lines:
        lnum = lnum + 1
        a = split(line)
        if a[0] != "0" and a[3] != "0":
            lab = int(a[3])
            if l > len(labels) or lab != labels[l]:
                sys.stdout.write("%s\n%s%d%s%d\n%s%d\n%s\n%s"%(
                       " WARNING : The two files have incompatible labels :",
		       "  b-file label ", lab, " at line ", lnum,
		       "  s-file label ", labels[l],
		       " New labels may be assigned incorrectly.",
		       " Continue ? : "))
                ch = raw_input()
                if ch != 'y' and ch != 'Y':
                    print "Rewrite discontinued. Recovering original files"
                    inf.close()
                    outf.close()
                    import os
                    os.remove(n2b)
                    return
            l = l + 1
            linelen = len(line)
            newsp = 0
            for i in range(4):
                oldsp = newsp
                newsp = find(line,' ',linelen - len(lstrip(line[oldsp:])))
            line = line[:oldsp] + ('%'+str(newsp-oldsp)+'d')%l + line[newsp:]
        outf.write(line)
    inf.close()
    outf.close()

if __name__ == '__main__':
    if len(sys.argv) == 5:
	inb = sys.argv[1]
	ins = sys.argv[2]
	outb = sys.argv[3]
	outs = sys.argv[4]
    else:
	inb = "fort.27"
	ins = "fort.28"
	outb = "fort.37"
	outs = "fort.38"
    relabel(inb,ins,outb,outs)
