#!/usr/bin/env python
#======================================================================
#======================================================================
#         Utility Program for Relabeling Labeled Solutions
#                        in AUTO Data Files
#======================================================================
#======================================================================

import bifDiag,sys

def relabel(n1b,n1s,n2b,n2s):
    bd = bifDiag.bifDiag(n1b,n1s)
    labels = bd().getLabels()
    l = 0
    for lab in bd.getLabels():
        lnum = 1
        if l > len(labels) or lab != labels[l]:
            k = 0
            for br in bd:
                lnum = lnum + len(br.headerlist)
                k = k + len(br.getLabels())
                if k > l:
                    break
                lnum = lnum + len(br)
            lnum = lnum + br(lab)["index"]
            sys.stdout.write("%s\n%s%d%s%d\n%s%d\n%s\n%s"%(
                    " WARNING : The two files have incompatible labels :",
                    "  b-file label ", lab, " at line ", lnum,
                    "  s-file label ", labels[l],
                    " New labels may be assigned incorrectly.",
                    " Continue ? : "))
            ch = raw_input()
            if ch != 'y' and ch != 'Y':
                print("Rewrite discontinued. Recovering original files")
                import os
                try:
                    os.remove(n2b)
                    os.remove(n2s)
                except:
                    pass
                return
        l = l + 1
    bd.uniquelyLabel()
    bd.writeFilename(n2b,n2s)

if __name__ == '__main__':
    import os
    sys.path.append(os.path.join(os.environ["AUTO_DIR"],"python"))
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
