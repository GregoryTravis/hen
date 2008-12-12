rm -f obj.i
mzscheme -r main.ss
rm -f vor ; make vor ; vor ; rm -rf vor.dSYM/ obj.i
