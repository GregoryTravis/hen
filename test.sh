mzscheme -j -r tst.ss | square-brackets backwards
exit

mkdir -p sb_tmp
rm -f sb_tmp/tmp_*.ss
square-brackets forwards *.ss

mzscheme -j -r sb_tmp/tmp_tst.ss | square-brackets backwards

#rm -f sb_tmp/tmp_*.ss
