mzscheme -j -r main.ss
exit

#cat src.ss | square-brackets forwards | tee sf
#echo "===="
#cat sf | square-brackets backwards | tee bs

#prog=main.ss
#tmp=tmp_$prog

mkdir -p sb_tmp
rm -f sb_tmp/tmp_*.ss
square-brackets forwards *.ss

#cat $prog |  > $tmp
mzscheme -j -r sb_tmp/tmp_main.ss | square-brackets backwards

#rm -f sb_tmp/tmp_*.ss
