file=main.ss
#file=imain.ss

hen $file 2>&1 | tee /tmp/main.out
#diff -q /tmp/main.out /tmp/correct.main.out
#cp /tmp/main.out /tmp/correct.main.out

#mzscheme -j -r main.ss | square-brackets backwards
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
