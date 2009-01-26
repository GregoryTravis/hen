hen src.ss
exit

rm -f butt.c.impl* butt.c.stub*
rm -f butt.c.t00.tu
g++ -c -fdump-translation-unit butt.c
rigg butt.c
gcc -c butt_c.impl.c
ls -l butt_c.impl.*
exit

./hum 2> errs
cat errs | grep -v "Unparsed data" | grep -v binfo | grep -v "at /Library/Perl"
exit

hen src.ss
