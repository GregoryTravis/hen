rigg vork
exit

./hum 2> errs
cat errs | grep -v "Unparsed data" | grep -v binfo | grep -v "at /Library/Perl"
exit

hen src.ss
