#!/usr/local/bin/gnuplot

set datafile separator ","
set term png size 1920,1080
set output filename.'.png'
set xrange [1:ncore]
plot filename.'_nop.csv' using 1:2:3 title 'NOP' with yerrorlines, \
     filename.'_pure.csv' using 1:2:3 title 'Pure' with yerrorlines, \
     filename.'_ctrie.csv' using 1:2:3 title 'Ctrie' with yerrorlines

set output filename.'_no_nop.png'
plot filename.'_pure.csv' using 1:2:3 title 'Pure' with yerrorlines, \
     filename.'_ctrie.csv' using 1:2:3 title 'Ctrie' with yerrorlines

exit
