set datafile separator ","
set term png size 1920,1080
set output filename.'.png'
set xrange [1:ncore]
set xlabel 'Thread number'
set ylabel 'Ops/ms'
plot filename.'_nop.csv' using 1:2:3 title 'NOP' with yerrorlines, \
     filename.'_pure.csv' using 1:2:3 title 'Compact Pure' with yerrorlines, \
     filename.'_cpure.csv' using 1:2:3 title 'Pure' with yerrorlines, \
     filename.'_ctrie.csv' using 1:2:3 title 'Ctrie' with yerrorlines, \
     filename.'_adaptive.csv' using 1:2:3 title 'Adaptive' with yerrorlines

set output filename.'_no_nop.png'
plot filename.'_pure.csv' using 1:2:3 title 'Pure' with yerrorlines, \
     filename.'_cpure.csv' using 1:2:3 title 'Compact Pure' with yerrorlines, \
     filename.'_ctrie.csv' using 1:2:3 title 'Ctrie' with yerrorlines, \
     filename.'_adaptive.csv' using 1:2:3 title 'Adaptive' with yerrorlines

exit
