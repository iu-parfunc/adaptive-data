set datafile separator ","
set terminal postscript eps enhanced color font 'Helvetica,20'
set output 'report.eps'
#set yrange [0:1]

plot 'report_ctrie.csv' using 1:2 title 'ctrie' \
     with linespoints, \
     'report_adaptive.csv' using 1:2 title 'adaptive'\
     with linespoints

exit
