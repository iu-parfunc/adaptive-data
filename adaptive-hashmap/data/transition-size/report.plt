set terminal postscript eps enhanced color font 'Helvetica,20'
set output "report.eps"
set format y "%g"
set grid xtics
set grid ytics
set grid ztics
set xlabel "Size"
set ylabel "Time in seconds"
plot "report/curve0.csv" using 1:2 with errorlines title "adaptive", "report/curve1.csv" using 1:2 with errorlines title "c-adaptive"
