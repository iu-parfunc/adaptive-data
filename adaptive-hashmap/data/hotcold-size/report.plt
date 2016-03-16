set terminal postscript eps enhanced color font 'Helvetica,20'
set output "report.eps"
set format y "%g"
set grid xtics
set grid ytics
set grid ztics
set xlabel "Operations"
set ylabel "Time in seconds"
set yrange [0:4]
plot "report/curve0.csv" using 1:2 with errorlines linewidth 1.5 pointtype 4 pointsize 1.0 title "pure", "report/curve1.csv" using 1:2 with errorlines linewidth 1.5 pointtype 7 pointsize 1.0 title "ctrie", "report/curve2.csv" using 1:2 with errorlines linewidth 1.5 pointtype 8 pointsize 1.0 title "adaptive"
