set terminal svg
set output "report.svg"
set format y "%g"
set grid xtics
set grid ytics
set grid ztics
set title "cold"
set xlabel "Threads"
set xtics 1
set ylabel "Time in seconds"
plot "report/curve0.csv" using 1:2 with errorlines linewidth 1.5 pointtype 4 pointsize 1.0 title "pure", "report/curve1.csv" using 1:2 with errorlines linewidth 1.5 pointtype 7 pointsize 1.0 title "ctrie", "report/curve2.csv" using 1:2 with errorlines linewidth 1.5 pointtype 8 pointsize 1.0 title "adaptive"
