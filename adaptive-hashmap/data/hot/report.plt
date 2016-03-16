set terminal postscript eps enhanced color font 'Helvetica,20'
set output "report.eps"
set format y "%g"
set grid xtics
set grid ytics
set grid ztics
set xlabel "Threads"
set xtics 1
set ylabel "Parallel speedup"
set yrange [0:8]
plot "report/curve0.csv" using 1:(0.10832136124372482/$2) with errorlines linewidth 1.5 pointtype 4 pointsize 2.0 title "pure", "report/curve1.csv" using 1:(9.654630348086357e-2/$2) with errorlines linewidth 1.5 pointtype 7 pointsize 2.0 title "ctrie", "report/curve2.csv" using 1:(0.10244127362966537/$2) with errorlines linewidth 1.5 pointtype 8 pointsize 2.0 title "adaptive"
