set terminal postscript eps enhanced color font 'Helvetica,20'
set output "report.eps"
set format y "%g"
set grid xtics
set grid ytics
set grid ztics
set xlabel "Threads"
set xtics 1
set ylabel "Parallel speedup"
plot "report/curve0.csv" using 1:(19.733493082225323/$2) with errorlines linewidth 1.5 pointtype 4 pointsize 2.0 title "pure", "report/curve1.csv" using 1:(46.252296874299645/$2) with errorlines linewidth 1.5 pointtype 7 pointsize 2.0 title "ctrie", "report/curve2.csv" using 1:(19.735413257032633/$2) with errorlines linewidth 1.5 pointtype 8 pointsize 2.0 title "adaptive"
