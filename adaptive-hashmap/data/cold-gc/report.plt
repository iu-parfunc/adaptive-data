set terminal postscript eps enhanced color font 'Helvetica,20'
set output "report.eps"
set format x "%.0tx10^%T"
set format y "%.1tx10^%T"
set grid xtics
set grid ytics
set grid ztics
set key top left
set xlabel "Ops"
set ylabel "Bytes copied during GC"
plot "report/curve0.csv" using 1:2 with errorlines linewidth 1.5 pointtype 4 pointsize 2.0 title "nop", "report/curve1.csv" using 1:2 with errorlines linewidth 1.5 pointtype 7 pointsize 2.0 title "pure", "report/curve2.csv" using 1:2 with errorlines linewidth 1.5 pointtype 8 pointsize 2.0 title "compact"
