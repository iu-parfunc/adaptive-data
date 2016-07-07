set terminal postscript eps enhanced color font 'Helvetica,20'
set output "report.eps"
set format y "%g"
set grid xtics
set grid ytics
set grid ztics
set xlabel "Threads"
set xtics 1
set ylabel "Factor speedup (logscale)"
set logscale y 2
set key bottom right

plot "report/curve1.csv" using 1:(3.8698415299877524/$2) with errorlines linewidth 1.5 pointtype 4 pointsize 2.0 title "pure", "report/curve2.csv" using 1:(3.8698415299877524/$2) with errorlines linewidth 1.5 pointtype 7 pointsize 2.0 title "ctrie", "report/curve0.csv" using 1:(3.8698415299877524/$2) with errorlines linewidth 1.5 pointtype 8 pointsize 2.0 title "adaptive"
