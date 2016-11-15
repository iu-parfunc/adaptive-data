set terminal postscript eps enhanced color font 'Helvetica,20'
set output "report.eps"
set grid xtics
set grid ytics
set xlabel "Threads"
set xtics 1
set ytics nomirror
set ylabel "Time in seconds for cold operations (logscale)"
set y2tics nomirror
set y2label "Time in seconds for hot operations (logscale)"
set logscale y
set logscale y2
set key bottom left samplen 2 font ",16" nobox width -4
plot "../cold/report/curve0.csv" using 1:2 \
     with errorlines linewidth 1.5 linecolor 1 pointtype 4 pointsize 2.0 \
     title "pure (cold)" axes x1y1, \
     "../cold/report/curve2.csv" using 1:2 \
     with errorlines linewidth 1.5 linecolor 2 pointtype 6 pointsize 2.0 \
     title "ctrie (cold)" axes x1y1, \
     "../cold/report/curve3.csv" using 1:2 \
     with errorlines linewidth 1.5 linecolor 3 pointtype 8 pointsize 2.0 \
     title "adaptive (cold)" axes x1y1, \
     "../hot/report/curve0.csv" using 1:2 \
     with errorlines linewidth 1.5 linecolor 1 pointtype 5 pointsize 2.0 \
     title "pure (hot)" axes x1y2, \
     "../hot/report/curve2.csv" using 1:2 \
     with errorlines linewidth 1.5 linecolor 2 pointtype 7 pointsize 2.0 \
     title "ctrie (hot)" axes x1y2, \
     "../hot/report/curve3.csv" using 1:2 \
     with errorlines linewidth 1.5 linecolor 3 pointtype 9 pointsize 2.0 \
     title "adaptive (hot)" axes x1y2
