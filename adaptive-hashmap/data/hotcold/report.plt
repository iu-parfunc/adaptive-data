set terminal postscript eps enhanced color font 'Helvetica,20'
set output "report.eps"
set format y "%g"
set grid xtics
set grid ytics
set grid ztics
set xlabel "Threads"
set xtics 1
set ylabel "Parallel speedup over CTrie/1-thread"
# set logscale x
set logscale y 2
set key bottom right

plot "report/curve1.csv" using 1:(4.484724368900061/$2) with errorlines linewidth 4 pointtype 4 pointsize 3.0 title "pure", "report/curve2.csv" using 1:(4.484724368900061/$2) with errorlines linewidth 4 pointtype 7 pointsize 3.0 title "ctrie", "report/curve0.csv" using 1:(4.484724368900061/$2) with errorlines linewidth 4 pointtype 8 pointsize 3.0 title "adaptive"
