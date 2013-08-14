# gnuplot < plot.gp

set terminal png enhanced size 800,600
set output "benchmark.png"

set title "Benchmarking 1 million searches"

set grid
set key autotitles
set style data linespoints
set xlabel "Needle length"
set ylabel "Duration in seconds"

#     'benchmark.log' i 3 t columnheader(1),\

plot 'benchmark.log' i 0 t columnheader(1),\
     'benchmark.log' i 1 t columnheader(1),\
     'benchmark.log' i 2 t columnheader(1),\
     'benchmark.log' i 3 t columnheader(1),\
     'benchmark.log' i 5 t columnheader(1),\
     'benchmark.log' i 6 t columnheader(1),\
     'benchmark.log' i 7 t columnheader(1),\
     'benchmark.log' i 8 t columnheader(1)
     

#     'benchmark.log' i 8 t columnheader(1),\
#     'benchmark.log' i 9 t columnheader(1)


# this feature is present in gnuplot 4.6
# plot for [IDX=0:1] 'benchmark.log' i IDX using 1:2 with lines title columnheader(1)