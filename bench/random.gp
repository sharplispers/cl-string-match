## plot the results of the random strings search

# gnuplot < random.gp

set terminal png enhanced size 800,600
set output "random.png"

set title "Benchmarking search on random strings"

set grid
set key autotitles
set style data linespoints
set xlabel "Needle length"
set ylabel "Duration index (search=1)"

set yrange [0:1.2]

plot 'random.log' using 1:($2/$2) title "search",\
     'random.log' using 1:($3/$2) title "brute-force",\
     'random.log' using 1:($4/$2) title "Boyer-Moore",\
     'random.log' using 1:($5/$2) title "Boyer-Moore-Horspool",\
     'random.log' using 1:($6/$2) title "Boyer-Moore-Horspool-8",\
     'random.log' using 1:($7/$2) title "Rabin-Karp",\
     'random.log' using 1:($8/$2) title "Knuth-Morris-Pratt"
