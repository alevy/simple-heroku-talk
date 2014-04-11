set terminal svg size 800,400 fname 'Verdana' fsize 10
set output 'performance.svg'

set style data histogram
set style histogram cluster gap 1
set grid noxtics ytics
set boxwidth 0.9

set style fill solid border rgb "black"
black = "#000000"; blue = "#D7191C"; green = "#ABDDA4"; yellow ="#FFFFBF"

#JAVA JETTY
#set label "82,275 R/S"   at -0.22,1.05 font "Cantarell, 16"
#set label "618 R/S"     at 0.82 ,1.05 font "Cantarell, 16"
#set label "23,118 R/S"   at 1.52 ,1.05 font "Helvetica, 16"
#set label "9,9434 R/S"   at 2.82 ,1.05 font "Helvetica, 16"
#HAILS
#set label "47,577 R/S"   at -0.66,0.63 font "Helvetica, 16"
#set label "479 R/S"    at 0.45 ,0.83 font "Helvetica, 16"
#set label "1,140 R/S"   at 1.42 ,0.10 font "Helvetica, 16"
#set label "1,370 R/S"   at 2.42 ,0.17 font "Helvetica, 16"


set border 3
set xtics font "Cantarell, 26" 
#set key box font "Helvetica, 18" spacing 2.5 at 1,10000
set nokey
set yrange [0:]
set ytics font "Cantarell, 12"
plot 'performance.csv' using 2:xtic(1) title col lc rgb blue;
