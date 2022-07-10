source("bayesian function chart.R")
s.min = read.csv("https://raw.githubusercontent.com/Andersonof30/chart/main/serie%20da%20minima%20da%20UR.csv", sep = " ")
plot.control(s.min$minima, type = 'unit', fase1 = .8, xlab = '', ylab = 'Umidade Relativa do Ar', 
             tit = ' ', size =  20, iter = 1000)
