source("bayesian function chart.R")
s.min = read.csv("https://raw.githubusercontent.com/Andersonof30/chart/main/serie%20minima%20atualizada%20(n%20200).csv", sep = " ")
plot.control(s.min$minima, type = 'unit', fase1 = .8, xlab = '', ylab = 'Umidade Relativa do Ar', 
             tit = ' ', size =  20, iter = 1000)
