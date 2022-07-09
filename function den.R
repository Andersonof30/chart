#função quantil
qUL = function (p, mi){
  package1 = "pracma"
  if(!require(package1, character.only = T)){
    install.packages(package1, dependencies = T)
  }
  a = 1/mi - 1
  
  n = pracma::lambertWn ( exp(-(1 + a))*(p-1)*(1 + a)) + 1 + a
  
  D = pracma::lambertWn ( exp(-(1 + a))*(p-1)*(1 + a)) + 1
  
  nd = n/D
  return(nd)
}

#estimador não viesado 
ul.mean.mlet = function(x){
  tx = function(x){
    return(sum(x/(1 - x)))
  }
  
  theta = 1/(2*tx(x))* (length(x) - tx(x) + sqrt(tx(x)^2 + 6*length(x)*tx(x) + length(x)^2))
  
  t_t = theta - (theta^5 + 7*theta^4 + 12*theta^3 + 8*theta^2 + 2*theta)/(((theta^2 + 4*theta + 2)^2)*length(x))
  return(1/(1+t_t))
}

pUL = function(x, mi){
  a = 1/mi - 1
  y =  1-(1- (a*x)/((a + 1)*(x - 1)))*exp(-a*x/(1-x))
  return(y)
  
}

quhn = function(p,t){
  ct = qnorm(.75)
  s = t/(ct*(1 - t))
  num = s*qnorm((p+1)/2)
  dem = 1 + s*qnorm((p+1)/2)
  r = num/dem
  return(r)
}

med.c = function(x){
  s = sqrt(mean((x/(1 - x))^2))
  ct = qnorm(.75)
  t = s*ct/(1 + s*ct) 
  return(t)
}

puhn = function(x,t){
  ct = qnorm(.75)
  s = t/(ct*(1 - t))
  r = 2*pnorm(x/(s*(1 -x))) - 1
  return(r)
}


