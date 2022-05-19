#função quantil
qUL = function (p, mi){
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

require(ggplot2)


plot.control = function(x, type = 'nome', fase1 = 0.7,alpha = .1,
                        xlab = 'a', ylab = 'b', tit = NULL, size = 10, ...){
  require(ggplot2)
  n1 = length(x)*fase1
  dados.f1 = data.frame(x[1:n1]); colnames(dados.f1) = 'val'
  dados = data.frame(x);colnames(dados) = 'val'
  sq = 1:length(x)
  if(type == 'unit'){
    
    md_uh = med.c(dados.f1$val)
    qi_uh = quhn(alpha/2,md_uh); qs_uh = quhn(1 - alpha/2, md_uh)
    
    p_uh = ggplot(data = dados) +
      geom_point(aes(x = sq, y = val), color = 
                   ifelse(dados$val < qi_uh | dados$val > qs_uh, 'green','black')) +
      geom_line(aes(x = sq, y = val)) +
      geom_line(aes(x = sq, y = qi_uh), color = 'red') +
      geom_line(aes(x = sq, y = md_uh), color = 'blue') +
      geom_line(aes(x = sq,y = qs_uh), color = 'red') +
      theme_classic(base_size = size) +
      labs(x = xlab, y = ylab, subtitle = 'UHN', title = tit)
    plot(p_uh)
    vp_uh = ks.test(dados.f1$val, puhn, md_uh)$p.value
    
    bt_ap = Rfast::beta.mle(dados.f1$val)$param['alpha'];bt_bt = Rfast::beta.mle(dados.f1$val)$param['beta']
    md_bt = bt_ap/(bt_ap + bt_bt)
    qi_bt = qbeta(alpha/2, bt_ap, bt_bt); qs_bt = qbeta( 1- alpha/2, bt_ap, bt_bt)
    
    p_bt = ggplot(data = dados) +
      geom_point(aes(x = sq, y = val), color = 
                   ifelse(dados$val < qi_bt | dados$val > qs_bt, 'green','black')) +
      geom_line(aes(x = sq, y = val)) +
      geom_line(aes(x = sq, y = qi_bt), color = 'red') +
      geom_line(aes(x = sq, y = md_bt), color = 'blue') +
      geom_line(aes(x = sq,y = qs_bt), color = 'red') +
      theme_classic(base_size = size) +
      labs(x = xlab, y = ylab, subtitle ='beta', title = tit)
    plot(p_bt)
    vp_bt = ks.test(dados.f1$val, pbeta, bt_ap, bt_bt)$p.value
    
    km_sp = Rfast2::kumar.mle(dados.f1$val)$param['shape'];km_sc = Rfast2::kumar.mle(dados.f1$val)$param['scale']
    md_km = (km_sc*gamma(1 + 1/km_sp)*gamma(km_sc))/(gamma(1 + 1/km_sp + km_sc))
    qi_km = VGAM::qkumar(alpha/2, km_sp, km_sc); qs_km = VGAM::qkumar(1 - alpha/2, km_sp, km_sc)
    
    p_km = ggplot(data = dados) +
      geom_point(aes(x = sq, y = val), color = 
                   ifelse(dados$val < qi_km | dados$val > qs_km, 'green','black')) +
      geom_line(aes(x = sq, y = val)) +
      geom_line(aes(x = sq, y = qi_km), color = 'red') +
      geom_line(aes(x = sq, y = md_km), color = 'blue') +
      geom_line(aes(x = sq,y = qs_km), color = 'red') +
      #geom_vline(xintercept = n1, color = 'black', linetype = 2, size = 1)+
      theme_classic(base_size = size) +
      labs(x = xlab, y = ylab, subtitle = 'kumar', title = tit)
    plot(p_km)
    vp_km = ks.test(dados.f1$val, VGAM::pkumar, km_sp, km_sc)$p.value
    
    md_ul = ul.mean.mlet(dados.f1$val)
    qi_ul = qUL(alpha/2, md_ul); qs_ul = qUL(1 - alpha/2, md_ul)
    p_ul = ggplot(data = dados) +
      geom_point(aes(x = sq, y = val), color = 
                   ifelse(dados$val < qi_ul | dados$val > qs_ul, 'green','black')) +
      geom_line(aes(x = sq, y = val)) +
      geom_line(aes(x = sq, y = qi_ul), color = 'red') +
      geom_line(aes(x = sq, y = md_ul), color = 'blue') +
      geom_line(aes(x = sq,y = qs_ul), color = 'red') +
      theme_classic(base_size = size) +
      labs(x = xlab, y = ylab, subtitle = 'UL', title = tit)
    plot(p_ul)
    vp_ul = ks.test(dados.f1$val, pUL, md_ul)$p.value
    dt_pv = data.frame(c('UHN', 'beta', 'kumar', 'UL'),
                       round(c(vp_uh,vp_bt, vp_km, vp_ul), 4), 
                       c('p_uh', 'p_bt', 'p_km', 'p_ul'),
                       row.names = c('', ' ', '  ', '    '))
    colnames(dt_pv) = c('distribuição','p-valor', 'plot')
    dt_pv = dt_pv[order(dt_pv$`p-valor`, decreasing = F),]
    print(dt_pv[, 1:2])
    #lapply(dt_pv$plot, get)
  }
  else if (type == 'count'){
  }
}


# Tentei utilizar a função get para plotar na ordem dos p-valores, não
# deu certo mas farei outras tentativas 


x = quhn(runif(30), .5)
plot.control(x, type = 'unit', xlab = 'Amostra', ylab = 'Taxa (%)', tit = 'Titulo aqui', size =  20)

