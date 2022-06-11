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
                        xlab = 'a', ylab = 'b', tit = NULL, size = 10, 
                        n, L, ...){
  require(ggplot2)
  n1 = length(x)*fase1
  dados.f1 = data.frame(x[1:n1]); colnames(dados.f1) = 'val'
  dados = data.frame(x);colnames(dados) = 'val'
  sq = 1:length(x)
  if(type == 'unit'){
    ### UH
    md_uh = med.c(dados.f1$val)
    qi_uh = quhn(alpha/2,md_uh); qs_uh = quhn(1 - alpha/2, md_uh)
    
    p_uh = ggplot(data = dados) +
      geom_point(aes(x = sq, y = val), color = 
                   ifelse(dados$val < qi_uh | dados$val > qs_uh, 'green','black')) +
      geom_line(aes(x = sq, y = val)) +
      geom_line(aes(x = sq, y = qi_uh), color = 'red') +
      geom_line(aes(x = sq, y = md_uh), color = 'blue') +
      geom_line(aes(x = sq,y = qs_uh), color = 'red') +
      geom_vline(xintercept =  length(dados.f1$val), lty = 2, col = 'black') +
      theme_classic(base_size = size) +
      labs(x = xlab, y = ylab, subtitle = 'UHN', title = tit)
    plot(p_uh)
    vp_uh = ks.test(dados.f1$val, puhn, md_uh)$p.value
  
  #### beta  
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
      geom_vline(xintercept =  length(dados.f1$val), lty = 2, col = 'black') +
      theme_classic(base_size = size) +
      labs(x = xlab, y = ylab, subtitle ='beta', title = tit)
    plot(p_bt)
    vp_bt = ks.test(dados.f1$val, pbeta, bt_ap, bt_bt)$p.value
    
    km_sp = Rfast2::kumar.mle(dados.f1$val)$param['shape'];km_sc = Rfast2::kumar.mle(dados.f1$val)$param['scale']
    md_km = (km_sc*gamma(1 + 1/km_sp)*gamma(km_sc))/(gamma(1 + 1/km_sp + km_sc))
    qi_km = VGAM::qkumar(alpha/2, km_sp, km_sc); qs_km = VGAM::qkumar(1 - alpha/2, km_sp, km_sc)
    
    #### kumar
    p_km = ggplot(data = dados) +
      geom_point(aes(x = sq, y = val), color = 
                   ifelse(dados$val < qi_km | dados$val > qs_km, 'green','black')) +
      geom_line(aes(x = sq, y = val)) +
      geom_line(aes(x = sq, y = qi_km), color = 'red') +
      geom_line(aes(x = sq, y = md_km), color = 'blue') +
      geom_line(aes(x = sq,y = qs_km), color = 'red') +
      geom_vline(xintercept =  length(dados.f1$val), lty = 2, col = 'black') +
      theme_classic(base_size = size) +
      labs(x = xlab, y = ylab, subtitle = 'kumar', title = tit)
    plot(p_km)
    vp_km = ks.test(dados.f1$val, VGAM::pkumar, km_sp, km_sc)$p.value
    
    ##### UL
    md_ul = ul.mean.mlet(dados.f1$val)
    qi_ul = qUL(alpha/2, md_ul); qs_ul = qUL(1 - alpha/2, md_ul)
    p_ul = ggplot(data = dados) +
      geom_point(aes(x = sq, y = val), color = 
                   ifelse(dados$val < qi_ul | dados$val > qs_ul, 'green','black')) +
      geom_line(aes(x = sq, y = val)) +
      geom_line(aes(x = sq, y = qi_ul), color = 'red') +
      geom_line(aes(x = sq, y = md_ul), color = 'blue') +
      geom_line(aes(x = sq,y = qs_ul), color = 'red') +
      geom_vline(xintercept =  length(dados.f1$val), lty = 2, col = 'black') +
      theme_classic(base_size = size) +
      labs(x = xlab, y = ylab, subtitle = 'UL', title = tit)
    plot(p_ul)
    vp_ul = ks.test(dados.f1$val, pUL, md_ul)$p.value
    dt_pv = data.frame(c('UHN', 'beta', 'kumar', 'UL'),
                       round(c(vp_uh,vp_bt, vp_km, vp_ul), 4), 
                       c('p_uh', 'p_bt', 'p_km', 'p_ul'),
                       row.names = c('', ' ', '  ', '    '))
    colnames(dt_pv) = c('distribuição','p-valor', 'plot')
    dt_pv = dt_pv[order(dt_pv$`p-valor`, decreasing = T),]
    print(dt_pv[, 1:2])
    #lapply(dt_pv$plot, get)
  }
  else if (type == 'count'){
    Bell.Chart.TOTAL = function(y = dados$val, dados.f1 = dados.f1$val,
                                n = n, L = L){
      package = "LambertW"
      if(!require(package, character.only = T)){
        install.packages(package, dependencies = T)
      }
      require(LambertW)
      mu_bl = mean(dados.f1)/n
      UCL_bl = n*mu_bl+L*sqrt((n*mu_bl*(1+W(mu_bl))))
      CL_bl = n*mu_bl
      LCL_bl = n*mu_bl-L*sqrt((n*mu_bl*(1+W(mu_bl))))
      UCL_bl = rep(UCL_bl, length(y))
      CL_bl = rep(CL_bl, length(y))
      LCL_bl = rep(LCL_bl, length(y))
      p_bl = ggplot(data = dados) +
      geom_point(aes(x = sq, y = val), color = 
                   ifelse(dados$val < LCL_bl | dados$val > UCL_bl, 'green','black')) +
      geom_line(aes(x = sq, y = val)) +
      geom_line(aes(x = sq, y = LCL_bl), color = 'red') +
      geom_line(aes(x = sq, y = CL_bl), color = 'blue') +
      geom_line(aes(x = sq,y = UCL_bl), color = 'red') +
      geom_vline(xintercept =  length(dados.f1), lty = 2, col = 'black') +
      theme_classic(base_size = size) +
      labs(x = xlab, y = ylab, subtitle = 'Bell', title = tit)
      #out = matrix(round(c(UCL[1], CL[1], LCL[1]), 4), nrow=1, ncol=3)
      #rownames(out) = c(" ")
      #colnames(out) = c("UCL", "CL", "LCL")
      #out
      plot(p_bl)
    }
    Bell.Chart.TOTAL(y = dados$val,dados.f1 = dados.f1$val,n,L = L)
  }
}


# Tentei utilizar a função get para plotar na ordem dos p-valores, não
# deu certo mas farei outras tentativas 

x = c(16,18,12,15,24,21,28,20,25,19,18,21,16,22,19,12,14,9,16,21,27,27,
      34,31,19,29,17,35,20,18,12,34,13,25,23,24,20,27,16,28)
plot.control(x, type = 'count', fase1 = 0.8, xlab = 'Amostra', ylab = '', 
             tit = 'Titulo aqui', size =  20,n = 100, L =3)

x = quhn(runif(50), .2)
plot.control(x, type = 'unit', fase1 = 0.8, xlab = 'Amostra', ylab = 'Taxa (%)', 
             tit = 'Titulo aqui', size =  20)
