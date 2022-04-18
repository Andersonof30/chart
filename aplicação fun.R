require(ggplot2)

dados = runif(100)
plot.control = function(dados, type = 'nome', fase1 = 0.7,alpha = .1, ...){
  require(ggplot2)
  n1 = length(dados)*.7 
  dados.f1 = data.frame(dados[1:n1]); colnames(dados.f1) = 'val'
  sq = 1:n1
  if(type == 'unit'){
    
    md_uh = med.c(dados.f1$val)
    qi_uh = quhn(alpha/2,md_uh); qs_uh = quhn(1 - alpha/2, md_uh)
    
    p_uh = ggplot(data = dados.f1) +
      geom_point(aes(x = sq, y = val), color = 
                   ifelse(dados.f1$val < qi_uh | dados.f1$val > qs_uh, 'green','black')) +
      geom_line(aes(x = sq, y = val)) +
      geom_line(aes(x = sq, y = qi_uh), color = 'red') +
      geom_line(aes(x = sq, y = md_uh), color = 'blue') +
      geom_line(aes(x = sq,y = qs_uh), color = 'red') +
      theme_minimal() +
      labs(x = 'amostra', y = 'valores', title = 'UHN')
    plot(p_uh)
    
    bt_ap = Rfast::beta.mle(dados.f1$val)$param['alpha'];bt_bt = Rfast::beta.mle(dados.f1$val)$param['beta']
    md_bt = bt_ap/(bt_ap + bt_bt)
    qi_bt = qbeta(alpha/2, bt_ap, bt_bt); qs_bt = qbeta( 1- alpha/2, bt_ap, bt_bt)
    
    p_bt = ggplot(data = dados.f1) +
      geom_point(aes(x = sq, y = val), color = 
                   ifelse(dados.f1$val < qi_bt | dados.f1$val > qs_bt, 'green','black')) +
      geom_line(aes(x = sq, y = val)) +
      geom_line(aes(x = sq, y = qi_bt), color = 'red') +
      geom_line(aes(x = sq, y = md_bt), color = 'blue') +
      geom_line(aes(x = sq,y = qs_bt), color = 'red') +
      theme_minimal() +
      labs(x = 'amostra', y = 'valores', title = 'beta')
    plot(p_bt)
    
    km_sp = Rfast2::kumar.mle(dados.f1$val)$param['shape'];km_sc = Rfast2::kumar.mle(dados.f1$val)$param['scale']
    md_km = (km_sc*gamma(1 + 1/km_sp)*gamma(km_sc))/(gamma(1 + 1/km_sp + km_sc))
    qi_km = VGAM::qkumar(alpha/2, km_sp, km_sc); qs_km = VGAM::qkumar(1 - alpha/2, km_sp, km_sc)
    
    p_km = ggplot(data = dados.f1) +
      geom_point(aes(x = sq, y = val), color = 
                   ifelse(dados.f1$val < qi_km | dados.f1$val > qs_km, 'green','black')) +
      geom_line(aes(x = sq, y = val)) +
      geom_line(aes(x = sq, y = qi_km), color = 'red') +
      geom_line(aes(x = sq, y = md_km), color = 'blue') +
      geom_line(aes(x = sq,y = qs_km), color = 'red') +
      theme_minimal() +
      labs(x = 'amostra', y = 'valores', title = 'kumar')
    plot(p_km)
    
    md_ul = ul.mean.mlet(dados.f1$val)
    qi_ul = qUL(alpha/2, md_ul); qs_ul = qUL(1 - alpha/2, md_ul)
    p_ul = ggplot(data = dados.f1) +
      geom_point(aes(x = sq, y = val), color = 
                   ifelse(dados.f1$val < qi_ul | dados.f1$val > qs_ul, 'green','black')) +
      geom_line(aes(x = sq, y = val)) +
      geom_line(aes(x = sq, y = qi_ul), color = 'red') +
      geom_line(aes(x = sq, y = md_ul), color = 'blue') +
      geom_line(aes(x = sq,y = qs_ul), color = 'red') +
      theme_minimal() +
      labs(x = 'amostra', y = 'valores', title = 'UL')
    plot(p_ul)
  
  }
  else if (type == 'count'){
  }
}

plot.control(dados, type = 'unit')

