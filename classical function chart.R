source('support functions.R')

plot.control = function(x, type = c('unit', 'count'), fase1 = 0.7,alpha = .1,
                        xlab = 'a', ylab = 'b', tit = NULL, size = 1000, 
                        L = 3, ...){
  
# instalando pacotes 
###########################

  package2 = "ggplot2"
  if(!require(package2, character.only = T)){
    install.packages(package2, dependencies = T)
  }
  require(ggplot2)
  
  package3 = "Rfast"
  if(!require(package3, character.only = T)){
    install.packages(package3, dependencies = T)
  }
  
  package4 = "Rfast2"
  if(!require(package4, character.only = T)){
    install.packages(package4, dependencies = T)
  }

  package5 = "VGAM"
  if(!require(package5, character.only = T)){
    install.packages(package5, dependencies = T)
  }

###########################
  
  
  
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
      geom_vline(xintercept =  length(dados.f1$val) + 0.5, lty = 2, col = 'black') +
      theme_classic(base_size = size) +
      labs(x = xlab, y = ylab, subtitle = 'unit-half-normal', title = tit)
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
      geom_vline(xintercept =  length(dados.f1$val) + 0.5, lty = 2, col = 'black') +
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
      geom_vline(xintercept =  length(dados.f1$val) + 0.5, lty = 2, col = 'black') +
      theme_classic(base_size = size) +
      labs(x = xlab, y = ylab, subtitle = 'kumaraswamy', title = tit)
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
      geom_vline(xintercept =  length(dados.f1$val) + 0.5, lty = 2, col = 'black') +
      theme_classic(base_size = size) +
      labs(x = xlab, y = ylab, subtitle = 'unit-Lindley', title = tit)
    plot(p_ul)
    vp_ul = ks.test(dados.f1$val, pUL, md_ul)$p.value
    dt_pv = data.frame(c('UHN', 'beta', 'kumar', 'UL'),
                       round(c(vp_uh,vp_bt, vp_km, vp_ul), 4), 
                       c('p_uh', 'p_bt', 'p_km', 'p_ul'),
                       row.names = c('', ' ', '  ', '    '))
    colnames(dt_pv) = c('Distribuição','P-valor', 'plot')
    dt_pv = dt_pv[order(dt_pv$`P-valor`, decreasing = T),]
    print(dt_pv[, 1:2])
    #lapply(dt_pv$plot, get)
  }
  else if (type == 'count'){

    PSu.c.chart = function(dados, samples1, L){
      h.mu = function(mu, log=F){
        p1 = 3*sqrt(3)*sqrt(7*mu^4+28*mu^3+171*mu^2+32*mu+5)*mu
        p2 = 8*mu^3+66*mu^2+6*mu+1
        p = (p1+p2)^(1/3)
        theta = log(p/mu-((mu-1)*(5*mu+1))/(mu*p)-(mu-1)/mu)-log(3)
        if (log) theta else exp(theta)
      }
      
      y.BAR = sapply(samples1, mean)
      y.TOT = sapply(samples1, sum)
      n = length(sq)
      mu = mean(y.BAR)/n
      a = h.mu(mu)
      desvio = sqrt(n*(a^5+4*a^4+14*a^3+28*a^2+24*a+12)/(a^2*(a^2+a+2)^2))
      UCL_su = n*mu+L*desvio
      CL_su = n*mu
      LCL_su = n*mu-L*desvio
      p_su = ggplot(data = dados) +
      geom_point(aes(x = sq, y = val), color = 
                   ifelse(dados$val < LCL_su | dados$val > UCL_su, 'green','black')) +
        geom_line(aes(x = sq, y = val)) +
        geom_line(aes(x = sq, y = LCL_su), color = 'red') +
        geom_line(aes(x = sq, y = CL_su), color = 'blue') +
        geom_line(aes(x = sq,y = UCL_su), color = 'red') +
        geom_vline(xintercept =  length(samples1) + 0.5, lty = 2, col = 'black') +
        theme_classic(base_size = size) +
        labs(x = xlab, y = ylab, subtitle = 'Poisson-Sujatha', title = tit)
     plot( p_su)
    }
    
    
    #Pois L
    PL.c.chart = function(dados, samples1, L){
      y.BAR = sapply(samples1, mean)
      y.TOT = sapply(samples1, sum)
      n = length(sq)
      mu = mean(y.BAR)/n
      a = -(1/(2*mu))*((mu-1)-sqrt(((mu-1)^2)+8*mu))
      desvio = sqrt(n*(a^3+(4*a^2)+6*a+2)/((a^2)*((a+1)^2)))
      UCL_pl = n*mu+L*desvio
      CL_pl = n*mu
      LCL_pl = n*mu-L*desvio
      p_pl = ggplot(data = dados) +
        geom_point(aes(x = sq, y = val), color = 
                     ifelse(dados$val < LCL_pl | dados$val > UCL_pl, 'green','black')) +
        geom_line(aes(x = sq, y = val)) +
        geom_line(aes(x = sq, y = LCL_pl), color = 'red') +
        geom_line(aes(x = sq, y = CL_pl), color = 'blue') +
        geom_line(aes(x = sq,y = UCL_pl), color = 'red') +
        geom_vline(xintercept =  length(samples1) + 0.5, lty = 2, col = 'black') +
        theme_classic(base_size = size) +
        labs(x = xlab, y = ylab, subtitle = 'Poisson-Lindley', title = tit)
      plot(p_pl)
    
    }
    
    PSh.c.chart = function(dados, samples1, L){
      y.BAR = sapply(samples1, mean)
      y.TOT = sapply(samples1, sum)
      n = length(sq)
      mu = mean(y.BAR)/n
      s.mu = (12*mu*sqrt(3*(4*mu^4+71*mu^2+8))+180*mu^2+8)^(1/3)
      a = (1/(3*mu))*(1+s.mu/2-2*(3*mu^2-1)/s.mu)
      desvio = sqrt(n*((a^5+a^4+3*a^3+4*a^2+2*a+2)/(a^2*((a^2+1)^2))))
      UCL_sh = n*mu+L*desvio
      CL_sh = n*mu
      LCL_sh = n*mu-L*desvio
      p_sh = ggplot(data = dados) +
        geom_point(aes(x = sq, y = val), color = 
                     ifelse(dados$val < LCL_sh | dados$val > UCL_sh, 'green','black')) +
        geom_line(aes(x = sq, y = val)) +
        geom_line(aes(x = sq, y = LCL_sh), color = 'red') +
        geom_line(aes(x = sq, y = CL_sh), color = 'blue') +
        geom_line(aes(x = sq,y = UCL_sh), color = 'red') +
        geom_vline(xintercept =  length(samples1) + 0.5, lty = 2, col = 'black') +
        theme_classic(base_size = size) +
        labs(x = xlab, y = ylab, subtitle = 'Poisson-Shanker', title = tit)
      plot(p_sh) 
    }
    
    P.c.chart = function(dados, samples1, L){
      y.BAR = sapply(samples1, mean)
      y.TOT = sapply(samples1, sum)
      n = length(sq)
      mu = mean(y.BAR)/n
      desvio = sqrt(mean(y.BAR))
      UCL_pl = n*mu+L*desvio
      CL_pl = n*mu
      LCL_pl = n*mu-L*desvio
      p_pl = ggplot(data = dados) +
        geom_point(aes(x = sq, y = val), color = 
                     ifelse(dados$val < LCL_pl | dados$val > UCL_pl, 'green','black')) +
        geom_line(aes(x = sq, y = val)) +
        geom_line(aes(x = sq, y = LCL_pl), color = 'red') +
        geom_line(aes(x = sq, y = CL_pl), color = 'blue') +
        geom_line(aes(x = sq,y = UCL_pl), color = 'red') +
        geom_vline(xintercept =  length(samples1) + 0.5, lty = 2, col = 'black') +
        theme_classic(base_size = size) +
        labs(x = xlab, y = ylab, subtitle = 'Poisson', title = tit)
      plot(p_pl)
      
    }
    
    P.c.chart(samples1 = dados.f1$val, dados = dados, L = L)
    
    PL.c.chart(samples1 = dados.f1$val, dados = dados, L = L)
    
    PSh.c.chart(samples1 = dados.f1$val, dados = dados, L = L)
    
    PSu.c.chart(samples1 = dados.f1$val, dados = dados, L = L)
    
  }
}

x = c(16,18,12,15,24,21,28,20,25,19,18,21,16,22,19,12,14,9,16,21,27,27,
      34,31,19,29,17,35,20,18,12,34,13,25,23,24,20,27,16,28)
plot.control(x, type = 'count', fase1 = 0.8, xlab = 'Amostra', ylab = '', 
             tit = 'Titulo aqui', size =  20,n = 40, L = 1.5)

x = runif(50)
plot.control(x, type = 'unit', fase1 = 0.8, xlab = 'Amostra', ylab = 'Taxa (%)', 
             tit = 'Titulo aqui', size =  20)
