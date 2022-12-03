source('support functions.R')

plot.control = function(x, type = c('unit', 'count'), fase1 = 0.7,alpha = .1, L = 3, iter = 1000,
                        xlab = 'a', ylab = 'b', tit = NULL, size = 20, samples = 5, grid = F){
  
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
  package6 = "LambertW"
  if(!require(package6, character.only = T)){
    install.packages(package6, dependencies = T)
  }
###########################
  
  
  
  n1 = length(x)*fase1
  dados.f1 = data.frame(x[1:n1]); colnames(dados.f1) = 'val'
  dados = data.frame(x);colnames(dados) = 'val'
  sq = 1:length(x)
  if(type == 'unit'){
    ### UH
    si_m = Rfast2::simplex.mle(dados.f1$val)$param['mean'];si_s = Rfast2::simplex.mle(dados.f1$val)$param['sigma']
    qi_uh = gamlss.dist::qSIMPLEX(alpha/2,si_m, si_s) 
    qs_uh = gamlss.dist::qSIMPLEX(1-alpha/2,si_m, si_s)

    p_uh = ggplot(data = dados) +
      geom_point(aes(x = sq, y = val), color = 
                   ifelse(dados$val < qi_uh | dados$val > qs_uh, 'green','black')) +
      geom_line(aes(x = sq, y = val)) +
      geom_line(aes(x = sq, y = qi_uh), color = 'red') +
      geom_line(aes(x = sq, y = si_m), color = 'blue') +
      geom_line(aes(x = sq,y = qs_uh), color = 'red') +
      geom_vline(xintercept =  length(dados.f1$val) + 0.5, lty = 2, col = 'black') +
      theme_classic(base_size = size) +
      labs(x = xlab, y = ylab, subtitle = 'simplex', title = tit)
    plot(p_uh)

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

    if(grid == T){
      gridExtra::grid.arrange(p_uh, p_bt, p_km, p_ul, ncol = 2)
    }
  }
  else if (type == 'count'){
    
    #Pois L
    PL.c.chart = function(dados, samples1, L, n = samples){
      
      mu = mean(samples1)
      
      a = -(1/(2*mu))*((mu-1)-sqrt(((mu-1)^2)+8*mu))
      desvio = sqrt(n*(a^3+(4*a^2)+6*a+2)/((a^2)*((a+1)^2)))
      UCL_pl = n*mu+L*desvio
      CL_pl = n*mu
      LCL_pl = n*mu-L*desvio
      p_pl = ggplot(data = dados) +
        geom_point(aes(x = sq, y = (val)*n), color = 
                     ifelse(dados$val*n < LCL_pl | dados$val*n > UCL_pl, 'green','black')) +
        geom_line(aes(x = sq, y = (val)*n)) +
        geom_line(aes(x = sq, y = LCL_pl), color = 'red') +
        geom_line(aes(x = sq, y = CL_pl), color = 'blue') +
        geom_line(aes(x = sq,y = UCL_pl), color = 'red') +
        geom_vline(xintercept =  length(samples1) + 0.5, lty = 2, col = 'black') +
        theme_classic(base_size = size) +
        labs(x = xlab, y = ylab, subtitle = 'Poisson-Lindley', title = tit)
      plot(p_pl)
      
    }  
    
    P.c.chart = function(dados, samples1, L, n = samples){
      
      mu = mean(samples1)
      
      desvio = sqrt(n*mu)
      UCL_pl = n*mu+L*desvio
      CL_pl = n*mu
      LCL_pl = n*mu-L*desvio
      p_pl = ggplot(data = dados) +
        geom_point(aes(x = sq, y = (val)*n), color = 
                     ifelse(dados$val*n < LCL_pl | dados$val*n > UCL_pl, 'green','black')) +
        geom_line(aes(x = sq, y = (val)*n)) +
        geom_line(aes(x = sq, y = LCL_pl), color = 'red') +
        geom_line(aes(x = sq, y = CL_pl), color = 'blue') +
        geom_line(aes(x = sq,y = UCL_pl), color = 'red') +
        geom_vline(xintercept =  length(samples1) + 0.5, lty = 2, col = 'black') +
        theme_classic(base_size = size) +
        labs(x = xlab, y = ylab, subtitle = 'Poisson', title = tit)
      plot(p_pl)
      
    }
    
    
    PSu.c.chart = function(dados, samples1, L, n = samples){
      h.mu = function(mu, log=F){
        p1 = 3*sqrt(3)*sqrt(7*mu^4+28*mu^3+171*mu^2+32*mu+5)*mu
        p2 = 8*mu^3+66*mu^2+6*mu+1
        p = (p1+p2)^(1/3)
        theta = log(p/mu-((mu-1)*(5*mu+1))/(mu*p)-(mu-1)/mu)-log(3)
        if (log) theta else exp(theta)
      }
      
      
      mu = mean(samples1)
      a = h.mu(mu)
      desvio = sqrt(n*(a^5+4*a^4+14*a^3+28*a^2+24*a+12)/(a^2*(a^2+a+2)^2))
      UCL_su = n*mu+L*desvio
      CL_su = n*mu
      LCL_su = n*mu-L*desvio
      p_su = ggplot(data = dados) +
        geom_point(aes(x = sq, y = (val)*n), color = 
                     ifelse(dados$val*n < LCL_su | dados$val*n > UCL_su, 'green','black')) +
        geom_line(aes(x = sq, y = (val)*n)) +
        geom_line(aes(x = sq, y = LCL_su), color = 'red') +
        geom_line(aes(x = sq, y = CL_su), color = 'blue') +
        geom_line(aes(x = sq,y = UCL_su), color = 'red') +
        geom_vline(xintercept =  length(samples1) + 0.5, lty = 2, col = 'black') +
        theme_classic(base_size = size) +
        labs(x = xlab, y = ylab, subtitle = 'Poisson-Sujatha', title = tit)
      plot( p_su)
    }
    

    PSh.c.chart = function(dados, samples1, L, n = samples){
      
      mu = mean(samples1)
      s.mu = (12*mu*sqrt(3*(4*mu^4+71*mu^2+8))+180*mu^2+8)^(1/3)
      a = (1/(3*mu))*(1+s.mu/2-2*(3*mu^2-1)/s.mu)
      desvio = sqrt(n*((a^5+a^4+3*a^3+4*a^2+2*a+2)/(a^2*((a^2+1)^2))))
      UCL_sh = n*mu+L*desvio
      CL_sh = n*mu
      LCL_sh = n*mu-L*desvio
      p_sh = ggplot(data = dados) +
        geom_point(aes(x = sq, y = (val)*n), color = 
                     ifelse(dados$val*n < LCL_sh | dados$val*n > UCL_sh, 'green','black')) +
        geom_line(aes(x = sq, y = (val)*n)) +
        geom_line(aes(x = sq, y = LCL_sh), color = 'red') +
        geom_line(aes(x = sq, y = CL_sh), color = 'blue') +
        geom_line(aes(x = sq,y = UCL_sh), color = 'red') +
        geom_vline(xintercept =  length(samples1) + 0.5, lty = 2, col = 'black') +
        theme_classic(base_size = size) +
        labs(x = xlab, y = ylab, subtitle = 'Poisson-Shanker', title = tit)
      plot(p_sh) 
    }
    
    
    P.c.bell = function(dados, samples1, L, n = samples){
      
      mu = mean(samples1)
      
      desvio = sqrt(n*(mu*(1+W(mu))))
      UCL_pl = n*mu+L*desvio
      CL_pl = n*mu
      LCL_pl = n*mu-L*desvio
      p_pl = ggplot(data = dados) +
        geom_point(aes(x = sq, y = (val)*n), color = 
                     ifelse(dados$val*n < LCL_pl | dados$val*n > UCL_pl, 'green','black')) +
        geom_line(aes(x = sq, y = (val)*n)) +
        geom_line(aes(x = sq, y = LCL_pl), color = 'red') +
        geom_line(aes(x = sq, y = CL_pl), color = 'blue') +
        geom_line(aes(x = sq,y = UCL_pl), color = 'red') +
        geom_vline(xintercept =  length(samples1) + 0.5, lty = 2, col = 'black') +
        theme_classic(base_size = size) +
        labs(x = xlab, y = ylab, subtitle = 'Bell', title = tit)
      plot(p_pl)
      
    }
    
    P.c.chart(samples1 = dados.f1$val, dados = dados, L = L, n = samples)
    
    PL.c.chart(samples1 = dados.f1$val, dados = dados, L = L, n = samples)
    
    PSh.c.chart(samples1 = dados.f1$val, dados = dados, L = L, n = samples)
    
    PSu.c.chart(samples1 = dados.f1$val, dados = dados, L = L, n = samples)
    
    P.c.bell(samples1 = dados.f1$val, dados = dados, L = L, n = samples)

  }
}

