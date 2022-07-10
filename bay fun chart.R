source('function den.R')

plot.control = function(x, type = c('unit', 'count'), fase1 = 0.7,alpha = .1,
                        xlab = 'a', ylab = 'b', tit = NULL, size = 10, 
                        L, iter = 10, ...){
  
  # instalando pacotes 
  ###########################
  package = "rstan"
  if(!require(package, character.only = T)){
    install.packages(package, dependencies = T)
  }
  require(rstan)  
  package2 = "ggplot2"
  if(!require(package2, character.only = T)){
    install.packages(package2, dependencies = T)
  }
  require(ggplot2)
  package3 = "VGAM"
  if(!require(package3, character.only = T)){
    install.packages(package3, dependencies = T)
  }
  require(VGAM)
  
  ###########################
  
  
  
  n1 = length(x)*fase1
  dados.f1 = data.frame(x[1:n1]); colnames(dados.f1) = 'val'
  dados = data.frame(x);colnames(dados) = 'val'
  sq = 1:length(x)
  if(type == 'unit'){
    ########### estimacao
    options(warn = -1) 
    
    fit_ul <- stan(file = 'ASN UL.stan',
                   data = list(Y=dados.f1$val,N=length(dados.f1$val)),
                   iter = iter,
                   chains = 1)
    fit_ul = summary(fit_ul)
    
    fit_km <- stan(file = 'ASN kuma.stan',
                   data = list(Y=dados.f1$val,N=length(dados.f1$val)),
                   iter = iter,
                   chains = 1)
    fit_km = summary(fit_km)
    
    fit_bt <- stan(file = 'ASN BETA.stan',
                   data = list(Y=dados.f1$val,N=length(dados.f1$val)),
                   iter = iter,
                   chains = 1)
    fit_bt = summary(fit_bt)
    
    
    
    fit_uhn <- stan(file = 'ASN UHN.stan',
                    data = list(Y=dados.f1$val,N=length(dados.f1$val)),
                    iter = iter,
                    chains = 1)
    fit_uhn = summary(fit_uhn)
    options(warn = 0)
    
    
    md_uh =  fit_uhn$summary[1,'50%']
    md_uh2 = fit_uhn$summary[1,'2.5%']
    md_uh3 = fit_uhn$summary[1,'97.5%']
    
    qi_uh = quhn(alpha/2,c (md_uh, md_uh2, md_uh3))
    qs_uh = quhn(1 - alpha/2, c(md_uh, md_uh2, md_uh3))
    
    p_uh = ggplot(data = dados) +
      geom_point(aes(x = sq, y = val), color = 
                   ifelse(dados$val < min(qi_uh) | dados$val > max(qs_uh), 
                          'green',
                          ifelse(dados$val < max(qi_uh) | dados$val > min(qs_uh), 
                                 'blue', 'black'  ))) +
      geom_line(aes(x = sq, y = val)) +
      geom_line(aes(x = sq, y = median(qi_uh)), color = 'red') +
      geom_ribbon(aes(x = sq, ymin= min(qi_uh), ymax= max(qi_uh)), alpha=0.1, fill = "red", 
                  color = "red", linetype = "dotted") +
      geom_line(aes(x = sq, y = md_uh), color = 'blue') +
      geom_line(aes(x = sq,y = median(qs_uh)), color = 'red') +
      geom_ribbon(aes(x = sq, ymin = min(qs_uh), ymax= max(qs_uh)), alpha=0.1, fill = "red", 
                  color = "red", linetype = "dotted") +
      geom_vline(xintercept =  length(dados.f1$val) + 0.5, lty = 2, col = 'black') +
      theme_classic(base_size = size) +
      labs(x = xlab, y = ylab, subtitle = 'unit-half-normal', title = tit)
    #plot(p_uh)
    vp_uh = ks.test(dados.f1$val, puhn, md_uh)$p.value
    
    #### beta  
    bt_ap =  fit_bt$summary[1,'50%']
    bt_ap2 = fit_bt$summary[1,'2.5%']
    bt_ap3 = fit_bt$summary[1,'97.5%']
    
    bt_bt =  fit_bt$summary[2,'mean']
    bt_bt2 = fit_bt$summary[2,'2.5%']
    bt_bt3 = fit_bt$summary[2,'97.5%']
    
    qi_uh = quhn(alpha/2,c (md_uh, md_uh2, md_uh3))
    qs_uh = quhn(1 - alpha/2, c(md_uh, md_uh2, md_uh3))
    
    md_bt = bt_ap/(bt_ap + bt_bt)
    qi_bt = qbeta(alpha/2, c(bt_ap, bt_ap2, bt_ap3), c(bt_bt, bt_bt2, bt_bt3))
    qs_bt = qbeta( 1- alpha/2, c(bt_ap, bt_ap2, bt_ap3), c(bt_bt, bt_bt2, bt_bt3))
    
    p_bt = ggplot(data = dados) +
      geom_point(aes(x = sq, y = val), color = 
                   ifelse(dados$val < min(qi_bt) | dados$val > max(qs_bt), 'green',
                          ifelse(dados$val < max(qi_bt) | dados$val > min(qs_bt), 
                                 'blue', 'black'  ))) +
      geom_line(aes(x = sq, y = val)) +
      geom_line(aes(x = sq, y = median(qi_bt)), color = 'red') +
      geom_ribbon(aes(x = sq, ymin= min(qi_bt), ymax= max(qi_bt)), alpha=0.1, fill = "red", 
                  color = "red", linetype = "dotted")+
      geom_line(aes(x = sq, y = md_bt), color = 'blue') +
      geom_line(aes(x = sq,y = median(qs_bt)), color = 'red') +
      geom_ribbon(aes(x = sq, ymin= min(qs_bt), ymax= max(qs_bt)), alpha=0.1, fill = "red", 
                  color = "red", linetype = "dotted")+
      geom_vline(xintercept =  length(dados.f1$val) + 0.5, lty = 2, col = 'black') +
      theme_classic(base_size = size) +
      labs(x = xlab, y = ylab, subtitle = 'beta', title = tit)
    #plot(p_bt)
    vp_bt = ks.test(dados.f1$val, pbeta, bt_ap, bt_bt)$p.value
    
    #kumar
    km_sp =  fit_km$summary['alp','50%']
    km_sp2 = fit_km$summary['alp','2.5%']
    km_sp3 = fit_km$summary['alp','97.5%']
    
    km_sc =  fit_km$summary['bet','50%']
    km_sc2 = fit_km$summary['bet','2.5%']
    km_sc3 = fit_km$summary['bet','97.5%']
    
    md_km = (km_sc*gamma(1 + 1/km_sp)*gamma(km_sc))/(gamma(1 + 1/km_sp + km_sc))
    qi_km = VGAM::qkumar(alpha/2, c(km_sp, km_sp2, km_sp3), c(km_sc, km_sc2, km_sc3))
    qs_km = VGAM::qkumar(1 - alpha/2, c(km_sp, km_sp2, km_sp3), c(km_sc, km_sc2, km_sc3))
    
    
    p_km = ggplot(data = dados) +
      geom_point(aes(x = sq, y = val), color = 
                   ifelse(dados$val < min(qi_km) | dados$val > max(qs_km), 'green',
                          ifelse(dados$val < max(qi_km) | dados$val > min(qs_km), 
                                 'blue', 'black'  ))) +
      geom_line(aes(x = sq, y = val)) +
      geom_line(aes(x = sq, y = median(qi_km)), color = 'red') +
      geom_ribbon(aes(x = sq, ymin= min(qi_km), ymax= max(qi_km)), alpha=0.1, fill = "red", 
                  color = "red", linetype = "dotted") +
      geom_line(aes(x = sq, y = md_km), color = 'blue') +
      geom_line(aes(x = sq,y = median(qs_km)), color = 'red') +
      geom_ribbon(aes(x = sq, ymin= min(qs_km), ymax= max(qs_km)), alpha=0.1, fill = "red", 
                  color = "red", linetype = "dotted") +
      geom_vline(xintercept =  length(dados.f1$val) + 0.5, lty = 2, col = 'black') +
      theme_classic(base_size = size) +
      labs(x = xlab, y = ylab, subtitle = 'kumaraswamy', title = tit)
    #plot(p_km)
    vp_km = ks.test(dados.f1$val, VGAM::pkumar, km_sp, km_sc)$p.value
    
    ##### UL
    md_ul =  fit_ul$summary[1,'50%']
    md_ul2 = fit_ul$summary[1,'2.5%']
    md_ul3 = fit_ul$summary[1,'97.5%']
    
    qi_ul = quhn(alpha/2,c(md_ul, md_ul2, md_ul3))
    qs_ul = quhn(1 - alpha/2, c(md_ul, md_ul2, md_ul3))
    
    p_ul = ggplot(data = dados) +
      geom_point(aes(x = sq, y = val), color = 
                   ifelse(dados$val < min(qi_ul) | dados$val > max(qs_ul), 'green',
                          ifelse(dados$val < max(qi_ul) | dados$val > min(qs_ul), 
                                 'blue', 'black'  ))) +
      geom_line(aes(x = sq, y = val)) +
      geom_line(aes(x = sq, y = median(qi_ul)), color = 'red') +
      geom_ribbon(aes(x = sq, ymin= min(qi_ul), ymax= max(qi_ul)), alpha=0.1, fill = "red", 
                  color = "red", linetype = "dotted") +
      geom_line(aes(x = sq, y = md_ul), color = 'blue') +
      geom_line(aes(x = sq,y = median(qs_ul)), color = 'red') +
      geom_ribbon(aes(x = sq, ymin= min(qs_ul), ymax= max(qs_ul)), alpha=0.1, fill = "red", 
                  color = "red", linetype = "dotted") +
      geom_vline(xintercept =  length(dados.f1$val) + 0.5, lty = 2, col = 'black') +
      theme_classic(base_size = size) +
      labs(x = xlab, y = ylab, subtitle = 'unit-Lindley', title = tit)
    #plot(p_ul)
    
    vp_ul = ks.test(dados.f1$val, pUL, md_ul)$p.value
    
    plot(p_bt); plot(p_km); plot(p_ul); plot(p_uh)
    
    #############
    dt_pv = data.frame(c('UHN', 'beta', 'kumar', 'UL'),
                       round(c(vp_uh,vp_bt, vp_km, vp_ul), 4), 
                       c('p_uh', 'p_bt', 'p_km', 'p_ul'),
                       row.names = c('', ' ', '  ', '    '))
    colnames(dt_pv) = c('Distribuição','P-valor', 'plot')
    dt_pv = dt_pv[order(dt_pv$`P-valor`, decreasing = T),]
    print(dt_pv[, 1:2])
    print(md_ul)
  }
  
  else if (type == 'count'){
    
    
    ##########estimacao
    
    #psh
    PSu.c.chart = function(dados, samples1, L){
      fitu <- stan(file = 'ASN Sujatha.stan',
                   data = list(Y=samples1,N=length(samples1)),
                   iter = iter,
                   chains = 1)
      fitu = summary(fitu)
      
      h.mu = function(mu, log=F){
        p1 = 3*sqrt(3)*sqrt(7*mu^4+28*mu^3+171*mu^2+32*mu+5)*mu
        p2 = 8*mu^3+66*mu^2+6*mu+1
        p = (p1+p2)^(1/3)
        theta = log(p/mu-((mu-1)*(5*mu+1))/(mu*p)-(mu-1)/mu)-log(3)
        if (log) theta else exp(theta)
      }
      
      y.BAR = fitu$summary[1, c('50%', '2.5%', '97.5%')]
      n = length(sq)
      mu = y.BAR
      a = h.mu(mu)
      desvio = sqrt((a^5+4*a^4+14*a^3+28*a^2+24*a+12)/(a^2*(a^2+a+2)^2))
      UCL_su = mu + L*desvio
      CL_su =  mu
      LCL_su = mu- L*desvio
      p_su = ggplot(data = dados) +
        geom_point(aes(x = sq, y = val), color = 
                     ifelse(dados$val < min(LCL_su) | dados$val > max(UCL_su), 'green',
                            ifelse(dados$val < max(LCL_su) | dados$val > min(UCL_su), 
                                   'blue', 'black'  ))) +
        geom_line(aes(x = sq, y = val)) +
        
        geom_line(aes(x = sq, y = median(LCL_su)), color = 'red') +
        geom_ribbon(aes(x = sq, ymin= min(LCL_su), ymax= max(LCL_su)), alpha=0.1, fill = "red", 
                    color = "red", linetype = "dotted") +
        geom_line(aes(x = sq, y = median(CL_su)), color = 'blue') +
        geom_line(aes(x = sq,y = median(UCL_su)), color = 'red') +
        geom_ribbon(aes(x = sq, ymin= min(UCL_su), ymax= max(UCL_su)), alpha=0.1, fill = "red", 
                    color = "red", linetype = "dotted") +
        
        geom_vline(xintercept =  length(samples1) + 0.5, lty = 2, col = 'black') +
        theme_classic(base_size = size) +
        labs(x = xlab, y = ylab, subtitle = 'Poisson-Sujatha', title = tit)
      plot( p_su)
    }
    
    
    #Pois L
    PL.c.chart = function(dados, samples1, L){
      
      fitl <- stan(file = 'ASN PLindley.stan',
                   data = list(Y=samples1,N=length(samples1)),
                   iter = iter,
                   chains = 1)
      fitl = summary(fitl)
      
      y.BAR = fitl$summary[1, c('50%', '2.5%', '97.5%')]
      n = length(sq)
      mu = y.BAR
      a = (-1/(2*mu))*((mu-1)-sqrt(((mu-1)^2)+8*mu))
      desvio = sqrt((a^3+(4*a^2)+6*a+2)/((a^2)*((a+1)^2)))
      UCL_pl = mu + L*desvio
      CL_pl = mu
      LCL_pl = mu - L*desvio
      p_pl = ggplot(data = dados) +
        geom_point(aes(x = sq, y = val), color = 
                     ifelse(dados$val < min(LCL_pl) | dados$val > max(UCL_pl), 'green',
                            ifelse(dados$val < max(LCL_pl) | dados$val > min(UCL_pl), 
                                   'blue', 'black'  ))) +
        geom_line(aes(x = sq, y = val)) +
        
        geom_line(aes(x = sq, y = median(LCL_pl)), color = 'red') +
        geom_ribbon(aes(x = sq, ymin= min(LCL_pl), ymax= max(LCL_pl)), alpha=0.1, fill = "red", 
                    color = "red", linetype = "dotted") +
        geom_line(aes(x = sq, y = median(CL_pl)), color = 'blue') +
        geom_line(aes(x = sq,y = median(UCL_pl)), color = 'red') +
        geom_ribbon(aes(x = sq, ymin= min(UCL_pl), ymax= max(UCL_pl)), alpha=0.1, fill = "red", 
                    color = "red", linetype = "dotted") +
        
        geom_vline(xintercept =  length(samples1) + 0.5, lty = 2, col = 'black') +
        theme_classic(base_size = size) +
        labs(x = xlab, y = ylab, subtitle = 'Poisson-Lindley', title = tit)
      plot(p_pl)
      
    }
    
    #psk
    PSh.c.chart = function(dados, samples1, L){
      fitk <- stan(file = 'ASN Shanker.stan',
                   data = list(Y=samples1,N=length(samples1)),
                   iter = iter,
                   chains = 1)
      fitk = summary(fitk)
      
      y.BAR = fitk$summary[1, c('50%', '2.5%', '97.5%')]
      n = length(sq)
      mu = y.BAR/n
      s.mu = (12*mu*sqrt(3*(4*mu^4+71*mu^2+8))+180*mu^2+8)^(1/3)
      a = (1/(3*mu))*(1+s.mu/2-2*(3*mu^2-1)/s.mu)
      desvio = sqrt(n*((a^5+a^4+3*a^3+4*a^2+2*a+2)/(a^2*((a^2+1)^2))))
      UCL_sh = n*mu+L*desvio
      CL_sh = n*mu
      LCL_sh = n*mu-L*desvio
      p_sh = ggplot(data = dados) +
        geom_point(aes(x = sq, y = val), color =
                     ifelse(dados$val < min(LCL_sh) | dados$val > max(UCL_sh), 'green',
                            ifelse(dados$val < max(LCL_sh) | dados$val > min(UCL_sh), 
                                   'blue', 'black'  ))) +
        
        geom_line(aes(x = sq, y = val)) +
        
        geom_line(aes(x = sq, y = median(LCL_sh)), color = 'red') +
        geom_ribbon(aes(x = sq, ymin= min(LCL_sh), ymax= max(LCL_sh)), alpha=0.1, fill = "red", 
                    color = "red", linetype = "dotted") +
        geom_line(aes(x = sq, y = median(CL_sh)), color = 'blue') +
        geom_line(aes(x = sq,y = median(UCL_sh)), color = 'red') +
        geom_ribbon(aes(x = sq, ymin= min(UCL_sh), ymax= max(UCL_sh)), alpha=0.1, fill = "red", 
                    color = "red", linetype = "dotted") +
        
        geom_vline(xintercept =  length(samples1) + 0.5, lty = 2, col = 'black') +
        theme_classic(base_size = size) +
        labs(x = xlab, y = ylab, subtitle = 'Poisson-Shanker', title = tit)
      plot(p_sh) 
    }
    
    #pois
    P.c.chart = function(dados, samples1, L){
      
      fitp1 <- stan(file = 'ASN POIS.stan',
                    data = list(Y=samples1,N=length(samples1)),
                    iter = iter,
                    chains = 1)
      
      fitp1 = summary(fitp1)
      
      y.BAR = fitp1$summary[1, c('50%', '2.5%', '97.5%')]
      n = length(sq)
      mu = y.BAR
      desvio = sqrt(mu)
      UCL_pos = mu + L*desvio
      CL_pos = mu 
      LCL_pos = mu - L*desvio
      p_pos = ggplot(data = dados) +
        geom_point(aes(x = sq, y = val), color = 
                     ifelse(dados$val < min(LCL_pos) | dados$val > max(UCL_pos), 'green',
                            ifelse(dados$val < max(LCL_pos) | dados$val > min(UCL_pos), 
                                   'blue', 'black'  ))) +
        
        geom_line(aes(x = sq, y = val)) +
        
        geom_line(aes(x = sq, y = median(LCL_pos)), color = 'red') +
        geom_ribbon(aes(x = sq, ymin= min(LCL_pos), ymax= max(LCL_pos)), alpha=0.1, fill = "red", 
                    color = "red", linetype = "dotted") +
        geom_line(aes(x = sq, y = median(CL_pos)), color = 'blue') +
        geom_line(aes(x = sq,y = median(UCL_pos)), color = 'red') +
        geom_ribbon(aes(x = sq, ymin= min(UCL_pos), ymax= max(UCL_pos)), alpha=0.1, fill = "red", 
                    color = "red", linetype = "dotted")+
        
        geom_vline(xintercept =  length(samples1) + 0.5, lty = 2, col = 'black') +
        theme_classic(base_size = size) +
        labs(x = xlab, y = ylab, subtitle = 'Poisson', title = tit)
      plot(p_pos)
      
    }
    
    P.c.chart(samples1 = dados.f1$val, dados = dados, L = L)
    
    PL.c.chart(samples1 = dados.f1$val, dados = dados, L = L)
    
    PSh.c.chart(samples1 = dados.f1$val, dados = dados, L = L)
    
    PSu.c.chart(samples1 = dados.f1$val, dados = dados, L = L)
  }
}
