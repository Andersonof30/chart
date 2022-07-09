source('function den.R')


plot.control = function(x, type = 'nome', fase1 = 0.7,alpha = .1,
                        xlab = 'a', ylab = 'b', tit = NULL, size = 10, 
                        n, L, iter = 10, ...){
  
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
    print('1')
    
    fit_ul <- stan(file = 'ASN UL.stan',
                   data = list(Y=dados.f1$val,N=length(dados.f1$val)),
                   iter = iter,
                   chains = 1)
    fit_ul = summary(fit_ul)
    print('2')
    
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
    
    fitl <- stan(file = 'ASN PL2.stan',
                data = list(Y=dadosp,N=length(dadosp)),
                iter = iter,
                chains = 1)
    fitl = summary(fitl)
    
    fitp1 <- stan(file = 'ASN POIS.stan',
                  data = list(Y=dadosp,N=length(dadosp)),
                  iter = iter,
                  chains = 1)
    
    fitp1 = summary(fitp1)
    
    fith <- stan(file = 'ASN SH.stan',
                 data = list(Y=dadosp,N=length(dadosp)),
                 iter = iter,
                 chains = 1)
    fith = summary(fith)
    
    fitk <- stan(file = 'ASN PSK.stan',
                 data = list(Y=dadosp,N=length(dadosp)),
                 iter = iter,
                 chains = 1)
    fitk = summary(fitk)
    
    Bell.Chart.TOTAL = function(y = dados$val, dados.f1 = dados.f1$val,
                                n = n, L = L){
      package6 = "LambertW"
      if(!require(package6, character.only = T)){
        install.packages(package6, dependencies = T)
      }
      require(LambertW)
      mu_bl = mean(dados.f1)/n
      UCL_bl = n*mu_bl+L*sqrt((n*mu_bl*(1+W(mu_bl))))
      CL_bl = n*mu_bl
      LCL_bl = n*mu_bl-L*sqrt((n*mu_bl*(1+W(mu_bl))))
      #UCL_bl = rep(UCL_bl, length(y))
      #CL_bl = rep(CL_bl, length(y))
      #LCL_bl = rep(LCL_bl, length(y))
      p_bl = ggplot(data = dados) +
        geom_point(aes(x = sq, y = val), color = 
                     ifelse(dados$val < LCL_bl | dados$val > UCL_bl, 'green','black')) +
        geom_line(aes(x = sq, y = val)) +
        geom_line(aes(x = sq, y = LCL_bl), color = 'red') +
        geom_line(aes(x = sq, y = CL_bl), color = 'blue') +
        geom_line(aes(x = sq,y = UCL_bl), color = 'red') +
        geom_vline(xintercept =  length(dados.f1) + 0.5, lty = 2, col = 'black') +
        theme_classic(base_size = size) +
        labs(x = xlab, y = ylab, subtitle = 'Bell', title = tit)
      plot(p_bl)
    }
    
    #psh
    PSu.c.chart = function(dados, samples1, L){
      h.mu = function(mu, log=F){
        p1 = 3*sqrt(3)*sqrt(7*mu^4+28*mu^3+171*mu^2+32*mu+5)*mu
        p2 = 8*mu^3+66*mu^2+6*mu+1
        p = (p1+p2)^(1/3)
        theta = log(p/mu-((mu-1)*(5*mu+1))/(mu*p)-(mu-1)/mu)-log(3)
        if (log) theta else exp(theta)
      }
      
      y.BAR = fith$summary[1, '50%']; y.BAR2 = fith$summary[1, '2.5%']; y.BAR3 = fith$summary[1, '97.5%']
      n = length(sq)
      mu = c(y.BAR,y.BAR2, y.BAR3)
      a = h.mu(mu)
      desvio = sqrt(n*(a^5+4*a^4+14*a^3+28*a^2+24*a+12)/(a^2*(a^2+a+2)^2))
      UCL_su = n*mu+L*desvio
      CL_su = n*mu
      LCL_su = n*mu-L*desvio
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
      y.BAR = fitl$summary[1, c('50%', '2.5%', '97.5%')]
      y.TOT = sapply(samples1, sum)
      n = length(sq)
      mu = y.BAR
      a = -(1/(2*mu))*((mu-1)-sqrt(((mu-1)^2)+8*mu))
      desvio = sqrt(n*(a^3+(4*a^2)+6*a+2)/((a^2)*((a+1)^2)))
      UCL_pl = n*mu+L*desvio
      CL_pl = n*mu
      LCL_pl = n*mu-L*desvio
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
      y.BAR = fitk$summary[1, c('50%', '2.5%', '97.5%')]
      y.TOT = sapply(samples1, sum)
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
      y.BAR = fitp1$summary[1, c('50%', '2.5%', '97.5%')]
      y.TOT = sapply(samples1, sum)
      n = length(sq)
      mu = y.BAR
      desvio = sqrt(mean(y.BAR))
      UCL_pos = n*mu+L*desvio
       CL_pos = n*mu
      LCL_pos = n*mu-L*desvio
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
    
    Bell.Chart.TOTAL(y = dados$val,dados.f1 = dados.f1$val,n,L = L)
  }
}


x = c(16,18,12,15,24,21,28,20,25,19,18,21,16,22,19,12,14,9,16,21,27,27,
      34,31,19,29,17,35,20,18,12,34,13,25,23,24,20,27,16,28)
x = rpois(50, 50)
plot.control(x, type = 'count', fase1 = 0.8, xlab = 'Amostra', ylab = '', 
             tit = 'Titulo aqui', size =  20,n = 40, L = 1.5)

x = VGAM::rkumar(200, 2, 3)
x = rbeta(100, 1 ,3)
x = qUL(runif(50), .3)
#x = runif(50)
plot.control(x, type = 'unit', fase1 = .8, xlab = 'Amostra', ylab = 'Taxa (%)', 
             tit = 'Titulo aqui', size =  20, iter = 100)




s.min = read.csv('serie minima atualizada (n 200).csv', sep = ' ')
s.max = read.csv('serie maximo atualizada (n 200).csv', sep = ' ')

tm1 = Sys.time()
plot.control(s.min$minima, type = 'unit', fase1 = .8, xlab = '', ylab = 'Umidade Relativa do Ar', 
             tit = ' ', size =  20, iter = 1000)
tm2 = Sys.time()


plot.control(s.max$maxima, type = 'unit', fase1 = .8, xlab = '', ylab = 'Umidade Relativa do Ar', 
             tit = ' ', size =  20, iter = 1000)





