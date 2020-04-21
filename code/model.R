# transmission model #

model.fun <- function (t, x, params) {
    s <- x[sindex] # susceptible
    v <- x[vindex] # vaccinated/quarantined (susceptible)
    e <- x[eindex] # exposed 
    q <- x[qindex] # exposed - quarantined 
    a <- x[aindex] #infectious, pre-symptomatic
    w <- x[windex] # infectious, pre-symptomatic, in isolation
    b <- x[bindex] # infectious - mild
    c <- x[cindex] # infectious -severe
    y <- x[yindex] ## infectious - mild, in isolation
    z <- x[zindex] ## infectious - severe, in isolation
    g <- x[gindex] #isolated - mild, not previously isolated 
    h <- x[hindex] # hospitalized
    icua <- x[icuaindex] # pre-ICU
    icub <- x[icubindex] # ICU
    icuc <- x[icucindex] # post-ICU
    r <- x[rindex] # recovered   
    d <- x[dindex] # dead
    inc <- x[incindex] # cumulative incidence
    k <- x[kindex] # cumulative number of mild cases detected and isolated (currently only among those not already quarantined)
    j <- x[jindex] # cumulative incidence of hospitalization
  
    if(params[["interv.fixed"]]==FALSE) {
      
      if(exists("interv.start")) {
        if( t>(interv.start+interv.duration)){
          interv.on <- 0
          rm(interv.start)
        }
      }
      
      if(!exists("interv.start") & sum(icub) >= threshold ){
        interv.on <- 1
        interv.start <- t
      }
      
    } else {
      interv.on <- params[["interv.on"]][t+1]
    }


    rho <- 1/params[["quarantine.dur"]] 
    epsilon <- 1/params[["latent.period"]] 
    gamma_p <- 1/params[["inf.period.presymp"]]
    gamma_s <- 1/params[["inf.period.severe"]]
    gamma_m <- 1/params[["inf.period.mild"]]
    time.to.detect <- if (interv.on==1) params[["time.to.detect.interv"]] else params[["time.to.detect"]]
    gamma_d <- 1/time.to.detect #rate of entry to isolated compartment
    gamma_i <- if_else(time.to.detect<params[["inf.period.mild"]], 1/(params[["inf.period.mild"]] - time.to.detect), 1) # rate of exit from isolated (and infectious) compartment
    beta <- params[["beta"]]
    rr_i <- if(interv.on==1) params[["rr.isolate.interv"]] else params[["rr.isolate"]]
    delta_s <- if(interv.on==1) params[["p.quarantine.s.interv"]] else params[["p.quarantine.s"]]
    delta_e <- if(interv.on==1) params[["p.quarantine.e.interv"]] else params[["p.quarantine.e"]]
    sigma_s <- params[["p.severe"]]
    sigma_i <- params[["p.icu"]]
    psi <- 1/params[["hosp.dur"]]
    pi_a <- 1/params[["icu.a.dur"]]
    pi_b <- 1/params[["icu.b.dur"]]
    pi_c <- 1/params[["icu.c.dur"]]
    cfr_icu <- params[["cfr.icu"]]
    theta <- if(interv.on==1) params[["cm"]] * params[["rr.contact.interv"]] else params[["cm"]]  * params[["rr.contact"]]
    omega <- params[["rw"]]
    omega_p <- omega[t+1,1] #as.numeric(rep(omega[t+1,1:16], n.j))
    omega_m <- omega[t+1,2]
    omega_s <- omega[t+1,3] #as.numeric(rep(omega[t+1,17:32], n.j))
    

    #calculate force of infection
    l.h <- beta * (omega_p*(a + rr_i*w) + omega_m*(b+rr_i*(g+y)) + omega_s*(c+rr_i*z))/(s+v+e+q+a+w+b+c+y+z+g+h+icua+icub+icuc+r)  
    lambda <- rowSums(sweep(theta, MARGIN=2, l.h, `*`), na.rm=TRUE)
    
    dsdt <- -lambda*s - delta_s*s + rho*v + aging%*%s +  births%*%(s+v+e+q+a+w+b+c+y+z+g+h+icua+icub+icuc+r) 
    dvdt <- delta_s*s - rho*v + aging%*%v  
    dedt <- (1-delta_e)*lambda*s - epsilon*e + aging%*%e 
    dqdt <- delta_e*lambda*s - epsilon*q + aging%*%q 
    dadt <- epsilon*e - gamma_p*a + aging%*%a 
    dwdt <- epsilon*q - gamma_p*w + aging%*%w 
    dbdt <- (1-sigma_s)*gamma_p*a - gamma_m*b - gamma_d*b + aging%*%b 
    dcdt <- sigma_s*gamma_p*a - gamma_s*c + aging%*%c 
    dydt <- (1-sigma_s)*gamma_p*w - gamma_m*y + aging%*%y 
    dzdt <- sigma_s*gamma_p*w - gamma_s*z + aging%*%z 
    dgdt <- gamma_d*b - gamma_i*g + aging%*%g 
    dhdt <-  (1-sigma_i)*gamma_s*(c+z) - psi*h + aging%*%h 
    dicuadt <- sigma_i*gamma_s*(c+z) - pi_a*icua + aging%*%icua  
    dicubdt <- pi_a*icua - pi_b*icub + aging%*%icub  
    dicucdt <-(1-cfr_icu)*pi_b*icub - pi_c*icuc +  aging%*%icuc  
    drdt <- gamma_i*g + gamma_m*(b+y) + psi*h + cfr_icu*pi_b*icub + pi_c*icuc + aging%*%r  

    dddt <-  cfr_icu*pi_b*icub #to keep deaths as absorbing state and pop size constant, moving all hosp back to recovered/removed state 
    dincdt <- lambda*s 
    dkdt <- gamma_d*b
    djdt <- gamma_s*(c+z) 
    
      
    res <- c(S=dsdt,V=dvdt,E=dedt,Q=dqdt, A=dadt, W=dwdt, 
             B=dbdt, C=dcdt, Y=dydt, Z=dzdt, G=dgdt, H=dhdt,
             ICUA=dicuadt, ICUB=dicubdt, ICUC=dicucdt, R=drdt, 
             D=dddt, Inc=dincdt, K=dkdt, J=djdt)
    list(res, interv_delay=interv.delay, interv_duration = interv.duration, interv_on=interv.on )
    
}
    
