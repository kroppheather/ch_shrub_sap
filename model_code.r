model{
  #################################
  #########Model likelihood########
  #################################
  
  for(i in 1:Nobs){
    #likelihood for each tree
    gs[i]~dnorm(mu.gs[i],tau.gs[ss.obs[i]])
    rep.gs[i]~dnorm(mu.gs[i],tau.gs[ss.obs[i]])
    
    #gs.rep[i]~dnorm(mu.gs[i],tau.gs)
    #model for mean gs
    mu.gs[i]<-oren.mod[i]*light[i]
    
    #light scaling function
    light[i]<-1-exp(-l.slope[ssDay[i]]*PAR[i])
    
    #oren model 1999 for mean gs
    oren.mod[i]<-gref[ssDay[i]]*(1-(S[ssDay[i]]*log(D[i])))
    
  }
  
  #################################
  ########Prior Specification######
  #################################
  for(j in 1:Nssday){
    l.slope[j]~dunif(0, 3)
    gref[j]~dunif(0, 600)
    S[j]~dunif(0, 10)
  }
  
  for(k in 1: NSS){
    tau.gs[k] <- pow(sigma[k], -2)
    sigma[k] ~ dunif(0, 1)
  }
  
}