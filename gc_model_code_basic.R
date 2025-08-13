###########################################################################
###########################################################################
############## Created by Heather Kropp in October 2017      ##############
############## The model code for the analysis of the        ##############
############## of canopy stomatal conductance calculated     ##############
############## from sapflow. The model is based on a         ##############
############## phenomenological model that describes         ##############
############## stomatal responses to light and VPD           ##############
############## used in Kropp et. al. 2017, Oren et. al. 1999,##############
############## Jarvis 1976, and White et. al. 1999.          ##############
############## The parameters of this model are considered   ##############
############## to vary with environmental drivers:           ##############
############## average daily air temperature and antecedent  ##############
############## precipitation.                                ##############
###########################################################################
###########################################################################

model{
  #################################
  #########Model likelihood########
  #################################
  for(i in 1:Nobs){
    #likelihood for each tree
    gs[i]~dnorm(mu.gs[i],tau.gs[spsID.obs[i]])
    rep.gs[i]~dnorm(mu.gs[i],tau.gs[spsID.obs[i]])
    
    #gs.rep[i]~dnorm(mu.gs[i],tau.gs)
    #model for mean gs
    mu.gs[i]<-oren.mod[i]*light[i]
    
    #light scaling function
    light[i]<-1-exp(-l.slope[spsDay[i]]*PAR[i])
    
    #oren model 1999 for mean gs
    oren.mod[i]<-gref[spsDay[i]]*(1-(S[spsDay[i]]*log(D[i])))
    
  }
  #################################
  #########parameter model ########
  #################################	
  for(i in 1:NspsDay){
    gref[i]<-dnorm(alpha[SPS[i]], alpha.tau[SPS[i]])
    S[i]<-dnorm(beta[SPS[i]], beta.tau[SPS[i]])
    slope.temp[i] <-dnorm(delta[SPS[i]], delta.tau[SPS[i]])
    #Log transform light function slope to avoid numerical traps
    #and allow for better mixing and faster convergence of the non-linear model
    l.slope[i]<-exp(slope.temp[i])

  }
  #################################
  #########priors          ########
  #################################	
  #define prior distributions for parameters
  #All parameters are given non-informative dist
  
  
  for(i in 1:NSPS){
    
    tau.gs[i]<-pow(sig.gs[i],-2)
    sig.gs[i]~dunif(0,1000)	
    
    
  }
  
  #################################
  #########priors model########
  #################################
  for(i in 1:NSPS){

      delta[i]~dnorm(0,0.0001)
      alpha[i]~dunif(0,1000)
      gamma[i]~dunif(0,10)
      alpha.tau[i]<-pow(sig.alpha[i],-2)
      sig.alpha[i]~dunif(0,1000)	
      beta.tau[i]<-pow(sig.beta[i],-2)
      sig.beta[i]~dunif(0,1000)	      
      delta.tau[i]<-pow(sig.delta[i],-2)
      sig.delta[i]~dunif(0,1000)	    
  }
  
}
    