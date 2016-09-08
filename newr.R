library(rethinking)
data("reedfrogs")

# surviving model,hierarchical
# can be framed as binomial,logistic
# Let us do it Rethinking style
d = reedfrogs
str(d)
d$tank = 1:nrow(d)

# No pooling each tank separate

m12.1 <- map2stan(
  alist(
    surv ~ dbinom( density , p ) ,
    logit(p) <- a_tank[tank] ,
    a_tank[tank] ~ dnorm( 0 , 5 )
  ),
  data=d )

# Applying multilevel, a_tank has now hyperparameter(alpha,tau) and it has hyperprior (





precis(m12.1,depth = 2)
precis(m12.2,prob = 0.95)
?precism12.2 <- map2stan(
  alist(
    surv ~ dbinom( density , p ) ,
    logit(p) <- a_tank[tank] ,
    a_tank[tank] ~ dnorm( a , sigma ) ,
    a ~ dnorm(0,1) ,
    sigma ~ dcauchy(0,1)
  ), data=d , iter=4000 , chains= 3)


#
m12.3 <- map2stan(
  alist(
    surv ~ dbinom( density , p ) ,
    p  <- a_tank[tank] ,
    a_tank[tank] ~ dunif(0.2,0.8) 
    
  ), data=d , iter=1000 ,chains= 2)




# you see a has shifted to 1.3 in men(0.81 to 1.80),let us Back transform
inv_logit(0.8)
inv_logit(1.80)

compare(m12.2,m12.1)



# Let us use kruschke JAGS style analysis

library(runjags)
library(rjags)

# Change directory

setwd("/home/anupam/Bayes")

# Read the data
myData = read.csv("BattingAverage.csv")

# Load the relevant model into R’s working memory:
source("Jags-Ybinom-XnomSsubjCcat-MbinomBetaOmegaKappa.R")
# Generate the MCMC chain:
mcmcCoda = genMCMC( data= d ,
                    zName="surv", NName="density", sName="tank",
                    cName="size", numSavedSteps= 5000 , thinSteps= 1 )
smryMCMC(mcmcCoda)
plotMCMC( mcmcCoda , data= d ,
          zName="surv", NName="density", sName="tank",
          cName="size",
          compVal=NULL ,
          diffCList=list( c("big","small") )
          
 str(mcmcCoda)  
 
 s1 = as.matrix(mcmcCoda)
 
 omega_diff <- s1[,"omega[2]"] - s1[,"omega[1]"] 
 hist(omega_diff) 
 
 
# simple jags model
 
 
 model_string <- "model{
  # Priors (here just using 'sloppy' uniform distributions)
 for(size in 1:2) {
 theta[[j]]~dbeta(a,b)
 }
 a ~ dunif(1,10)
 b ~ dunif(1,10)
 
 
 for(i in 1:48) {
 surv[i] <- dbinom(density[i],p[i])
 p[i] ~ theta[[i]]
 }
 }"

params_to_monitor <- c("a","b" ,"theta"  )
# Running the model
 ?jags.model
model <- jags.model( textConnection(model_string), data = d, n.chains = 3, n.adapt= 1000)

update(model, 1000); # Burning 1000 samples to the MCMC gods...
mcmc_samples <- coda.samples(model, params_to_monitor, n.iter=3000)
 

