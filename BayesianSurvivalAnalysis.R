#BBayesian Survival Analysis (
#  https://rawgit.com/petrkeil/Blog/master/2015_05_13_Survival_analysis/survival_analysis.html)

  time = 0:100
  mu = 50 # mean time to death in weeks
  lambda.t = rep(1/mu, times=length(time)) # hazard, here constant
  f.t = (exp(-time/mu))/mu # death density (number of deaths per week)
  S.t = exp(-time/mu) # survival function
  F.t = 1- S.t
  
  par(mfrow=c(2,2), mai=c(0.8,0.8,0,0))
  plot(time, lambda.t, type="l")
  plot(time, f.t, type="l")
  plot(time, S.t, type="l")
  plot(time, F.t, type="l")
  
  
  seedlings <- read.table("http://goo.gl/chMvEo", header=TRUE)
  
  summary(seedlings)
  
  attach(seedlings)
  status <- 1*(death>0)
  status # there are no censored observations
  library(survival)
  
  model.par <- survreg(Surv(death)~1, dist="exponential")
  model.par
  
  mu = exp(model.par$coeff)
  
  time=0:25
  S.t = exp(-time/mu)
  f.t = exp(-time/mu)/mu 
  deaths <- tapply(X=status, INDEX=death, FUN = sum)
  survs <- (sum(deaths)-cumsum(deaths))/sum(deaths) 
  death.data <- data.frame(day=as.numeric(names(survs)), 
                           survs=as.numeric(survs))
  
  par(mfrow=c(1,2))
  plot(death.data, pch=19, ylab="S(t)", xlab="Weeks",
       main="Survival")
  lines(time, S.t, col="red")
  hist(seedlings$death, freq=FALSE, main="Failure density",
       xlab="Weeks", ylim=c(0,0.15))
  lines(time, f.t, col="red")
  
  # Same model in jags
  library(runjags)
  library(coda)
  new.t <- seq(0,25, by=0.5) # this will be used for prediction
  
  # put the data into list for JAGS
  surv.data = list(t.to.death = seedlings$death,
                   N = nrow(seedlings),
                   new.t = new.t,
                   new.N = length(new.t))
  
  
  cat("
    model
      {
      # prior
      lambda ~ dgamma(0.01, 0.01)
      
      # likelihood
      for(t in 1:N)
      {
      t.to.death[t] ~ dexp(lambda)
      }
      # mean time to death
      mu <- 1/lambda
      
      # predictions
      for(i in 1:new.N)
      {
      S.t[i] <- exp(-new.t[i]/mu)
      }
      }
      ", file="survival_exp.txt")
  
  seedlingfit <- run.jags(data = surv.data, 
                 model = "survival_exp.txt", 
                 monitor = c("mu","lambda"),
                 sample = 1000, burnin = 1000, n.chains = 3)
  
  densplot(as.mcmc(seedlingfit), show.obs=FALSE)
  
  seedlingfit
  
  S.t <- run.jags(data = surv.data, 
                  model = "survival_exp.txt", 
                  monitor = c("S.t"),
                  sample = 2000, burnin = 1000, n.chains = 3)
  S.t <- summary(S.t)
  
  plot(death.data, pch=19, xlab="Weeks", ylab="S(t)",
       main="Survival", ylim=c(0,1))
  lines(new.t, S.t[,'Lower95'], lty=2, col="red")
  lines(new.t, S.t[,'Median'], col="red")
  lines(new.t, S.t[,'Upper95'], lty=2, col="red")
  legend("topright", legend=c("Median prediction","95 perc. prediction"), 
         lty=c(1,2), col=c("red", "red"))
  
 
  detach(seedlings) 
# Second study  (Censored data)
  
  
  
  cancer <- read.table("http://goo.gl/3cnoam", header=TRUE)
summary(cancer)

# preparing data for jags

censored <- cancer$status==0
is.censored <- censored*1
t.to.death <- cancer$death
# codiing censored individuals as NA,thus retaining only uncensored individuals
t.to.death[censored] <- NA
t.to.death

# times of those who have been censored, rest coded 0
t.cen <- rep(0, times=length(censored))
t.cen[censored] <- cancer$death[censored] 
t.cen
# put the data together for JAGS
cancer.data <- list(t.to.death = t.to.death,
                    t.cen = t.cen,
                    N = nrow(cancer),
                    group = rep(1:4, each=30))

# Model

cat("
    model
    {
      # priors
      for(j in 1:4)
      {
        # prior lambda for each group
        lambda[j] ~ dgamma(0.001, 0.001)
        mu[j] <- 1/lambda[j] # mean time to death
      }
      # likelihood
      for(i in 1:N)
      {
        is.censored[i] ~ dinterval(t.to.death[i], t.cen[i])
        t.to.death[i] ~ dexp(lambda[group[i]])
      }
   }
   ", file="survival_cancer.txt")

library(runjags)
library(coda)

cancer.fit <- run.jags(data = cancer.data, 
                       model = "survival_cancer.txt", 
                       monitor = c("mu","lambda"),
                       sample = 1000, burnin = 1000, n.chains = 3)

# plotting
densplot(as.mcmc(cancer.fit), xlim=c(0,20))

cancer.fit

# mu[1:3] are for the treatments, mu[4] is the placebo.

