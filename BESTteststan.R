library(BEST)
library(rjags)
library(rethinking)

y1 = rnorm(30,60,10)
y2 = rnorm(40,100,20)
Sex = c(rep(0,30),rep(1,40))
y = c(y1,y2)
z = data.frame(y,Sex)
dataMat = as.matrix(z)
nPerChain = 2000
mcmcChain
lm1 <- lm(y ~ Sex, data = z)
summary(lm1)

# JAGS

falcon.data.2 <- list(y= z$y,
                      male=z$Sex,
                      N=70)

cat("
model
{
  # priors
    mu.f ~ dnorm(0, 0.001)
    delta ~ dnorm(0, 0.001)
    tau <- 1/(sigma*sigma)
    sigma ~ dunif(0,100)

  # likelihood
    for(i in 1:N)
    {
      y[i] ~ dnorm(mu[i], tau)
      mu[i] <- mu.f + delta*male[i]
    }

  # derived quantity
    mu.m <- mu.f + delta
}    
", file="t-test2.bug")
library(R2jags)
model.fit <- jags(data=falcon.data.2, 
                  model.file="t-test2.bug",
                  parameters.to.save=c("mu.f", "mu.m", "sigma", "delta"),
                  n.chains=3,
                  n.iter=2000,
                  n.burnin=1000,
                  DIC=FALSE)


plot(as.mcmc(model.fit))
model.fit

# jags.model version 
nchains=2
n.iter=2000
n.burnin=1000
thinSteps = 1
parameters = c("mu.f", "mu.m", "sigma", "delta")
jagsModel = jags.model( "t-test2.bug", data=falcon.data.2   ,  
                        n.chains= nchains , n.adapt= n.burnin)
codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                            n.iter=n.iter , thin=thinSteps )
mcmcChain = as.matrix( codaSamples )

HPDinterval(codaSamples)

# playing with samples
MChain = as.data.frame(mcmcChain)
samples <- sample(MChain$delta,  size=1e4 , replace=TRUE )
plot(samples)
dens(samples)

# CompVal
sum( samples < 50) / 1e4
# ROPE
sum( samples > 10 & samples < 30 ) / 1e4
# HPDI
rethinking::HPDI(samples,prob= 0.95)




openGraph(width=7,height=7)
autocorr.plot( codaSamples[[1]] , ask=FALSE )
show( gelman.diag( codaSamples ) )
effectiveChainLength = effectiveSize( codaSamples ) 
show( effectiveChainLength )
mcmcChain = as.matrix( codaSamples )
mcmcChain
diagMCMC(codaSamples)
DbdaAcfPlot(codaSamples)
DbdaDensPlot(codaSamples)
HDIofMCMC(mcmcChain)


# Rethinking

library(rethinking)

f <- alist(
  
 
  
  y ~ dnorm(mu,sigma),
  mu <- a + b*Sex ,
  a ~ dnorm(0,10),
  b ~ dnorm(0,10),
  sigma~dunif(0,100)
  
)


fit <- map( 
  f , 
  data= z
  
  
)
precis(fit)
fit2stan = map2stan(f,data =z,chains = 2)
precis(fit2stan)

post <- extract.samples( fit2stan )
str(post)
pairs(post)
pairs(fit2stan)
# Model Check
rstan::show(fit2stan)
# Traceplots
rstan::plot(fit2stan)

# playing with samples
SChain = as.data.frame(fit2stan@stanfit@sim$samples)

samples1 <- sample(SChain$b,  size=1e3, replace = TRUE)
plot(samples1)
dens(samples1)

# CompVal
sum( samples1 < 50) / 1e3
# ROPE
sum( samples1 > 10 & samples1 < 30 ) / 1e3
# HPDI
rethinking::HPDI(samples1,prob= 0.95)


# Stan to coda

library(coda)
stan2coda <- function(fit) {
  mcmc.list(lapply(1:ncol(fit), function(x) mcmc(as.array(fit)[,x,])))
}
s = stan2coda(fit2stan@stanfit) # extracting stanfit object from fit2stan, i have to use @ since , it is S4 object
s1 = as.matrix(s) # converting list to matrix, as easy to do calculations later


# using DBDA kruschke utilities with coda object,
#read function input parameters with interest,it is easy

diagMCMC(s)
DbdaDensPlot(s)
DbdaAcfPlot(s)
HDIofMCMC(y1)
summarizePost(y1)
female = s1[,"a"] # Extracted sample vector:column from coda object s1
Difference = s1[,"b"]
summarizePost(Difference,ROPE = c(10,30),compVal = 50)
plotPost(Difference,cenTend = "mean",ROPE = c(10,38),compVal = 40)





HDIofMCMC(female)
# we can now apply all coda functionalities

gelman.diag(s)
effectiveSize(s)


s1 = as.matrix(s)
s1[1]

str(fit2stan@stanfit@sim$samples$a)


