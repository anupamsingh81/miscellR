require(rjags)
x=rnorm(15,25,2)
data=list(x=x,n=length(x))
hyper=list(a=3,b=11,cc=10,d=1/100)
init=list(mu=0,tau=1)
nchains = 2
nadapt = 500
nIter = 10000
thinchain = 1
parameters = c("mu","tau")
modelstring="
  model {
    for (i in 1:n) {
      x[i]~dnorm(mu,tau)
    }
    mu~dnorm(cc,d)
    tau~dgamma(a,b)
  }
"


model=jags.model(textConnection(modelstring), data=append(data,hyper), inits=init,n.chains=nchains,n.adapt = nadapt)
update(model,n.iter=100)
output=coda.samples(model=model,variable.names= parameters, n.iter= nIter, thin= thinchain)
print(summary(output))
plot(output)

# Posterior predictive check
library(jagsUI)
params = c('mu','tau')

out <- jags(data = append(data,hyper),
            
            parameters.to.save = params,
            model.file = textConnection(modelstring),
            n.chains = nchains,
            n.adapt = nadapt,
            n.iter = nIter,
            n.burnin = 500,
            n.thin = thinchain)

pp.check(out, actual='fit', new='fitn')

# doing Kruschke type analysis
getwd()
source("bayes.R")
output1 = as.matrix(output)

diagMCMC(output)
DbdaDensPlot(output)
DbdaAcfPlot(output)
HDIofMCMC(output1[,1])
summarizePost(output1[,1])
mu = output1[,1] # Extracted sample vector:column from coda object s1
tau = output1[,2]
summarizePost(mu,ROPE = c(12,17),compVal = 14)
plotPost(mu,cenTend = "mean",ROPE = c(10,38),compVal = 10)

# using ggmcmc with coda (http://xavier-fim.net/packages/ggmcmc/)
library(ggmcmc)
S = ggs(output)
# output is alist with 2 mcmc chains, ggs() produces a data frame object with four variables, namely:
#Iteration Number of iteration.
#Chain Number of the chain.
#Parameter Name of the parameter.
#value value sampled.
#More specifically, ggs() produces a data frame tbl, which is a wrapper in dplyr that improves printing data frames. In this case, calling the object produces a compact view of its contents.
S
str(S)
p = ggmcmc(S)
ggsave("p.pdf")

ggs_histogram(S)
ggs_density(S)
ggs_traceplot(S)
ggs_running(S)

ggs_compare_partial(S)

ggs_autocorrelation(S)

ggs_crosscorrelation(S)
ggs_Rhat(S) + xlab("R_hat")
ggs_geweke(S)
ggs_caterpillar(S)

ci(S)



HDIofMCMC(female)


# Second Study
require(rjags)
# Linear regreesion,ttest, imp state precision in jags instead of SD contrast from rethinking
y1 = rnorm(30,60,10)
y2 = rnorm(40,100,20)
Sex = c(rep(0,30),rep(1,40))
y = c(y1,y2)



hyper1 = list(
         
  k =0 ,
  l = 0.001,
         m = 0,
         o = 0.001,
         e = 0 ,
         f = 100) 
data1=list(y=y,Sex=Sex,n= length(y))      
init1=list(a= 60,b = 20, sigma = 5)
nchains1 = 2
nadapt1 = 500
nIter1 = 10000
thinchain1 = 1
parameters1 = c("a","b","sigma")
modelstring1="
  model {
    for (i in 1:n) {
      
    y[i]~ dnorm(mu[i],tau)
  mu[i] <- a + b*Sex[i] }

  a ~ dnorm(k,l)
  b ~ dnorm(m,o)
  tau <- pow(sigma, -2)
  sigma~dunif(e,f)
  
   
  }
"

# Kruschke implementation

model1=jags.model(textConnection(modelstring1), data = append(data1,hyper1), inits=init1,n.chains=nchains1,n.adapt = nadapt1)
update(model1,n.iter=100)
output2=coda.samples(model=model1,variable.names= parameters1, n.iter= nIter1, thin= thinchain1)
print(summary(output2))
plot(output2)
