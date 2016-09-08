require(rjags)
x=rnorm(15,25,2)
data=list(x=x,n=length(x))
hyper=list(a=3,b=11,cc=10,d=1/100)
init=list(mu=0,tau=1)
modelstring="
  model {
    for (i in 1:n) {
      x[i]~dnorm(mu,tau)
    }
    mu~dnorm(cc,d)
    tau~dgamma(a,b)
  }
"
model=jags.model(textConnection(modelstring), data=append(data,hyper), inits=init)
update(model,n.iter=100)
output=coda.samples(model=model,variable.names=c("mu", "tau"), n.iter=10000, thin=1)
print(summary(output))
plot(output)