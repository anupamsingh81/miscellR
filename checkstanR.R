source("bpm.R")

x1 = rnorm(30,5,2) 
x2 = rnorm(30,10,6)
y = 2*x1 + 5*x2 + 23 + rnorm(30,0,4)

y[4] = 56
y[6] = 58
D = list(y=y,x1=x1,x2=x2,N=length(y))
P = c("alpha","beta1","beta2","sigma")
I = 2000
B = 500
C =2

D1 = list(N = 32,y = append(y,c(NA,NA)), x1 = append(x1,c(5,7)) , x2 = append(x2,c(10,12)))
P1 = c("y")

M = "
model{
for (i in 1:N ){
y[i]~dnorm(alpha+beta1*x1[i]+beta2*x2[i],1/sigma^2)
}
# prior 
alpha~ dnorm(0,0.001)
beta1~ dnorm(0,0.001)
beta2 ~ dnorm(0,0.001)
sigma~ dunif(0,50)

}
"

Mcheck = "
model{
for (i in 1:N ){
y[i]~dnorm(mu[i],1/sigma^2)
mu[i]  <-alpha+beta1*x1[i]+beta2*x2[i]
}
# Assess model fit using a sum-of-squares-type discrepancy
for (i in 1:N) {
predicted[i] <- mu[i]             # Predicted values
residual[i] <- y[i]-predicted[i]  # Residuals for observed data                                     
sq[i] <- pow(residual[i], 2)      # Squared residuals

# Generate replicate data and compute fit statistics for them
y.new[i]~dnorm(mu[i], 1/sigma^2)        # One new data set at each MCMC iteration
sq.new[i] <- pow(y.new[i]-predicted[i], 2)  # Squared residuals for new data
}
fit <- sum(sq[])              # Sum of squared residuals for actual data set
fit.new <- sum(sq.new[])      # Sum of squared residuals for new data set
test <- step(fit.new-fit) 		# Test whether new data set more extreme
bpvalue <- mean(test) 		  	# Bayesian p-value

# prior 
alpha~ dnorm(0,0.001)
beta1~ dnorm(0,0.001)
beta2 ~ dnorm(0,0.001)
sigma~ dunif(0,50)

}
"



params <- c("alpha","beta1","beta2","sigma", "fit", "fit.new", "bpvalue", "residual", "predicted")

# normal Result
Result = bayespom(M = M,D,P = P ,B,I,C)


Resultcheck = bayespom(M = Mcheck,D,P = params ,B,I,C)
Resultcheck[1]

#predict

Resultpredict = bayespom(M,D = D1,P =P1,B,I,C)

predcheck = bayespom(M = Mpredcheck,D = D1,P =params,B,I,C)

predcheck[1]

Resultpredict[1]

prediction = Resultpredict[2]
newprediction = data.frame(prediction)
newpreds = newprediction[,31:32]
summary(newpreds)
hist(newpreds$y.31.)

# similarly can calculatr ROPE and compVal
+
# posterior predictive checks in JAGS ,will deal later


# Let us try to do it in stan
library(rstan)



stanmodelcode = " 
data {                      // Data block
  int<lower=1> N;           // Sample size
  vector[N] x1;
  vector[N] x2;
  vector[N] y;
}

  parameters {                // Parameters block
    real beta1;                        // Coefficient vector
    real beta2 ;
    real alpha;
    real<lower=0> sigma;      // Error scale
    real y_rep[N]; // post. pred.
  }
model {                    
 
  // priors
  beta1 ~ normal(0, 10);
  beta2 ~ normal(0, 10);
  alpha ~ normal(0, 100);
  sigma ~ cauchy(0, 5);     // With sigma bounded at 0, this is half-cauchy
  // likelihood
  y ~ normal(alpha+beta1*x1+beta2*x2, sigma);
  y_rep ~ normal(alpha+beta1*x1+beta2*x2,sigma);

}

generated quantities{
    real minimum;
    real maximum;
    minimum <- min(y_rep);
    maximum <- max(y_rep);
}

  "

fit <- stan(model_code =stanmodelcode, data=list(y = y, x1 =x1, x2 =x2, N= length(y)),
            iter=2000,warmup =500,chains=2)   
summary(fit)

launch_shinystan(fit)

# Extracting samples
fitnu = extract(fit)

# can calculate ROPE and compVal

hist(fitnu$alpha)
# check

n_sims <- length(fitnu$lp__)
J = length(y)
# we have cretaed an empty array of 30 y variables to be simulated 3000 times
y_Repl <- array(NA, c(n_sims, J))

# we have tried to simulate 3000 rows of y with calculated relevant alpha,beta,sigma for all 30 columns
for (s in 1:n_sims)
  y_Repl[s,] <- rnorm(J,fitnu$alpha[s]+fitnu$beta1[s]*x1+fitnu$beta2[s]*x2, fitnu$sigma[s] )


str(y_Repl)
rm(yRep)
min_repl = apply(y_Repl, 2, min)
min_y = min(y)
hist(min_rep, main=''); abline(v=min_y)
c(mean(min_rep), min_y)

prop.table(table(min_rep>min_y))

par(mfrow=c(5,4), mar=c(4,4,2,2))
hist(y, xlab="", main="y")
for(s in 1:19)
  hist(y_Repl[s,], xlab="", main=paste("y_rep",s))

# test

test <- function(y){
  y_sort <- rev(sort(y))
  return(y_sort[1] - y_sort[2])
}

?rev
t_y <- test(y)
t_rep <- rep(NA, n_sims)
for(s in 1:n_sims)
  t_rep[s] <- test(y_Repl[s,])
#

par(mfrow=c(1,1))
cat("T(y) =", round(t_y,1), " and T(y_rep) has mean",
    round(mean(t_rep),1), "and sd", round(sd(t_rep),1),
    "\nPr (T(y_rep) > T(y)) =", round(mean(t_rep>t_y),2), "\n
")
hist0 <- hist(t_rep, xlim=range(t_y,t_rep), xlab="T(y_r
ep)")
lines(rep(t_y,2), c(0,1e6))
text(t_y, .9*max(hist0$count), "T(y)", adj=0)

sort(y)[1:5]
sort(y_Repl)[1:5]
launch_shinystan(fit)




# launch shinystan
library(shinystan)
launch_shinystan(fit)


# rethinking package is very intuitive,does things with minimal fuss
library(rethinking)
D1 = list(y=y,x1=x1,x2=x2)
m1 <- map2stan( 
  alist(
   y ~ dnorm(mu,sigma),
   mu <- alpha + beta1*x1 + beta2* x2,
   alpha ~ dnorm(5,25),
   beta1 ~ dnorm(0,10),
   beta2 ~ dnorm(0,20),
   sigma ~ dcauchy(0,1)
  ) ,
  data = D1 , warmup=1000 , iter= 2000 , chains= 2)

# summary
precis(m1)
# postcheck
postcheck(m1)
# predict by link function
d2 = list(x1 = c(28,56),x2 = c(37,42))

link.m1 <- link( m1 , data=d2)
apply( link.m1 , 2 , mean )
apply( link.m1 , 2 , PI )
apply(link.m1,2, HPDI,prob =0.95 )

# pretty neat


m1fit = m1@stanfit
summary(m1fit)
# 
m1fitnu = extract(m1fit)

library(coda)
stan2coda <- function(fit) {
  mcmc.list(lapply(1:ncol(fit), function(x) mcmc(as.array(fit)[,x,])))
}

s  = stan2coda(m1@stanfit)
s1 = as.matrix(s)
s1$alpha

# for particular value of x1 = 50, x2 =68

predxn = s1[,"alpha"] + s1[,"beta1"]*50 + s1[,"beta2"]*68
quantile(predxn,c(0.025,0.975))

# for vectors

predxnvector = s1[,"alpha"] + s1[,"beta1"]*d2$x1 + s1[,"beta2"]*d2$x2


m1@coef
# let us plot residuals and fits
ypredicted = mean(s1[,"alpha"]) + mean(s1[,"beta1"])*x1 + mean(s1[,"beta2"])*x2
residual  = ypredicted - y

# predictive check
plot(residual,x1)
plot(residual,x2)
plot(y,ypredicted)


# new predictions say

sims = arm::sim(m1fit,1000)

postcheck(fit)

# sim function in rethinking
sim.height <- sim( m4.3 , data=list(weight=weight.seq) , n=1e4 )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )


post <- extract.samples(m4.3, n=1e4)
weight.seq <- 25:70
sim.height <- sapply( weight.seq , function(weight)
  rnorm(
    n=nrow(post) ,
    mean=post$a + post$b*weight ,
    sd=post$sigma ) )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

# link function in rethinking

post <- extract.samples(m4.3)
mu.link <- function(weight) post$a + post$b*weight
weight.seq <- seq( from=25 , to=70 , by=1 )
mu <- sapply( weight.seq , mu.link )
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89 )




# post check function

pred <- link(fit,n=n)
sims <- sim(fit,n=n)

mu <- apply( pred , 2 , mean )
mu.PI <- apply( pred , 2 , PI , prob=prob )
y.PI <- apply( sims , 2 , PI , prob=prob )


# link function simulated

fitter  = extract(fit)

mu.link <- function(x1,x2) {A = fitter$alpha + fitter$beta1*x1 + fitter$beta2*x2
 return(A)}

beta = cbind(mean(fitter$alpha),mean(fitter$beta1),mean(fitter$beta2))
beta = as.matrix(beta)
intercept = c(1,1)
m1 = c(30,60)
m2  = c(70,90)
X = cbind(intercept,m1,m2)
X = as.matrix(X)
fitter$beta1
A = X %*% beta
str(A)


mu = mu.link(x1 = m1, x2 =m2)
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.95 )










-