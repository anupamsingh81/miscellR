n1 <- 60				# Number of females
n2 <- 40				# Number of males
mu1 <- 105				# Population mean of females
mu2 <- 77.5				# Population mean of males
sigma <- 2.75				# Average population SD of both
n <- n1+n2				# Total sample size
y1 <- rnorm(n1, mu1, sigma)		# Data for females
y2 <- rnorm(n2, mu2, sigma)		# Date for males
y <- c(y1, y2)				# Aggregate both data sets
x <- rep(c(0,1), c(n1, n2))		# Indicator for male

n= length(y)
# Define BUGS model

cat("
model
{
  # priors
    mu.f ~ dnorm(0, 0.001)
    delta ~ dnorm(0, 0.001)
    tau <- 1/(sigma*sigma)
    sigma ~ dunif(0,100)

  # likelihood
    for(i in 1:n)
    {
      y[i] ~ dnorm(mu[i], tau)
      mu[i] <- mu.f + delta*male[i]
    }

  # derived quantity
    mu.m <- mu.f + delta
}    
", file="t-test2.bug")

modelstring="
    model {
    
    # Priors
    mu1 ~ dnorm(0,0.001)			
    delta ~ dnorm(0,0.001)		
    tau <- 1/ (sigma * sigma)
    sigma ~ dunif(0, 10)
    
    # Likelihood
    for (i in 1:n) 
{
    y[i] ~ dnorm(mu[i], tau) 
    mu[i] <- mu1 + delta*x[i]
    residual[i] <- y[i] - mu[i]		# Define residuals
    }
    
    # Derived quantities: one of the greatest things about a Bayesian analysis
    mu2 <- mu1 + delta			# Difference in wingspan
    }
    "


# Bundle data
win.data <- list(y=y,x=x,n=n) # list y=y,x=x,n=n,difference from winbuggs where "x","y","n"

# Inits function
inits <- function(){list(mu1=rnorm(1), delta=rnorm(1), sigma = rlnorm(1))}

# Parameters to estimate
params <- c("mu1","mu2", "delta", "sigma" )

# MCMC settings
nc <- 2		# Number of chains
ni <- 5000	# Number of draws from posterior for each chain
nb <- 500		# Number of draws to discard as burn-in
nt <- 1		# Thinning rate


model=jags.model(textConnection(modelstring), data= win.data, n.chains = nc)
update(model, n.iter = nb) # keep gap n.iter after model
output=coda.samples(model=model,variable.names= params, n.iter= ni, thin= nt)
summary(output)

source("bayes.R")

# Kruschke style diagnostics

diagMCMC(output)
DbdaDensPlot(output)
DbdaAcfPlot(output)
HDIofMCMC(y)

output1 = as.matrix(output) # extracting coda samples for summary,plot kruschke style
 
Difference = output1[,"delta"] # Extracted sample vector:column from coda object s1
summarizePost(Difference,ROPE = c(-10,2),compVal = -20)
plotPost(Difference,cenTend = "mean",ROPE = c(-27,-20),compVal = -27)

# using ggmcmc 
library(ggmcmc)
S = ggs(output)
ggs_histogram(S)



#convert matrix to dataframe for ggplot
output2 = as.data.frame(output1)
ggplot(output2,aes = output2$delta)+geom_histogram()
qplot(output2$delta)
ROPE = c(-26,-23)
compVal = -27
Vec = output2$delta
percInROPE = ( sum( Vec > ROPE[1] & Vec < ROPE[2] )/ length( Vec ) )
pGtCompVal = sum( Vec > compVal ) / length( Vec ) 
HDIofMCMC(Vec)

plot(output)