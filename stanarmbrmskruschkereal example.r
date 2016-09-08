setwd("/home/anupam/stan")
library(lme4)
library(rstan)
library(rstanarm)
library(brms)
library(shinystan)


newdata = data.frame(y,time,treatment,person)
M1 = lmer(y ~ time:treatment + (1 + time | person) )

     
summary(M1) 

# MErtOOLs
library(merTools)
shinyMer(M1, simData = newdata[1:100, ])

M2 = stan_lmer(y ~ time:treatment + (1+ time | person) ,prior = normal(0,5),prior_intercept = normal(0,5))
launch_shinystan(M2)
#brms
M3 = brm(y ~ time:treatment + (1+ time | person) ,data = newdata,family= gaussian,prior = c(set_prior("normal(0,5)",class= "b"),set_prior("normal(0,5)",class= "sd")),n.warmup =500,n.iter= 2000,n.chains = 2)
launch_shiny(M3)
summary(M3)

# constructing kruschke style plots

source(("/home/anupam/bayes.R"))# kruschke functions for histogram
# stan2 coda function for converting stanfit object to coda
stan2coda <- function(fit) {
  mcmc.list(lapply(1:ncol(fit), function(x) mcmc(as.array(fit)[,x,])))}


# extracting stanfit from stanarm object

str(M2) # we note there is a stanfit object

M2fit = M2$stanfit (M2 is s3 so M2$ stanfit successfully extracts)

 matM2fit = as.matrix(M2fit) # converting list to matrix, as easy to do calculations laterdaM2 = stan2coda(M2fit)
 str(matM2fit)
 colnames(matM2fit) # names of parameters
 Interxn = matM2fit[,"time:treatment"] # Extracted sample vector:column from coda object s1
 plotPost(Interxn,cenTend = "mean",ROPE = c(-0.25,0.2),compVal = -0.3)
 summarizePost(Interxn,ROPE = c(-0.25,0.2),compVal = -0.3)
# 
# # extracting stanfit from brms object
 str(M3)
 M3fit = M3$fit (M3 is s3 object so M2$ stanfit successfully extracts) # note here it is named fit not stanfit,be careful
 matM3fit = as.matrix(M3fit) # converting list to matrix, as easy to do calculations laterdaM2 = stan2coda(M2fit)
 str(matM3fit)
 colnames(matM3fit) # names of parameters
 Interxn2 = matM3fit[,"b_time:treatment"] #
 plotPost(Interxn2,cenTend = "mean",ROPE = c(-0.25,0.2),compVal = -0.3)
 summarizePost(Interxn2,ROPE = c(-0.25,0.2),compVal = -0.3)
 
 
 
library(shinystan)
launch_shinystan_demo()



