library(ClinicalRobustPriors)

x = -1
y = 0.8
RRlow = 1- y
RRhigh =1- x

a= c(log(RRlow))
b= c(log(RRhigh))
CI = c(a,b)

# calculate mean and sd from 95% ci of logRR
LRRlow = x - 1.96* y
LRRhigh = x + 1.96*y
A =  matrix(data = c(1,-1.96,1,1.96),nrow =2,ncol=2,byrow =TRUE)
B = matrix (data = CI,nrow =2,ncol=1,byrow =FALSE)
round(solve(A,B),2)
G= round(solve(A,B),2)
equivN = 4/G[2]^2
equivN

priorparams = c(G[1],G[2],equivN)

logrelrat <- function(
  a1,N1,
  a2,N2) {
  log(((a1+.5)/(N1+.5))/
        ((a2+.5)/(N2+.5)))
}
sdlogrelrat <- function(
  a1,N1,
  a2,N2 ) {
  sqrt(1/(a1+.5)+1/(N1+.5) +1
       /(a2+.5)+1/(N2+.5))
}

RRstudy = logrelrat(13,163,40,165)

sdstudy = sdlogrelrat(13,163,40,165)
equivM = 4/sdstudy^2


realparams = c(RRstudy,sdstudy,equivM)

Cauchy.Normal(-0.46,-1.086,0.59,0.33,11.49,36.06)
Berger.Normal(-0.46,-1.086,0.59,0.33,11.49,36.06)
library(epitools)
M1 = matrix(c(13,150,40,125),2,2,byrow=TRUE)
M1

riskratio(M1)
