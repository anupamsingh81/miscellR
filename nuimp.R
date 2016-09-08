library(ClinicalRobustPriors)

x =0.5
y = -1
ORlow = 1- y
ORhigh =1- x

a= c(log(ORlow))
b= c(log(ORhigh))
CI = c(a,b)

# calculate mean and sd from 95% ci of logOR
LORlow = x - 1.96* y
LORhigh = x + 1.96*y
A =  matrix(data = c(1,-1.96,1,1.96),nrow =2,ncol=2,byrow =TRUE)
B = matrix (data = CI,nrow =2,ncol=1,byrow =FALSE)
round(solve(A,B),2)
G= round(solve(A,B),2)
equivN = 4/G[2]^2
equivN

priorparams = c(G[1],G[2],equivN)

logoddrat <- function(
  a1,b1=N1-a1,N1=a1+b1,
  a2,b2=N2-a2,N2=a2+b2) {
  log(((a1+.5)/(b1+.5))/
        ((a2+.5)/(b2+.5)))
}
sdlogoddrat <- function(
  a1,b1=N1-a1,N1=a1+b1,
  a2,b2=N2-a2,N2=a2+b2) {
  sqrt(1/(a1+.5)+1/(b1+.5) +1
       /(a2+.5)+1/(b2+.5))
}

ORstudy = logoddrat(13,150,163,40,125,165)

sdstudy = sdlogoddrat(13,150,163,40,125,165)
equivM = 4/sdstudy^2


realparams = c(ORstudy,sdstudy,equivM)

Cauchy.Normal(0,-1.28,0.35,0.34,32.3,35.3)
Berger.Normal(0,-1.28,0.35,0.34,32.3,35.3)