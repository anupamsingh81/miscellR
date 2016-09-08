#Start with example spieghalter book pg.69
# Prior Calculation

x =  -1 # corrsponds to 100%  incr in mortality
y = 0.5 # corresponds to maximum 50% reduction in mortality
ORlow = 1- y
ORhigh =1- x

a= c(log(ORlow))
b= c(log(ORhigh))
CI = c(a,b) # confidence interval of log Odds ratio
CI

# calculate mean and sd from 95% ci of logOR
LORlow = x - 1.96* y
LORhigh = x + 1.96*y
A =  matrix(data = c(1,-1.96,1,1.96),nrow =2,ncol=2,byrow =TRUE)
B = matrix (data = CI,nrow =2,ncol=1,byrow =FALSE)
round(solve(A,B),2)
G= round(solve(A,B),2)# first item on mean of logOR = -0.26 ,second item sd of logOR
ORprior = G[1]
sdprior = G[2]
equivN = 4/G[2]^2 # 4/sd^2 is equivalent N of study
equivN


priorparams = c(ORprior,sdprior,equivN) #(1st item is mean,2nd item is sd,third item equivalent number)
priorparams

# New study
#Mortality in test (13,150,163) # 13 deaths-a1 ,150 alive-b1,163 patients-N1
a1 = 13
b1 = 150
N1 = 163
#Mortality in control (23,125,148) # 23 deaths -a2,125 alive-b2,148 patients-N2.
a2= 40
b2 =125
N2 = 165

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

ORstudy = logoddrat(a1,b1,N1,a2,b2,N2)
sdstudy= sdlogoddrat(a1,b1,N1,a2,b2,N2)

equivM = 4/sdstudy^2

realparams = c(ORstudy,sdstudy,equivM)

realparams



library(ClinicalRobustPriors)

Berger.Normal (ORprior,ORstudy,sdprior,sdstudy,equivN,equivM)

# Manual calculation
lastlogOR = (priorparams[3]*priorparams[1]+realparams[3]*realparams[1])/(priorparams[3]+realparams[3])
lastlogOR
lastSD = 2/sqrt(equivN+equivM)
lastSD
lastlogORCIup=lastlogOR+1.96*lastSD
lastlogORCIlow=lastlogOR - 1.96*lastSD
m  = exp(lastlogORCIup)
n = exp(lastlogORCIlow)
finalOR = c(m,n)
finalOR
