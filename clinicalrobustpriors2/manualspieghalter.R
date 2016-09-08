#Start with Fuquene et al pg.22 (http://ba.stat.cmu.edu/journal/2009/vol04/issue04/fuquene.pdf )




# real data 
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

# Data from prior stuy
a1= 8
b1 =1120
N1 = 1128
a2 = 92
b2 = 1023
N2 =1045


ORstudy = logoddrat(a1,b1,N1,a2,b2,N2)

sdstudy = sdlogoddrat(a1,b1,N1,a2,b2,N2)

equivN = 4/sdstudy^2


priorparams = c(ORstudy,sdstudy,equivN)

# Data from new study
c1= 70
d1 =1042
N3 = 1112
c2 = 135
d2 = 960
N4 = 1095


ORstudy2 = logoddrat(c1,d1,N3,c2,d2,N4)

sdstudy2 = sdlogoddrat(c1,d1,N3,c2,d2,N4)
equivM = 4/sdstudy2^2

realparams = c(ORstudy2,sdstudy2,equivM)



library(ClinicalRobustPriors)
Berger.Normal (ORstudy,ORstudy2,sdstudy,sdstudy2,equivN,equivM)

# Manual Calculation from Spieghalter book

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