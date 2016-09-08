
# rstan example,hierarchical linear model

# Simulate data

# Set Seed
set.seed(1234)

# Number of Households
H=100

# True parameters
true.b.mu = c(10,-5)
true.b.sig = c(3,2)
true.y.sig = 3

# Storage 
b =  matrix(0,100,2)
ctr = 0
# Each "household" can have between 5 and 10 reps
Ti = sample(5:10,H,replace=T)
id = x = y = matrix(0,sum(Ti),1)

# Simulate Data
for(i in 1:100) {
  b[i,1] = rnorm(1,true.b.mu[1],true.b.sig[1])
  b[i,2] = rnorm(1,true.b.mu[2],true.b.sig[2])	
  for(j in 1:Ti[i]) {
    ctr = ctr + 1
    x[ctr] = runif(1)*3 + 1
    y[ctr] = b[i,1]+b[i,2]*x[ctr]+rnorm(1,0,true.y.sig)
    id[ctr] = i 
  }
}

