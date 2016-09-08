x = rnorm(100,5,2)
y  = 265 + 3.2*x + rnorm(30,0,2)
x[91:100] <- NA

library(rethinking)
# Missing predictor values


f5 <- alist(
  y ~ dnorm( mu , sigma ),
  mu <- a + b*x,
  x ~ dnorm( mu_x, sigma_x ),
  mu_x ~ dnorm( 0 , 100 ),
  sigma_x ~ dcauchy(0,2),
  a ~ dnorm( 0 , 100 ),
  b ~ dnorm( 0  , 10 ),
  
  sigma ~ dcauchy(0,2)
)
m5 <- map2stan( f5 , data=list(y=y,x=x) )

postcheck(m5,prob = 0.95)
