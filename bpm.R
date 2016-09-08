


bayespom = function( M,D, P, B, I,C){

library(rjags)
fit = jags.model(textConnection(M),data = D,n.chains = C )
# Burning
update(fit,n.iter = B)
# output as coda samples
output = coda.samples(model = fit, variable.names = P ,n.iter = I ) 

outputM  = as.matrix(output)
summary = summary(output)

 list(summary,outputM)

}





