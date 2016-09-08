n1 = 52 # men
y1 = 9  # left-handed men
n2 = 480 # women
y2 = 400  # left-handed women
a= 10
b=10
c=1
d =1


# SIMULATION
I = 10000 # simulations
theta1 = rbeta(I, y1+a, (n1-y1)+b) 
theta2 = rbeta(I, y2+c, (n2-y2)+d)
diff = theta1-theta2  # simulated diffs

# OUTPUT
quantiles = quantile(diff,c(0.005,0.025,0.5,0.975,0.995))

perc.rank <- function(x, xo) { length(x[x <= xo])/length(x)*100}
A = perc.rank(diff,-0.3)
B = perc.rank(diff, 0.3)
A-B

# VISUALIZATION
#png(file="bayesContingency.png")
hist(diff,col ="blue",main="Distribution of Difference",
       xlab="Difference in proportion in two groups")
