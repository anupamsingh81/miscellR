# Ice cream data, glm in R(http://www.magesblog.com/2015/08/generalised-linear-models-in-r.html)

icecream <- data.frame(
  temp=c(11.9, 14.2, 15.2, 16.4, 17.2, 18.1, 
         18.5, 19.4, 22.1, 22.6, 23.4, 25.1),
  units=c(185L, 215L, 332L, 325L, 408L, 421L, 
          406L, 412L, 522L, 445L, 544L, 614L)
)
str(icecream)

# Basic plot
basicPlot <- function(...){
  plot(units ~ temp, data=icecream, bty="n", lwd=2,
       main="Number of ice creams sold", col="#00526D", 
       xlab="Temperature (Celsius)", 
       ylab="Units sold", ...)
  axis(side = 1, col="grey")
  axis(side = 2, col="grey")
}
basicPlot()

# Least square line

lsq.mod <- lsfit(icecream$temp, icecream$units)
basicPlot()
abline(lsq.mod, col="orange", lwd=2)
legend(x="topleft", bty="n", lwd=c(2,2), lty=c(NA,1),
       legend=c("observation", "linear least square"),
       col=c("#00526D","orange"),  pch=c(1,NA))
# GLM model
lin.mod <- glm(units ~ temp, data=icecream, 
               family=gaussian(link="identity"))
library(arm) # for 'display' function only
display(lin.mod)

# Thus, to mimic my data I could generate random numbers from the following Normal distribution:
#yi∼N(μi,σ2) with μi=−159.5+30.1xi and σ=38.1
#yi∼N(μi,σ2) with μi=−159.5+30.1xi and σ=38.1
#Although the linear model looks fine in the range of temperatures observed, it doesn't make much sense at 0ºC. The intercept is at -159, which would mean that customers return on average 159 units of ice cream on a freezing day. Well, I don't think so.

#Log-transformed linear regression
#Ok, perhaps I can transform the data first. Ideally I would like ensure that the transformed data has only positive values. The first transformation that comes to my mind in those cases is the logarithm.

log.lin.mod <- glm(log(units) ~ temp, data=icecream, 
                   family=gaussian(link="identity"))
display(log.lin.mod)

log.lin.sig <- summary(log.lin.mod)$dispersion
log.lin.pred <- exp(predict(log.lin.mod) + 0.5*log.lin.sig)
basicPlot()
lines(icecream$temp, log.lin.pred, col="red", lwd=2)
legend(x="topleft", bty="n", lwd=c(2,2), lty=c(NA,1),
       legend=c("observation", "log-transformed LM"),
       col=c("#00526D","red"), pch=c(1,NA))

# This plot looks a little better than the previous linear model and it predicts that I would sell, on average, 82 ice creams when the temperature is 0ºC:
exp(coef(log.lin.mod)[1] + 0.5 * log.lin.sig)

# Poisson
pois.mod <- glm(units ~ temp, data=icecream, 
                family=poisson(link="log"))
display(pois.mod)

pois.pred <- predict(pois.mod, type="response")
basicPlot()
lines(icecream$temp, pois.pred, col="blue", lwd=2)
legend(x="topleft", bty="n", lwd=c(2,2), lty=c(NA,1),
       legend=c("observation", "Poisson (log) GLM"),
       col=c("#00526D","blue"), pch=c(1,NA))


#From the coefficients I can read off that a 0ºC I expect to sell exp(4.45)=94expa(4.45)=94 ice creams and that with each one degree increase in temperature the sales are predicted to increase by exp(0.076)−1=7.9%expa(0.076)−1=7.9%.

#Note, the exponential function turns the additive scale into a multiplicative one.

# However, my model will also predict that I should expect to sell over 1000 ice creams if the temperature reaches 32ºC:
predict(pois.mod, newdata=data.frame(temp=32), type="response")

#Perhaps the exponential growth in my model looks a little too good to be true. Indeed, I am pretty sure that my market will be saturated at around 800. Warning: this is a modelling assumption from my side!
  #Binomial regression
#Ok, let's me think about the problem this way: If I have 800 potential sales then I'd like to understand the proportion sold at a given temperature.

#This suggests a binomial distribution for the number of successful sales out of 800. The key parameter for the binomial distribution is the probability of success, the probability that someone will buy ice cream as a function of temperature.

#Dividing my sales statistics by 800 would give me a first proxy for the probability of selling all ice cream.

#Therefore I need an S-shape curve that maps the sales statistics into probabilities between 0 and 100%.
market.size <- 800
icecream$opportunity <- market.size - icecream$units
bin.glm <- glm(cbind(units, opportunity) ~ temp, data=icecream, 
               family=binomial(link = "logit"))
display(bin.glm)

bin.pred <- predict(bin.glm, type="response")*market.size
basicPlot()
lines(icecream$temp, bin.pred, col="purple", lwd=2)
legend(x="topleft", bty="n", lwd=c(2,2), lty=c(NA,1),
       legend=c("observation", "Binomial (logit) GLM"),
       col=c("#00526D","purple"),  pch=c(1,NA))
plogis(coef(bin.glm)[1])*market.size
# Sales at 35 Celsius
plogis(coef(bin.glm)[1] +  coef(bin.glm)[2]*35)*market.size


# Prediction for all temperatures

temp <- 0:35
p.lm <- predict(lin.mod, data.frame(temp=temp), type="response")
p.log.lm <- exp(predict(log.lin.mod, data.frame(temp=0:35), type="response") + 
                  0.5 * summary(log.lin.mod)$dispersion)
p.pois <- predict(pois.mod, data.frame(temp=temp), type="response")
p.bin <- predict(bin.glm, data.frame(temp=temp), type="response")*market.size 
basicPlot(xlim=range(temp), ylim=c(-20,market.size))
lines(temp, p.lm, type="l", col="orange", lwd=2)
lines(temp, p.log.lm, type="l", col="red", lwd=2)
lines(temp, p.pois, type="l", col="blue", lwd=2)
lines(temp, p.bin, type="l", col="purple", lwd=2)
legend(x="topleft", 
       legend=c("observation", 
                "linear model",
                "log-transformed LM",
                "Poisson (log) GLM",
                "Binomial (logit) GLM"),
       col=c("#00526D","orange", "red", 
             "blue", "purple"),  
       bty="n", lwd=rep(2,5), 
       lty=c(NA,rep(1,4)),
       pch=c(1,rep(NA,4)))

# simulation
n <- nrow(icecream)
A <- model.matrix(units ~ temp, data=icecream)
set.seed(1234)
(rand.normal <- rnorm(n,
                      mean=A %*% coef(lin.mod),
                      sd=sqrt(summary(lin.mod)$dispersion)))
(rand.logtrans <- rlnorm(n,
                         meanlog=A %*% coef(log.lin.mod),
                         sdlog=sqrt(summary(log.lin.mod)$dispersion)))

(rand.pois <- rpois(n,
                    lambda=exp(A %*% coef(pois.mod))))

(rand.bin <- rbinom(n,
                    size=market.size,
                    prob=plogis(A %*% coef(bin.glm))))

basicPlot(ylim=c(100,700))
cols <- adjustcolor(c("orange", "red", "blue", "purple"), 
                    alpha.f = 0.75)
points(icecream$temp, rand.normal, pch=19, col=cols[1])
points(icecream$temp, rand.logtrans, pch=19, col=cols[2])
points(icecream$temp, rand.pois, pch=19, col=cols[3])
points(icecream$temp, rand.bin, pch=19, col=cols[4])
legend(x="topleft",
       legend=c("observation",
                "linear model",
                "log-transformed LM",
                "Poisson (log) GLM",
                "Binomial (logit) GLM"),
       col=c("#00526D",cols), lty=NA,
       bty="n", lwd=rep(2,5),
       pch=c(1,rep(19,4)))


# Bayesian approach

# Data again
temp <- c(11.9,14.2,15.2,16.4,17.2,18.1,18.5,19.4,22.1,22.6,23.4,25.1)
units <- c(185L,215L,332L,325L,408L,421L,406L,412L,522L,445L,544L,614L)
log_units <- log(units)
n <- length(units)
market.size <- rep(800, n)

library(brms)
# Linear Gaussian model
lin.mod <- brm(units ~ temp, family="gaussian")
# Log-transformed Linear Gaussian model
log.lin.mod <- brm(log_units ~ temp, family="gaussian")
# Poisson model
pois.mod <- brm(units ~ temp, family="poisson")
# Binomial model
bin.mod <- brm(units | trials(market.size) ~ temp, family="binomial")

modelData <- data.frame(
  Model=factor(c(rep("Linear model", n), 
                 rep("Log-transformed LM", n),
                 rep("Poisson (log)",n),
                 rep("Binomial (logit)",n)),  
               levels=c("Linear model", 
                        "Log-transformed LM",
                        "Poisson (log)",
                        "Binomial (logit)"), 
               ordered = TRUE),
  Temperature=rep(temp, 4),
  Units_sold=rep(units, 4),
  rbind(predict(lin.mod),
        exp(predict(log.lin.mod) + 
              0.5 * mean(extract(log.lin.mod$fit)[["sigma_log_units"]])),
        predict(pois.mod),
        predict(bin.mod)
  ))

library(lattice)
key <- list(
  rep=FALSE, 
  lines=list(col=c("#00526D", "blue"), type=c("p","l"), pch=1),
  text=list(lab=c("Observation","Estimate")),
  rectangles = list(col=adjustcolor("yellow", alpha.f=0.5), border="grey"),
  text=list(lab="95% Prediction credible interval"))
xyplot(l.95..CI + u.95..CI + Estimate + Units_sold ~ Temperature | Model, 
       data=modelData, as.table=TRUE, main="Ice cream model comparision",
       xlab="Temperatures (C)", ylab="Units sold", 
       scales=list(alternating=1), key=key,
       panel=function(x, y){
         n <- length(x)
         k <- n/2
         upper <- y[(k/2+1):k]
         lower <- y[1:(k/2)]
         x <- x[1:(k/2)]
         panel.polygon(c(x, rev(x)), c(upper, rev(lower)),
                       col = adjustcolor("yellow", alpha.f = 0.5), 
                       border = "grey")
         panel.lines(x, y[(k+1):(k+n/4)], col="blue")
         panel.points(x, y[(n*3/4+1):n], lwd=2, col="#00526D")
       })

A <- function(samples){
  as.matrix(samples[,c("b_Intercept" ,"b_temp")])
}
x <- c(1, 35)
prob <- 0.975 
lin.samples <- posterior_samples(lin.mod)
n <- nrow(lin.samples)
mu <- A(lin.samples) %*% x
sigma <- lin.samples[,"sigma_units"]
(lin.q <- quantile(rnorm(n, mu, sigma), prob))
#    97.5% 
# 1025.077
log.lin.samples <- posterior_samples(log.lin.mod)
mu <- A(log.lin.samples) %*% x
sigma <- log.lin.samples[,"sigma_log_units"]
(log.lin.q <- quantile(exp(rnorm(n, mu +  0.5*sigma^2, sigma)), prob))
#    97.5% 
# 2460.108
pois.samples <- posterior_samples(pois.mod)
mu <- exp(A(pois.samples) %*% x)
(pois.q <- quantile(rpois(n, mu) , prob))
#  97.5% 
#   1515 
bin.samples <- posterior_samples(bin.mod)
inv.logit <- function(u) exp(u)/(1+exp(u))
mu <- inv.logit( A(bin.samples) %*% x)
(bin.q <- quantile(rbinom(n, size = 800, mu), prob))
#   97.5% 
# 761.025
percentiles <- c(lin.q, log.lin.q, pois.q, bin.q)
b <- barplot(percentiles, 
             names.arg = c("Linear", "Log-transformed",
                           "Poisson", "Binomial"),
             ylab="Predicted ice cream units",
             main="Predicted 97.5%ile at 35ºC")
text(b, percentiles-75, round(percentiles))

newdata <- data.frame(temp = 35, market.size = 800)
predict(lin.mod, newdata = newdata)
predict(log.lin.mod, newdata = newdata, transform = exp)
predict(bin.mod, newdata = newdata)

# Extracting stancode
stancode(lin.mod)

# Creating code
stanLogTransformed <-"
data {
int N;
vector[N] units;
vector[N] temp;
}
transformed data {  
vector[N] log_units;        
log_units <- log(units);
}
parameters {
real alpha;
real beta;
real tau;
}
transformed parameters {
real sigma;
sigma <- 1.0 / sqrt(tau);
}
model{
// Model
log_units ~ normal(alpha + beta * temp, sigma);
// Priors
alpha ~ normal(0.0, 1000.0);
beta ~ normal(0.0, 1000.0);
tau ~ gamma(0.001, 0.001);
}
generated quantities{
vector[N] units_pred;
for(i in 1:N)
units_pred[i] <- exp(normal_rng(alpha + beta * temp[i], sigma));
}
"

# Fitting model in stan (http://www.magesblog.com/2015/08/visualising-predictive-distribution-of.html)


library(rstan)
stanmodel <- stan_model(model_code = stanLogTransformed)
fit <- sampling(stanmodel,
                data = list(N=length(units),
                            units=units,
                            temp=temp),
                iter = 1000, warmup=200)
stanoutput <- extract(fit)

## Extract generated posterior predictive quantities
Sims <- data.frame(stanoutput[["units_pred"]])

## Calculate summary statistics
SummarySims <- apply(Sims, 2, summary)
colnames(SummarySims) <- paste(icecream$temp,"ºC")

## Extract estimated parameters
(parms <- sapply(stanoutput[c("alpha", "beta", "sigma")], mean))
## Use parameters to predict median and mean
PredMedian <- exp(parms['alpha'] + parms['beta']*temp)
PredMean <- exp(parms['alpha'] + parms['beta']*temp + 0.5*parms['sigma']^2)

## Compare predictions based on parameters with simulation statistics
round(rbind(SummarySims, PredMedian, PredMean),1)
