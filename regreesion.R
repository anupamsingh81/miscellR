# Linear regression example with brms, rethinking, rstanarm and Stan
# Matti Vuorre (mv2521@columbia.edu)

# setup -------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(brms)
library(rethinking)
library(rstan)
theme_set(theme_minimal() + theme(panel.grid = element_blank()))
library(rstanarm)

# data ----------------------------------------------------------------

# Example data: Stopping distance of cars at different speeds in 1920

p1 <- ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  scale_y_continuous("stopping distance", limits = c(-36, 125)) +
  scale_x_continuous(limits = c(0, 26))
p1

# ordinary linear regression ----------------------------------------------
# Look at data first
library(psych)
str(cars)
describe(cars)

# Linear regression using lm() (difficulty: 0)

olr <- lm(dist ~ speed, cars)
summary(olr)$coef
summary(olr)

# predict
predict(olr,newdata = data.frame(speed = c(20,10))

p1 + geom_smooth(method = "lm")

# residuals plot
# see plot of residuals
plot(olr)

# Also need to plot residuals against all variables in dataset including those not in model
names(cars)
plot(resid(olr)~dist, data = cars)
plot(sqrt(abs(resid(olr)))~cars$dist )
scatter.smooth(sqrt(abs(resid(olr)))~cars$dist )

# appears residuals getting out of hand at outliers/extremes
plot(cars$speed~ cars$dist)
scatter.smooth(cars$speed~ cars$dist)
summary(olr)

olr2 = lm(dist~speed+speed^2, data = cars)
summary(olr2)

scatter.smooth(sqrt(abs(resid(olr2)))~cars$dist )




# simulation based 
library(arm)
str(olr)
summary(olr)$sigma
olr$coefficients
nsim = n.sim =1000
olrsim = sim(olr,n.sim=1000)
str(olrsim)
str(coef(olrsim))
Intercept = coef(olrsim)[,1]
quantile(Intercept,prob = c(0.025,0.975))
# Intercept greater than some value(compVal)
sum(Intercept > -10)/nsim

Speed = coef(olrsim)[,2]
quantile(Speed,prob = c(0.025,0.975))








# brms --------------------------------------------------------------------

# Bayesian regression using brms package (difficulty: 1)

brmsfit <- brm(
  data = cars, 
  dist ~ speed, 
  family = gaussian,
  prior = c(set_prior("normal(0, 100)", coef = "Intercept"),
            set_prior("normal(0, 100)", coef = "speed")),
  n.iter = 2000,
  n.chains = 4
)

summary(brmsfit)
plot(brmsfit)
brms::stancode(brmsfit)

brmpredict = predict(brmsfit, newdata = data.frame(speed = c(20)))
describe(brmpredict)


preds <- cbind(as.data.frame(predict(brmsfit)),
               speed = cars$speed)
p1 + geom_line(data = preds, aes(x = speed, y = Estimate)) +
  geom_ribbon(data = preds, alpha = .2,
              aes(ymin = `2.5%ile`, ymax = `97.5%ile`, y = Estimate)) +
  geom_smooth(method="lm")


# rstanarm




#specify prior
str(cars)

# use stan_glm for continuous data not stan_lm
mm <- stan_glm(dist ~ speed , data = cars, prior = normal(0,100),prior_intercept = normal(0,100) )
 
mm
# Estimates are in median, let us get confidence interval 
ci95speed <- posterior_interval(mm, prob = 0.95, pars = "speed")
round(ci95speed, 2)

summary(mm)
mmpredict = posterior_predict(mm, newdata = data.frame(speed = c(20,10)))
describe(mmpredict)

pp_check(mm)

# another example
mm1 <- stan_glm (mpg ~ ., data=mtcars, prior=normal (0, 8))
summary(mm1)


# rethinking --------------------------------------------------------------

# Bayesian linear regression using rethinking package (difficulty: 2)

refit <- map2stan(
  alist(
    dist ~ dnorm(mu, sigma),
    mu <- Intercept + b_speed*speed,
    Intercept ~ dnorm(0, 100),
    b_speed ~ dnorm(0, 100),
    sigma ~ dcauchy(0, 2)
  ),
  data = cars,
  iter = 2000,
  chains = 4
)

precis(refit, prob=.95)
coeftab_plot(coeftab(refit), prob=.95)
pairs(refit)

# prediction

link.refit <- link(refit, data = data.frame(speed = c(20,10)))

# predictions

apply(link.refit,2,mean)
apply(link.refit,2,PI)

# posterior predictions marginal of intercept
samples2 = extract.samples(refit)
str(samples2)







                   
postcheck(refit); devAskNewPage(ask = F)

glimmer(dist ~ speed, data = cars)

# Stan -------------------------------------------

# bayesian regression with Stan (difficulty: 3 - Inf)

stan_data <- list(
  N = nrow(cars),
  speed = cars$speed,
  dist = cars$dist)

options(mc.cores = parallel::detectCores())
stanfit <- stan("cars_regression.stan",
                data = stan_data,
                iter = 2000,
                chains = 4)

# inference from Stan model -------------------------------------------

# Print model summary
print(stanfit, probs = c(.05, .5, .95), 
      pars = c("beta_0", "beta_1", "sigma_y"))

# Convert posterior samples to a data.frame
posterior <- as.data.frame(stanfit)[,1:2]

# Point estimates vs. posterior samples
coef(olr)
head(posterior)
ggplot(reshape2::melt(posterior), aes(x = value)) +
  geom_histogram(fill = "skyblue", col = "black") +
  facet_wrap( ~ variable, scales = "free")

# Plot a sample of posterior regression lines
p2 <- p1 + 
  geom_abline(data = sample_n(posterior, 40), 
              aes(intercept = beta_0, slope = beta_1),
              col = "dodgerblue", size = .35, alpha = .5)
p2

# 90% Credible interval of regression line
pred_data <- expand.grid(speed = seq(4, 25, by = 1))
Xmat <- model.matrix( ~ 1 + speed, data = pred_data)
fitmat <- matrix(ncol = nrow(posterior), nrow = nrow(pred_data))
for (i in 1:nrow(posterior)) {
  fitmat[,i] <- Xmat %*% as.matrix(posterior)[i,] 
}
pred_data$lower <- apply(fitmat, 1, quantile, prob=0.05)
pred_data$median <- apply(fitmat, 1, quantile, prob=0.5)
pred_data$upper <- apply(fitmat, 1, quantile, prob=0.95)
p1 + geom_ribbon(data = pred_data,
                 aes(ymin = lower, ymax = upper, y = median),
                 alpha = .8)

# 90% Credible Interval of regression slope
b1 <- posterior$beta_1
hist(posterior$beta_1, breaks=50)
abline(v=quantile(b1, probs = c(0.05, 0.95)), col="red", lwd=3)

# explore the Stan model --------------------------------------------------

library(shinystan)
Y <- cars$dist
launch_shinystan(stanfit)
