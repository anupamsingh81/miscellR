# source(https://rpubs.com/rasmusab/bayesian_tutorial_jags_exercise)

library(rjags)

data_list <- list(
  y1 =c(0, 1, 0, 0, 0, 0, 1, 0, 0, 0),
  y2 = c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1))

# The model specification
model_string <- "model{
# Priors
theta1 ~ dbeta(1, 1)
theta2 ~ dbeta(1, 1)

# The data model
for(i in 1:length(y1) ) {
y1[i] ~ dbern(theta1)
}
for(i in 1:length(y2) ) {
y2[i] ~ dbern(theta2)
}
}"

params_to_monitor <- c("theta1", "theta2")

# Running the model
model <- jags.model( textConnection(model_string), data_list, n.chains = 3, n.adapt= 1000)
update(model, 1000); # Burning 1000 samples to the MCMC gods...
mcmc_samples <- coda.samples(model, params_to_monitor, n.iter=5000)

# Inspect the results
summary(mcmc_samples)
plot(mcmc_samples)

# Manipulate samples
s <- as.matrix(mcmc_samples)
head(s)

# # The probability that the rate theta1 is smaller than theta2
mean( s[, "theta1"] < s[, "theta2"] )
# The distribution of the difference between theta1 and theta2
hist(s[, "theta2"] - s[, "theta1"])

# Probability of difference less than 0.2
mean(abs(s[,"theta2"] - s[,"theta1"]) < 0.2)

# cows and disease

#Q4, like t test with normal distribution

diet_milk <- c(651, 679, 374, 601, 401, 609, 767, 709, 704, 679)
normal_milk <- c(798, 1139, 529, 609, 553, 743, 151, 544, 488, 555, 257, 692, 678, 675, 538)

data_list <- list(y1 = diet_milk, y2 = normal_milk)

# The new model using the Normal distribution 
model_string <- "model{
  # Priors (here just using 'sloppy' uniform distributions)
  mu1 ~ dunif(0, 10000)
  mu2 ~ dunif(0, 10000)
  sigma1 ~ dunif(0, 10000)
  sigma2 ~ dunif(0, 10000)

  # The data model
  for(i in 1:length(y1) ) {
    y1[i] ~ dnorm(mu1, 1 / pow( sigma1, 2))
  }
  for(i in 1:length(y2) ) {
    y2[i] ~ dnorm(mu2, 1 / pow( sigma2, 2))
  }
}"

params_to_monitor <- c("mu1", "mu2", "sigma1", "sigma2")

# Running the model
model <- jags.model( textConnection(model_string), data_list, n.chains = 3, n.adapt= 1000)

update(model, 1000); # Burning 1000 samples to the MCMC gods...
mcmc_samples <- coda.samples(model, params_to_monitor, n.iter=5000)

# Inspect the results
par(mar = c(4.2, 3, 2, 1))
plot(mcmc_samples)

#
s1 <- as.matrix(mcmc_samples)
mu_diff <- s1[,"mu2"] - s1[,"mu1"] 
hist(mu_diff)
mean(mu_diff > 0)

# Q5 with mutant cows

diet_milk <- c(651, 679, 374, 601, 4000, 401, 609, 767, 3890, 704, 679)
normal_milk <- c(798, 1139, 529, 609, 553, 743, 3,151, 544, 488, 15, 257, 692, 678, 675, 538)

# Tip: Basically we have an outlier problem. A conventional trick in this situation is to supplement the normal distribution for a distribution with wider tails that is more sensitive to the central values and disregards the far away values (this is a little bit like trimming away some amount of the data on the left and on the right). A good choice for such a distribution is the t distribution which is like the normal but with a third parameter called the degrees of freedom. The lower this parameter the wider the tails, when this parameter is larger than about 50 the t is practically the same as the normal. A good choice for the problem with the mutant cows would be to use a t distribution with around 3 degrees of freedom:
data_list <- list(y1 = diet_milk, y2 = normal_milk)

# The new model using the t-distribution with 3 degrees of freedom instead of the Normal
model_string <- "model{
  # Priors (here just using 'sloppy' uniform distributions)
  mu1 ~ dunif(0, 10000)
  mu2 ~ dunif(0, 10000)
  sigma1 ~ dunif(0, 10000)
  sigma2 ~ dunif(0, 10000)

  # The data model
  for(i in 1:length(y1) ) {
    y1[i] ~ dt(mu1, 1 / pow( sigma1, 2), 3)
  }
  for(i in 1:length(y2) ) {
    y2[i] ~ dt(mu2, 1 / pow( sigma2, 2), 3)
  }
}"

params_to_monitor <- c("mu1", "mu2", "sigma1", "sigma2")

# Running the model
model <- jags.model( textConnection(model_string), data_list, n.chains = 3, n.adapt= 1000)

update(model, 1000); # Burning 1000 samples to the MCMC gods...
mcmc_samples <- coda.samples(model, params_to_monitor, n.iter=5000)

# Inspect the results
par(mar = c(4.2, 3, 2, 1))
plot(mcmc_samples)
s <- as.matrix(mcmc_samples)
mu_diff <- s[,"mu2"] - s[,"mu1"]
hist(mu_diff)
mean(mu_diff > 0) 

# chicken and egges per week,poisson distributuion

diet_eggs <- c(6, 4, 2, NA, 4, 3, 0, 4, 0, 6, 3)
normal_eggs <- c(4, 2, 1, 1, 2, 1, 2, 1, NA, 2, 1)

data_list <- list(y1 = diet_eggs, y2 = normal_eggs)

# The new model using the t-distribution with 3 degrees of freedom instead of the Normal
model_string <- "model{
  # Priors (here just using 'sloppy' uniform distributions)
  lambda1 ~ dunif(0, 10000)
  lambda2 ~ dunif(0, 10000)

  # The data model
  for(i in 1:length(y1) ) {
    y1[i] ~ dpois(lambda1)
  }
  for(i in 1:length(y2) ) {
    y2[i] ~ dpois(lambda2)
  }
}"

params_to_monitor <- c("lambda1", "lambda2")

# Running the model
model <- jags.model( textConnection(model_string), data_list, n.chains = 3, n.adapt= 1000)
update(model, 1000); # Burning 1000 samples to the MCMC gods...
mcmc_samples <- coda.samples(model, params_to_monitor, n.iter=5000)

# Inspect the results
par(mar = c(4.2, 3, 2, 1))
plot(mcmc_samples)

#
s <- as.matrix(mcmc_samples)
lambda_diff <- s[,"lambda1"] - s[,"lambda2"] 
hist(lambda_diff)
summary(lambda_diff)

mean(lambda_diff > 0)

# data frame
d <- 
  structure(list(milk = c(651, 679, 374, 601, 401, 609, 767, 709, 
                          704, 679, 798, 1139, 529, 609, 553, 743, 151, 544, 488, 555, 
                          257, 692, 678, 675, 538), group = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                              1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)), .Names = c("milk", 
                                                                                                                           "group"), row.names = c(NA, -25L), class = "data.frame")

data_list <- list(y = d$milk, group = d$group, n_group = length(unique(d$group)))

model_string <- "model{
  # Priors (here just using 'sloppy' uniform distributions)
  for(group_i in 1:n_group) {
    mu[group_i] ~ dunif(0, 10000) 
    sigma[group_i] ~ dunif(0, 10000)
  }

  for(i in 1:length(y)) {
    y[i] ~ dnorm(mu[group[i]], 1 / pow( sigma[group[i]], 2)) 
  }
}"

params_to_monitor <- c("mu", "sigma")

# Running the model
model <- jags.model( textConnection(model_string), data_list, n.chains = 3, n.adapt= 1000)

update(model, 1000); # Burning 1000 samples to the MCMC gods...
mcmc_samples <- coda.samples(model, params_to_monitor, n.iter=5000)

# Inspect the results
par(mar = c(4.2, 3, 2, 1))
plot(mcmc_samples)

# cow example with three groups and more data
d <- 
  structure(list(milk = c(651, 679, 374, 601, 401, 609, 767, 709, 
                          704, 679, 798, 1139, 529, 609, 553, 743, 151, 544, 488, 555, 
                          257, 692, 678, 675, 538, 1061, 721, 595, 784, 877, 562, 800, 
                          684, 741, 516), group = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 
                                                    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 
                                                    3, 3)), .Names = c("milk", "group"), row.names = c(NA, 35L), class = "data.frame")

data_list <- list(y = d$milk, group = d$group, n_group = length(unique(d$group)))

model <- jags.model( textConnection(model_string), data_list, n.chains = 3, n.adapt= 1000)
s <- as.matrix(mcmc_samples)
hist(s[,"mu[3]"] - s[,"mu[1]"] )
hist(s[,"mu[3]"] - s[,"mu[2]"] )

mean(s[,"mu[3]"] - s[,"mu[1]"] > 0)

#Q8,turning into regression model
d <- 
  structure(list(milk = c(685, 691, 476, 1151, 879, 725, 
                          1190, 1107, 809, 539, 298, 805, 820, 498, 1026, 1217, 1177, 684, 
                          1061, 834), hours = c(3, 7, 6, 10, 6, 5, 10, 11, 9, 3, 6, 6, 
                                                3, 5, 8, 11, 12, 9, 5, 5)), .Names = c("milk", "hours"
                                                ), row.names = c(NA, -20L), class = "data.frame")

data_list <- list(y = d$milk, x = d$hours)

model_string <- "model{
  # Priors (here just using 'sloppy' uniform distributions)
  beta0 ~ dunif(-10000, 10000) 
  beta1 ~ dunif(-10000, 10000) 
  sigma ~ dunif(0, 10000)

  for(i in 1:length(y)) {
    mu[i] <- beta0 + x[i] * beta1
    y[i] ~ dnorm(mu[i], 1 / pow( sigma, 2)) 
  }
}"

params_to_monitor <- c("beta0", "beta1", "sigma")

# Running the model
model <- jags.model( textConnection(model_string), data_list, n.chains = 3, n.adapt= 1000)

update(model, 1000); # Burning 1000 samples to the MCMC gods...
mcmc_samples <- coda.samples(model, params_to_monitor, n.iter=5000)

# Inspect the results
par(mar = c(4.2, 3, 2, 1))
plot(mcmc_samples)

post_sum <- summary(mcmc_samples)
post_sum

plot(d$hours, d$milk)

# Plotting a "best guess" for the regression line
median_post_beta0 <- post_sum$quantiles["beta0", "50%"]
median_post_beta1 <- post_sum$quantiles["beta1", "50%"]
abline(median_post_beta0, median_post_beta1, col = "blue", lwd = 3)

# Adding a sample of the posterior draws to the plot in order to visualize the
# uncertainty of the regression line.
s <- as.matrix(mcmc_samples)
for(i in sample(nrow(s), size = 20)) {
  abline(s[i,"beta0"], s[i,"beta1"], col = "gray")
}

# Q10

d <-
  structure(
    list(
      cow = c(
        1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
        4, 4, 4, 4, 4, 4, 4, 4
      ), hours = c(
        2, 3, 5, 6, 8, 9, 11, 12, 2, 3, 5, 6,
        8, 9, 11, 12, 2, 3, 5, 6, 8, 9, 11, 12, 2, 3, 5, 6, 8, 9, 11, 12
      ),
      milk = c(
        672, 1013, 808, 486, 648, 720, 1100, 950, 746, 721, 654, 1156, 964, 505,
        1104, 903, 560, 817, 829, 975, 1169, 1044, 1722, 1672, 457, 977,
        896, 794, 1116, 1155, 1228, 1243
      )
    ), .Names = c("cow", "hours",
                  "milk"), class = "data.frame", row.names = c(NA,-32L)
  )


data_list <- list(y = d$milk, x = d$hours, cow = d$cow, n_cow = length(unique(d$cow)))

model_string <- "model{
  # Priors (here just using 'sloppy' uniform distributions)
  for(cow_i in 1:n_cow) {
    beta0[cow_i] ~ dunif(-10000, 10000) 
    beta1[cow_i] ~ dunif(-10000, 10000)
    sigma[cow_i] ~ dunif(0,10000)
  }

  for(i in 1:length(y)) {
    mu[i] <- beta0[cow[i]] + x[i] * beta1[cow[i]]
    y[i] ~ dnorm(mu[i], 1 / pow( sigma[cow[i]], 2)) 
  }
}"
params_to_monitor <- c("beta0","beta1")

# Running the model
model <- jags.model( textConnection(model_string), data_list, n.chains = 3, n.adapt= 1000)
update(model, 1000); # Burning 1000 samples to the MCMC gods...
mcmc_samples <- coda.samples(model, params_to_monitor, n.iter=3000)

# Inspect the results
par(mar = c(4.2, 3, 2, 1))
plot(mcmc_samples)

summary(mcmc_samples)

s <- as.matrix(mcmc_samples)

par(mfcol = c(4,4), mar = c(4.2, 0, 2, 2))
slope_range <- range(s[, paste0("beta1[", 1:4, "]")] )
for(i in 1:4) {
  for(j in 1:4) {
    slope_i_name <- paste0("beta1[", i, "]")
    slope_j_name <- paste0("beta1[", j, "]")
    slope_i <- s[,slope_i_name]
    slope_j <- s[,slope_j_name]
    slope_diff <- slope_i - slope_j
    hist(slope_diff, 30, main = "", xlim = slope_range, 
         xlab = paste("Cow ", i, " - ", "Cow ", j), col = "lightblue")
  }
}

# same model run as hierarchical distribution,note shrinkage of beta_0 and beta_1
model_string1 <- "model{
  # Priors (here just using 'sloppy' uniform distributions)
  for(cow_i in 1:n_cow) {
    beta0[cow_i] ~ dnorm(beta0mu, 1 / pow( beta0sigma, 2)) 
    beta1[cow_i] ~ dnorm(beta1mu, 1 / pow( beta1sigma, 2))
    sigma[cow_i] ~ dunif(0,10000)
  }
  beta0mu ~ dunif(-10000,10000)
  beta1mu ~ dunif(-10000,10000)
  beta0sigma ~ dunif(0,10000)
  beta1sigma ~ dunif(0,10000)

  for(i in 1:length(y)) {
    mu[i] <- beta0[cow[i]] + x[i] * beta1[cow[i]]
    y[i] ~ dnorm(mu[i], 1 / pow( sigma[cow[i]], 2)) 
  }
}"

model <- jags.model( textConnection(model_string1), data_list, n.chains = 3, n.adapt= 1000)
update(model, 1000); # Burning 1000 samples to the MCMC gods...
mcmc_samples <- coda.samples(model, params_to_monitor, n.iter=3000)

# Inspect the results
par(mar = c(4.2, 3, 2, 1))
plot(mcmc_samples)

summary(mcmc_samples)


# changing priors

model_string2 <- "model{
  # Priors (here just using 'sloppy' uniform distributions)
for(cow_i in 1:n_cow) {
beta0[cow_i] ~ dnorm(beta0mu, 1 / pow( beta0sigma, 2)) 
beta1[cow_i] ~ dnorm(beta1mu, 1 / pow( beta1sigma, 2))
sigma[cow_i] ~ dunif(0,10000)
}
beta0mu ~ dnorm(500,1 / 2500^2)
beta1mu ~ dunif(-10000,10000)
beta0sigma ~ dunif(0,10000)
beta1sigma ~ dunif(0,10000)

for(i in 1:length(y)) {
mu[i] <- beta0[cow[i]] + x[i] * beta1[cow[i]]
y[i] ~ dnorm(mu[i], 1 / pow( sigma[cow[i]], 2)) 
}
}"

model <- jags.model( textConnection(model_string2), data_list, n.chains = 3, n.adapt= 1000)
update(model, 1000); # Burning 1000 samples to the MCMC gods...
mcmc_samples <- coda.samples(model, params_to_monitor, n.iter=3000)

# Inspect the results
par(mar = c(4.2, 3, 2, 1))
plot(mcmc_samples)

summary(mcmc_samples)

# Ten type of cows
d <- structure(list(cow = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3,
                            3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5,
                            6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 
                            8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10), 
                    hours = c(2, 3, 5, 6, 8, 9, 11, 12, 2, 3, 5, 6, 8, 9, 11, 12, 
                              2, 3, 5, 6, 8, 9, 11, 12, 2, 3, 5, 6, 8, 9, 11, 12, 2, 3, 5, 6,
                              8, 9, 11, 12, 2, 3, 5, 6, 8, 9, 11, 12, 2, 3, 5, 6, 8, 9, 11, 12,
                              2, 3, 5, 6, 8, 9, 11, 12, 2, 3, 5, 6, 8, 9, 11, 12, 2, 3, 5, 6, 8, 
                              9, 11, 12), milk = c(891, 742, 796, 761, 674, 1166, 955, 485, 806, 605, 552,
                                                   755, 660, 752, 839, 660, 941, 891, 806, 1371, 1379, 1322, 1733, 1817, 849, 864, 
                                                   921, 840, 876, 903, 924, 1064, 435, 1165, 1061, 639, 870, 902, 
                                                   1239, 1110, 834, 869, 1049, 1422, 1286, 1726, 1296, 1814, 732, 
                                                   805, 945, 823, 964, 881, 978, 1307, 694, 617, 795, 1022, 518, 
                                                   157, 824, 483, 501, 863, 640, 472, 791, 747, 814, 910, 579, 809, 
                                                   689, 826, 1032, 927, 828, 1149)), .Names = c("cow", "hours", "milk"),
               class = "data.frame", row.names = c(NA, -80L))

data_list <- list(y = d$milk, x = d$hours, cow = d$cow, n_cow = length(unique(d$cow)))

model_string <- "model{
  # Priors (here just using 'sloppy' uniform distributions)
  for(cow_i in 1:n_cow) {
    beta0[cow_i] ~ dnorm(beta0mu, 1 / pow( beta0sigma, 2)) 
    beta1[cow_i] ~ dnorm(beta1mu, 1 / pow( beta1sigma, 2))
    sigma[cow_i] ~ dunif(0,10000)
  }
  beta0mu ~ dunif(-10000,10000)
  beta1mu ~ dunif(-10000,10000)
  beta0sigma ~ dunif(0,10000)
  beta1sigma ~ dunif(0,10000)

  for(i in 1:length(y)) {
    mu[i] <- beta0[cow[i]] + x[i] * beta1[cow[i]]
    y[i] ~ dnorm(mu[i], 1 / pow( sigma[cow[i]], 2)) 
  }
}"

params_to_monitor <- c("beta0mu","beta1mu", "beta0sigma", "beta1sigma"   )
# Running the model
model <- jags.model( textConnection(model_string), data_list, n.chains = 3, n.adapt= 1000)

update(model, 1000); # Burning 1000 samples to the MCMC gods...
mcmc_samples <- coda.samples(model, params_to_monitor, n.iter=3000)

# Inspect the results
par(mar = c(4.2, 3, 2, 1))
plot(mcmc_samples)
plot(d$hours, d$milk, type = "n", ylim = c(0, 2000))
for(i in unique(d$cow)) {
  lines(d$hours[ d$cow == i], d$milk[ d$cow == i], type = "l", col = "blue", lwd = 2)  
}

for(i in sample(nrow(s), 20)) {
  beta0 <- rnorm(1, s[i, "beta0mu"], s[i, "beta0sigma"])
  beta1 <- rnorm(1, s[i, "beta1mu"], s[i, "beta1sigma"])
  abline(beta0, beta1, col = "grey")
}

s <- as.matrix(mcmc_samples)

quantile(s[,"beta1mu"], c(0.025, 0.5, 0.975))

plot(d$hours, d$milk, type = "n", ylim = c(0, 2000))
for(i in unique(d$cow)) {
  lines(d$hours[ d$cow == i], d$milk[ d$cow == i], type = "l", col = "blue", lwd = 2)  
}

for(i in sample(nrow(s), 20)) {
  beta0 <- rnorm(1, s[i, "beta0mu"], s[i, "beta0sigma"])
  beta1 <- rnorm(1, s[i, "beta1mu"], s[i, "beta1sigma"])
  abline(beta0, beta1, col = "grey")
}

fit <- lmer(milk ~ 1 + hours + (1 + hours | cow), data=d)
summary(fit)

# Logistic regression Q12
d <- structure(list(test_intensity = c(6, 6, 6, 6, 6, 6, 6, 6, 6, 
                                       6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 
                                       9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 
                                       10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 
                                       12, 12, 12, 12, 12, 12, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 
                                       14, 14, 14, 14, 14, 14, 14, 14, 14, 14), choice = c(0, 0, 1, 
                                                                                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                           1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 
                                                                                           0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 
                                                                                           1, 1, 0)), .Names = c("test_intensity", "choice"), row.names = c(NA,-90L), 
               class = "data.frame")


data_list <- list(response = d$choice, test_intensity = d$test_intensity, 
                  n_stim = length(d$test_intensity), test_intensity_mean = mean(d$test_intensity))

model_string <- "model{
  for(i in 1:n_stim) {
    response[i] ~ dbern (p[i])
  logit(p[i]) <- alpha + beta * (test_intensity[i] - test_intensity_mean)
}
  # Priors
  alpha ~ dunif(-1000,1000)
  beta ~ dunif(-1000,1000)
}"

params_to_monitor <- c("alpha", "beta")

# Running the model
model <- jags.model( textConnection(model_string), data_list, n.chains = 3, n.adapt= 1000)

update(model, 1000); # Burning 1000 samples to the MCMC gods...
mcmc_samples <- coda.samples(model, params_to_monitor, n.iter=3000)

# Inspect the results
par(mar = c(4.2, 3, 2, 1))
plot(mcmc_samples)
summary(mcmc_samples)
s <- as.matrix(mcmc_samples)

dev.new(width=10, height=10)
par(mar = c(5, 4.5, 2, 1))

post_sum <- summary(mcmc_samples)
post_sum
Test_intensity <- seq(from = 6, to = 14, by = 1)
p1 <- c(mean(d$choice[1:10]),mean(d$choice[11:20]),mean(d$choice[21:30]),mean(d$choice[31:40]),
        mean(d$choice[41:50]), mean(d$choice[51:60]), mean(d$choice[61:70]), mean(d$choice[71:80]),
        mean(d$choice[81:90]))
plot(Test_intensity, p1, col="black", type="p", xlab = "Test Stimulus Intensity", ylab = "Probability", lwd = 2)

x2 <- seq(from = 6, to = 14, by = 0.1)
p2 <- 1/(1+exp(-1*(post_sum$quantiles["alpha", "50%"] + post_sum$quantiles["beta", "50%"] * (x2 - mean(d$test_intensity)))))
lines(x2, p2, col="red", type="l", lwd = 3)

# Adding a sample of the posterior draws to the plot in order to visualize the
# uncertainty of the psychometric function
for(i in sample(nrow(s), size = 50)) {
  p3 <- 1/(1+exp(-1*(s[i,"alpha"] + s[i,"beta"] * (x2 - mean(d$test_intensity)))))
  lines(x2, p3, col="grey", type="l")
}

lines(x2, p2, col="red", type="l", lwd = 3)


dev.new <- function(width = 7, height = 7) { platform <- sessionInfo()$platform if (grepl("linux",platform)) 
{ x11(width=width, height=height) } 
else if (grepl("pc",platform)) 
{ windows(width=width, height=height) } 
else if (grepl("apple", platform)) 
{ quartz(width=width, height=height) } }


plot(1:20)