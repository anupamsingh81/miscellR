library(rstanarm)
library(foreign)

# downloading stata file from website,converting to csv, have to use foreign library
download.file("http://www.stat.columbia.edu/~gelman/arm/examples/child.iq/kidiq.dta", "kidiq.dta")
 kid_iq <- read.dta(file="kidiq.dta", convert.underscore=TRUE)
 
 
# Look at data
 describe(kid_iq)
 # median,mean near for most variable, appear normally distributed
 
lm1  = lm(kid.score~mom.hs ,data = kid_iq)

lm2  = lm(kid.score~mom.iq ,data = kid_iq)
lm3 = lm(kid.score~mom.iq + mom.hs ,data = kid_iq)
lm4 = lm(kid.score~mom.iq*mom.hs ,data = kid_iq)
lm5 = lm(kid.score~mom.hs*mom.iq ,data = kid_iq)


# Look at plots of lm4
# important resource(http://strengejacke.de/sjPlot/sjp.int/)



layout(matrix(c(1,2,3,4),2,2)) # optional layout
plot(lm4) # diagnostic plots
library(sjPlot)
sjp.int(lm4)
sjp.int(lm5)
sjp.int(lm5, type = "eff")

sjp.lm(lm4)
rm(sjp.int)
# prepare fousr models

post1 <- stan_glm(kid.score ~ mom.hs, data = kid_iq, family = gaussian(link = "identity"))
                  
post2 <- update(post1, formula = . ~ mom.iq)
post3 <- update(post1, formula = . ~ mom.hs + mom.iq)
post4 <- update(post1, formula = . ~ mom.hs * mom.iq)

summary(post4)
base <- ggplot(kid_iq, aes(x = mom.hs, y = kid.score)) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) + 
  scale_x_continuous(breaks = c(0,1), labels = c("No HS", "HS"))

base + geom_abline(intercept = coef(post1)[1], slope = coef(post1)[2], 
                   color = "skyblue4", size = 1)

# First model
draws <- as.data.frame(post1)
colnames(draws)[1:2] <- c("a", "b")

base + 
  geom_abline(data = draws, aes(intercept = a, slope = b), 
              color = "skyblue", size = 0.2, alpha = 0.25) + 
  geom_abline(intercept = coef(post1)[1], slope = coef(post1)[2], 
              color = "skyblue4", size = 1)

# Second model
draws <- as.data.frame(as.matrix(post2))
colnames(draws)[1:2] <- c("a", "b")
ggplot(kid_iq, aes(x = mom.iq, y = kid.score)) + 
  geom_point(size = 1) +
  geom_abline(data = draws, aes(intercept = a, slope = b), 
              color = "skyblue", size = 0.2, alpha = 0.25) + 
  geom_abline(intercept = coef(post2)[1], slope = coef(post2)[2], 
              color = "skyblue4", size = 1)

# Third model

reg0 <- function(x, ests) cbind(1, 0, x) %*% ests 
reg1 <- function(x, ests) cbind(1, 1, x) %*% ests
args <- list(ests = coef(post3))
kid_iq$clr <- factor(kid_iq$mom.hs, labels = c("No HS", "HS"))
lgnd <- guide_legend(title = NULL)

base2 <- ggplot(kid_iq, aes(x = mom.iq, fill = relevel(clr, ref = "HS"))) + 
  geom_point(aes(y = kid.score), shape = 21, stroke = .2, size = 1) + 
  guides(color = lgnd, fill = lgnd) + 
  theme(legend.position = "right")

base2 + 
  stat_function(fun = reg0, args = args, aes(color = "No HS"), size = 1.5) +
  stat_function(fun = reg1, args = args, aes(color = "HS"), size = 1.5)

# Model 4

reg0 <- function(x, ests) cbind(1, 0, x, 0 * x) %*% ests 
reg1 <- function(x, ests) cbind(1, 1, x, 1 * x) %*% ests
args <- list(ests = coef(post4))

base2 +
  stat_function(fun = reg0, args = args, aes(color = "No HS"), size = 1.5) + 
  stat_function(fun = reg1, args = args, aes(color = "HS"), size = 1.5)




# compare models
# Compare them with loo
loo1 <- loo(post1)
loo2 <- loo(post2)
loo3 <- loo(post3)
loo4 <- loo(post4)
compare(loo1, loo2, loo3, loo4)


