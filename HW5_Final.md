####  H. Tai, L. ONeill, V. Delgado

library(tidyverse)
#  Subgroup Created. 
#  I created a subgroup of entry level ages 20-25, employed, and working full time with a generally high entry level salary degree of engineering.
attach(acs2017_ny)
use_varb <- (AGE >= 20) & (AGE <=25) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 40) & (DEGFIELD== "Engineering")
dat_use <- subset(acs2017_ny,use_varb) 
detach()
attach(dat_use)

summary(AGE)
summary(LABFORCE)
summary(WKSWORK2)
summary(UHRSWORK)
summary(DEGFIELD=="engineering")
summary(female)

#  Linear Regression

model_temp1 <- lm(INCWAGE~AGE+female + Hispanic+ Asian+ AfAm + Asian + Amindian + race_oth)
install.packages("stargazer")
library(stargazer)
require(stargazer)
stargazer(model_temp1, type = "text")

install.packages("AER")
library(AER)
require(AER)
NNobs <-length(INCWAGE)
set.seed(12345)
graph_obs <- (runif(NNobs) < 0.1)
dat_graph <-subset(dat_use,graph_obs)

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = dat_graph)

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)

## entry level age female 
to_be_predicted2 <- data.frame(AGE = 20:25, female = 1, AfAm = 1, Asian = 0, Amindian = 0, race_oth = 0, Hispanic = 1, DEGFIELD="Engineering")
to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)

## Entry level age, not female, not Afa/AM or Hispanic
to_be_predicted3 <- data.frame(AGE = 20:25, female = 0, AfAm = 0, Asian = 0, Amindian = 0, race_oth = 0, Hispanic = 0, DEGFIELD="Engineering")
to_be_predicted3$yhat <- predict(model_temp1, newdata = to_be_predicted3)

lines(yhat ~ AGE, data = to_be_predicted3)

## The model shows that entry level income in the Engineering field is significantly influenced by gender and race. 
