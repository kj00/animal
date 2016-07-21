library(MASS)
library(rattle)


train[, OutcomeType := as.factor(OutcomeType)]
clist <- colnames(train)
train1 <- train[, clist[c(3:4, 6, 11:21)], with = F]
train1df <- as.data.frame(train1)


m1 <- polr(OutcomeType ~ color1 + gender + AnimalType, data = train1)
summary(predict(m1, newdata = train1[1:20]))
summary(m1)

stepAIC(m1)


m1df <- polr(OutcomeType ~ color1 + gender + AnimalType, data = train1df)
summary(m1df)
(pnorm(fitted(m1))/(1-pnorm(fitted(m1))))

m1$fitted.values
fitted(m1)
str(m1$lp)


##
rattle()

pred <- m1$fitted.values


log(pred)


log(0.2) ##benchmark
