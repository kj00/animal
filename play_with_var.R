library(vars)
library(reshape2)
library(forecast)

vdata<- train[, .(as.numeric(.N), DateTime),  by = .(OutcomeType)]
vdata <- acast(vdata, DateTime~OutcomeType)
vdata <- as.data.table(vdata)

VARselect(vdata, lag.max = 7, type = "both")

var1 <- VAR(vdata, type = "both")

summary(var1)
normality.test(var1)
plot(var1)

irf1 <- irf(var1)

plot(irf1) #see how adoption reacts Euthanasia 

plot(
predict(var1, ahead = 100)
  )
fanchart(predict(var1, ahead = 300))

ur.df(var1)
ur.df(vdata[, Died], type = "trend", lag =2)

vecm <- vdata %>%  ca.jo( type = "trace", ecdet = "trend", K = 3, spec = "transitory")
 %>% 
  summary(vecm)

plot(vecm)
irf(vecm)
