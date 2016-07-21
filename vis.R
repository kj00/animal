##
hist(train[, .N, by = Breed][, N], breaks = 1000, xlim = c(0, 100)) #before separating
hist(train[, .N, by = breed1][, N], breaks = 1000, xlim = c(0, 100), ylim = c(0, 1200)) #after separating

hist(train[, .N, by = Color][, N], breaks = 1000, xlim = c(0, 100)) #before separating
hist(train[, .N, by = color1][, N], breaks = 1000, xlim = c(0, 100), ylim = c(0, 10)) #after separating

hist(train[, .N, by = Name][, N], breaks = 1000, xlim = c(0, 200))
##


byplot(dt = train, outcome = "OutcomeType", byval = "AnimalType",inv = T, position = "fill")
byplot(dt = train, outcome = "OutcomeType", byval = "bmixed", inv = T, position = "dodge")
byplot(dt = train, outcome = "OutcomeType", byval = "cmixed", inv = F, position = "dodge")
byplot(train, "OutcomeType", byval = "gender")
byplot(train, "OutcomeType", byval = "gentreat")
byplot(train, "OutcomeType", "color1")
byplot(train, "OutcomeType", "color1", type = "hist", binwidth = 500)
byplot(train, "OutcomeType", "DateTime", type = "line") + facet_wrap(~OutcomeType)
byplot(train, "AnimalType", "DateTime", type = "line")
byplot(train, "OutcomeType", "age_num", type = "line")
byplot(train, "OutcomeType", "age_cat", type =  "bar")

list_outcometype <- train[, unique(OutcomeType)]

for (i in 1:length(list_outcometype)) {
  acf(train[, .N, by = .(DateTime, OutcomeType)][order(DateTime)][OutcomeType == list_outcometype[i]][, N]
      , type = "correlation"
      , plot = TRUE, main = list_outcometype[i], )
}
