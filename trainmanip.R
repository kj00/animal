##
library(data.table)
library(ggplot2)
library(MASS)

#
source("byplot.R")

#
train <- fread("train.csv", na.strings = c("", "Unknown"), stringsAsFactors = T) 


##
colnames(train)
summary(train)

###
train[, `:=` (
               male = grepl("Male", SexuponOutcome)
              , bmixed = grepl("Mix|/", Breed)
              , cmixed = grepl("/", Color)
              , Breed = gsub(" Mix", "", Breed)
             )
]

##
ptr1 <- train[, lapply(.SD, length), by = .(OutcomeType, AnimalType)][order(AnimalType)]
ggptr1 <- ggplot(ptr1, aes(x = "OutcomeType", y = "AnimalID"))
ggptr1 + geom_bar(stat = "identity") + aes(fill = "AnimalType")



##
byplot(dt = train, count = "OutcomeType", byval = "AnimalType", inv = T)
byplot(dt = train, count = "OutcomeType", byval = "bmixed", inv = T)
byplot(dt = train, count = "OutcomeType", byval = "cmixed")
byplot(dt = train, count = "OutcomeType", byval = "male")







###
m1 <- polr(OutcomeType ~ bmixed + cmixed + male , data = train)
summary(m1)
stepAIC(m1)







