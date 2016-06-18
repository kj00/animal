##
library(data.table)
library(ggplot2)
library(MASS)
library(magrittr)
#
source("byplot.R")

#
train <- fread("train.csv", na.strings = c("", "Unknown"), stringsAsFactors = F) 


##
train[, `:=`(
  
    male = grepl("Male", SexuponOutcome)
  , bmixed = grepl("Mix|/", Breed)
  , cmixed = grepl("/", Color)
  , Breed  = gsub("Mix", "", Breed))][
   
    , `:=`( 
      
      breed1 = Breed %>% strsplit("/") %>% lapply(function(x) x[1]) %>% as.character()
    , breed2 = Breed %>% strsplit("/") %>% lapply(function(x) x[2]) %>% as.character()
    , breed3 = Breed %>% strsplit("/") %>% lapply(function(x) x[3]) %>% as.character()
    , color1 = Color %>% strsplit("/") %>% lapply(function(x) x[1]) %>% as.character()
    , color2 = Color %>% strsplit("/") %>% lapply(function(x) x[2]) %>% as.character()
  )
  ]


unique(train[,color3])


##
hist(train[, .N, by = Breed][, N], breaks = 1000, xlim = c(0, 100)) #before separating
hist(train[, .N, by = breed1][, N], breaks = 1000, xlim = c(0, 100), ylim = c(0, 1200)) #after separating

hist(train[, .N, by = Color][, N], breaks = 1000, xlim = c(0, 100)) #before separating
hist(train[, .N, by = color2][, N], breaks = 1000, xlim = c(0, 100), ylim = c(0, 10)) #after separating




##
byplot(dt = train, outcome = "OutcomeType", byval = "AnimalType", inv = T, position = "fill")
byplot(dt = train, outcome = "OutcomeType", byval = "bmixed", inv = T, position = "dodge")
byplot(dt = train, outcome = "OutcomeType", byval = "cmixed", inv = F, position = "dodge")
byplot(train, "OutcomeType", byval = "male")
byplot(train, "OutcomeType", "color1")
byplot(train, "OutcomeType", "color1", type = "hist", binwidth = 500)



###
m1 <- polr(OutcomeType ~ bmixed + cmixed + male , data = train)
summary(m1)
stepAIC(m1)

