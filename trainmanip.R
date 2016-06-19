##
library(data.table)
library(ggplot2)
library(MASS)
library(magrittr)

##
source("byplot.R") #categrize data.table and plot

##
train <- fread("train.csv", na.strings = c("", "Unknown"), stringsAsFactors = F) 


##data manupilation
train[, `:=`(
  
    bmixed = grepl("Mix|/", Breed) #whether breed is mixed anyway
  , cmixed = grepl("/", Color) #whether color is mixed
  , Breed  = gsub("Mix", "", Breed) #exclude "Mixed" letters
  , DateTime = as.Date(DateTime, format = "%Y-%m-%d")) #into Date class
  ][  #linking because of Breed treatment
   
    , `:=`( 
    
      breed1 = Breed %>% strsplit("/") %>% sapply(function(x) x[1]) #separating Breed
    , breed2 = Breed %>% strsplit("/") %>% sapply(function(x) x[2])
    , breed3 = Breed %>% strsplit("/") %>% sapply(function(x) x[3])
    , color1 = Color %>% strsplit("/") %>% sapply(function(x) x[1]) #separating color
    , color2 = Color %>% strsplit("/") %>% sapply(function(x) x[2])
    , gentreat = SexuponOutcome %>% strsplit(" ") %>% sapply(function(x) x[1]) #separating gender treatment status
    , gender = SexuponOutcome %>% strsplit(" ") %>% sapply(function(x) x[2]) #separating gender status
    , age_num = AgeuponOutcome %>% gsub("year|years", "*12", .) %>% gsub("month|months", "*1", .) %>% gsub("day|days", "*1/30", .) %>% gsub("week|weeks", "*7/30", .) # this doesn't work; %>% sapply(train[, age], function(x) eval(parse(text = x)))
        )
  ]

age_temp <- sapply(train[, age_num], function(x) round(eval(parse(text = x)),1)) #parse text equations and evaluate 

train[, age_num := age_temp][
  , age_cat := as.character(age_num) #for treating age as categorical variable
  ]


##
hist(train[, .N, by = Breed][, N], breaks = 1000, xlim = c(0, 100)) #before separating
hist(train[, .N, by = breed1][, N], breaks = 1000, xlim = c(0, 100), ylim = c(0, 1200)) #after separating

hist(train[, .N, by = Color][, N], breaks = 1000, xlim = c(0, 100)) #before separating
hist(train[, .N, by = color2][, N], breaks = 1000, xlim = c(0, 100), ylim = c(0, 10)) #after separating


##
byplot(dt = train, outcome = "OutcomeType", byval = "AnimalType",inv = T, position = "fill")
byplot(dt = train, outcome = "OutcomeType", byval = "bmixed", inv = T, position = "dodge")
byplot(dt = train, outcome = "OutcomeType", byval = "cmixed", inv = F, position = "dodge")
byplot(train, "OutcomeType", byval = "gender")
byplot(train, "OutcomeType", byval = "gentreat")
byplot(train, "OutcomeType", "color1")
byplot(train, "OutcomeType", "color1", type = "hist", binwidth = 500)
byplot(train, "OutcomeType", "DateTime", type = "line")
byplot(train, "AnimalType", "DateTime", type = "line")
byplot(train, "OutcomeType", "age_num", type = "line")
byplot(train, "OutcomeType", "age_cat", type = "bar")



