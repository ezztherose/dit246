library(stringr)
indata <- read.csv("./data/data_autumn2020.csv", header = TRUE, sep=";")

tp <- indata$tp 
hist(tp)