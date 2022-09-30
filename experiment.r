library(stringr)

indata <- read.csv("./data/data_autumn2020.csv", header = TRUE, sep=";")
#print(head(indata)) # sanity check

# setting the correct categories?
ME <- indata[indata$category == "ME",]
LE <- indata[indata$category == "LE",]
NT <- indata[indata$technique == "NT",]
OT <- indata[indata$technique == "OT",]

# mappling correlating data with eachother?
NT_LE <- LE[LE$technique == "NT",]
OT_LE <- LE[LE$technique == "OT",] # missing sub. 36?
NT_ME <- ME[ME$technique == "NT",]
OT_ME <- ME[ME$technique == "OT",]

# plotting both ME and LE
tp <- indata$tp 
hist(tp)

# Plotting hist ME
hist(ME$tp)

# Plotting hist LE
hist(LE$tp)