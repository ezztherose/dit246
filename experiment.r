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
print("Data for tp")
print(summary(tp))
print(var(tp))

# Plotting hist ME
hist(ME$tp)
print("Data for ME")
print(summary(ME$tp))
print(var(ME$tp))

# Plotting hist LE
hist(LE$tp)
print("Data for LE")
print(summary(LE$tp))
print(var(LE$tp))

# *** likelihood calculation ***
# norm TP
mean_tp <- mean(tp)
std_tp <- sd(tp)
norm_tp <- dnorm(tp, mean_tp, std_tp)

# norm ME
mean_me <- mean(ME$tp)
std_me <- sd(ME$tp)
norm_me <- dnorm(ME$tp, mean_me, std_me)

# norm Le
mean_le <- mean(LE$tp)
std_le <- sd(LE$tp)
norm_le <- dnorm(LE$tp, mean_le, std_le)

# printing
#print(paste0("norm TP: ", norm_tp))
#print(paste0("norm LE: ", norm_le))
#print(paste0("norm ME: ", norm_me))
# get mean for each value


fucking_list <- list(
    tp = indata$tp,
    sub_id = as.integer(indata$subject),
    me_num = ifelse(indata$ME == "ME", 1L, 2L),
    le_num = ifelse(indata$LE == "LE", 1L, 2L)
)


# sequense 
x <- seq(-1,1,length=length(norm_tp)) 
plot(x, norm_tp, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))