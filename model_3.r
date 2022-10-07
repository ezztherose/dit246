library(stringr)
library(rethinking)

indata <- read.csv("./data/data_autumn2020.csv", header = TRUE, sep=";")

inv_list <- list(
    tp = indata$tp,
    sub_id = as.integer(indata$subject),
    c_num = ifelse(indata$category == "ME", 1L, 2L),
    t_num = ifelse(indata$technique == "NT", 1L, 2L)
)


# model 3
m3 <- ulam(
    alist(
        tp ~ dgampois(lambda, phi),
        lambda <- a + t_num + c_num,
        a ~dnorm(0, 0.5),
        c(t_num, c_num) ~ dnorm(0, 0.5),
        c(t_num, c_num) ~ dnorm(0, 0.25),
        phi ~ dexp(1)
    ), data=inv_list, chains=4  , cmdstan=TRUE,
)

prior <- extract.prior(m3)
p <- exp(prior$a)
dens(p, adj = 0.1)
sims3 <- sim(m3, post = prior)

print(sims3)
hist(sims3)
print("Summary for both a & b:")
print(summary(sims3))