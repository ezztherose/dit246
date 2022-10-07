library(stringr)
library(rethinking)

indata <- read.csv("./data/data_autumn2020.csv", header = TRUE, sep=";")

inv_list <- list(
    tp = indata$tp,
    sub_id = as.integer(indata$subject),
    c_num = ifelse(indata$category == "ME", 1L, 2L),
    t_num = ifelse(indata$technique == "NT", 1L, 2L)
)

# model 4
m4 <- ulam(
    alist(
        tp ~ dgampois(lambda, phi),
        lambda <- t_num + c_num,
        a ~ dnorm(1,0.5),
        c(t_num, c_num) ~ dnorm(0,0.2),
        phi ~ dexp(1)
    ), data=inv_list, chains=4  , cmdstan=TRUE,
)

prior_m4 <- extract.prior(m4)
p_4 <- exp(prior_m4$a)
dens(p_4, adj = 0.1)
sims4 <- sim(m4, post = prior_m4)

print(sims4)
hist(sims4)
print("Summary for both a & b:")
print(summary(sims4))