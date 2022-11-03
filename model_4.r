library(stringr)
library(rethinking)

indata <- read.csv("./data/data_autumn2020.csv", header = TRUE, sep=";")

inv_list <- list(
    tp = indata$tp,
    sub_id = as.integer(indata$subject),
    c_num = ifelse(indata$category == "ME", 1L, 2L),
    t_num = ifelse(indata$technique == "NT", 1L, 2L)
)

m4 <- ulam(
    alist(
        tp ~ dpois(lambda),
    log(lambda) <- alpha + bt[t_num] + bc[c_num],
    alpha ~ dnorm(5, 2),
    bt[t_num] ~ dnorm (0, 1),
    bc[c_num] ~ dnorm (0, 1),
    #phi ~ dexp(1)
  ), data=inv_list, chains = 4, log_lik = TRUE, cmdstan = TRUE
)

prior_m4 <- extract.prior(m4)
p_4 <- exp(prior_m4$alpha)
dens(p_2, adj = 0.1)
sims4 <- sim(m4, post = prior_m4)

print(sims4)
hist(sims4)
print("Summary:")
print(summary(sims4))
print(summary(p_4))