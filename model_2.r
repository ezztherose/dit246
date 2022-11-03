library(stringr)
library(rethinking)
library(rstan)

indata <- read.csv("./data/data_autumn2020.csv", header = TRUE, sep=";")

inv_list <- list(
    tp = indata$tp,
    sub_id = as.integer(indata$subject),
    c_num = ifelse(indata$category == "ME", 1L, 2L),
    t_num = ifelse(indata$technique == "NT", 1L, 2L)
)

m2 <- ulam(
  alist(
    tp ~ dgampois(lambda, phi),
    log(lambda) <- alpha + bt[t_num] + bc[c_num],
    alpha ~ dnorm(5, 2),
    bt[t_num] ~ dnorm (0, 1),
    bc[c_num] ~ dnorm (0, 1),
    phi ~ dexp(1)
  ),data=inv_list, chains = 4, log_lik = TRUE, cmdstan = TRUE)

prior_m2 <- extract.prior(m2)
p_2 <- exp(prior_m2$alpha)
dens(p_2, adj = 0.1)
sims2 <- sim(m2, post = prior_m2)

print(sims2)
hist(sims2)
print("Summary:")
print(summary(sims2))
print(summary(p_2))

# ***** DIFFERENCE MODEL 2 *****
post2 <- extract.samples(m2)
dft <- post2$bt[,2] - post2$bt[,1]
dfc <- post2$bc[,2] - post2$bc[,1]
print(precis(list(diffTech=dft)))
print(precis(list(diffCat=dfc)))


# ***** TRACEPLOT *****
plot(traceplot(m2))