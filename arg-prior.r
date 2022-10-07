library(stringr)
library(rethinking)
library(rstanarm)

indata <- read.csv("./data/data_autumn2020.csv", header = TRUE, sep=";")

inv_list <- list(
    tp = indata$tp,
    sub_id = as.integer(indata$subject),
    c_num = ifelse(indata$category == "ME", 1L, 2L),
    t_num = ifelse(indata$technique == "NT", 1L, 2L)
)

# null hypothesis
null_hyp <- ulam(
    alist(
        tp ~ dgampois(lambda, phi),
        log(lambda) <- alpha,
        alpha ~ dnorm(0.5, 0.2),
        phi ~ dexp(1)
    ), data=inv_list, chains = 4, log_lik = TRUE
)
prior_null <- extract.prior(null_hyp)
p_null <- exp(prior_null$a)
dens(p_null, adj = 0.1)

sims_n <- sim(null_hyp, data = list(t_num = c(1,1), c_num=c(1,2)))

# model 1
model_1 <- ulam(
    alist(
        tp ~ dgampois(lambda, phi),
        lambda <- alpha[t_num] + beta[c_num],
        alpha[t_num] ~ dnorm(0.0,1),
        beta[c_num] ~ dnorm(1.0, 0.5),
        phi ~ dexp(1)
    ), data=inv_list, chains=4, log_lik = TRUE, cmdstan=TRUE,
)

# priors for all
prior <- extract.prior(model_1)
p <- exp(prior$a + prior$b)
dens(p, adj = 0.1)

sims_p <- sim(model_1, post = prior)
sims_f <- sim(model_1, data = list(t_num = c(1,1), c_num=c(1,2)))
# c(1,1),Exp=c(1,2))
print("Summary Model 1 raw data:")
print(summary(sims_f))

# likelihood for p
s=mean(p)
r=1/mean(1/p)
b=function(b) {
  K=1/mean(1/(b + p))
  return((b^2 - b*(2*r+K) + r*(s+K))^2)
}
b_mle=optim(1, b, method="SANN")$par
like <- sqrt(s/b_mle + b_mle/r -2)
print(like)

# plotting model 1 for alpha and beta
plot(precis(model_1, depth = 2))

# model 2
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
sims2 <- sim(m2, data = list(t_num = c(1,1), c_num=c(1,2)))

# model 3
m3 <- ulam(
    alist(
        tp ~ dgampois(lambda, phi),
        lambda <- a + t_num + c_num,
        a ~dnorm(0, 0.5),
        c(t_num, c_num) ~ dnorm(0, 0.5),
        c(t_num, c_num) ~ dnorm(0, 0.25),
        phi ~ dexp(1)
    ), data=inv_list, chains=4, log_lik = TRUE, cmdstan=TRUE,
)

prior3 <- extract.prior(m3)
p3 <- exp(prior3$a)
dens(p, adj = 0.1)
sims3 <- sim(m3, data = list(t_num = c(1,1), c_num=c(1,2)))

# calculatin likelihoods
# null
s=mean(p_null)
r=1/mean(1/p_null)
b=function(b) {
  K=1/mean(1/(b + p_null))
  return((b^2 - b*(2*r+K) + r*(s+K))^2)
}
b_mle=optim(1, b, method="SANN")$par
like_null <- sqrt(s/b_mle + b_mle/r -2)

# likelihood for p
s=mean(p)
r=1/mean(1/p)
b=function(b) {
  K=1/mean(1/(b + p))
  return((b^2 - b*(2*r+K) + r*(s+K))^2)
}
b_mle=optim(1, b, method="SANN")$par
like <- sqrt(s/b_mle + b_mle/r -2)

# model 2
s=mean(p_2)
r=1/mean(1/p_2)
b=function(b) {
  K=1/mean(1/(b + p_2))
  return((b^2 - b*(2*r+K) + r*(s+K))^2)
}
b_mle=optim(1, b, method="SANN")$par
like2 <- sqrt(s/b_mle + b_mle/r -2)

# likelihood for p3
s=mean(p3)
r=1/mean(1/p3)
b=function(b) {
  K=1/mean(1/(b + p3))
  return((b^2 - b*(2*r+K) + r*(s+K))^2)
}
b_mle=optim(1, b, method="SANN")$par
like3 <- sqrt(s/b_mle + b_mle/r -2)


h = hist(sims_f)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE)

# Likelihood
print(like_null)
print(like)
print(like2)
print(like3)

# WAIC
print(compare(null_hyp, model_1, m2, m3))
plot(compare(null_hyp, model_1, m2, m3))

# *** R hat ***
# R hat null
print("R hat model null")
print(Rhat(sims = sims_n))
print(ess_bulk(sims_n))
print(ess_tail(sims_n))
# model 1
print("R hat model 1")
print(Rhat(sims = sims_f))
print(ess_bulk(sims_f))
print(ess_tail(sims_f))

# model 2
print("R hat model 2")
print(Rhat(sims = sims2))
print(ess_bulk(sims2))
print(ess_tail(sims2))

# model 3
print("R hat model 3")
print(Rhat(sims = sims3))
print(ess_bulk(sims3))
print(ess_tail(sims3))

#trace.plot(sims_f, true = NULL, BurnIn = NULL,BurnInAdaptive=NULL)