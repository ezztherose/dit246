library(stringr)
library(rethinking)

indata <- read.csv("./data/data_autumn2020.csv", header = TRUE, sep=";")

# sanity test
# plotting both ME and LE
tp <- indata$tp 
print("Data for tp")
print(summary(tp))

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


# model 1
model_1 <- ulam(
    alist(
        tp ~ dgampois(lambda, phi),
        lambda <- alpha[t_num] + beta[c_num],
        alpha[t_num] ~ dnorm(0.5,1),
        beta[c_num] ~ dnorm(1, 0.2),
        phi ~ dexp(1)
    ), data=list(tp=6) , chains=4, log_lik = TRUE, cmdstan=TRUE,
)

# sanity check
# plot( precis(model_1,omit="sigma") )

# priors for all
prior <- extract.prior(model_1)
p <- exp(prior$a + prior$b)
dens(p, adj = 0.1)

#sims <- sim(model_1, post = prior)
sims_f <- sim(model_1, data = list(t_num = 1, c_num=2))
#print(sims_f)
#hist(sims_f)
print("Summary Model 1:")
print(summary(sims_f))

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
sims3 <- sim(m3, data = list(t_num = 1, c_num = 2))
#print(sims3)
#hist(sims3)

# printing results
print(summary(p_null))
print("summary model 1:")
print(summary(sims_f))
print("summary model 3:")
print(summary(sims3))

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

# likelihood for p3
s=mean(p3)
r=1/mean(1/p3)
b=function(b) {
  K=1/mean(1/(b + p3))
  return((b^2 - b*(2*r+K) + r*(s+K))^2)
}
b_mle=optim(1, b, method="SANN")$par
like3 <- sqrt(s/b_mle + b_mle/r -2)
print(like3)

# compare
compare(model_1, m3)
plot(compare(model_1, m3))

# Dumpig in some code from the main.r file

#trace(plot(model_1, depth=2))

#hist(sim(null_hyp), col = "green", xlim = c(0,20))
#hist(sim(model_1), col = "blue", xlim = c(0,20))
hist(sim(model_1), xlim = c(0,20))

#hist(sim(null_hyp), col = "green", xlim = c(0,20))
#hist(sim(model_1), col = "blue", xlim = c(0,20))
#hist(sim(m2), col = "red", xlim = c(0,20))

post <- extract.samples(model_1)
diffTech <- post$alpha[,2] - post$alpha[,1]
diffCat <- post$beta[,2] - post$beta[,1]
print(precis(list(diffTech=diffTech)))
print(precis(list(diffCat=diffCat)))