# importing libaries
library(stringr)
library(rethinking)
library(rstanarm)

# importing csv file
indata <- read.csv("./data/data_autumn2020.csv", header = TRUE, sep=";")

# converting from discrete to integers
inv_list <- list(
    tp = indata$tp,
    sub_id = as.integer(indata$subject),
    c_num = ifelse(indata$category == "ME", 1L, 2L),
    t_num = ifelse(indata$technique == "NT", 1L, 2L)
)

# ******** MODELS ***********
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
prior <- extract.prior(model_1)
p <- exp(prior$a + prior$b)
dens(p, adj = 0.1)
sims_p <- sim(model_1, post = prior)
sims_f <- sim(model_1, data = list(t_num = c(1,1), c_num=c(1,2)))
#sims_f <- sim(model_1, data = list(t_num=2, c_num=2)) # for testing different likelihoods

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

print("summary")
print(summary(inv_list$c_num))
print("summary data")
print(summary(inv_list$t_num))


# ******** MODEL COMPARE **********
# WAIC
print(compare(null_hyp, model_1, m2, m3))
plot(compare(null_hyp, model_1, m2, m3))

# ************ R hat **************
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

# ***** MORE PLOTTING DATA ****
print(precis(null_hyp, depth=2))
print(precis(model_1, depth=2))
print(precis(m2, depth=2))
print(precis(m3, depth=2))

# ***** DIFFERENCE MODEL 1 *****
post <- extract.samples(model_1)
diffTech <- post$alpha[,2] - post$alpha[,1]
diffCat <- post$beta[,2] - post$beta[,1]
print(precis(list(diffTech=diffTech)))
print(precis(list(diffCat=diffCat)))

# ***** DIFFERENCE MODEL 2 *****
post2 <- extract.samples(m2)
dft <- post2$bt[,2] - post2$bt[,1]
dfc <- post2$bc[,2] - post2$bc[,1]
print(precis(list(diffTech=dft)))
print(precis(list(diffCat=dfc)))

#plot(hist(indata$tp, xlim = c(0,20)))

# traceplot model 2
#plot(traceplot(m2, true = NULL))
