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

model_1 <- ulam(
    alist(
        tp ~ dgampois(lambda, phi),
        lambda <- alpha[t_num] + beta[c_num],
        alpha[t_num] ~ dnorm(0,1),
        beta[c_num] ~ dnorm(1, 0.5),
        phi ~ dexp(1)
    ), data=inv_list , chains=4  , cmdstan=TRUE,
)

# sanity check
# plot( precis(model_1,omit="sigma") )

# priors for all
prior <- extract.prior(model_1)
p <- exp(prior$a + prior$b)
dens(p, adj = 0.1)

# likelihood
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

#sims <- sim(model_1, post = prior)

#sims <- sim(model_1, post = prior)
sims <- sim(model_1, post = prior)
print(sims)
hist(sims)
print("Summary:")
print(summary(p))

print("null")
print(summary(p_null))

