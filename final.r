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
        alpha[t_num] ~ dnorm(0.5,1),
        beta[c_num] ~ dnorm(1, 0.2),
        phi ~ dexp(1)
    ), data=inv_list , chains=4  , cmdstan=TRUE,
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
    ), data=inv_list, chains=4  , cmdstan=TRUE,
)

prior3 <- extract.prior(m3)
p3 <- exp(prior3$a)
dens(p, adj = 0.1)
sims3 <- sim(m3, data = list(t_num = 1, c_num = 2))
#print(sims3)
#hist(sims3)

# printing results
print("Summary Model 1:")
print(summary(sims_f))
print("Summary Model 3:")
print(summary(sims3))