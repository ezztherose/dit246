library(stringr)
library(rethinking)

indata <- read.csv("./data/data_autumn2020.csv", header = TRUE, sep=";")

# sanity test
# plotting both ME and LE
tp <- indata$tp 
#print("Data for tp")
#print(summary(tp))

inv_list <- list(
    tp = indata$tp,
    sub_id = as.integer(indata$subject),
    c_num = ifelse(indata$category == "ME", 1L, 2L),
    t_num = ifelse(indata$technique == "NT", 1L, 2L)
)

# model 2
m2 <- ulam(
    alist(
        tp ~ binomial(alpha, beta),
        logit(beta) <- a[t_num] + b[c_num],
        a[t_num] ~ normal(0, 1.5),
        b[c_num] ~ normal(0, 1)
    ), data=inv_list , chains=4 , cores=4 , cmdstan=TRUE
)

prior_m2 <- extract.prior(m2)
p_2 <- exp(prior_m2$a + prior_m2$b)
dens(p_2, adj = 0.1)
sims2 <- sim(m2, post = prior_m2)