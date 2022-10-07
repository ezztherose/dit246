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

num_test <- 6

model_1 <- ulam(
    alist(
        tp ~ dgampois(lambda, phi),
        lambda <- alpha[t_num] + beta[c_num],
        alpha[t_num] ~ dnorm(0,1),
        beta[c_num] ~ dnorm(1, 0.5),
        phi ~ dexp(1)
    ), data=inv_list, chains = 4, log_lik = TRUE, cmdstan = TRUE
)

# sanity check
# plot( precis(model_1,omit="sigma") )

# priors for all
prior <- extract.prior(model_1)
p <- exp(prior$a + prior$b)
#dens(p, adj = 0.1)

# null hypothesis
null_hyp <- ulam(
    alist(
        tp ~ dgampois(lambda, phi),
        log(lambda) <- alpha,
        alpha ~ dnorm(5, 2),
        phi ~ dexp(1)
    ), data=list(tp=num_test)
)
prior_null <- extract.prior(null_hyp)
p_null <- exp(prior_null$alpha)
#dens(p_null, adj = 0.1)

# likelihood for p
#s=mean(p_null)
#r=1/mean(1/p_null)
#b=function(b) {
#  K=1/mean(1/(b + p_null))
#}
#b_mle=optim(1, b, method="SANN")$par
#like_null <- sqrt(s/b_mle + b_mle/r -2)



#sims <- sim(model_1, post = prior)

#sims <- sim(model_1, post = prior)
sims <- sim(model_1, post = prior)
print(sims)
hist(sims)
print("Summary:")
print(summary(p))
print("mean for the model")
print(mean(sim(model_1, post = prior)))
plot(sim(model_1))

print("null")
print(summary(p_null))



#if(mean(p) == mean(p_null))
#{
#    print("Null hypothesis NOT rejected")
#} else {
#   print("Null hypotesis REJECTED")
#}

# likelihood for p
#s=mean(p)
#r=1/mean(1/p)
#b=function(b) {
#  K=1/mean(1/(b + p))
#  return((b^2 - b*(2*r+K) + r*(s+K))^2)
#}
#b_mle=optim(1, b, method="SANN")$par
#like <- sqrt(s/b_mle + b_mle/r -2)
#print(like)
#print(like_null)

post <- extract.samples(model_1)
diffTech <- post$alpha[,2] - post$alpha[,1]
diffCat <- post$beta[,2] - post$beta[,1]
print(precis(list(diffTech=diffTech)))
print(precis(list(diffCat=diffCat)))
