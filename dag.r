library(ggdag)
library(ggplot2)

DAG <- dagitty::dagitty("DAG {
    Y <- X
    X <- Z
    Y <- Z
    X [exposure]
    Y [outcome]
    }"
)

tidy_dag <- tidy_dagitty(DAG)
tidy_dag

# Working DAG
DAG2 <- dagify(
    y ~ x + z,
    x ~ y,
    z ~ x,
    exposure = "x",
    outcome = "y"
)


out <- ggdag(DAG2) + theme_dag()
print(out)

# The dag is a fork.

test <- ggdag::dagify(t ~ c, tp ~ c, tp ~t)
out <- ggdag::ggdag(test)+theme_dag()
print(out)