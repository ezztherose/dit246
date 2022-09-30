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