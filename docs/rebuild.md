# Rebuilding rstan for mac Ventura 13.0

open the R-terminal run 
```r
library(cmdstanr)
```

afterwards, run 
```r
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
```

Now we have to rebuild rstan. Run the following:
```r
rebuild_cmdstan()
```

Now your golden to continue to work with your R-projects.