
############################# BIENVENUE #############################

############# I SOLEMNLY SWEAR THAT I AM UP TO NO GOOD ##############

#################################
## Loading packages

library(lavaan)

#################################

n <- 5485 ## sample size

rm <- matrix(c( ## correlations reported by Luo et al. (2026)
  
   1.00,  0.44, -0.59, -0.58,
   0.44,  1.00, -0.51, -0.48,
  -0.59, -0.51,  1.00,  0.84,
  -0.58, -0.48,  0.84,  1.00), nrow=4)

colnames(rm) <- rownames(rm) <- c("FMF","LO","PS","DS") ## names of variables

#################################
## Alternative model

altmod <- "

## Loadings

CSE =~ start(-0.5)*FMF+start(-0.5)*LO+1*PS+DS

## (Error) variances

FMF ~~ FMF
LO ~~ LO
PS ~~ PS
DS ~~ DS

CSE ~~ CSE

"

alt2 <- paste(altmod, "\nPS~~DS")

fit.alt <- lavaan(altmod, sample.cov=rm, ## fitting model to data 
            sample.nobs=n, meanstructure=F)

summary(fit.alt, fit.measures=T, ci=T, standardized=T, rsq=T) ## the results

fit2 <- lavaan(alt2, sample.cov=rm, ## fitting model to data 
                  sample.nobs=n, meanstructure=F)

summary(fit2, fit.measures=T, ci=T, standardized=T, rsq=T) ## the results


########################## MISCHIEF MANAGED #########################

############################# AU REVOIR #############################



