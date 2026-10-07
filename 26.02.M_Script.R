
############################# BIENVENUE #############################

############# I SOLEMNLY SWEAR THAT I AM UP TO NO GOOD ##############

#################################
## Loading packages

library(lavaan)

#################################

n <- 803 ## sample size

rm <- matrix(c( ## correlations reported by Qiu et al. (2026)
  
   1.000, -0.383,  0.565, -0.439,
  -0.383,  1.000, -0.362,  0.422,
   0.565, -0.362,  1.000, -0.383,
  -0.439,  0.422, -0.383,  1.000), nrow=4)

colnames(rm) <- rownames(rm) <- c("PWS","LS","SD","SS") ## names of variables

#################################
## Alternative model

altmod <- "

## Loadings

CSE =~ start(-0.5)*PWS+1*SS+start(-0.5)*SD+LS

## (Error) variances

PWS ~~ PWS
SS ~~ SS
SD ~~ SD
LS ~~ LS

CSE ~~ CSE

"

alt2 <- paste(altmod, "\nSS~~LS") ## with correlated residuals

fit.alt <- lavaan(altmod, sample.cov=rm, ## fitting model to data 
            sample.nobs=n, meanstructure=F)

summary(fit.alt, fit.measures=T, ci=T, standardized=T, rsq=T) ## the results

fit2 <- lavaan(alt2, sample.cov=rm, ## fitting model to data 
                  sample.nobs=n, meanstructure=F)

summary(fit2, fit.measures=T, ci=T, standardized=T, rsq=T) ## the results


########################## MISCHIEF MANAGED #########################

############################# AU REVOIR #############################

