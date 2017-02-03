## Generic code for blocking with different blocking weights
rm(list=ls())
require(randomizr)    # randomizr package for complete random assignment
require(sandwich)
require(lmtest)

## Sample block sizes and create block weights
set.seed(2201)
x <- seq(20,80, by=5) # create possible block sizes
numblocks <- 4 # 4 blocks
(n <- as.vector(sample(x, numblocks))) # sample block sizes
weights <- as.vector(n/N) # create weights per block
N <- sum(n) # check population size
w <- c(rep(weights[1],n[1]), rep(weights[2], n[2]), 
       rep(weights[3], n[3]), rep(weights[4], n[4])) # create vector of block weights
inv_w <- 1/w
blocks <- factor(rep(c('1','2', '3', '4'),c(n))) # create blocks
            
## Create some data
set.seed(2202)
y0 <- rnorm(n=N,mean=100,sd=2)
tau <- .5
y1 <- y0+tau
Z <- block_ra(block_var=blocks)
Y <- Z*y1 + (1-Z)*y0
dat <- cbind(Y, Z, w, inv_w) 

ate_int <- lm(Y~Z+blocks+Z*blocks)
ate_lsdv <- lm(Y~Z+blocks)
ate_ipw <- lm(Y~Z, weights=inv_w)

## ATE and SEs
coeftest(ate_int, vcov = vcovHC(ate_reg, type = "HC2"))["Z",c(1,2)]
coeftest(ate_lsdv, vcov = vcovHC(ate_reg, type = "HC2"))["Z",c(1,2)]
coeftest(ate_ipw, vcov = vcovHC(ate_reg, type = "HC2"))["Z",c(1,2)]
