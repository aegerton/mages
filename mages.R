n <- 100
m <- 6
set.seed(1)
mu <- -0.4
sig <- 0.12
x <- matrix(data=rlnorm(n*m, mu, sig), nrow=m)
View(x)
library(fitdistrplus)
## Fit a log-normal distribution to the 50 random data set
f <- apply(x, 2,  fitdist, "lnorm")

## Plot the results 
for(i in 1:n)
  plot(f[[i]])

## Save plot in an animated GIF-file
## library(animation)
## saveGIF({for(i in 1:n) plot(f[[i]])})

apply((sapply(f, "[[", "estimate")),1, summary)

## How much variance can we expect in the mean and std?
## Expected mean
ExpectedMean <- function(mu, sig){
  exp(mu+ sig^2/2)
}
## Expected std
ExpectedStd <- function(mu, sig){
  sqrt((exp(sig^2)-1)*exp(2*mu + sig^2))
}
summary(apply(sapply(f, "[[", "estimate"), 2, 
              function(x) ExpectedMean(x[1], x[2])))

summary(apply(sapply(f, "[[", "estimate"), 2, 
              function(x) ExpectedStd(x[1], x[2])))

## Let's look at the goodness of fit statistics to get an
## idea how much variance we can expect there:
gof.ln <- lapply(f, gofstat)
gof.test <- lapply(gof.ln, function(x){
  data.frame(x[c("chisqpvalue", "cvm", "ad", "ks")])
})
apply(do.call("rbind", gof.test), 2, summary)
