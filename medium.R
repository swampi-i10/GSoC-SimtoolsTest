library(mvtnorm)
library(plot3D)

MH_sampler<- function(n, mean, sigma)
{
  p <- length(mean)
  x <- matrix(0, ncol=p, nrow=n)
  x[1,] <- rnorm(p, mean = mean, sd = 1)
  for(i in 2:n) {
    curr_x <- x[i-1,]
    proposed_x <- curr_x + rnorm(p, mean = 0, sd = 1)
    ratio <- (dmvnorm(proposed_x, mean, sigma)/dmvnorm(curr_x, mean, sigma))
    if(runif(1) < ratio)
    {
      x[i,] <- proposed_x
    }else 
    {
      x[i,] <- curr_x
    }
  }
  return(x)
}

set.seed(30)
# Testing & Plotting for p=2;  Bivariate Case
example_sample1 = MH_sampler(n=1e4, mean=c(-2,3), sigma = matrix(c(1,0.3,0.3,2.4), nrow=2))

x <- example_sample1[,1]
y <- example_sample1[,2]
x_bin <- seq(floor(min(x)), ceiling(max(x)), length=40)
y_bin <- seq(floor(min(y)), ceiling(max(y)), length=40)
x_c <- cut(x, breaks = x_bin)
y_c <- cut(y, breaks = y_bin)
z <- table(x_c, y_c)

hist3D(z=z, border="black",zlab = "Frequency")
title('Samples Frequency')
image2D(z=z, x=x_bin, y=y_bin)
title('Samples Frequency 2D plot')

# Testing & plotting for p=7
exmu= c(10,0,3,68,-0.3)
# Random Covariance Matrix
A <- matrix(runif(n= 25,min = 0, max = 2), nrow=5)
exsig <- A %*% t(A)
example_sample2 <- MH_sampler(n=1e4, mean= exmu, sigma = exsig)

cat("Results of Example with p=7 :")
cat("True Mean: ",exmu)
cat("\nSample Mean:",colMeans(example_sample2))
cat("\nTrue Variance: ",diag(exsig))
colVars <- rep(0,5)
for(i in 1:5)
{
  colVars[i] <- var(example_sample2[,i])
}
cat("\nSample Variance:",colVars)

