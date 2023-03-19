# AR(1) Process

# Function Arguments Description
# n : number of Periods to Simulate
# u : Mean of AR(1) Process
# p : Autocorrelation of AR(1) Process
# x_0 : Initial Value of AR(1) Process
# gauss_var : variance of the Gaussian Noise 

AR_1 <- function(n=10, u=0, p=0.5, x_0=0, gauss_var=1)
{ 
  if(gauss_var < 0 )
  {
    stop("ERROR: negative noise variance")
  }
  if(n <=1)
  {
    stop("ERROR: number of periods should be atleast 2")
  }
  # Initializing vector
  x <- vector(mode = "numeric", length = n)
  # vector of Gaussion Noise
  err <- rnorm(n, mean=0, sd= sqrt(gauss_var))
  # Initial value
  x[1] <- x_0
  # beta 
  b <- (1-p)*u
  for(i in 2 : n)
  {
    x[i] <- b + p*x[i-1]  + err[i]
  }
  return(x)
}



# Examples

library(ggplot2)
# for reproducibility
set.seed(42)

X1 <- AR_1(n=100, u=2, p=0.3, x_0=-1, gauss_var=0.2)
# Create a data frame with the time index and the AR(1) process values
df <- data.frame(time = 1:length(X), value = X1)
# Plot the AR(1) process
ggplot(df, aes(x = time, y = value)) + geom_line() + labs(x = "Time", y = "X(t)", title = "AR(1) Process Simulation", subtitle ="inital_value = -1, autocorrelation = 0.3, process_mean = 2, gaussian_noise_var = 0.2")

X2 <- AR_1(n=100, u=2, p=0.3, x_0=0, gauss_var=7)
# Create a data frame with the time index and the AR(1) process values
df <- data.frame(time = 1:length(X), value = X2)
# Plot the AR(1) process
ggplot(df, aes(x = time, y = value)) + geom_line() + labs(x = "Time", y = "X(t)", title = "AR(1) Process Simulation" , subtitle ="inital_value = 0, autocorrelation = 0.3, process_mean = 2, gaussian_noise_var = 7")

X3 <- AR_1(n=100, u=0, p=0.8, x_0=0, gauss_var=7)
# Create a data frame with the time index and the AR(1) process values
df <- data.frame(time = 1:length(X), value = X3)
# Plot the AR(1) process
ggplot(df, aes(x = time, y = value)) + geom_line() + labs(x = "Time", y = "X(t)", title = "AR(1) Process Simulation", subtitle ="inital_value = 0, autocorrelation = 0.8, process_mean = 0, gaussian_noise_var = 7")
