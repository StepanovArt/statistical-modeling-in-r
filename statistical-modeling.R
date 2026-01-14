#
#     R-code to generate data for F79MA Assignment 1.  This code MUST be
#     placed at the start of your own R-script.  You must edit
#     the argument to the set.seed( ) function to fit your own
#     registration number
#
#     Please also ensure you have created all required variables 
#     (Quantity1, Quantity2, Quantity3, Quantity4, Quantity5, 
#     Quantity6, Quantity7, Quantity8, Quantity9, Quantity10) 
#     and include code for all of your plots
#
RNGkind(sample.kind = "Rejection")
set.seed(8079) ## Change this number
#
My_theta <- runif(1)
if (My_theta < 0.01){ # ensure My_theta at least 0.01
  My_theta <- 0.01
}
if (My_theta > 0.99){ # ensure My_theta at most 0.99
  My_theta <- 0.99
}
#
#     Generate pseudorandom data for Parts 1 and 2
#
n <- 100  # size of each sample
m <- 1000 # number of samples
# generate m*n U(0,1) sample
u <- runif(n*m,-0.5, 0.5) 
# generate samples for my distribution using inverse transform method
x<- My_theta - sign(u) * log(1 - 2*abs(u))
samples <- matrix(x, nrow = m, ncol = n)
#
#####################################################################
# Please insert your R code after this line
#####################################################################

#Question 4
# take first simulated sample 
first_sample = samples[1, ]
# it was concluded that our estimator is the median of the sample
Quantity1 = median(first_sample)  
Quantity1
#check the equality which must give 0 
score_function = sum(sign(first_sample-Quantity1)) 
score_function




#####################################################################
#Question 5 
# log-likelihood which we found 
log_likelihood = function(theta, x) {
  -length(x)*log(2)-sum(abs(x-theta))
}

# Value of log-likelihood at theta head
Quantity2 = log_likelihood(Quantity1, first_sample)
Quantity2

# Evaluate log-likelihood over a fine θ-grid 
theta_x = seq(min(first_sample), max(first_sample), length.out = 5000)
loglikeligood_values = vapply(theta_x, log_likelihood, numeric(1), x = first_sample)
# make a graph
theta_hat_max = theta_x[which.max(loglikeligood_values)] # finf the max theta 
plot(theta_x, loglikeligood_values, type = "l",
     xlab = expression(theta),
     ylab = "Log-likelihood",
     main = "Log-likelihood of the first 100 samples")
# to make clear the intersection 
abline(v = theta_hat_max, col = "red", lwd = 1)
# show the value of theta  
axis(1, at = theta_hat_max, labels = round(theta_hat_max, 3), col.axis = "red", las = 1)

#####################################################################



#Question 6 

n = length(first_sample)  # sample size = 100
sig_level = 0.1 
z = qnorm(sig_level/2)   # since this is a two-tail hypothesis testing 
standart_error = 1/sqrt(n) # Variance was found in the theoretical part
standart_error
# Endpoints 
Quantity3 = Quantity1+z*standart_error   # lower bound
Quantity4 = Quantity1-z*standart_error   # upper bound 

Quantity3
Quantity4




#####################################################################


#Question 7

chi_stat = qchisq(0.90, df=1) #critical point

deviance = function(theta){
  2*(Quantity2-log_likelihood(theta, first_sample))
}

# Deviance on the grid D(theta)
dev_x = 2*(Quantity2 - loglikeligood_values)
# Keep thetas where deviance is below the threshold 
inside = dev_x<chi_stat
theta_ci = theta_x[inside]
# Endpoints 
Quantity5 = theta_ci[1]                         # lower limit
Quantity6 = theta_ci[length(theta_ci)]          # upper limit

Quantity5
Quantity6

#####################################################################


#Question 8
# find median for each of 1000 rows
theta_hat2 = apply(samples, 1, median)

Quantity7 = mean(theta_hat2)-My_theta   # bias
Quantity8 = var(theta_hat2)            # variance
Quantity9 = Quantity7^2+Quantity8         # MSE

Quantity7
Quantity8
Quantity9



#####################################################################


#Question 9
Quantity10 = 1/n
Quantity10






#####################################################################

#Question 10

n_values = c(10,20,30,40,50,60,70,80,90,100)
# make free vectors 
bias = c()
variance = c()
mse = c()
# compute values for each sample size i
for (i in n_values) {
  # Calculate vector of MLE
  theta_hats_i = apply(samples[, 1:i, drop = FALSE], 1, median)
  # Metrics for this n
  bias_i = mean(theta_hats_i) - My_theta
  var_i  = var(theta_hats_i)
  mse_i  = bias_i^2 + var_i     
  # Save the data 
  bias = c(bias, bias_i)
  variance = c(variance, var_i)
  mse = c(mse, mse_i)
}

# draw final plots 
# bias
plot(n_values, bias, type = "l",
     xlab = "n", ylab = "Bias",
     main = "Bias of MLE")
#var
plot(n_values, variance, type = "l", 
     xlab = "n", ylab = "Variance",
     main = "Variance of MLE")
crlb = 1/n_values
lines(n_values, crlb, col = "red", lwd = 2, lty = 3) #draw bounds
#make a legend
legend("topright",
       legend = c("Variance", "CRLB bound"),
       col = c("black", "red"),
       lty = c(1, 2), lwd = 1)
# MSE
plot(n_values, mse, type = "l",
     xlab = "n", ylab = "MSE",
     main = "MSE of MLE")