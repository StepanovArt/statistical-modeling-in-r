#
#     R-code to generate data for F79MA Assignment 2.  
#     This code MUST be placed at the start of your own R-script. 
#     You must edit the variable last_four_digits to fit 
#     your own student ID number
#
RNGkind(sample.kind = "Rejection")
last_four_digits = 8079 ## change this number

set.seed(last_four_digits)

dataset <- read.csv(file = "count_data.csv", header = TRUE)
Full_Data <- dataset$x
My_Data <- sample(Full_Data,size=1000,replace=FALSE)


#####################################################################
# Please insert your R code after this line
#####################################################################
# compute mle for p
k = 5 #given parameter
xbar = mean(My_Data) # sample mean
Quantity1 = k/xbar
Quantity1

####################################################################################
n = 1000 #number of observations
alpha_post = n*k
beta_post = sum(My_Data)-n * k+0.5
# posterior mean 
Quantity2 = alpha_post/(alpha_post+beta_post)
Quantity2
####################################################################################

# obtained from earlier work
alpha_prior = 0
beta_prior  = 0.5
p_grid = seq(0, 1, length = 1000)
# use direct equation since the power is negative 
prior_density = 1/(p_grid)*(1-p_grid)^(beta_prior-1)
#posterior
posterior_density = dbeta(p_grid, alpha_post, beta_post)

plot(p_grid, prior_density,type="l", lwd=2, col="blue",xlab="p", ylab="Density",
     main="Prior and Posterior for p")
lines(p_grid, posterior_density, lwd=2, col="red")
legend("topright", legend=c("Prior", "Posterior"), col=c("blue","red"), lwd=2)

####################################################################################
# posterior pmf for future z
predictive_dist = function(z,k,alpha_post,beta_post){
  # For the NegBin z >= k
  if (z < k) return(0)
  log_num = lbeta(alpha_post+k,beta_post +(z-k))
  log_den = lbeta(alpha_post,beta_post)
  choose(z-1, k-1) * exp(log_num-log_den)
}
Quantity3 = predictive_dist(5, k, alpha_post, beta_post)
Quantity3

###############################################################################

sim_pred = numeric(n)
for (j in 1:n){
   p_sim = rbeta(1, alpha_post, beta_post)# sample p from posterior
  # rnbinom returns number of failures and add k successes
  sim_pred[j] = rnbinom(1,size = k, prob = p_sim)+k
}
#make same х axes for both hists 
xmin = min(min(sim_pred),min(Full_Data))
xmax = max(max(sim_pred),max(Full_Data))

# posterior histogram
hist(sim_pred,breaks = 50,freq = FALSE,col = "green3",
     main = "Posterior predictive distribution",xlab = "Simulated mutation counts",xlim = c(xmin, xmax))
# full data histogram
hist(Full_Data,breaks = 50,freq = FALSE,col = "red3",
     main = "Histogram of full data",xlab = "Observed mutation counts",xlim = c(xmin, xmax))

# mean and var of predictive data
pred_mean = mean(sim_pred)
pred_var  = var(sim_pred)
# mean and var of original data
full_mean = mean(Full_Data)
full_var  = var(Full_Data)


