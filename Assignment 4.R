library(ggplot2)
library(hrbrthemes)

set.seed(3141)

## Method of Moments Estimate

theta_true=2.0
n=100                   
simulations=10000

samples=matrix(nrow = simulations, ncol = n)
for(i in 1:simulations) {
  samples[i, ]=runif(n)^(1/(theta_true+1))
}

sample_means=rowMeans(samples)
theta_mom_estimates1=(1-2*sample_means)/(sample_means-1)

plot_data1=data.frame(theta_mom_estimates1)

ggplot(plot_data1, aes(x = theta_mom_estimates1)) +
  geom_histogram(aes(y = after_stat(density)),bins=50,fill = "blue",colour = "black") +
  geom_vline(xintercept = theta_true, colour = "red",linetype = "dashed", size = 1) +
  labs(title = "Histogram of MoM Estimates for θ",x = "θ estimate", y = "Density") +
  theme_test()

##Maximum Likelihood Estimate

theta_true=2.0
n=100                    
simulations=10000

samples=matrix(nrow=simulations, ncol = n)
for(i in 1:simulations) {
  samples[i, ]=runif(n)^(1/(theta_true+1))
}

sum_log_samples=apply(samples, 1, function(x) sum(log(x)))

theta_mle_estimates1=-1 - n / sum_log_samples

plot_data2=data.frame(theta_mle_estimates1)

ggplot(plot_data2, aes(x = theta_mle_estimates)) +
  geom_histogram(aes(y = after_stat(density)),bins=50, fill = "blue", colour = "black") +
  geom_vline(xintercept = theta_true, colour = "red",linetype = "dashed",size = 1) +
  labs(title = "Histogram of MLE Estimates for θ ",
       x = "θ estimate",
       y = "Density") +
  theme_test()

##Asymptotic Variance

theta_true=2.0
n=100                    
simulations=10000

samples=matrix(nrow=simulations, ncol = n)
for(i in 1:simulations) {
  samples[i, ]=runif(n)^(1/(theta_true+1))
}

sum_log_samples=apply(samples, 1, function(x) sum(log(x)))

theta_mle_estimates2=-1 - n / sum_log_samples

asymptotic_var=(theta_true + 1)^2 / n

empirical_var=var(theta_mle_estimates2)

plot_data3=data.frame(theta_mle_estimates2)

ggplot(plot_data3, aes(x = theta_mle_estimates2)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "blue", alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = theta_true, sd = sqrt(asymptotic_var)),
                color = "red", size = 1) +
  geom_vline(xintercept = theta_true, color = "green", linetype = "dashed", size = 1) +
  labs(title = "Histogram of MLE Estimates with Asymptotic Normal Distribution",
       x = "θ estimate",
       y = "Density") +
  theme_test()
