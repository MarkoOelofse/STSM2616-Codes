library(ggplot2)
library(hrbrthemes)
library(boot)

set.seed(3141)

##Question 1

log_likelihood=function(p) {
  5*log(p)+5*log(1-p)
}

p_values=seq(0.01, 0.99, length.out = 100)

log_lik_values=log_likelihood(p_values)

data=data.frame(p = p_values, log_lik = log_lik_values)

ggplot(data, aes(x = p, y = log_lik)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_vline(xintercept = 0.5, colour = "red", linetype = "dashed") +
  geom_hline(yintercept = log_likelihood(0.5), colour = "green", linetype = "dashed") +
  labs(title = "Log Likelihood Function (n=10, X=5)",x = "p",y = "Log Likelihood") +
  theme_test()

##Question 3

theta_true=2.0
n=100                   
simulations=10000

samples=matrix(nrow = simulations, ncol = n)
for(i in 1:simulations) {
  samples[i, ]=sqrt(-2*theta_true^2*log(runif(n)))
}

sample_means=rowMeans(samples)
theta_mom_estimates=sample_means*sqrt(2 / pi)

plot_data=data.frame(theta_mom_estimates)

ggplot(plot_data, aes(x = theta_mom_estimates)) +
  geom_histogram(aes(y = after_stat(density)),bins = 50,fill = "blue",colour = "black",) +
  geom_vline(xintercept = theta_true, colour = "red",linetype = "dashed", size = 1) +
  labs(title = "Histogram of MoM Estimates for θ",x = "θ estimate", y = "Density") +
  theme_test()

##Question 4

theta_true=2.0
n=100                    
simulations=10000

samples=matrix(nrow=simulations, ncol = n)
for(i in 1:simulations) {
  samples[i, ]=sqrt(-2*theta_true^2*log(runif(n)))
}

sample_means_squared=rowMeans(samples^2)
theta_mle_estimates=sqrt(sample_means_squared*(1/2))

plot_data=data.frame(theta_mle_estimates)

ggplot(plot_data, aes(x = theta_mle_estimates)) +
  geom_histogram(aes(y = after_stat(density)),bins = 50, fill = "blue", colour = "black") +
  geom_vline(xintercept = theta_true, colour = "red",linetype = "dashed",size = 1) +
  labs(title = "Histogram of MLE Estimates for θ",
       x = "θ estimate",
       y = "Density") +
  theme_test()

##Question 5

data(iris)
data5=iris$Sepal.Length

mean_func=function(data, indices) {
  return(mean(data[indices]))
}

np_boot = boot(data = data5, 
                statistic = mean_func, 
                R = 10000)

ggplot(iris, aes(x = Sepal.Length)) +
  geom_density(fill = "lightblue", alpha = 0.5) + 
  labs(title = "Density Plot of Sepal Length",
       x = "Sepal Length (cm)",
       y = "Density") +
  theme_test()

mu_hat = mean(data5)    
sigma_hat = sd(data5)   
n = length(data5)       

parametric_bootstrap = function(data, indices) {

new_sample = rnorm(n = length(data), mean = mu_hat, sd = sigma_hat)
  return(mean(new_sample))
}

p_boot = boot(data = data5, 
               statistic = parametric_bootstrap, 
               R = 10000)


cat("Nonparametric Bootstrap:\n")
print(np_boot)
boot.ci(np_boot, type = "perc")

cat("\nParametric Bootstrap:\n")
print(p_boot)
boot.ci(p_boot, type = "perc")

par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(np_boot, main = "Nonparametric Bootstrap")
plot(p_boot, main = "Parametric Bootstrap")