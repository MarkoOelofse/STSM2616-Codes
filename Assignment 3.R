set.seed(3141)

library(dplyr)
library(ggplot2)
library(tidyverse)

data=storms$wind

data_frame=data.frame(data)

ggplot(data_frame,aes(x=data))+
  geom_histogram(colour="darkgreen",fill="blue")+
  labs(title="Layout of the population values",x="Wind speed",y="Frequency")+
  theme_test()

sample=data%>%sample(500)

sample_frame=data.frame(sample)

ggplot(sample_frame,aes(x=sample))+
  geom_histogram(colour="darkgreen",fill="blue")+
  labs(title="Layout of the sample values",x="Wind speed",y="Frequency")+
  theme_test()


sample_mean=mean(sample)
sample_mean

sample_sd=sd(sample)
sample_sd

z=qnorm(0.975)
z

x_values=seq(sample_mean - 4*sample_sd, sample_mean + 4*sample_sd, length.out = 1000)
y_values=dnorm(x_values, mean = sample_mean, sd = sample_sd/500)
normal_frame=data.frame(x = x_values, y = y_values)

ggplot(normal_frame,aes(x=x, y=y))+
  geom_line(colour="blue", size=1.2)+
  geom_vline(xintercept=sample_mean,linetype="dashed", colour="red")+
  geom_vline(xintercept=46.60358,linetype="dashed", colour="orange")+
  geom_vline(xintercept=50.85642,linetype="dashed", colour="orange")+
  geom_vline(xintercept=46.63092,linetype="dashed", colour="black")+
  geom_vline(xintercept=50.82908,linetype="dashed", colour="black")+
  labs(title="Sampling distriburtion of average wind speeds", x="Wind speed",y="Density")+
  theme_test()

ggplot(normal_df,aes(x=x, y=y))+
  geom_line(colour="blue", size=1.2)+
  geom_vline(xintercept=sample_mean,linetype="dashed", colour="red")+
  geom_vline(xintercept=46.60358,linetype="dashed", colour="orange")+
  geom_vline(xintercept=50.85642,linetype="dashed", colour="orange")+
  geom_vline(xintercept=46.63092,linetype="dashed", colour="black")+
  geom_vline(xintercept=50.82908,linetype="dashed", colour="black")+
  labs(title="Sampling distriburtion of average wind speeds for x-values between 40 and 60",x="Wind speed",y="Density")+
  theme_test()+
  xlim(40,60)

ggplot(normal_df,aes(x=x, y=y))+
  geom_line(colour="blue", size=1.2)+
  geom_vline(xintercept=sample_mean,linetype="dashed", colour="red")+
  geom_vline(xintercept=46.60358,linetype="dashed", colour="orange")+
  geom_vline(xintercept=50.85642,linetype="dashed", colour="orange")+
  geom_vline(xintercept=46.63092,linetype="dashed", colour="black")+
  geom_vline(xintercept=50.82908,linetype="dashed", colour="black")+
  labs(title="Sampling distriburtion of average wind speeds for x-values between 46 and 51", x="Wind speed",y="Density")+
  theme_test()+
  xlim(46,51)

