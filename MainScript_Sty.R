########
# Assignment part 1 scrip
# Authors:
#      Sty, Caroline, Pablo
#
# library
library(ggplot2)
library(tidyverse)
theme_set(theme_minimal()) 

####-----
# Part 1
# 1a)
n = 10000
trials = rexp(n = 10000, rate = 1/2)
sum(trials)/n #Result is 2.000524. Very close to 2
# 1b)
n2=200
data=data.frame(value=rexp(n = n2, rate = 1/2))

# Plot histogram and theoretical pdf
p <- ggplot(data, aes(x=value)) + 
  geom_histogram(bins=30)
p

# 1c)

# 1d)

# 1e)

# 1f)

# 1g)

####------- 
# Part 2
data = read.csv("https://github.com/StatisticsSU/STM/raw/main/assignment/bugs.csv", 
                header = TRUE)
View(data)

# 2a).
y = data$nBugs # number of bugs, a vector with n = 91 observations
lambda=mean(y)
## Simple plot
ggplot() + 
  geom_histogram(aes(data_p2$nBugs,after_stat(density)),bins = 20,color="black",alpha=0.4, fill="#dde542")+
  geom_function(fun = function(x) lambda^x*exp(-lambda)/factorial(x))+
  theme_minimal() +
  labs(title = "Data and poisson distribution",
       subtitle = "Comparing the distribution of number of bugs and the poisson distribution") +
  xlab("Count of y") +
  ylab("Pr(y<Y)")

#Extra. Which is the best value of bins? 
#Maybe try a sub plot
ggplot() + 
  geom_histogram(aes(data_p2$nBugs,after_stat(density)),bins = 20,color="black",alpha=0.4, fill="#dde542")+
  geom_histogram(aes(data_p2$nBugs,after_stat(density)),bins = 25,color="black",alpha=0.4, fill="#76fc00")+
  geom_histogram(aes(data_p2$nBugs,after_stat(density)),bins=  30,color="black",alpha=0.4, fill="#00fcf8")+
  geom_function(fun = function(x) lambda^x*exp(-lambda)/factorial(x))+
  theme_minimal() +
  labs(title = "Data and poisson distribution",
       subtitle = "Comparing the distribution of number of bugs and 3 different poisson distributions") +
  xlab("Count of y") +
  ylab("Pr(y<Y)")

# 2b).
# Try a negative binomial
# Plot 3 curves, the previous one. r = 1, 3, 100. 


#Generate the data
df <- data.frame(VNeg1=rnbinom(500,mu = lambda,size=1),
                 VNeg2=rnbinom(500,mu = lambda,size=3),
                 VNeg3=rnbinom(500,mu = lambda,size=100) )
#First try
df %>% pivot_longer(everything()) %>%
  ggplot(aes(x=value,color=name))+
  geom_density(linewidth = 1.2)

ggplot() +
  geom_density(aes(df$VNeg1), color = "#FF9E0D", linewidth = 1.0) +
  geom_density(aes(df$VNeg2), color = "#1BE2DC", linewidth = 1.0) +
  geom_density(aes(df$VNeg3), color = "#09E920", linewidth = 1.0) +
  geom_histogram(aes(data_p2$nBugs,after_stat(density)),bins = 20,color="black",alpha=0.4, fill="#dde542" )+
  theme_minimal() +
  labs(title = "Data and Negative Binomial",
       subtitle = "Comparing the distribution of number of bugs and 3 theoretical curves") +
  xlab("Count of y") +
  ylab("Pr(y<Y)") +
  theme(legend.position="right")

## Final Plot
ggplot() + 
  geom_function(fun = function(x) lambda^x*exp(-lambda)/factorial(x), color = "#fc1b00", linewidth = 1.0 )+ 
  geom_density(aes(df$VNeg1), color = "#FF9E0D", linewidth = 1.0) +
  geom_density(aes(df$VNeg2), color = "#1BE2DC", linewidth = 1.0) +
  geom_density(aes(df$VNeg3), color = "#09E920", linewidth = 1.0) +
  geom_histogram(aes(data_p2$nBugs,after_stat(density)),bins = 20,color="black",alpha=0.4, fill="#dde542")+
  theme_minimal() +
  labs(title = "Data, poisson and negative binomial distributions",
       subtitle = "Comparing the distribution of number of bugs and 4 theoretical curves") +
  xlab("Count of y") +
  ylab("Pr(y<Y)")

####------
# Part 3

