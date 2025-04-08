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
data_p2 = read.csv("https://github.com/StatisticsSU/STM/raw/main/assignment/bugs.csv", 
                header = TRUE)
View(data_p2)

# 2a).
y = data$nBugs # number of bugs, a vector with n = 91 observations
lambda=mean(y)
## Simple plot
ggplot() + 
  geom_histogram(aes(data_p2$nBugs,after_stat(density)),bins = 20)+
  geom_function(fun = function(x) lambda^x*exp(-lambda)/factorial(x)) 
# 2b).

####------
# Part 3

