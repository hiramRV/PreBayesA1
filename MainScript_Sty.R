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
  ylab("Pr(y<Y)")+
  xlim(0,35)

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
                 VNeg3=rnbinom(500,mu = lambda,size=100),
                 Pois=rpois(500,lambda = lambda))
## Final Plot
colors <- c("Poisson" = "#fc1b00","BNr1" = "#FF9E0D", "BNr3" = "#1BE2DC", "BNr100"="#09E920")

ggplot() + 
  geom_density(aes(df$Pois, color = "Poisson"), linewidth = 1.0, bw = 1.0)+ 
  geom_density(aes(df$VNeg1, color = "BNr1"), linewidth = 1.0, bw = 1.0) +
  geom_density(aes(df$VNeg2, color = "BNr3"), linewidth = 1.0,bw = 1.0) +
  geom_density(aes(df$VNeg3, color = "BNr100"), linewidth = 1.0,bw = 1.0) +
  geom_histogram(aes(data$nBugs,after_stat(density)),bins = 20,color="black",alpha=0.4, fill="#dde542")+
  theme_minimal() +
  labs(title = "Data, poisson and negative binomial distributions",
       subtitle = "Comparing the distribution of number of bugs and 4 theoretical curves",
       color = "Legend") +
  xlab("Count of y")+xlim(-1,40)+ylim(0,0.18)+
  ylab("Pr(y<Y)")+scale_color_manual(values = colors,labels = c("BNr1"="NB r=1","BNr100"="NB r=100", "BNr3"="NB r=3","Poisson"="Poisson" ))
####------
#First try
df %>% pivot_longer(everything()) %>%
  ggplot(aes(x=value,color=name))+
  geom_density(linewidth = 1.2)

# Extra. Example with tibble
theo_data <- c(seq(from = 0, to = 20, by = .01))
dist_theo1 <- tibble(theo_data, dnbinom(2001,mu = lambda,size=1) ) %>% rename(X = 1, Y = 2) %>% mutate(dist = "Beta = 1")
dist_theo2 <- tibble(theo_data, dnbinom(2001,mu = lambda,size=3)) %>% rename(X = 1, Y = 2) %>% mutate(dist = "Beta = 2")
dist_theo3 <- tibble(theo_data, dnbinom(2001,mu = lambda,size=100)) %>% rename(X = 1, Y = 2) %>% mutate(dist = "Beta = 3")

dists <- dist_theo1 %>%
  rbind(., dist_theo2) %>%
  rbind(., dist_theo3) %>%
  mutate(dist = factor(dist, levels = c("Beta = 1", "Beta = 2", "Beta = 3")))


col1 <- "#FF9E0D"
col2 <- "#1BE2DC"
col3 <- "#09E920"

dists %>%
  ggplot() +
  geom_line(aes(X, Y, color = dist, group = dist)) +
  scale_color_manual(values = c(col1, col2, col3)) +
  theme_minimal() +
  labs(title = "Histogram of the distribution",
       subtitle = "Comparing theoretical distribution with randomly generated data",
       color = "Theoretical Distribution") +
  xlab("Count of X") +
  ylab("X")

