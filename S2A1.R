### Sem 2 Assignment 1  
###
### Código elaborado por Pablo Paras Ochoa

### Paquetes & Setup ----
library(pacman)
p_load(readxl, tidyverse, ggplot2, janitor, e1071, scales, ggridges)

Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen=999) 

set.seed(42)

### 1A ----
dist <- rexp(10000, 1/2)

mean(dist)

### 1B ----
dist <- tibble(rexp(200, 1/2)) %>% rename(X = 1)

dist_theo <- tibble(c(seq(from = 0, to = 20, by = .01)), dexp(c(seq(from = 0, to = 20, by = .01)), rate = 1/2)) %>% rename(X = 1, Y = 2)

ggplot() +
  geom_histogram(aes(dist$X, after_stat(density)), bins = 30, fill = "steelblue", color = "black") +
  geom_line(aes(dist_theo$X, dist_theo$Y)) +
  theme_minimal() +
  labs(title = "Histogram of the distribution",
       subtitle = "Comparing theoretical distribution with randomly generated data") +
  xlab("Count of X") +
  ylab("X") 

### 1C ----
theo_data <- c(seq(from = 0, to = 20, by = .01))

dist_theo1 <- tibble(theo_data, dexp(theo_data, rate = 1/1)) %>% rename(X = 1, Y = 2)

dist_theo2 <- tibble(theo_data, dexp(theo_data, rate = 1/2)) %>% rename(X = 1, Y = 2)

dist_theo3 <- tibble(theo_data, dexp(theo_data, rate = 1/3)) %>% rename(X = 1, Y = 2)

ggplot() +
  geom_histogram(aes(dist$X, after_stat(density)), bins = 30, fill = "steelblue", color = "black") +
  geom_line(aes(dist_theo1$X, dist_theo1$Y), color = "#FF9E0D", linetype = "dotdash") +
  geom_line(aes(dist_theo2$X, dist_theo2$Y), color = "#1BE2DC") +
  geom_line(aes(dist_theo3$X, dist_theo3$Y), color = "#09E920", linetype = "dotdash") +
  theme_minimal() +
  labs(title = "Histogram of the distribution",
       subtitle = "Comparing theoretical distribution with randomly generated data") +
  xlab("Count of X") +
  ylab("X") 

### 1D ----
dist_theo1 <- tibble(rexp(100000, 1/1)) %>% rename(X = 1)

dist_theo2 <- tibble(rexp(100000, 1/2)) %>% rename(X = 1)

dist_theo3 <- tibble(rexp(100000, 1/3)) %>% rename(X = 1)

ggplot() +
  stat_ecdf(aes(dist$X), color = "black") +
  stat_ecdf(aes(dist_theo1$X), color = "#FF9E0D") +
  stat_ecdf(aes(dist_theo2$X), color = "#1BE2DC") +
  stat_ecdf(aes(dist_theo3$X), color = "#09E920") +
  theme_minimal() +
  labs(title = "Cumulative distribution",
       subtitle = "Comparing theoretical cumulative distribution with randomly generated data") +
  xlab("Count of X") +
  ylab("X") 




