library(drc)
library(tidyverse)
library(plyr)
source("functions.R")

group <- c("a", "a", "a", "b", "b", "b")
x <- c(0, 5, 10, 0, 5, 10)
y <- c(6, 5.5, 5, 5, 5.5, 6)
df <- data.frame(individual=group, age=x, score=y)
group1 <- subset(df, individual == "a")
model <- drm(score ~ age, data = group1, fct = L.4(), type = "continuous")
k <- model$coefficients[[1]]
summary <- all_ks(df, "age", "score", "individual")


