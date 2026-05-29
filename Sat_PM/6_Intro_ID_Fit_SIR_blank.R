# Start from a fresh workspace
rm(list=ls()) # clear workspace
if (!is.null(dev.list())){dev.off()} # clear figures
cat("\014") # clear console

# Required Libraries
if (!require("stats")) install.packages("stats")
library(stats)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)


# Initial conditions
N <- 763
I_init <- 3
R_init <- 0
S_init <- N - I_init - R_init

# Initial guesses
alpha <- 0.1
beta <- 0.001

# Initial conditions for state variables
init_cond <- # FILL IN

# Data from the table in Example 5.1
Idata <- c(3, 6, 25, 75, 227, 296, 258, 236, 192, 126, 71, 28, 11, 7)

# Time span
tspan <- seq(1, 14, by = 0.01)

# Vector with the initial guesses
par <- # FILL IN

# SIR model function
SIR_model <- function(t_vec, y_init, par) {
  dt <- t_vec[2] - t_vec[1]
  alpha <- par[1]
  beta <- par[2]
  
  n <- length(t_vec)
  S <- numeric(n)
  I <- numeric(n)
  R <- numeric(n)
  
  S[1] <- y_init[1]
  I[1] <- y_init[2]
  R[1] <- y_init[3]
  
  for (t in 1:(n - 1)) {
    S[t + 1] <- S[t] - (beta * S[t] * I[t]) * dt
    I[t + 1] <- I[t] + (beta * S[t] * I[t] - alpha * I[t]) * dt
    R[t + 1] <- R[t] + alpha * I[t] * dt
  }
  cbind(S, I, R)
}

# Error function
err_in_dataSIR <- function(par, Idata, init_cond, tspan) {
  y <- # FILL IN
  indices <- round(seq(1, length(tspan), length.out = length(Idata)))
  Infected <- y[indices,2]
  error_in_data <- # FILL IN
  is.na(error_in_data)
  if (is.na(error_in_data)){
    error_in_data <- 1e99
  }
  error_in_data
}

# Optimization to estimate parameters
opt <- optim(# FILL IN )
par_est <- opt$par

# Solve system with estimated parameters
y_est <- SIR_model(tspan, init_cond, par_est)

# Plot solution along with data
# data frame for model output
df_plot <- data.frame(
  Time = tspan,
  Infected = y_est[, 2]
)
# data frame for data
df_data <- data.frame(
  Time = 1:14,
  Infected = Idata
)

ggplot() +
  geom_line(data = df_plot, aes(x = Time, y = Infected), color = "blue", size = 1) +
  geom_point(data = df_data, aes(x = Time, y = Infected), color = "red", size = 3) +
  labs(x = "Time in days", y = "Number of infected individuals") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  ggtitle("Fitted Model and Data") +
  scale_x_continuous(breaks = 1:14) +
  theme(legend.position = "none")

# Plot the residuals
indices <- round(seq(1, length(tspan), length.out = length(Idata)))
Infected <- y_est[indices, 2]
Residuals <- abs(Infected - Idata)

df_residuals <- data.frame(
  Time = 1:14,
  Residuals = Residuals
)

ggplot(df_residuals, aes(x = Time, y = Residuals)) +
  geom_segment(aes(x = Time, xend = Time, y = 0, yend = Residuals), size = 1) +
  geom_point(size = 2) +
  labs(x = "Time in days", y = "Residuals") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  ggtitle("Residuals of the Fit")