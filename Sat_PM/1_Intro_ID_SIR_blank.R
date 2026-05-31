# Start from a fresh workspace
rm(list=ls()) # clear workspace
if (!is.null(dev.list())){dev.off()} # clear figures
cat("\014") # clear console

# Required Libraries
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyverse")) install.packages("tidyverse")
library(ggplot2)
library(tidyverse)

# choose parameter values
parameters <- # FILL IN

# initial state variables
inits <- # FILL IN

# time to run simulation
time_vector <- # FILL IN

# pre-allocate array for variables
S = rep(NA,length(time_vector))
I = # FILL IN
R = # FILL IN

# place initial conditions in first position of array
nTotal = sum(inits)
S[1] = inits["S"]
I[1] = # FILL IN
R[1] = # FILL IN

# shorter access for parameters
beta = parameters["beta"]
gamma = parameters["gamma"]
kappa = parameters["kappa"]

# loop over time vector
for (t in 1:(length(time_vector)-1)){
  # Epidemic events
  newly_infected = # FILL IN
  recovered_today= # FILL IN
  losing_immunity_today = # FILL IN
  
  # Updating epidemic equations
  S[t+1] = S[t] - newly_infected                   + losing_immunity_today
  I[t+1] = I[t] + newly_infected - recovered_today
  R[t+1] = R[t]                  + recovered_today - losing_immunity_today
  
}

# place output in data frame for plotting
df <- as.data.frame(time_vector)
df <- cbind(df,S,I,R)

# pivot to long data frame
df.long = pivot_longer(df, cols = c("S", "I", "R"), # select the columns you want to make longer
                       names_to = "compartment", # new column name that contains the names of the old columns (S, I, R)
                       values_to = "population") # values from the old columns (S, I, R)


# plot
ggplot(data = df.long, aes(x = time_vector, y = population, color = compartment) ) + # Data and aesthetics
  geom_line() +  # Line plot
  labs(x = "Time, days",                                # x label
       y = "Population size",                           # y label
       title = "SIR model",                             # plot title
       color = "") +                                    # legend title
  theme_minimal()+
  scale_color_discrete(breaks=c('S', 'I', 'R'))         # reorder lines


# calculating R0
R0 <- # FILL IN
print(R0)

