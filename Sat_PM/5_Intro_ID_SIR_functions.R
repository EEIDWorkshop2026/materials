# Start from a fresh workspace
rm(list=ls()) # clear workspace
if (!is.null(dev.list())){dev.off()} # clear figures
cat("\014") # clear console

# Required Libraries
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# source file with functions
source("4_Epi_functions.R")

# define 
n = 5 # number of patches
# create n identical patches
IDs = seq(1,n) # ID of patch
initialInf_vec = rep(1,n) # initial number of infected individuals
totalPop_vec = sample(10000,n) # total population size
beta_vec = rep(2/3,n) # transmission parameter
kappa_vec = rep(1/30,n) # waning immunity parameter
gamma_vec = rep(1/3,n) # recovery parameter
day_list = seq(0,50,by=1) # days for simulation


# initialize populations
HPop1 <- InitiatePop(IDs,initialInf_vec,totalPop_vec,beta_vec,gamma_vec,kappa_vec)

# run simulation
HPop1 <- runSimNoMobility(HPop1,day_list)

# rename columns
HPop1$all_spread <- HPop1$all_spread %>% rename_with(~ paste0("I", .), is.numeric)

# pivot to long data frame
df.long = pivot_longer(HPop1$all_spread, cols = c(colnames(HPop1$all_spread[,1:n+1])), # columns you want to make longer
                       names_to = "compartment", # new column name that contains the names of the old columns (S, I, R)
                       values_to = "population") # values from the old columns (S, I, R)


# plot
ggplot(data = df.long, aes(x = Iday, y = population, color = compartment) ) + # Data and aesthetics
  geom_line() +  # Line plot
  labs(x = "Time, days",                                # x label
       y = "Population size",                           # y label
       title = "SIR model",                             # plot title
       color = "") +                                    # legend title
  theme_minimal()



