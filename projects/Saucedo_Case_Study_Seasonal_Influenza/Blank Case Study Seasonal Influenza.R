# Start from a fresh workspace
rm(list=ls()) # clear workspace
if (!is.null(dev.list())){dev.off()} # clear figures
cat("\014") # clear console

# If you are missing any packages, use the following code. 
# if (!require("NameofPackage")) install.packages("NameofPackage")


######################################

# Simulation of system of equations

######################################

# Load all required libraries
library(tidyr)
library(ggplot2)

# Number of patches
n_patches <- 3

# Parameters (assumed equal across patches for simplicity, but can be customized)
beta  <- #FILL OUT# 
sigma <- #FILL OUT# 
gamma <- #FILL OUT# 
mu <-#FILL OUT# 

# Define per-captia movement rates (m_ij = from j to i)
m_21 <- 0.10; m_31 <- 0.15  # Leaving Patch 1
m_12 <- 0.15; m_32 <- 0.15  # Leaving Patch 2
m_13 <- 0.25; m_23 <- 0.25  # Leaving Patch 3

# Laplacian matrix 
migration_mat <- matrix(c(
  -(m_21 + m_31),          m_12,          m_13, 
  m_21, -(m_12 + m_32),          m_23,           
  m_31,          m_32, -(m_13 + m_23)           
), nrow = 3, ncol = 3, byrow = TRUE)

# Initial Conditions
S_init <- c(#FILL OUT#, #FILL OUT#, #FILL OUT# )
E_init <- c(#FILL OUT#, #FILL OUT#, #FILL OUT# )
I_init <- c(#FILL OUT#, #FILL OUT#, #FILL OUT# )
R_init <- c(#FILL OUT#, #FILL OUT#, #FILL OUT# )

time_vector <- seq(0, #FILL OUT# , 1)

n_steps <- length(time_vector)

# Pre-allocate arrays (Rows = Time, Columns = Patches)
S <- matrix(NA, nrow = n_steps, ncol = n_patches)
E <- matrix(NA, nrow = n_steps, ncol = n_patches)
I <- matrix(NA, nrow = n_steps, ncol = n_patches)
R <- matrix(NA, nrow = n_steps, ncol = n_patches)

# Insert initial conditions
S[1, ] <- S_init
E[1, ] <- E_init
I[1, ] <- I_init
R[1, ] <- R_init

# System of difference equations 
for (t in 1:(n_steps - 1)) {
  
  # Total population in each patch at time t
  nTotal <- S[t, ] + E[t, ] + I[t, ] + R[t, ]
  
  # Disease dynamics within each patch
  newly_infected        <- #FILL OUT# 
  newly_infectious      <- #FILL OUT# 
  recovered_today       <- #FILL OUT# 
  death_susceptible     <- #FILL OUT# 
  death_exposed         <- #FILL OUT# 
  death_infected        <- #FILL OUT# 
  death_recovered       <- #FILL OUT# 
  
  # Net migration for each compartment 
  # migration_mat %*% Vector calculates incoming minus outgoing individuals per patch
  S_migration <- migration_mat %*% S[t, ]
  E_migration <- migration_mat %*% E[t, ]
  I_migration <- migration_mat %*% I[t, ]
  R_migration <- migration_mat %*% R[t, ]
  
  # Update equations 
  S[t+1, ] <- S[t, ] - newly_infected - death_susceptible + S_migration
  E[t+1, ] <- E[t, ] + newly_infected - (newly_infectious+death_exposed)       + E_migration
  I[t+1, ] <- I[t, ] + newly_infectious - (recovered_today+death_infected)     + I_migration
  R[t+1, ] <- R[t, ] + recovered_today - death_recovered + R_migration
}


# Convert matrices to data frames (df) and label patches
df_list <- list()
for(p in 1:n_patches){
  df_list[[p]] <- data.frame(
    time = time_vector,
    Patch = paste("Patch", p),
    S = S[, p], E = E[, p], I = I[, p], R = R[, p]
  )
}

# Combines into single data frame
df_master <- do.call(rbind, df_list)

# Pivot to long format
df_long <- pivot_longer(df_master, cols = c("S", "E", "I", "R"), 
                        names_to = "compartment", values_to = "population")


# Filter the data to keep only E_t and I_t
df_filtered <- subset(df_long, compartment %in% c("E", "I"))

# Plot the exposed and infected compartments 
ggplot(data = df_filtered, aes(x = time, y = population, color = compartment)) +
  geom_line(size = 1) +
  facet_wrap(~Patch) +  
  labs(x = "Time, days", 
       y = "Population size", 
       title = "SEIRS Model: Exposed and Infected Populations",
       color = "Compartment") +
  theme_minimal() +
  scale_color_discrete(breaks = c('E', 'I')) # Clean up legend breaks



# Plot all compartments 
#ggplot(data = df_long, aes(x = time, y = population, color = compartment)) +
#  geom_line(size = 1) +
#  facet_wrap(~Patch) +  # Creates a separate subplot for each patch
#  labs(x = "Time, days", 
#       y = "Population size", 
#       title = "3-Patch SEIRS Epidemic Model with Migration",
#       color = "Compartment") +
#  theme_minimal() +
#  scale_color_discrete(breaks = c('S', 'E', 'I', 'R'))



######################################

# Fitting for a single patch 

######################################

# Initial conditions
N <- #FILL OUT# 
I_init <- #FILL OUT#       
E_init <- #FILL OUT#       
R_init <- #FILL OUT# 
S_init <- N - E_init - I_init - R_init

# Initial guesses
beta  <- #FILL OUT#      
sigma <- #FILL OUT#      
gamma <- #FILL OUT#      
mu    <- #FILL OUT#    

# Vector with the four initial guesses
par <- c(beta, sigma, gamma, mu)

# Initial conditions vector
init_cond <- c(S_init, E_init, I_init, R_init)

# Data parsed from your 31-day table
Idata <- #FILL OUT# 

# Time span updated to match 31 days
tspan <- seq(1, #FILL OUT# , by = 0.01)

# SEIR model function 
SEIR_standard_incidence_model <- function(t_vec, y_init, par) {
  dt    <- t_vec[2] - t_vec[1]
  beta  <- par[1]
  sigma <- par[2]
  gamma <- par[3]
  mu    <- par[4]
  
  n <- length(t_vec)
  S <- numeric(n)
  E <- numeric(n)
  I <- numeric(n)
  R <- numeric(n)
  
  S[1] <- y_init[1]
  E[1] <- y_init[2]
  I[1] <- y_init[3]
  R[1] <- y_init[4]
  
  for (t in 1:(n - 1)) {
    # Calculate current dynamic total population
    N_t <- S[t] + E[t] + I[t] + R[t]
    
    # Avoid division by zero if population dies out completely
    if (N_t <= 0) N_t <- 1e-6 
    
    # Standard incidence force of infection term
    lambda <- (beta * I[t]) / N_t
    
    S[t + 1] <- S[t] - (lambda * S[t] + mu * S[t]) * dt
    E[t + 1] <- E[t] + (lambda * S[t] - sigma * E[t] - mu * E[t]) * dt
    I[t + 1] <- I[t] + (sigma * E[t] - gamma * I[t] - mu * I[t]) * dt
    R[t + 1] <- R[t] + (gamma * I[t] - mu * R[t]) * dt
  }
  cbind(S, E, I, R)
}

# Error function for optimization
err_in_dataSEIR <- function(par, Idata, init_cond, tspan) {
  # Prevent negative parameters during search steps
  if(any(par < 0)) return(1e99)
  
  y <- SEIR_standard_incidence_model(tspan, init_cond, par)
  
  # Map indices to the 31 daily data points
  indices <- round(seq(1, length(tspan), length.out = length(Idata)))
  Infected <- y[indices, 3] # 'I' is column 3
  
  error_in_data <- sum((Infected - Idata)^2)
  
  if (is.na(error_in_data)){
    error_in_data <- 1e99
  }
  error_in_data
}

# Optimization to estimate the 4 parameters
opt <- optim(par, fn = #FILL OUT# , Idata = #FILL OUT# , init_cond = init_cond, tspan = tspan)
par_est <- opt$#FILL OUT# 

# Print the estimated parameters
cat("Estimated Parameters",
    "Beta: ", par_est[1], "\n",
    "Sigma:", par_est[2], "\n",
    "Gamma:", par_est[3], "\n",
    "Mu:   ", par_est[4], "\n")

# Solve system with estimated parameters
y_est <- SEIR_standard_incidence_model(tspan, init_cond, par_est)

# Data frame for model output curve
df_plot <- data.frame(
  Time = tspan,
  Infected = y_est[, 3]
)

# Data frame for actual table observations
df_data <- data.frame(
  Time = 1:31,
  Infected = Idata
)

# Plotting
ggplot() +
  geom_line(data = df_plot, aes(x = Time, y = Infected), color = "blue", size = 1) +
  geom_point(data = df_data, aes(x = Time, y = Infected), color = "red", size = 3) +
  labs(x = "Time in days", y = "Number of infected individuals") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  ggtitle("Fitted Standard Incidence SEIR Model vs Data") +
  scale_x_continuous(breaks = seq(1, 31, by = 2)) +
  theme(legend.position = "none")



#################################

# Estimating adjacency matrix

################################

# Load all required libraries
library(Matrix)
library(limSolve)

# Proportion of time an individual spends at a particular location.  
Mdata <- matrix(c(#FILL OUT# , #FILL OUT# , #FILL OUT# ,
  #FILL OUT# , #FILL OUT# , #FILL OUT# ,
                  #FILL OUT# , #FILL OUT# , #FILL OUT# ), nrow = 3, byrow = TRUE)

# Size of matrix 
n <- nrow(Mdata)

# Find the inverse of the data matrix 
vn <- solve(Mdata)  

D <- diag(n)  
T_mat <- (vn - diag(n)) %*% D  

# Vectorize matrix T 
d <- as.vector(T_mat)  

#  Building the multi-dimensional array A 
A <- array(0, dim = c(n, n, n))
for (i in 1:n) {
  A[i, , i] <- rep(1, n)
}

#  Extract slices into a list for block diagonal creation
M <- vector("list", n)
for (i in 1:n) {
  M[[i]] <- A[, , i]
}

#  Create the block diagonal matrix and calculate C
W <- as.matrix(bdiag(M))
C <- W - diag(nrow(W))

#  Setting up the Aeq equality constraint matrix
Aeq <- matrix(0, nrow = n^2, ncol = n^2)
for (i in 1:n) {
  diagpos <- i
  ind <- (i - 1) * n + diagpos
  Aeq[ind, ind] <- 1
}

beq <- rep(0, nrow(Aeq))
lb <- rep(0, nrow(Aeq))

# Run the Constrained Linear Least Squares Optimization
# E and F = objective, A and B = equalities, lower = lower bounds
Estimation_results <- lsei(E = C, 
                           F = d, 
                           A = Aeq, 
                           B = beq, 
                           lower = lb, tol = 1e-10)

#  Reshape back into an (n x n) Matrix
x <- Estimation_results$X
Adjacency <- matrix(x, nrow = n, ncol = n)


##############################

#Visualization 

##############################

# Load all required libraries
library(sf)
library(terra)
library(igraph)   
library(stringr)  

# From the GitHub, download the files named gadm41_USA_2 and extensions .dbf, .prj, .shp, .shx, and .cpg.
# The data was acquired from https://gadm.org/data.html 

# Reads shapefile using sf
USoA <- read_sf(dsn = ".", layer = "gadm41_USA_2")
Virginia <- subset(USoA, NAME_1 == "Virginia") # Gets the geoinfo for Virginia. 

# To get an idea of the counties in Virginia, visit: https://gisgeography.com/virginia-county-map/ 

# Select your specific 3 counties 
selected_counties <- subset(Virginia, NAME_2 %in% c("#FILL OUT#", "#FILL OUT#", "#FILL OUT#"))

# Extract the names vector explicitly in the spatial dataframe order
county_names <- selected_counties$NAME_2
n <- length(county_names) # Dynamically set n to 3

# Handle your matrix data and assign names
VAdata <- Adjacency
Matrixdata <- as.matrix(VAdata[1:n, 1:n])
rownames(Matrixdata) <- county_names
colnames(Matrixdata) <- county_names

# Creates network from original adj matrix and creates edgelist for plotting. 
VANetwork <- graph_from_adjacency_matrix(Matrixdata, mode = "directed", weighted = TRUE, diag = FALSE)
edgeList <- as_edgelist(VANetwork, names = TRUE)

# Clean edge strings if necessary
nameLat <- str_replace_all(as.vector(edgeList[, 1]), "[[:punct:]]", " ")
nameLon <- str_replace_all(as.vector(edgeList[, 2]), "[[:punct:]]", " ")

#  Calculate centroids ONLY for the 3 selected counties
trueCentroids <- st_centroid(selected_counties)
coords <- st_coordinates(trueCentroids)

# Create the layout dataframe using the isolated spatial subset
LLC <- data.frame(
  "name" = county_names, 
  "lon"  = coords[, 1], 
  "lat"  = coords[, 2]
)

# Convert coordinates to an igraph layout matrix
LLCmatrix <- as.matrix(LLC[, c("lon", "lat")])

# Plotting the Network over the full Virginia Map
plot(st_geometry(Virginia), border = "black") # Plots full base map
plot.igraph(
  VANetwork, 
  layout = LLCmatrix, 
  add = TRUE, 
  rescale = FALSE, 
  edge.arrow.size = 0.5, 
  edge.curved = TRUE, 
  vertex.label = V(VANetwork)$name, # Displays county names on the nodes
  vertex.label.cex = 0.8,           # Font size for labels
  vertex.label.color = "black",
  vertex.size = 4,
  vertex.color = "red"
)



####################################

# Computing the basic reproduction number and elasticities 

#####################################


# Load all required libraries
library(numDeriv)


# Write R0 as a function 
R0<- function(beta, sigma, gamma, mu){
  
  #FILL OUT#
  
}


# Parameter values
pars <- c(
  beta=par_est[1], 
  sigma=par_est[2], 
  gamma=par_est[3], 
  mu=par_est[4]
  
)

# Function to use for numDeriv
func <- function(par){
  
  beta <- par[1]
  sigma     <- par[2]
  gamma     <- par[3]
  mu  <- par[4]
  
  R0(beta, sigma, gamma, mu)
}


# Compute patch specific R0
R0eval <- as.numeric(func(pars))


# The remaining part in this subsection computes R0 via Next Generation Matrix 

# Define global parameters per patch using estimated parameters
beta_vec  <- rep(par_est[1], n_patches)
sigma_vec <- rep(par_est[2], n_patches)
gamma_vec <- rep(par_est[3], n_patches)
mu_vec    <- rep(par_est[4], n_patches)

# Build sub-matrices (size n_patches x n_patches)
I_matrix <- diag(n_patches) # Identity matrix for alignment

B_sub     <- diag(beta_vec)
Sigma_sub <- diag(sigma_vec + mu_vec)
Gamma_sub <- diag(gamma_vec + mu_vec)
Trans_sub <- diag(sigma_vec) # E -> I transition rate

# Collect the large 2n x 2n Block Matrices
# F Matrix (New infections)
F_top    <- cbind(matrix(0, n_patches, n_patches), B_sub)
F_bottom <- cbind(matrix(0, n_patches, n_patches), matrix(0, n_patches, n_patches))
F_matrix <- rbind(F_top, F_bottom)

# V Matrix (Transitions, Outflows, and Network Migration)
V_top    <- cbind(Sigma_sub - migration_mat, matrix(0, n_patches, n_patches))
V_bottom <- cbind(-Trans_sub,                 Gamma_sub - migration_mat)
V_matrix <- rbind(V_top, V_bottom)

# Compute the Next Generation Matrix: K = F %*% Vector Inverse of V
FInV_matrix <- F_matrix %*% solve(V_matrix)

# Extract the eigenvalues of K
eigenvalues <- eigen(FInV_matrix)$values

# R0 is the largest absolute eigenvalue 
R0_network <- max(abs(eigenvalues))



####################################

# Computing the elasticities 

#####################################



# Compute partial derivatives of I* with respect to the four parameters
partials <- grad(func, pars)


# Compute elasticities (E = (dR0/dp)*(p/R0))
elasticities <- #FILL OUT#

# Put results into data frame. 
results <- data.frame(
  Parameter  = names(pars),
  Derivative = partials,
  Elasticity = elasticities,
  row.names = NULL
)

# Print the results in the console 
print(results)





