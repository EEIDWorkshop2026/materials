# ==============================================================================
# EEID Workshop: Chronic Wasting Disease (CWD) Spatial Simulation
# ==============================================================================

rm(list = ls())
if (!require("av")) install.packages("av")
library(av) # Required to record and compile the .mp4 video output

# --- 1. LOAD HABITAT MAP AND MOVEMENT MATRIX ---
scenario_file <- "cwd_data_patchy.rds" 
if (!file.exists(scenario_file)) {
  stop(paste("Could not find", scenario_file, "- Check your working directory!"))
}

data <- readRDS(scenario_file)
M             <- data$M             # Deer movement matrix (where they travel)
habitat_types <- data$habitat_types # Underlying landscape properties
grid_dim      <- data$grid_dim      # Width/height of our map
n_cells       <- grid_dim^2         # Total number of landscape patches

# --- 2. SET INITIAL HOST POPULATIONS ---
S <- rep(25, n_cells)      # Start with 25 healthy deer in every patch
I <- rep(0, n_cells)       # Start with zero infected deer everywhere...
P <- rep(0, n_cells)       # ...and clean soil with zero prions

# Introduce the disease by dropping 2 sick deer into one random patch
set.seed(10) 
index_cell <- sample(1:n_cells, 1)
I[index_cell] <- 2         

# --- 3. RUNTIME AND TIME SERIES DATA TRACKING ---
timesteps <- 200
total_S <- rep(0, timesteps) # Tracks global healthy population over time
total_I <- rep(0, timesteps) # Tracks global infected population over time
total_P <- rep(0, timesteps) # Tracks global environmental prion load over time

# --- 4. BIOLOGICAL PARAMETERS ---
beta_d <- 0.012   # Direct transmission (Deer-to-Deer contact rate)
beta_p <- 0.002   # Indirect transmission (Deer-to-Soil contact rate)
sigma  <- 0.20   # Shedding rate (Amount of prions left in soil per sick deer)
gamma  <- 0.01   # Environmental decay rate (How fast prions degrade in soil)
mort   <- 0.06   # Disease-induced mortality rate (How fast sick deer die)

# --- 5. GRAPHICS ENGINE  ---
render_frame <- function(t_current, current_I, current_P) {
  
  # Standardized color palettes
  infected_palette <- colorRampPalette(c("#FFFFFF", "#FFE082", "#FF7043", "#B71C1C"))(100)
  prion_palette    <- colorRampPalette(c("#FFFFFF", "#E1BEE7", "#BA68C8", "#4A148C"))(100)
  
  # Set fixed visual thresholds
  abs_max_I  <- 12      
  abs_max_P  <- 100     
  abs_max_Y2 <- 25000   
  
  # Calculate current global sums for the title (rounded to nearest whole animal)
  current_total_S <- round(total_S[t_current])
  current_total_I <- round(total_I[t_current])
  
  # Partition the screen neatly
  layout_matrix <- matrix(c(
    1, 2,  3, 4,
    5, 5,  6, 6
  ), nrow = 2, byrow = TRUE)
  layout(layout_matrix, widths = c(5, 1, 5, 1), heights = c(1, 1))
  
  # Font scale set to 0.85 (balanced readability and safety)
  par(cex = 0.85)
  
  # SUB-PLOT 1: Map of Infected Deer Density
  par(mar = c(2.0, 2.8, 2.0, 0.5)) 
  I_matrix <- matrix(current_I, nrow = grid_dim, ncol = grid_dim)
  image(1:grid_dim, 1:grid_dim, I_matrix, col = infected_palette, zlim = c(0, abs_max_I),
        xlab = "", ylab = "", main = paste("Infected Host Density (T =", t_current, ")"), axes = FALSE)
  axis(1) 
  axis(2, las = 1) 
  box() 
  
  # SUB-PLOT 2: Infected Deer Scale Bar
  par(mar = c(2.0, 0.2, 2.0, 2.5))
  legend_strip_I <- matrix(seq(0, abs_max_I, length.out = 100), nrow = 1)
  image(x = 1, y = seq(0, abs_max_I, length.out = 100), z = legend_strip_I, 
        col = infected_palette, zlim = c(0, abs_max_I), axes = FALSE, xlab = "", ylab = "")
  axis(4, at = c(0, abs_max_I/2, abs_max_I), labels = c("0.0", round(abs_max_I/2, 1), round(abs_max_I, 1)), las = 1)
  
  # SUB-PLOT 3: Map of Environmental Prions in Soil
  par(mar = c(2.0, 2.8, 2.0, 0.5)) 
  P_matrix <- matrix(current_P, nrow = grid_dim, ncol = grid_dim)
  image(1:grid_dim, 1:grid_dim, P_matrix, col = prion_palette, zlim = c(0, abs_max_P),
        xlab = "", ylab = "", main = "Prions in Soil", axes = FALSE)
  axis(1)
  axis(2, las = 1)
  box()
  
  # SUB-PLOT 4: Environmental Prion Scale Bar
  par(mar = c(2.0, 0.2, 2.0, 2.5))
  legend_strip_P <- matrix(seq(0, abs_max_P, length.out = 100), nrow = 1)
  image(x = 1, y = seq(0, abs_max_P, length.out = 100), z = legend_strip_P, 
        col = prion_palette, zlim = c(0, abs_max_P), axes = FALSE, xlab = "", ylab = "")
  axis(4, at = c(0, abs_max_P/2, abs_max_P), labels = c("0.0", round(abs_max_P/2, 1), round(abs_max_P, 1)), las = 1)
  
  # SUB-PLOT 5: Global Disease Prevalence Curve (%)
  par(mar = c(3.2, 4.2, 2.0, 1.0)) 
  living_herd <- total_S[1:t_current] + total_I[1:t_current]
  prevalence  <- ifelse(living_herd > 0, (total_I[1:t_current] / living_herd) * 100, 0)
  
  # Dynamically embeds (S = #, I = #) right into the main header
  title_prev <- paste0("Prevalence (S = ", current_total_S, ", I = ", current_total_I, ")")
  
  plot(1:t_current, prevalence, type = "l", col = "darkred", lwd = 2.0,
       xlim = c(1, timesteps), ylim = c(0, 50), 
       xlab = "Time Step", ylab = "Prevalence (%)", main = title_prev, las = 1)
  grid()
  
  # SUB-PLOT 6: Global Prion Accumulation Curve over time
  par(mar = c(3.2, 4.2, 2.0, 1.0)) 
  plot(1:t_current, total_P[1:t_current], type = "l", col = "purple", lwd = 2,
       xlim = c(1, timesteps), ylim = c(0, abs_max_Y2), 
       xlab = "Time Step", ylab = "Global Pathogen Load", main = "Total Prion Accumulation", las = 1)
  grid()
}

# --- 6. CORE SIMULATION RUNTIME ENGINE ---
I_history <- matrix(0, nrow = timesteps, ncol = n_cells)
P_history <- matrix(0, nrow = timesteps, ncol = n_cells)

cat("Processing mathematical engine and rendering video layers...\n")

# Open background recorder (Enlarged to a balanced square 1000x1000 resolution)
av_capture_graphics({
  for (t in 1:timesteps) {
    
    # --------------------------------------------------------------------
    # [STUDENT EXERCISE : INSERT MANAGEMENT/MITIGATION POLICIES HERE
    # --------------------------------------------------------------------
    
    # Step A: Calculate Biological Transitions (SIWR Difference Equations)
    new_inf <- (beta_d * S * I) + (beta_p * S * P)
    new_inf <- pmin(new_inf, S) 
    
    S <- S - new_inf
    I <- I + new_inf - (mort * I)
    P <- P * (1 - gamma) + (sigma * I) 
    
    # Step B: Spatial Host Migration (Eulerian Matrix Translocation)
    S <- as.vector(S %*% M)
    I <- as.vector(I %*% M)
    
    # Step C: Log Data Streams to Storage History
    total_S[t] <- sum(S)
    total_I[t] <- sum(I)
    total_P[t] <- sum(P)
    
    I_history[t, ] <- I
    P_history[t, ] <- P
    
    render_frame(t, I, P)
  }
}, output = "CWD_Simulation_Output.mp4", width = 1000, height = 1000, framerate = 15)

# Safely close background video tools and hand screen controls back to RStudio
graphics.off() 

# --- 7. SNAPSHOT PLOT COMPILER (Saves to RStudio Plots History Panel) ---
cat("Populating your RStudio Plots history pane...\n")
snapshot_times <- c(1, 50, 100, 150, timesteps)

# Enforce a fresh canvas configuration before entering the dashboard loop
layout(1)

for (snapshot_t in snapshot_times) {
  # Attempt to generate the detailed multi-plot layout frame
  result <- try({
    render_frame(snapshot_t, I_history[snapshot_t, ], P_history[snapshot_t, ])
  }, silent = TRUE)
  
  # Educational Catch: If a student's monitor space is too tight, print guidance
  if (inherits(result, "try-error")) {
    cat("\n----------------------------------------------------------------------")
    cat("\n[NOTE] Dashboard snapshots could not render in your RStudio Plots tab.")
    cat("\n       -> REASON: Your physical 'Plots' window pane is too small.")
    cat("\n       -> FIX: Use your mouse to stretch the Plots pane wider & taller,")
    cat("\n               then highlight and re-run.")
    cat("\n----------------------------------------------------------------------\n\n")
    
    # Critical step: Break out of the loop and safely reset layout parameters
    # so the broken state doesn't ruin the final habitat map in Section 8!
    layout(1) 
    break
  }
}

# ==============================================================================
# Step 8: Plot Underlying Habitat Study Map as a reference overlay at the very end
# ==============================================================================
cat("Rendering final static habitat landscape map...\n")

habitat_palette <- c("darkgreen", "yellowgreen", "orange")
H_matrix        <- matrix(habitat_types, nrow = grid_dim, ncol = grid_dim)

# Set up the 2-column layout (Map on left, Legend panel on right)
layout(matrix(c(1, 2), nrow = 1), widths = c(4, 1))
par(mar = c(4.5, 4.5, 3, 1))

# 1. Draw the actual habitat map
image(1:grid_dim, 1:grid_dim, H_matrix, 
      col = habitat_palette, zlim = c(1, 3),
      main = "Landscape",
      xlab = "X (Grid Cells)", ylab = "Y (Grid Cells)", axes = TRUE)

# 2. Draw your clean rectangle legend panel
par(mar = c(4.5, 0.5, 3, 1))
plot(c(0, 1), c(0, 3), type = "n", axes = FALSE, xlab = "", ylab = "")
rect(0, 0:2, 0.4, 1:3, col = habitat_palette)
text(0.5, 0.5:2.5, labels = c("Forest (1)", "Agriculture (2)", "Suburban (3)"), adj = 0, cex = 1.0)

# Clear states and cleanly reset layout AND margins completely to safe factory configurations
layout(1) 
par(mar = c(5.1, 4.1, 4.1, 2.1)) 
cat("\nProcess Complete!\n")

