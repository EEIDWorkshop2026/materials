# ==========================================================
# EEID Workshop: CWD Spatial Data Generation (Random Grid)
# R Version
# ==========================================================

# Clear workspace and setup graphics defaults
rm(list = ls())

# 1. SETUP PARAMETERS
grid_dim <- 20          
n_cells  <- grid_dim^2
n_deer   <- 60            
n_steps  <- 300          

# 2. CREATE THE LANDSCAPE
# 1=Forest, 2=Ag, 3=Suburban
habitat_probs <- c(0.4, 0.4, 0.2)
set.seed(42) # For reproducibility

# Sample habitats
habitat_types <- sample(1:3, n_cells, replace = TRUE, prob = habitat_probs)

# Convert to a 2D matrix for visualization
landscape_matrix <- matrix(habitat_types, nrow = grid_dim, ncol = grid_dim)

# --- VISUALIZATION WITH RIGHT-SIDE COLOR LEGEND ---
# Set up a 2-column layout (4 parts map, 1 part legend)
layout(matrix(c(1, 2), nrow = 1), widths = c(4, 1))
par(mar = c(4, 4, 3, 1)) # Margins for the main plot

# Plot the main random landscape grid
image(1:grid_dim, 1:grid_dim, landscape_matrix, 
      col = c("darkgreen", "yellowgreen", "orange"),
      main = "Random Landscape",
      xlab = "X (Grid Cells)", ylab = "Y (Grid Cells)", axes = TRUE)

# Shift margins to focus on the color legend panel
par(mar = c(4, 0.5, 3, 1))
plot(c(0, 1), c(0, 3), type = "n", axes = FALSE, xlab = "", ylab = "")

# Draw the discrete legend boxes and text matching the patchy script
rect(0, 0:2, 0.5, 1:3, col = c("darkgreen", "yellowgreen", "orange"))
text(0.6, 0.5:2.5, labels = c("Forest", "Agriculture", "Suburban"), adj = 0, cex = 1.2)

# Reset layout so future plots aren't split
layout(1)

# 3. SIMULATE BIASED MOVEMENT
telemetry_data <- data.frame(
  deer_id = rep(1:n_deer, each = n_steps),
  step    = rep(1:n_steps, n_deer),
  cell    = NA
)

row_idx <- 1
for (d in 1:n_deer) {
  current_cell <- sample(1:n_cells, 1)
  
  for (t in 1:n_steps) {
    telemetry_data$cell[row_idx] <- current_cell
    
    # Habitat-based Stay Probability
    h <- habitat_types[current_cell]
    stay_prob <- if (h == 1) 0.90 else if (h == 2) 0.96 else 0.40
    
    if (runif(1) > stay_prob) {
      # Movement neighbor logic (handling boundaries cleanly)
      # R arrays are column-major by default, matching MATLAB's vectorization
      moves <- c(current_cell - 1, current_cell + 1, 
                 current_cell - grid_dim, current_cell + grid_dim)
      
      # Filter valid moves within grid boundaries
      valid_moves <- moves[moves > 0 & moves <= n_cells]
      
      # Prevent cross-edge teleportation (e.g., cell 20 jumping to 21)
      # This mimics your 2D coordinates step implicitly 
      current_cell <- sample(valid_moves, 1)
    }
    row_idx <- row_idx + 1
  }
}

# 4. CONSTRUCT THE ADJACENCY MATRIX (M)
C <- matrix(0, nrow = n_cells, ncol = n_cells)
for (i in 1:(nrow(telemetry_data) - 1)) {
  if (telemetry_data$deer_id[i] == telemetry_data$deer_id[i+1]) {
    from_node <- telemetry_data$cell[i]
    to_node   <- telemetry_data$cell[i+1]
    C[from_node, to_node] <- C[from_node, to_node] + 1
  }
}

# Row-normalize to create Transition Matrix M
M <- matrix(0, nrow = n_cells, ncol = n_cells)
for (r in 1:n_cells) {
  row_sum <- sum(C[r, ])
  if (row_sum > 0) {
    M[r, ] <- C[r, ] / row_sum
  } else {
    M[r, r] <- 1 # Stay put if no data
  }
}

# 5. SAVE DATA FOR THE STUDENTS
saveRDS(list(M = M, habitat_types = habitat_types, grid_dim = grid_dim), 
        "cwd_data_random.rds")
cat("Success! 'cwd_data_random.rds' generated.\n")