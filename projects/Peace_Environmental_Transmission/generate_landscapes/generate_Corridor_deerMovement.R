# ==========================================================
# EEID Workshop: CWD Spatial Data Generation (Bottleneck Grid)
# R Version - Scenario 3
# ==========================================================

rm(list = ls())

# 1. SETUP PARAMETERS
grid_dim <- 20          
n_cells  <- grid_dim^2
n_deer   <- 60            
n_steps  <- 300          

# 2. CREATE A BOTTLENECK LANDSCAPE (Two Counties Connected by a Gap)
# Start by filling the entire landscape with Suburban Barrier (Type 3)
habitat_matrix <- matrix(3, nrow = grid_dim, ncol = grid_dim)

# County 1: Big Ag/Forest Block on the Left (Columns 1 to 8)
habitat_matrix[, 1:8] <- 1        # Forest base
habitat_matrix[1:10, 1:5] <- 2    # Ag hotspot in Top-Left

# County 2: Big Ag/Forest Block on the Right (Columns 13 to 20)
habitat_matrix[, 13:20] <- 1      # Forest base
habitat_matrix[11:20, 16:20] <- 2 # Ag hotspot in Bottom-Right

# The Bottleneck: Create a narrow Forest Corridor (Type 1) through the Suburban Wall
# Right in the middle rows (rows 10-11) through columns 9-12
habitat_matrix[10:11, 9:12] <- 1

# Flatten matrix to vector
habitat_types_vec <- as.vector(habitat_matrix)

# --- VISUALIZATION WITH RIGHT-SIDE COLOR LEGEND ---
layout(matrix(c(1, 2), nrow = 1), widths = c(4, 1))
par(mar = c(4, 4, 3, 1))

image(1:grid_dim, 1:grid_dim, habitat_matrix, 
      col = c("darkgreen", "yellowgreen", "orange"),
      main = "Two-County Bottleneck",
      xlab = "X (Grid Cells)", ylab = "Y (Grid Cells)", axes = TRUE)

# Draw the legend panel
par(mar = c(4, 0.5, 3, 1))
plot(c(0, 1), c(0, 3), type = "n", axes = FALSE, xlab = "", ylab = "")
rect(0, 0:2, 0.5, 1:3, col = c("darkgreen", "yellowgreen", "orange"))
text(0.6, 0.5:2.5, labels = c("Forest", "Agriculture", "Suburban"), adj = 0, cex = 1.2)

layout(1) # Reset layout

# 3. SIMULATE BIASED MOVEMENT
telemetry_data <- data.frame(
  deer_id = rep(1:n_deer, each = n_steps),
  step    = rep(1:n_steps, n_deer),
  cell    = NA
)

row_idx <- 1
for (d in 1:n_deer) {
  # Start deer evenly across the live habitable zones (Forest or Ag)
  habitable_cells <- which(habitat_types_vec != 3)
  current_cell <- sample(habitable_cells, 1)
  
  for (t in 1:n_steps) {
    telemetry_data$cell[row_idx] <- current_cell
    
    h <- habitat_types_vec[current_cell]
    stay_prob <- if (h == 1) 0.90 else if (h == 2) 0.96 else 0.40
    
    if (runif(1) > stay_prob) {
      c <- ((current_cell - 1) %/% grid_dim) + 1
      r <- ((current_cell - 1) %% grid_dim) + 1
      
      dr <- c(-1, 1, 0, 0)
      dc <- c(0, 0, -1, 1)
      
      valid <- (r + dr) >= 1 & (r + dr) <= grid_dim & (c + dc) >= 1 & (c + dc) <= grid_dim
      possible_indices <- ((c + dc[valid]) - 1) * grid_dim + (r + dr[valid])
      
      # Bias logic
      weights <- rep(1, length(possible_indices))
      for (m in 1:length(possible_indices)) {
        target_h <- habitat_types_vec[possible_indices[m]]
        if (target_h == 2) weights[m] <- 5.0
        if (target_h == 3) weights[m] <- 0.1
      }
      
      current_cell <- sample(possible_indices, size = 1, prob = weights)
    }
    row_idx <- row_idx + 1
  }
}

# 4. CONSTRUCT THE ADJACENCY MATRIX (M)
C <- matrix(0, nrow = n_cells, ncol = n_cells)
for (i in 1:(nrow(telemetry_data) - 1)) {
  if (telemetry_data$deer_id[i] == telemetry_data$deer_id[i+1]) {
    C[telemetry_data$cell[i], telemetry_data$cell[i+1]] <- 
      C[telemetry_data$cell[i], telemetry_data$cell[i+1]] + 1
  }
}

M <- matrix(0, nrow = n_cells, ncol = n_cells)
for (r in 1:n_cells) {
  row_sum <- sum(C[r, ])
  if (row_sum > 0) {
    M[r, ] <- C[r, ] / row_sum
  } else {
    M[r, r] <- 1
  }
}

# 5. SAVE
saveRDS(list(M = M, habitat_types = habitat_types_vec, grid_dim = grid_dim), 
        "cwd_data_corridor.rds")
cat("Success! 'cwd_data_bottleneck.rds' generated.\n")