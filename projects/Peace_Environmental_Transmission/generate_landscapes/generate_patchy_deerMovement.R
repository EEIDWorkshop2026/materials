# ==========================================================
# EEID Workshop: CWD Spatial Data Generation (Patchy Grid)
# R Version
# ==========================================================

rm(list = ls())

# 1. SETUP PARAMETERS
grid_dim <- 20          
n_cells  <- grid_dim^2
n_deer   <- 60            
n_steps  <- 300          

# 2. CREATE A STRUCTURED LANDSCAPE (The "Patchy" Environment)
# Start with a 2D matrix filled with Forest (Type 1)
habitat_matrix <- matrix(1, nrow = grid_dim, ncol = grid_dim)

# Create a Large "Ag" Patch (Type 2) in the Top-Left (Rows 1-8, Cols 1-8)
habitat_matrix[1:8, 1:8] <- 2

# Create a "Suburban" Semi-Barrier (Type 3) - Vertical strip (Cols 11-13)
# Leave a "gap" at the bottom (Rows 15-20) for a movement corridor
habitat_matrix[1:14, 11:13] <- 3

# Flatten matrix to vector using column-major order (matches MATLAB behavior)
habitat_types_vec <- as.vector(habitat_matrix)

# Visualize the patchy landscape with explicit legend space on right
layout(matrix(c(1, 2), nrow = 1), widths = c(4, 1))
par(mar = c(4, 4, 3, 1))

image(1:grid_dim, 1:grid_dim, habitat_matrix, 
      col = c("darkgreen", "yellowgreen", "orange"),
      main = "Structured Landscape",
      xlab = "X (Grid Cells)", ylab = "Y (Grid Cells)", axes = TRUE)

# Manual discrete color legend drawing
par(mar = c(4, 0.5, 3, 1))
plot(c(0, 1), c(0, 3), type = "n", axes = FALSE, xlab = "", ylab = "")
rect(0, 0:2, 0.5, 1:3, col = c("darkgreen", "yellowgreen", "orange"))
text(0.6, 0.5:2.5, labels = c("Forest", "Agriculture", "Suburban"), adj = 0, cex = 1.2)

# Reset layout
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
    
    h <- habitat_types_vec[current_cell]
    stay_prob <- if (h == 1) 0.90 else if (h == 2) 0.96 else 0.40
    
    if (runif(1) > stay_prob) {
      # 2D coordinate calculations matching array indices
      # R matrices use 1-based indexing, math changes slightly from MATLAB
      c <- ((current_cell - 1) %/% grid_dim) + 1
      r <- ((current_cell - 1) %% grid_dim) + 1
      
      dr <- c(-1, 1, 0, 0)
      dc <- c(0, 0, -1, 1)
      
      potential_r <- r + dr
      potential_c <- c + dc
      
      # Filter bounds
      valid <- potential_r >= 1 & potential_r <= grid_dim & 
        potential_c >= 1 & potential_c <= grid_dim
      
      moves_r <- potential_r[valid]
      moves_c <- potential_c[valid]
      
      # Convert 2D coordinates back to 1D linear vector index
      possible_indices <- (moves_c - 1) * grid_dim + moves_r
      
      # --- BIAS TOWARD GOOD HABITAT ---
      weights <- rep(1, length(possible_indices))
      for (m in 1:length(possible_indices)) {
        target_h <- habitat_types_vec[possible_indices[m]]
        if (target_h == 2) weights[m] <- 5.0   # Prefer Ag
        if (target_h == 3) weights[m] <- 0.1   # Avoid Suburban
      }
      
      # R's built-in sample function handles weights beautifully out of the box
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

# 5. SAVE DATA FOR THE STUDENTS
saveRDS(list(M = M, habitat_types = habitat_types_vec, grid_dim = grid_dim), 
        "cwd_data_patchy.rds")
cat("Success! 'cwd_data_patchy.rds' generated.\n")