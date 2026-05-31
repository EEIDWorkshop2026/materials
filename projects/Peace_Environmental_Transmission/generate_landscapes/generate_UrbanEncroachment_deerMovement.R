# ==========================================================
# EEID Workshop: CWD Spatial Data Generation (Encroachment Grid)
# R Version - Scenario 5 (Option 3)
# ==========================================================

rm(list = ls())

# 1. SETUP PARAMETERS
grid_dim <- 20          
n_cells  <- grid_dim^2
n_deer   <- 60            
n_steps  <- 300          

# 2. CREATE AN ENCROACHMENT GRADIENT LANDSCAPE
# Initialize a 2D matrix
habitat_matrix <- matrix(1, nrow = grid_dim, ncol = grid_dim)

# Left Side: A large Agricultural tract nested in the wilderness (Rows 3-12, Cols 2-6)
habitat_matrix[3:12, 2:6] <- 2

# Right Side: Heavy Suburban Encroachment (Columns 12 to 20 are completely suburbanized)
habitat_matrix[, 12:20] <- 3

# Transition Zone: Add some scattered suburban pockets breaking into the forest (Columns 9-11)
set.seed(777) # For reproducible fragmentation
fragmentation_cells <- sample(which(habitat_matrix[, 9:11] == 1), 12)
# Adjust indices relative to the sub-matrix selection
sub_indices <- which(habitat_matrix == 1, arr.ind = TRUE)
sub_indices <- sub_indices[sub_indices[,2] %in% 9:11, ]
habitat_matrix[sub_indices[fragmentation_cells, ]] <- 3

# Flatten matrix to vector
habitat_types_vec <- as.vector(habitat_matrix)

# --- VISUALIZATION WITH RIGHT-SIDE COLOR LEGEND ---
layout(matrix(c(1, 2), nrow = 1), widths = c(4, 1))
par(mar = c(4, 4, 3, 1))

image(1:grid_dim, 1:grid_dim, habitat_matrix, 
      col = c("darkgreen", "yellowgreen", "orange"),
      main = "Suburban Encroachment Landscape",
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
  # Deer can start anywhere, but they will naturally sort themselves out
  current_cell <- sample(1:n_cells, 1)
  
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
        if (target_h == 2) weights[m] <- 5.0   # Highly drawn to the Ag block
        if (target_h == 3) weights[m] <- 0.1   # Highly repelled by the Suburban wall
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
        "cwd_data_encroachment.rds")
cat("Success! 'cwd_data_encroachment.rds' generated.\n")