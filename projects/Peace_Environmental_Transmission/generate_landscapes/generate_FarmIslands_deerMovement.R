# ==========================================================
# EEID Workshop: CWD Spatial Data Generation (Archipelago Grid)
# R Version - Scenario 4 (Option 2)
# ==========================================================

rm(list = ls())

# 1. SETUP PARAMETERS
grid_dim <- 20          
n_cells  <- grid_dim^2
n_deer   <- 60            
n_steps  <- 300          

# 2. CREATE AN ARCHIPELAGO LANDSCAPE (Isolated Food Islands)
# Start by filling the entire landscape with Forest (Type 1)
habitat_matrix <- matrix(1, nrow = grid_dim, ncol = grid_dim)

# Add a fragmented archipelago of 2x2 Agricultural patches (Type 2)
# These act as localized high-density aggregation hubs
ag_hubs <- list(
  c(3, 3),   c(4, 15), 
  c(9, 8),   c(10, 2),
  c(14, 14), c(15, 6),
  c(17, 17)
)

for (hub in ag_hubs) {
  r_start <- hub[1]
  c_start <- hub[2]
  habitat_matrix[r_start:(r_start+1), c_start:(c_start+1)] <- 2
}

# Add some scattered Suburban pockets (Type 3) to create localized disturbance
set.seed(123) # For consistent suburban noise
suburban_elements <- sample(which(habitat_matrix == 1), 35)
habitat_matrix[suburban_elements] <- 3

# Flatten matrix to vector
habitat_types_vec <- as.vector(habitat_matrix)

# --- VISUALIZATION WITH RIGHT-SIDE COLOR LEGEND ---
layout(matrix(c(1, 2), nrow = 1), widths = c(4, 1))
par(mar = c(4, 4, 3, 1))

image(1:grid_dim, 1:grid_dim, habitat_matrix, 
      col = c("darkgreen", "yellowgreen", "orange"),
      main = "Farm Archipelago",
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
  current_cell <- sample(1:n_cells, 1)
  
  for (t in 1:n_steps) {
    telemetry_data$cell[row_idx] <- current_cell
    
    h <- habitat_types_vec[current_cell]
    stay_prob <- if (h == 1) 0.65 else if (h == 2) 0.90 else 0.05
    
    if (runif(1) > stay_prob) {
      c <- ((current_cell - 1) %/% grid_dim) + 1
      r <- ((current_cell - 1) %% grid_dim) + 1
      
      dr <- c(-1, 1, 0, 0)
      dc <- c(0, 0, -1, 1)
      
      valid <- (r + dr) >= 1 & (r + dr) <= grid_dim & (c + dc) >= 1 & (c + dc) <= grid_dim
      possible_indices <- ((c + dc[valid]) - 1) * grid_dim + (r + dr[valid])
      
      # Bias logic
      weights <- rep(1, length