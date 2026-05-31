############################################################
# workshop_modelfx.R
#
# Simple spatial SIR/SIRS model functions for workshop use.
#
# This version intentionally matches the model structure in
# 4_Epi_functions.R:
#   - Each patch has S, I, and R compartments.
#   - Waning immunity is stored as HPop$kappa.
#   - Infection, recovery, and loss of immunity are updated patch by patch.
#   - Mobility moves infectious individuals only.
#   - Mobility is stochastic, using rbinom(), and is scaled by 0.1.
############################################################


##### Initiate populations  ####
InitiatePop <- function(IDs,
                        initialInf_vec,
                        totalPop_vec,
                        beta_vec,
                        gamma_vec) {
  list(
    ID = IDs,
    I = initialInf_vec,
    S = totalPop_vec - initialInf_vec,
    R = rep(0, length(initialInf_vec)),
    beta = beta_vec,
    gamma = gamma_vec,
    kappa = kappa_vec,
    nTotal = totalPop_vec,
    frac_I = initialInf_vec / totalPop_vec
  )
}


##### Epidemic functions: infection, recovery, loss of immunity  ####
epidemic_timestep <- function(HPop) {
  
  for (i in 1:length(HPop$R)) {
    # Epidemic events
    newly_infected <- HPop$I[i] * HPop$S[i] * HPop$beta[i] / HPop$nTotal[i]
    recovered_today_i <- HPop$I[i] * HPop$gamma[i]
    losing_immunity_today_i <- HPop$R[i] * HPop$kappa[i]
    
    # Updating epidemic equations
    HPop$S[i] <- HPop$S[i] - newly_infected                       + losing_immunity_today_i
    HPop$I[i] <- HPop$I[i] + newly_infected - recovered_today_i
    HPop$R[i] <- HPop$R[i]                    + recovered_today_i - losing_immunity_today_i
  }
  
  HPop
}


###### Master function without mobility  ####
runSimNoMobility <- function(HPop, day_list) {
  
  epidemic_curve <- data.frame(day = c(),
                               inf = c(),
                               stringsAsFactors = FALSE)
  
  all_spread <- matrix(0, length(day_list), length(HPop$I))
  all_spread_today <- matrix(0, length(day_list), length(HPop$I))
  colnames(all_spread) <- HPop$ID
  
  for (current_day in 1:length(day_list)) {
    
    print(day_list[current_day])
    
    HPop <- epidemic_timestep(HPop)
    
    epidemic_curve <- rbind(epidemic_curve,
                            data.frame(day = day_list[current_day],
                                       inf = sum(HPop$I)))
    all_spread[current_day, ] <- HPop$I
  }
  
  all_spread_2 <- data.frame(day = day_list)
  all_spread_2 <- cbind(all_spread_2, all_spread)
  
  list(HPop = HPop,
       epidemic_curve = epidemic_curve,
       all_spread = all_spread_2)
}


##### Movement functions: infection, recovery, loss of immunity  ####
movement_timestep <- function(HPop, mobmat) {
  
  movement_matrix <- matrix(0, length(HPop$ID), length(HPop$ID))
  daily_move <- subset(mobmat,
                       !is.na(fr_pat) &
                         !is.na(to_pat) &
                         !is.na(fr_users) &
                         !is.na(movers))
  
  daily_move_mat <- daily_move[, is.element(names(daily_move),
                                            c("fr_pat", "to_pat", "fr_users", "movers"))]
  daily_move_mat <- as.matrix(daily_move_mat)
  
  col1 <- which(colnames(daily_move_mat) == "fr_pat")
  col2 <- which(colnames(daily_move_mat) == "to_pat")
  colmove <- which(colnames(daily_move_mat) == "movers")
  colusers <- which(colnames(daily_move_mat) == "fr_users")
  
  movement_matrix[daily_move_mat[, c(col1, col2)]] <-
    daily_move_mat[, colmove] / daily_move_mat[, colusers]
  
  movement_matrix <- movement_matrix * 0.1
  
  for (i in 1:length(HPop$ID)) {
    movement_matrix[i, i] <- 1 - sum(movement_matrix[i, -i])
  }
  
  HPop$I <- ceiling(HPop$I)
  
  moved_matrix <- matrix(0,
                         length(HPop$ID),
                         length(HPop$ID),
                         dimnames = list(HPop$ID, HPop$ID))
  
  for (i in 1:dim(moved_matrix)[1]) {
    moved_matrix[i, ] <- rbinom(length(moved_matrix[i, ]),
                                HPop$I[i],
                                prob = movement_matrix[i, ])
    
    if (sum(moved_matrix[i, ]) > 0) {
      moved_matrix[i, ] <- moved_matrix[i, ] / sum(moved_matrix[i, ]) * HPop$I[i]
    }
  }
  
  HPop$I <- HPop$I - rowSums(moved_matrix) + colSums(moved_matrix)
  
  HPop
}


###### Master function with mobility  ####
runSim <- function(HPop, mobmat, day_list) {
  
  epidemic_curve <- data.frame(day = c(),
                               inf = c(),
                               stringsAsFactors = FALSE)
  
  all_spread <- matrix(0, length(day_list), length(HPop$I))
  all_spread_today <- matrix(0, length(day_list), length(HPop$I))
  colnames(all_spread) <- HPop$ID
  
  for (current_day in 1:length(day_list)) {
    
    print(day_list[current_day])
    
    HPop <- epidemic_timestep(HPop)
    HPop <- movement_timestep(HPop, mobmat)
    
    epidemic_curve <- rbind(epidemic_curve,
                            data.frame(day = day_list[current_day],
                                       inf = sum(HPop$I)))
    all_spread[current_day, ] <- HPop$I
  }
  
  all_spread_2 <- data.frame(day = day_list)
  all_spread_2 <- cbind(all_spread_2, all_spread)
  
  list(HPop = HPop,
       epidemic_curve = epidemic_curve,
       all_spread = all_spread_2)
}
