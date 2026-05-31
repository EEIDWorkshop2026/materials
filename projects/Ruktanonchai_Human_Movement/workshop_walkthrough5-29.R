library(sf)
library(tidyr)
library(ggplot2)
source("workshop_modelfx.R")


shapefile <- read_sf(dsn = "VA_shapefile_wpop", layer = "VA_shapefile_wpop")
movement_data <- read.csv("VA_mobility.csv")

# -----------------------------------------------------------------------------
# Step 1: Build a patch-level population object
# -----------------------------------------------------------------------------
make_patch_table <- function(seed_county = "Montgomery",
                             initial_infections = 10,
                             beta = 2/6,
                             gamma = 1/6) {

    pat_locator <- data.frame(
      ID = shapefile$STCOFIPS,
      name = shapefile$NAME,
      inf = 0,
      pop = shapefile$pop
    )

  
  pat_locator$inf[pat_locator$name == seed_county] <- initial_infections
  pat_locator$transmission <- beta
  pat_locator$recovery <- gamma
  pat_locator
}

# Helper for mobility scenarios. The model itself has no movement_multiplier
# argument, so we alter the movement input data before passing it to runSim().
scale_mobility_data <- function(mobility_data, movement_multiplier = 1) {
  mobility_data$movers <- mobility_data$movers * movement_multiplier
  mobility_data
}

simulate_scenario <- function(seed_county = "Montgomery",
                              beta = 2/6,
                              gamma = 1/6,
                              kappa = 0,
                              movement_multiplier = 1,
                              days = 120,
                              intervention_counties = NULL,
                              beta_reduction = 0.5,
                              use_mobility = TRUE) {

  pat_locator <- make_patch_table(seed_county,
                                  beta = beta,
                                  gamma = gamma)
  
  if (!is.null(intervention_counties)) {
    pat_locator$transmission[pat_locator$name %in% intervention_counties] <-
      pat_locator$transmission[pat_locator$name %in% intervention_counties] * beta_reduction
  }
  
  kappa_vec <<- rep(kappa, nrow(pat_locator))
  
  HPop <- InitiatePop(
    pat_locator$ID,
    pat_locator$inf,
    pat_locator$pop,
    pat_locator$transmission,
    pat_locator$recovery
  )
  
  if (use_mobility && movement_multiplier > 0) {
    scenario_movement_data <- scale_mobility_data(movement_data, movement_multiplier)
    runSim(HPop, scenario_movement_data, day_list = 1:days)
  } else {
    runSimNoMobility(HPop, day_list = 1:days)
  }
}

plot_total_curve <- function(sim_result, title = "Total infections over time") {
  ggplot(sim_result$epidemic_curve, aes(x = day, y = inf)) +
    geom_line(linewidth = 1) +
    labs(x = "Day", y = "Number infectious", title = title) +
    theme_bw(base_size = 16)
}

plot_patch_curves <- function(sim_result, title = "Patch-level infections") {
  output_data <- pivot_longer(sim_result$all_spread, cols = -day)
  ggplot(output_data, aes(x = day, y = value, group = name)) +
    geom_line(alpha = 0.35) +
    scale_y_continuous(trans = "log10") +
    labs(x = "Day", y = "Number infectious, log scale", title = title) +
    theme_bw(base_size = 16) +
    theme(legend.position = "none")
}

plot_map_day <- function(sim_result, day_chosen = 50) {
  if (is.null(shapefile)) {
    message("Map skipped because shapefile is unavailable.")
    return(NULL)
  }
  
  output_data <- pivot_longer(sim_result$all_spread, cols = -day)
  day_data <- subset(output_data, day == day_chosen)
  shapefile_with_inf <- merge(shapefile, day_data, by.x = "STCOFIPS", by.y = "name")
  
  ggplot() +
    geom_sf(data = shapefile, fill = "grey90", color = "white") +
    geom_sf(data = subset(shapefile_with_inf, value > 0), aes(fill = value)) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1) +
    labs(title = paste("Infections on day", day_chosen), fill = "Infectious") +
    theme_void(base_size = 16)
}

# -----------------------------------------------------------------------------
# Live walkthrough
# -----------------------------------------------------------------------------

# Run the baseline scenario.
baseline <- simulate_scenario(seed_county = "Montgomery", days = 120)
plot_total_curve(baseline, "Baseline: Montgomery seed, R0 approximately 2")
plot_patch_curves(baseline, "Baseline spread across patches")
plot_map_day(baseline, 50)

# Change beta/gamma. What happens when R0 is lower?
# Since gamma = 1/6, beta = 2/6 gives beta/gamma = 2.
lower_R0 <- simulate_scenario(seed_county = "Montgomery",
                              beta = 1.2/6,
                              gamma = 1/6,
                              days = 120)
plot_total_curve(lower_R0, "Lower transmission: R0 approximately 1.2")

# Change mobility strength. What happens to timing and spatial spread?
# The model has a fixed internal 0.1 mobility scale. For scenario comparison,
# we scale the input movers column before passing the data into runSim().
no_mobility <- simulate_scenario(seed_county = "Montgomery",
                                 use_mobility = FALSE,
                                 days = 120)
high_mobility <- simulate_scenario(seed_county = "Montgomery",
                                   movement_multiplier = 2.5,
                                   days = 120)

compare_mobility <- rbind(
  data.frame(no_mobility$epidemic_curve, scenario = "No mobility"),
  data.frame(baseline$epidemic_curve, scenario = "Baseline mobility"),
  data.frame(high_mobility$epidemic_curve, scenario = "High mobility")
)

ggplot(compare_mobility, aes(x = day, y = inf, linetype = scenario)) +
  geom_line(linewidth = 1) +
  labs(x = "Day", y = "Number infectious", title = "Mobility changes epidemic timing and spatial reach") +
  theme_bw(base_size = 16)

# Add a targeted intervention by reducing beta in the seeded county.
target_source <- simulate_scenario(
  seed_county = "Montgomery",
  intervention_counties = "Montgomery",
  beta_reduction = 0.5,
  days = 120
)

compare_intervention <- rbind(
  data.frame(baseline$epidemic_curve, scenario = "No intervention"),
  data.frame(target_source$epidemic_curve, scenario = "Reduce beta in source county")
)

ggplot(compare_intervention, aes(x = day, y = inf, linetype = scenario)) +
  geom_line(linewidth = 1) +
  labs(x = "Day", y = "Number infectious", title = "Spatial targeting: intervene at the source") +
  theme_bw(base_size = 16)

# Some project ideas:
# 1. Seed a different county. Does spatial spread change?
# 2. Reduce beta in a destination county rather than the source county. What changes?
# 3. Set kappa = 0.02 in simulate_scenario(). How does SIR become SIRS?
# 4. Change movement_multiplier. Which outcomes are most sensitive: peak size, timing, or spread?
