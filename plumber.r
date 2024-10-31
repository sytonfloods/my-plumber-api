# Load necessary libraries
library(plumber)

# Initialize a data storage (in-memory for simplicity)
# Ideally, in a production setup, this could be a database
river_data <- new.env()

#* Log incoming water level data
#* @param river_name The name of the river (must match one in the Shiny app)
#* @param water_level The water level of the river in cm
#* @post /waterlevel/<river_name>
function(river_name, water_level) {
  # Store the water level in the environment
  river_data[[river_name]] <- as.numeric(water_level)
  list(message = paste("Water level for", river_name, "updated to", water_level, "cm"))
}

#* Retrieve current water level for a specified river
#* @param river_name The name of the river
#* @get /waterlevel/<river_name>
function(river_name) {
  if (exists(river_name, envir = river_data)) {
    list(river_name = river_name, water_level = river_data[[river_name]])
  } else {
    list(error = paste("No data available for", river_name))
  }
}

#* Retrieve water levels for all rivers
#* @get /waterlevels
function() {
  all_levels <- as.list(river_data)
  if (length(all_levels) == 0) {
    list(message = "No data available")
  } else {
    all_levels
  }
}

