library(rgl)

# Clear previous scene
clear3d()

# === Parameters ===
base_inner_radius <- 1.0
wall_thickness <- 0.3
height <- 5
cell_radius <- 0.15
dz <- 2 * cell_radius
n_layers <- floor(height / dz)

n_theta <- 60
n_z <- 50
theta_vals <- seq(0, 2 * pi, length.out = n_theta)
z_vals <- seq(0, height, length.out = n_z)

# Dilation control
current_dilation_steps <- 0
max_dilation_steps <- 5
dilation_increment <- 0.1  # how much inner radius grows each step

# Chemokine parameters
chemokines <- data.frame(
  x = numeric(0),
  y = numeric(0),
  z = numeric(0),
  radius = numeric(0),
  id = integer(0)
)
chemokine_speed <- 0.05  # Upward speed
chemokine_diffusion <- 0.01  # Random motion

# Thresholds for dilation / contraction
dilation_threshold <- 200   # number of chemokines to trigger dilation
contraction_threshold <- 50 # below this, contract

# Colors for cholangiocytes (inactive, activated)
cell_color_inactive <- "goldenrod"
cell_color_activated <- "orange"

# Function to calculate inner radius at height z, with current dilation
inner_radius_func <- function(z) {
  base_inner_radius + dilation_increment * current_dilation_steps + (z / height)^2
}

# Function to draw bile duct walls and cholangiocytes
draw_duct_and_cells <- function() {
  clear3d(type = "shapes")
  
  r_vals <- inner_radius_func(z_vals)
  outer_r_vals <- r_vals + wall_thickness
  x <- outer(cos(theta_vals), outer_r_vals)
  y <- outer(sin(theta_vals), outer_r_vals)
  z_mat <- matrix(rep(z_vals, each = n_theta), nrow = n_theta, ncol = n_z, byrow = FALSE)
  
  surface3d(x, y, z_mat, color = "lightblue", alpha = 0.2, front = "lines")
  
  for (j in 0:(n_layers - 1)) {
    z_layer <- j * dz + cell_radius
    r_current <- inner_radius_func(z_layer) - 0.05
    circumference <- 2 * pi * r_current
    n_cells <- floor(circumference / (2 * cell_radius))
    
    # Activation color scales with dilation steps
    activation_ratio <- current_dilation_steps / max_dilation_steps
    col <- colorRampPalette(c(cell_color_inactive, cell_color_activated))(max_dilation_steps + 1)[current_dilation_steps + 1]
    
    for (i in 0:(n_cells - 1)) {
      theta <- 2 * pi * i / n_cells
      x_cell <- r_current * cos(theta)
      y_cell <- r_current * sin(theta)
      dilation_factor <- 1 + (z_layer / height)^2
      cell_size <- cell_radius * dilation_factor
      
      spheres3d(x_cell, y_cell, z_layer,
                radius = cell_size,
                color = col, alpha = 0.95)
    }
  }
}

# Initialize scene
draw_duct_and_cells()

# Function to add new chemokines at random positions near base (z ~ 0)
add_chemokines <- function(n) {
  new_chemokines <- data.frame(
    x = numeric(n),
    y = numeric(n),
    z = numeric(n),
    radius = rep(0.05, n),
    id = integer(n)
  )
  
  for (i in 1:n) {
    z_pos <- runif(1, 0, height * 0.1) # Start near base
    r_inner <- inner_radius_func(z_pos) - 0.1
    r_pos <- runif(1, 0, r_inner)
    theta <- runif(1, 0, 2 * pi)
    
    new_chemokines$x[i] <- r_pos * cos(theta)
    new_chemokines$y[i] <- r_pos * sin(theta)
    new_chemokines$z[i] <- z_pos
  }
  
  chemokines <<- rbind(chemokines, new_chemokines)
}

# Add initial chemokines
add_chemokines(100)

# Main animation loop
for (frame in 1:200) {
  # Remove old chemokines from scene
  if (length(chemokines$id) > 0) {
    rgl.pop(id = chemokines$id)
  }
  
  # Update chemokine positions: move upward + random diffusion
  chemokines$z <- chemokines$z + chemokine_speed + rnorm(nrow(chemokines), 0, chemokine_diffusion)
  
  # Keep chemokines inside the lumen radius and height
  for (i in seq_len(nrow(chemokines))) {
    r_current <- sqrt(chemokines$x[i]^2 + chemokines$y[i]^2)
    r_inner <- inner_radius_func(chemokines$z[i]) - 0.1
    
    # Reflect chemokine if outside lumen radius
    if (r_current > r_inner) {
      # Move back inside by scaling position vector
      scale <- r_inner / r_current
      chemokines$x[i] <- chemokines$x[i] * scale
      chemokines$y[i] <- chemokines$y[i] * scale
    }
    # Clamp z inside [0, height]
    chemokines$z[i] <- max(0, min(height, chemokines$z[i]))
  }
  
  # Remove chemokines that reached top (simulate leaving duct)
  to_remove <- which(chemokines$z >= height)
  if (length(to_remove) > 0) {
    chemokines <- chemokines[-to_remove, ]
  }
  
  # Add some new chemokines continuously near the base
  add_chemokines(10)
  
  # Draw chemokines and save their IDs
  chemokines$id <- sapply(1:nrow(chemokines), function(i) {
    spheres3d(chemokines$x[i], chemokines$y[i], chemokines$z[i],
              radius = chemokines$radius[i],
              color = "red", alpha = 0.8)
  })
  
  # --- Check chemokine count for dilation / contraction ---
  n_chemo <- nrow(chemokines)
  
  if (n_chemo > dilation_threshold && current_dilation_steps < max_dilation_steps) {
    current_dilation_steps <- current_dilation_steps + 1
    cat(sprintf("Dilation step increased to %d at frame %d\n", current_dilation_steps, frame))
    draw_duct_and_cells()
  } else if (n_chemo < contraction_threshold && current_dilation_steps > 0) {
    current_dilation_steps <- current_dilation_steps - 1
    cat(sprintf("Dilation step decreased to %d at frame %d\n", current_dilation_steps, frame))
    draw_duct_and_cells()
  }
  
  Sys.sleep(0.05) # adjust animation speed
}
