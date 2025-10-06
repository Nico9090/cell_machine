library(rgl)

# Clear existing 3D scene
clear3d()

# === Parameters ===
bile_duct_inner_radius <- 1.0        # Inner radius at base
wall_thickness <- 0.3                # Wall thickness
height <- 5                          # Height of the bile duct segment
cell_radius <- 0.15                  # Radius of spherical cholangiocytes
dz <- 2 * cell_radius                # Vertical step = cell height
n_layers <- floor(height / dz)      # How many vertical layers

# === Geometry for dilated duct ===
n_theta <- 60                        # Number of angles around the circle
n_z <- 50                            # Number of vertical slices for wall
theta_vals <- seq(0, 2 * pi, length.out = n_theta)
z_vals <- seq(0, height, length.out = n_z)

# Duct inner radius increases toward the top (quadratic dilation)
r_vals <- bile_duct_inner_radius + (z_vals / height)^2         # e.g., 1.0 → 2.0
outer_r_vals <- r_vals + wall_thickness                        # Outer wall

# Create outer wall mesh (x, y, z coordinates)
x <- outer(cos(theta_vals), outer_r_vals)
y <- outer(sin(theta_vals), outer_r_vals)
z <- matrix(rep(z_vals, each = n_theta),
            nrow = n_theta, ncol = n_z, byrow = FALSE)

# Render the outer duct wall
surface3d(x, y, z, color = "lightblue", alpha = 0.2, front = "lines")

# === Add cholangiocytes ===

# Loop through layers and angles to place spheres along inner surface
for (j in 0:(n_layers - 1)) {
  z_layer <- j * dz + cell_radius             # Center of each layer
  # Compute current radius at this z-layer (dilated)
  r_current <- bile_duct_inner_radius + (z_layer / height)^2 - 0.05
  
  # Circumference at this layer
  circumference <- 2 * pi * r_current
  n_cells <- floor(circumference / (2 * cell_radius))   # Number of cells per layer
  
  for (i in 0:(n_cells - 1)) {
    theta <- 2 * pi * i / n_cells
    x_cell <- r_current * cos(theta)
    y_cell <- r_current * sin(theta)
    
    # Increase cell size slightly to match dilation visually
    dilation_factor <- 1 + (z_layer / height)^2
    cell_size <- cell_radius * dilation_factor
    
    spheres3d(x_cell, y_cell, z_layer,
              radius = cell_size,
              color = "goldenrod", alpha = 0.95)
  }
}
# === INSERT CHEMOKINE CODE HERE ===
# Store chemokine object IDs
chemokine_ids <- integer(0)

# Animation loop
for (frame in 1:100) {
  # Remove previous chemokines
  #if (length(chemokine_ids) > 0) {
  #  rgl.pop(id = chemokine_ids) #removes the previous chemokine
  #}
  
  # Redraw chemokines with new positions
  chemokine_ids <- integer(0)  # Reset ID list for this frame
  
  for (i in 1:50) {
    z_pos <- runif(1, 0, height)
    r_inner <- bile_duct_inner_radius + (z_pos / height)^2 - 0.1
    r_pos <- runif(1, 0, r_inner)
    theta <- runif(1, 0, 2 * pi)
    
    x_pos <- r_pos * cos(theta)
    y_pos <- r_pos * sin(theta)
    
    # Store object ID of each chemokine
    chemokine_ids[i] <- spheres3d(x_pos, y_pos, z_pos,
                                  radius = 0.05,
                                  color = "red", alpha = 0.8)
  }
  
  # Pause for animation effect
  Sys.sleep(0.1)
}

# Axes and label
#axes3d()
