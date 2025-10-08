library(rgl)

draw_bile_duct_cross_section <- function(){
  open3d()
  bg3d("white")
  
  # --- Parameters ---
  n_cells <- 12
  lumen_radius <- 2
  cell_radius <- 0.6
  inner_membrane_radius <- lumen_radius + cell_radius * 0.8
  basement_radius <- lumen_radius + cell_radius * 1.5
  collagen_outer_radius <- lumen_radius + cell_radius * 3
  
  angles <- seq(0, 2*pi, length.out = n_cells + 1)[- (n_cells + 1)]
  
  # --- 1. Lumen ---
  shade3d(cylinder3d(
    center = matrix(c(0, 0, -0.5, 0, 0, 0.5), ncol = 2),
    radius = lumen_radius,
    sides = 50
  ), color = "lightyellow", alpha = 0.2)
  
  # --- 2. Cholangiocytes (cells around lumen) ---
  for (a in angles) {
    cx <- (lumen_radius + cell_radius) * cos(a)
    cy <- (lumen_radius + cell_radius) * sin(a)
    
    cell <- scale3d(cube3d(), cell_radius, cell_radius, 0.5)
    cell <- translate3d(cell, cx, cy, 0)
    shade3d(cell, color = "skyblue", alpha = 0.9)
  }
  
  # --- 3. Basement membrane (thin ring) ---
  shade3d(cylinder3d(
    center = matrix(c(0, 0, -0.5, 0, 0, 0.5), ncol = 2),
    radius = basement_radius,
    sides = 80
  ), color = "lightgray", alpha = 0.3)
  
  # --- 4. Integrins (dots on basement membrane) ---
  n_integrins <- 60
  integrin_angles <- seq(0, 2*pi, length.out = n_integrins)
  for (a in integrin_angles) {
    ix <- basement_radius * cos(a)
    iy <- basement_radius * sin(a)
    spheres3d(ix, iy, 0.1, radius = 0.1, color = "darkred")
  }
  
  # --- 5. Collagen fibers (outer layer) ---
  for (a in seq(0, 2*pi, length.out = 100)) {
    r <- collagen_outer_radius + runif(1, -0.2, 0.2)
    x <- r * cos(a)
    y <- r * sin(a)
    z <- runif(1, -0.5, 0.5)
    spheres3d(x, y, z, radius = 0.08, color = "tan3")
  }
  
  title3d(xlab = "", ylab = "", zlab = "", main = "Bile Duct Cross Section")
  view3d(theta = 0, phi = 0, zoom = 0.9)
}

draw_bile_duct_cross_section()
