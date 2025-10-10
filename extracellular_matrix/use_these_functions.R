#functions
make_cube_vertices <- function(center = c(0, 0, 0), size = 1) {
  s <- size / 2
  cx <- center[1]; cy <- center[2]; cz <- center[3]
  
  matrix(c(
    cx - s, cy - s, cz - s,
    cx + s, cy - s, cz - s,
    cx + s, cy + s, cz - s,
    cx - s, cy + s, cz - s,
    cx - s, cy - s, cz + s,
    cx + s, cy - s, cz + s,
    cx + s, cy + s, cz + s,
    cx - s, cy + s, cz + s
  ), ncol = 3, byrow = TRUE)
}
draw_wire_cube <- function(verts, col = "steelblue", point_col = "red", lwd = 2,
                           amplitude = 0.05,frequency = 3) {
  edges <- list(
    c(1,2), c(2,3), c(3,4), c(4,1),  # bottom
    c(5,6), c(6,7), c(7,8), c(8,5),  # top
    c(1,5), c(2,6), c(3,7), c(4,8)   # vertical
  )
  
  for (e in edges) {
    #lines3d(
    #  verts[e, 1], verts[e, 2], verts[e, 3],
    #  col = col, lwd = lwd
    #)
    p1 <- verts[e[1], ]
    p2 <- verts[e[2], ]
    draw_wavy_edge(p1, p2, amplitude = amplitude, frequency = frequency, col = col, lwd = lwd)
  }
  
  points3d(verts[,1], verts[,2], verts[,3], col = point_col, size = 8)
}

draw_wavy_edge <- function(p1, p2, amplitude = 0.1, frequency = 5, col = "steelblue", lwd = 2) {
  n <- 100  # number of points along the edge
  t <- seq(0, 1, length.out = n)
  
  # Linear interpolation between p1 and p2
  x <- (1 - t) * p1[1] + t * p2[1]
  y <- (1 - t) * p1[2] + t * p2[2]
  z <- (1 - t) * p1[3] + t * p2[3]
  
  # Get edge direction
  dir <- p2 - p1
  dir <- dir / sqrt(sum(dir^2))  # unit vector
  
  # Find a vector perpendicular to dir (to wave along)
  perp <- if (all(dir == c(0, 0, 1))) c(1, 0, 0) else c(-dir[2], dir[1], 0)
  perp <- perp / sqrt(sum(perp^2))
  
  # Apply sinusoidal offset
  x <- x + amplitude * sin(2 * pi * frequency * t) * perp[1]
  y <- y + amplitude * sin(2 * pi * frequency * t) * perp[2]
  z <- z + amplitude * sin(2 * pi * frequency * t) * perp[3]
  
  # Draw wavy line
  lines3d(x, y, z, col = col, lwd = lwd)
}
draw_filled_outer_cube <- function(verts, col = "steelblue", alpha = 0.4) {
  faces <- list(
    c(1,2,3,4),  # bottom
    c(5,6,7,8),  # top
    c(1,2,6,5),  # side
    c(2,3,7,6),
    c(3,4,8,7),
    c(4,1,5,8)   # side
  )
  
  for (face in faces) {
    quads3d(verts[face, ], col = col, alpha = alpha)
  }
}
draw_filled_cube <- function(verts, col = "steelblue", alpha = 0.4, amplitude = 0.05, frequency = 3, res = 10) {
  faces <- list(
    c(1,2,3,4),  # bottom
    c(5,6,7,8),  # top
    c(1,2,6,5),  # side
    c(2,3,7,6),
    c(3,4,8,7),
    c(4,1,5,8)   # side
  )
  
  for (face in faces) {
    draw_wavy_face(
      p1 = verts[face[1], ],
      p2 = verts[face[2], ],
      p3 = verts[face[3], ],
      p4 = verts[face[4], ],
      col = col,
      alpha = alpha,
      amplitude = amplitude,
      frequency = frequency,
      res = res
    )
  }
}
# -------------------------------

draw_wavy_edge <- function(p1, p2, amplitude = 0.1, frequency = 5, col = "steelblue", lwd = 2) {
  n <- 100  # number of points along the edge
  t <- seq(0, 1, length.out = n)
  
  # Linear interpolation between p1 and p2
  x <- (1 - t) * p1[1] + t * p2[1]
  y <- (1 - t) * p1[2] + t * p2[2]
  z <- (1 - t) * p1[3] + t * p2[3]
  
  # Get edge direction
  dir <- p2 - p1
  dir <- dir / sqrt(sum(dir^2))  # unit vector
  
  # Find a vector perpendicular to dir (to wave along)
  perp <- if (all(dir == c(0, 0, 1))) c(1, 0, 0) else c(-dir[2], dir[1], 0)
  perp <- perp / sqrt(sum(perp^2))
  
  # Apply sinusoidal offset
  x <- x + amplitude * sin(2 * pi * frequency * t) * perp[1]
  y <- y + amplitude * sin(2 * pi * frequency * t) * perp[2]
  z <- z + amplitude * sin(2 * pi * frequency * t) * perp[3]
  
  # Draw wavy line
  lines3d(x, y, z, col = col, lwd = lwd)
}

draw_wavy_face <- function(p1, p2, p3, p4, amplitude = 0.05, frequency = 3, col = "red4", alpha = 0.5, res = 8) {
  for (i in 1:(res - 1)) {
    for (j in 1:(res - 1)) {
      u0 <- (i - 1) / (res - 1)
      u1 <- i / (res - 1)
      v0 <- (j - 1) / (res - 1)
      v1 <- j / (res - 1)
      
      # Bilinear interpolation between corners
      interp <- function(u, v) {
        a <- (1 - u) * p1 + u * p2
        b <- (1 - u) * p4 + u * p3
        p <- (1 - v) * a + v * b
        
        # Apply wave along face plane
        wave <- amplitude * sin(2 * pi * frequency * u) * cos(2 * pi * frequency * v)
        
        # Choose a normal vector (approximate)
        n <- crossprod(matrix(p2 - p1, nrow = 1), matrix(p4 - p1, nrow = 1))
        n <- n / sqrt(sum(n^2))  # normalize
        p + wave * as.vector(n)
      }
      
      # Get 4 corners of this sub-quad
      v1 <- interp(u0, v0)
      v2 <- interp(u1, v0)
      v3 <- interp(u1, v1)
      v4 <- interp(u0, v1)
      
      quads3d(rbind(v1, v2, v3, v4), col = col, alpha = alpha)
    }
  }
}

draw_integrins <- function(cell_centers){
  
  for (center in cell_centers){
    radius <- 0.1
    distance <- 1
    sphere1_center <- c(center, 0, 0.5)
    sphere2_center <- c(center,-distance, 0.5)
    spheres3d(sphere1_center[1], sphere1_center[2], sphere1_center[3], 
              radius = radius, color = "green4") #integrin top
    spheres3d(sphere2_center[1], sphere2_center[2], sphere2_center[3], 
              radius = radius, color = "green4") #integrin bottom
    lines3d(rbind(sphere1_center, sphere2_center), 
            color = "green4", lwd = 2) #connect
  }
  
}
draw_basement_membrane <- function(start = 0,
                                   end = 10,
                                   y_location = -1.25){
  radius <- (end - start)/2
  cuboid <- scale3d(cube3d(), radius, 0.25, 1) #so that y and z are the size of cells
  cuboid <- translate3d(cuboid, 5, y_location, 0) 
  shade3d(cuboid, color = "yellow3",alpha = 0.2)
}
