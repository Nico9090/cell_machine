#plot 3D cell
library(rgl)
close3d() # closes current device
rgl.quit() 

meshGrid <- function(){
  theta <- seq(0, 2*pi, length.out = 50)  # cut off at thetaSlice
  phi <- seq(0, pi, length.out = 50)
  
  # Create meshgrid
  thetaGrid <- matrix(rep(theta, each = length(phi)), nrow = length(phi))
  phiGrid <- matrix(rep(phi, times = length(theta)), nrow = length(phi))
  return(list(thetaGrid = thetaGrid,
              phiGrid = phiGrid))

}

# Outer sphere coordinates

drawRibosomes <- function(location,radius,
                          thetaGrid,
                          phiGrid,
                          color){
  
  x <- radius * sin(phiGrid) * cos(thetaGrid)
  x <- x + location[1]
  y <- radius * sin(phiGrid) * sin(thetaGrid)
  y <- y + location[2]
  z <- radius * cos(phiGrid)
  z <- z + location[3]
  surface3d(x,y,z,
            color = color,
            alpha = 0.7)
}
drawGolgi <- function(center, rx, ry, rz, color = "orange", alpha = 0.8) {
  # Create theta (azimuthal) and phi (polar) angles
  theta <- seq(0, pi, length.out = 50)
  phi <- seq(0, pi, length.out = 50)
  
  # Meshgrid
  thetaGrid <- matrix(rep(theta, each = length(phi)), nrow = length(phi))
  phiGrid <- matrix(rep(phi, times = length(theta)), nrow = length(phi))
  
  # Unit sphere
  x <- sin(phiGrid) * cos(thetaGrid)
  y <- sin(phiGrid) * sin(thetaGrid)
  z <- cos(phiGrid)
  
  # Apply scaling
  x <- rx * x
  y <- ry * y
  z <- rz * z
  
  # Apply translation (center offset)
  x <- x + center[1]
  y <- y + center[2]
  z <- z + center[3]
  
  # Draw the surface
  surface3d(x, y, z, color = color, alpha = alpha)
}

drawRNA <- function(color = "white",
                    center = c(0, 0, 0),
                    scale = 1) {
  t <- seq(0, 4 * pi, length.out = 200)  # parameter
  
  # Create helix coordinates
  x <- scale * cos(t) + center[1]
  y <- scale * sin(t) + center[2]
  z <- scale * t / (2 * pi) + center[3]  # vertical movement
  
  lines3d(x, y, z, col = color, lwd = 3)
}
drawDoubleStrandedRNA <- function(center = c(0, 0, 0),
                                  scale = 1,
                                  color1 = "forestgreen",
                                  color2 = "darkgreen", 
                                  strandSeparation = 0.02) {
  t <- seq(0, 4 * pi, length.out = 300)  # parameter
  
  # First strand (helix 1)
  x1 <- scale * cos(t) + center[1]
  y1 <- scale * sin(t) + center[2]
  z1 <- scale * t / (2 * pi) + center[3]
  
  # Second strand (helix 2), offset slightly around the circle
  phase_shift <- pi  # 180 degrees out of phase
  x2 <- scale * cos(t + phase_shift) + center[1] + strandSeparation
  y2 <- scale * sin(t + phase_shift) + center[2] + strandSeparation
  z2 <- scale * t / (2 * pi) + center[3]
  
  # Draw both strands
  lines3d(x1, y1, z1, col = color1, lwd = 3)
  lines3d(x2, y2, z2, col = color2, lwd = 3)
  
  # Optional: base pairs as lines connecting the strands
  for (i in seq(1, length(t), by = 15)) {
    p1 <- c(x1[i], y1[i], z1[i])
    p2 <- c(x2[i], y2[i], z2[i])
    
    base_pair <- cylinder3d(
      rbind(p1, p2),           # start and end points
      radius = 0.05,          # thickness of the base pair
      sides = 8,               # smoothness
      closed = -2              # cap both ends
    )
    
    shade3d(base_pair, color = "white")
  }
}
drawPeptide <- function(start = c(0, 0, 0), 
                        n = 2, 
                        spacing = 0.7,
                        radius = 0.3,
                        color = c("orange","blue4"),
                        direction = c(1,0,0)) {
  direction <- direction / sqrt(sum(direction^2))
  for (i in 0:(n - 1)) {
    # Position of each amino acid
    pos <- start + i*spacing*direction
    spheres3d(pos[1], pos[2], pos[3], radius = radius, color = color[i+1])
    
    # Draw bond to next amino acid (if not last)
    if (i < n - 1) {
      next_pos <- start + (i+1)*spacing*direction
      
      # Draw cylinder as peptide bond
      bond <- cylinder3d(rbind(pos, next_pos), 
                         radius = radius * 0.4, 
                         sides = 8, 
                         closed = -2)
      
      shade3d(bond, color = "white")
    }
  }
}


plotCell <- function(innerRadius,
                     outerRadius,
                     thetaSlice) {
  # thetaSlice based on cartesian coordinates
  clear3d()
  # Create a grid for theta (horizontal angle) and phi (vertical angle)
  theta <- seq(0, thetaSlice, length.out = 50)  # cut off at thetaSlice
  phi <- seq(0, pi, length.out = 50)
  
  # Create meshgrid
  thetaGrid <- matrix(rep(theta, each = length(phi)), nrow = length(phi))
  phiGrid <- matrix(rep(phi, times = length(theta)), nrow = length(phi))
  
  # Outer sphere coordinates
  xOuter <- outerRadius * sin(phiGrid) * cos(thetaGrid)
  yOuter <- outerRadius * sin(phiGrid) * sin(thetaGrid)
  zOuter <- outerRadius * cos(phiGrid)
  
  # Inner sphere coordinates
  xInner <- innerRadius * sin(phiGrid) * cos(thetaGrid)
  yInner <- innerRadius * sin(phiGrid) * sin(thetaGrid)
  zInner <- innerRadius * cos(phiGrid)
  
  # Draw outer shell slice
  surface3d(xOuter,
            yOuter,
            zOuter, color = "steelblue", alpha = 0.7)
  
  # Draw inner sphere 
  surface3d(xInner,
            yInner,
            zInner, color = "red", alpha = 0.9)
  nucleusRadius <- 1
  nucleusPosition <- c(0.4, 0, 0)
  spheres3d(x = nucleusPosition[1],
            y = nucleusPosition[2],
            z = nucleusPosition[3],
            radius = nucleusRadius,
            color = "purple",
            alpha = 0.3)
  
  #golgi 
  golgiCenters <- seq(2, 2.5, length.out = 5) 
  for (c in golgiCenters) {
    drawGolgi(center = c(2, 0, c),
              rx = 1, ry = 1, rz = 0.5)
  }
  
  #RNA
  drawDoubleStrandedRNA()
  
  #protein
  drawPeptide(start = c(2,0,0))
  drawPeptide(start = c(0,4.5,0),spacing = 1.4,
              direction = c(1,6,1))
  
  #ribosomes
  meshGrid <- meshGrid()
  ribosomeThetaGrid <- meshGrid$thetaGrid
  ribsosomePhiGrid <- meshGrid$phiGrid
  drawRibosomes(location = c(-0.5,0,3),
                radius = 0.5,thetaGrid = ribosomeThetaGrid,
                phiGrid = ribsosomePhiGrid,color = "lightblue1")
  # Axes
  axes3d()
  title3d(xlab = "X", ylab = "Y", zlab = "Z")
}
plotCell(innerRadius = 4.5,
         outerRadius = 5,
         thetaSlice = -3*pi/2)
