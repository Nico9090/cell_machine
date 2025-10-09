library(shiny)
library(rgl)

# ----- Function to draw wavy cubes -----

library(shiny)
library(rgl)
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
# -------------------------------

# SHINY APP

# -------------------------------

ui <- fluidPage(
  titlePanel("Interactive Wavy 3D Cells"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("amplitude", "Wave amplitude:", 0, 0.5, 0.1, step = 0.01),
      sliderInput("frequency", "Wave frequency:", 1, 10, 3, step = 0.5)
    ),
    mainPanel(
      rglwidgetOutput("cells3d", width = "100%", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  output$cells3d <- renderRglwidget({
    # Create new rgl device for offscreen rendering
    rgl.open(useNULL = TRUE)
    rgl.clear()
    
    # Build cube
    outer_verts <- make_cube_vertices(center = c(0.5, 0.5, 0.5), size = 1)
    draw_wire_cube(
      verts = outer_verts,
      amplitude = input$amplitude,
      frequency = input$frequency
    )
    
    # Return widget
    rglwidget()
  })
}




shinyApp(ui, server)
