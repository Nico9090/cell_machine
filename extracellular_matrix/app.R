library(shiny)
library(rgl)

# ----- Function to draw wavy cubes -----

library(shiny)
library(rgl)

# Generate a wavy cube based on user parameters
wavy_cube <- function(radius = 0.5, amplitude = 0.1, frequency = 3) {
  # Start with a cube
  base <- cube3d()
  
  # Extract the vertices
  verts <- base$vb  # 4 x N matrix (homogeneous coordinates)
  
  # Apply wave deformation to X and/or Y or Z axes
  # For example, modulate Z as a function of X and Y
  for (i in 1:ncol(verts)) {
    x <- verts[1, i]
    y <- verts[2, i]
    verts[3, i] <- verts[3, i] + amplitude * sin(frequency * pi * x) * sin(frequency * pi * y)
  }
  
  base$vb <- verts
  
  # Scale the wavy cube
  base <- scale3d(base, radius, radius, radius)
  
  return(base)
}

# Draw the scene with multiple wavy cells
draw_cell <- function(radius = 0.5, spacing = 1, amplitude = 0.1, frequency = 3, alpha = 0.5) {
  clear3d()
  xpos <- seq(0, 5) * spacing
  
  inner_radius <- radius * 2/3
  
  for (x in xpos) {
    # Outer wavy cube
    shade3d(translate3d(wavy_cube(radius, amplitude, frequency), x, 0, 0),
            color = "steelblue", alpha = alpha * 0.4)
    
    # Inner cube (no waves)
    shade3d(translate3d(scale3d(cube3d(), inner_radius,
                                inner_radius, inner_radius), x, 0, 0),
            color = "red4", alpha = alpha)
  }
  rglwidget()
}


# -------------------------------

# SHINY APP

# -------------------------------

ui <- fluidPage(
  titlePanel("Interactive Wavy 3D Cells"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("radius", "Cell radius:", 0.1, 1, 0.5, step = 0.05),
      sliderInput("spacing", "Cell spacing:", 0.5, 2, 1, step = 0.1),
      sliderInput("amplitude", "Wave amplitude:", 0, 0.5, 0.1, step = 0.01),
      sliderInput("frequency", "Wave frequency:", 1, 10, 3, step = 0.5),
      sliderInput("alpha", "Transparency:", 0, 1, 0.5, step = 0.05)
    ),
    mainPanel(
      rglwidgetOutput("cells3d", width = "100%", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  output$cells3d <- renderRglwidget({
    draw_cell(
      radius = input$radius,
      spacing = input$spacing,
      amplitude = input$amplitude,
      frequency = input$frequency,
      alpha = input$alpha
    )
  })
}


shinyApp(ui, server)
