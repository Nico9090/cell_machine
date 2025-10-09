library(shiny)
library(rgl)

# ----- Function to draw wavy cubes -----

draw_cell <- function(){
  clear3d()
  xpos <- c(0, 1, 2, 3, 4, 5)
  cell_radius <- 0.5
  inner_radius <- cell_radius *2/3
  for (x in xpos){
    shade3d(translate3d(scale3d(cube3d(), cell_radius,
                                cell_radius, cell_radius), x, 0, 0),
            color = "steelblue", alpha = 0.2)
    shade3d(translate3d(scale3d(cube3d(), inner_radius,
                                inner_radius, inner_radius), x, 0, 0),
            color = "red4", alpha = 0.8)
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
    draw_cell()
  })
}

shinyApp(ui, server)

