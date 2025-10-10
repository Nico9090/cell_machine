library(shiny)
library(rgl)
source("functions.R")
source("objects.R")
#also working
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("amplitude", "wave amplitude:", 0, 0.5, 0.1, step = 0.01),
      sliderInput("frequency", "wave frequency:", 1, 10, 3, step = 0.5)
    ),
    mainPanel(
      rglwidgetOutput("cells3d",width = "100%", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  # Debounce input values outside reactive contexts
  debounced_amplitude <- debounce(reactive(input$amplitude), 200)
  debounced_frequency <- debounce(reactive(input$frequency), 200)
  
  output$cells3d <- renderRglwidget({
    # Get debounced input values
    ampl <- debounced_amplitude()
    freq <- debounced_frequency()
    
    # Avoid redrawing every primitive live
    par3d(skipRedraw = TRUE)
    
    # Clear and set background
    rgl.clear()
    bg3d("white")
    
    # Static structures
    for (cell in cells){  # from objects.R
      draw_wire_cube(
        verts = cell,
        amplitude = ampl,
        frequency = freq
      )
    }
    
    for (cell in outer_membrane){  # from objects.R
      draw_filled_outer_cube(
        verts = cell,
        col = "steelblue",
        alpha = 0.4
      )
    }
    
    # Dynamic structures
    for (cell in inner_membrane){  # from objects.R
      draw_filled_cube(
        verts = cell,
        col = "red4",
        alpha = 0.8,
        amplitude = ampl,
        frequency = freq
      )
    }
    
    par3d(skipRedraw = FALSE)
    
    # Return the rendered widget
    rglwidget()
  })
}





shinyApp(ui, server)
