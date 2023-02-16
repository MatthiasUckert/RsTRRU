library(shiny)

# Define the u-shaped curve function
u_shape <- function(x) {
  y <- -x^2
  return(y)
}

# Define the Shiny UI
ui <- fluidPage(
  titlePanel("Rate Your Project"),
  sidebarLayout(
    sidebarPanel(
      HTML("<h4>Instructions:</h4>"),
      HTML("<p>Move the blue point along the curve by clicking on the curve.</p>"),
      actionButton("save_button", "Save Response"),
      br(),
      br(),
      shinyWidgets::radioGroupButtons(
        inputId = "radio_group",
        label = "Choose an option:",
        choices = c(
          "A01", "A02", "A03", "A04", "A05",
          "A06", "A07", "A08", "B01", "B02",
          "B03", "B04", "B05", "B06", "B07",
          "B08", "B09", "B10", "C01", "C02", "C03"
        ), individual = TRUE
      )
    ),
    mainPanel(
      plotOutput("plot", height = "600px", hover = "plot_hover", click = "plot_click"),
      br(),
      tags$div(
        class = "row",
        style = "text-align:center;",
        verbatimTextOutput("point_coords")
      )
    )
  ),
  tags$head(tags$style(HTML("
    .container {
      max-width: 960px;
    }
    .well {
      background-color: #f5f5f5;
      border: none;
      padding: 20px;
    }
    .row {
      margin-bottom: 20px;
    }
  ")))
)


# Define the Shiny server
server <- function(input, output, session) {
  # Set up the initial point at (0, u_shape(0))
  point <- reactiveValues(x = 0, y = u_shape(0))
  point_size <- 4
  point_color <- "red"
  point_fill <- "white"
  
  
  # Define the plot
  output$plot <- renderPlot({
    # Plot the u-shaped curve
    x <- seq(-5, 5, length.out = 100)
    y <- u_shape(x)
    plot(x, y, type = "l", xlim = c(-5, 5), ylim = c(-25, 0), xlab = "Transparancy", ylab = "Outcome", 
         xaxt="n", yaxt="n")
    
    # Add the point
    points(point$x, point$y, pch = 21, bg = point_fill, col = point_color, cex = point_size)
  })
  
  # Update the point when the mouse is moved
  observeEvent(input$plot_click, {
    # Get the x and y coordinates of the mouse
    point$x <- input$plot_click$x
    point$y <- u_shape(input$plot_click$x)
  })
  
  # Display the coordinates of the point
  output$point_coords <- renderText({
    paste("Point coordinates: (", round(point$x, 2), ", ", round(point$y, 2), ")")
  })
}

# Run the Shiny app
shinyApp(ui, server)
