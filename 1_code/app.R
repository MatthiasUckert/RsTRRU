library(shiny)
library(dplyr)

# Define the u-shaped curve function
u_shape <- function(x) {
  y <- -x^2
  return(y)
}

choice <- c(
  "A01", "A02", "A03", "A04", "A05",
  "A06", "A07", "A08", "B01", "B02",
  "B03", "B04", "B05", "B06", "B07",
  "B08", "B09", "B10", "C01", "C02", "C03"
)

# Define the Shiny UI
ui <- fluidPage(
  titlePanel("Classify Your Project"),
  sidebarLayout(
    sidebarPanel(
      HTML("<h4>Instructions:</h4>"),
      HTML("<p>Move the red point along the curve by clicking on the curve.</p>"),
      actionButton("save_button", "Save Response"),
      br(),
      br(),
      shinyWidgets::radioGroupButtons(
        inputId = "group_your_project",
        label = "Please choose your project",
        choices = choice, 
        individual = TRUE
      ),
      shinyWidgets::radioGroupButtons(
        inputId = "group_rate_project",
        label = "Please choose the project you want to rate",
        choices = choice, 
        individual = TRUE
      ),
      verbatimTextOutput("choice")
    ),
    mainPanel(
      HTML("<h4>Transparancy-Outcome Curve</h4>"),
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
    if (input$plot_click$x < -5) {
      point$x <- -5
      point$y <- u_shape(-5)
    } else if (input$plot_click$x > 5) {
      point$x <- 5
      point$y <- u_shape(5)
    } else {
      point$x <- input$plot_click$x
      point$y <- u_shape(input$plot_click$x)
    }

  })
  
  # Display the coordinates of the point
  output$point_coords <- renderText({
    text_ <- case_when(
      point$x <= -5   ~ "Are you sure you study transparancy? :-D",
      point$x <= -2.5 ~ "Your project studies a setting where we have limited transparancy and in turn a limited outcome",
      point$x > -.5 & point$x < .5    ~ "Congratulation the transparancy within your project achieves the highest outcome",
      point$x < 0     ~ "Your project studies a setting where we have decent transparancy and a high outcome",
      point$x <= 2.5  ~ "Your project studies a setting where we have high transparancy but only a limited outcome",
      point$x < 5     ~ "Your project studies a setting where we have extremely high transparancy with adverse effects on the outcome",
      point$x >= 5    ~ "Maximum transparancy but no outcome! :("
    )
    
    paste("Point coordinates: (", round(point$x, 2), ", ", round(point$y, 2), ")", "\n", text_)
  })
  
  output$choice <- renderText({
case_when(
      input$group_your_project == input$group_rate_project ~ "You want to rate your own project",
      input$group_your_project != input$group_rate_project ~ "Are you sure you want to rate another project?"
    )
    
  })
}

# Run the Shiny app
shinyApp(ui, server)
