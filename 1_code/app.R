library(shiny)
library(dplyr)
library(shinyWidgets)

# Define the u-shaped curve function
u_shape <- function(x) {
  -x^2
}

filter_pr_list <- function(.tab, .id, .col) {
  .tab[[.col]][.tab$id == .id]
} 

tab_pr <- openxlsx::read.xlsx("../0_data/projects.xlsx")
tab_cr <- openxlsx::read.xlsx("../0_data/credentials.xlsx")


# Define the Shiny UI
ui <- fluidPage(
  titlePanel("Classify Your Project"),
  sidebarLayout(
    sidebarPanel(
      HTML("<h4>Instructions:</h4>"),
      HTML("<p>Move the red point along the curve by clicking on the curve.</p>"),
      splitLayout(
        textInputIcon("name", "Username", placeholder = "Enter your credentials"),
        textInputIcon("pw", "Password", placeholder = "Enter your password"),
      ),
      actionButton("login", "Log In"),
      hr(),
      verbatimTextOutput("pr_title"),
      hr(),
      actionButton("save_button", "Save Response"),
      br(),
      br(),
      shinyWidgets::radioGroupButtons(
        inputId = "group_rate_project",
        label = "Please choose the project you want to classify",
        choices = sort(unique(tab_pr$id)),
        individual = TRUE
      ),
      htmlOutput("text_rate_project"),
      uiOutput("link_rate"),
      br(),
      verbatimTextOutput("choice")
    ),
    mainPanel(
      HTML("<h4>Transparancy-Outcome Curve</h4>"),
      sliderInput("slider", NULL, -5, 5, 0, .1, width = "100%"),
      splitLayout(
        plotOutput("plot0", height = "600px", hover = "plot_hover", click = "plot_click"),
        plotOutput("plot1", height = "600px", hover = "plot_hover", click = "plot_click")
      ),

      br(),
      tags$div(
        class = "row",
        style = "text-align:center;",
        verbatimTextOutput("point_coords")
      ),
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


  observeEvent(input$login, {
    has_user <- which(tab_cr$user == input$name)
    has_user <- ifelse(length(has_user) == 0, 0L, has_user)
    pw_correct <- ifelse(has_user == 0, FALSE, tab_cr$pw[has_user] == input$pw)
    
    if (pw_correct) {
      prompt_ <- tab_pr %>%
        dplyr::filter(id == tab_cr$id[has_user]) %>%
        dplyr::mutate(tmp = paste0(
          "You successfully logged into project: ", id, "\n",
          title
        )) %>%
        dplyr::pull(tmp)
    } else {
      prompt_ <- ""
    }
    
    output$pr_title <- renderText({
      dplyr::case_when(
        has_user == 0 ~ "Wrong Username",
        !pw_correct ~ "Wrong Password",
        pw_correct ~ prompt_
        
      )
    })

  })
  
  

  # Define the plot
  output$plot0 <- renderPlot({
    # Plot the u-shaped curve
    x <- seq(-5, 5, length.out = 100)
    y <- u_shape(x)
    plot(x, y,
      type = "l", xlim = c(-5, 5), ylim = c(-25, 0), xlab = "Transparancy", ylab = "Outcome",
      xaxt = "n", yaxt = "n", main = "Classify a Project"
    )

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
  
  # Update the point when the mouse is moved
  observeEvent(input$slider, {
    point$x <- input$slider
    point$y <- u_shape(input$slider)

  })

  # Display the coordinates of the point
  output$point_coords <- renderText({
    text_ <- dplyr::case_when(
      point$x <= -5 ~ "Are you sure you study transparancy? :-D",
      point$x <= -2.5 ~ "Your project studies a setting where we have limited transparancy and in turn a limited outcome",
      point$x > -.5 & point$x < .5 ~ "Congratulation the transparancy within your project achieves the highest outcome",
      point$x < 0 ~ "Your project studies a setting where we have decent transparancy and a high outcome",
      point$x <= 2.5 ~ "Your project studies a setting where we have high transparancy but only a limited outcome",
      point$x < 5 ~ "Your project studies a setting where we have extremely high transparancy with adverse effects on the outcome",
      point$x >= 5 ~ "Maximum transparancy but no outcome! :("
    )

    paste("Point coordinates: (", round(point$x, 2), ", ", round(point$y, 2), ")", "\n", text_)
  })

  output$choice <- renderText({
    dplyr::case_when(
      input$group_your_project == input$group_rate_project ~ "You want to rate your own project",
      input$group_your_project != input$group_rate_project ~ "Are you sure you want to rate another project?"
    )
  })

  output$text_your_project <- renderUI({
    h4(filter_pr_list(tab_pr, input$group_your_project, "title"))
  }) 

  output$text_rate_project <- renderUI({
    h4(filter_pr_list(tab_pr, input$group_rate_project, "title"))
  }) 
  
  
  output$plot1 <- renderPlot({
    # Plot the u-shaped curve
    x <- seq(-5, 5, length.out = 100)
    y <- u_shape(x)
    plot(x, y,
         type = "l", xlim = c(-5, 5), ylim = c(-25, 0), xlab = "Transparancy", ylab = "Outcome",
         xaxt = "n", yaxt = "n", main = "Your Responses"
    )
  })
  
  
  observeEvent(input$save_button, {
    tab_ <- tibble::tibble(
      time = Sys.time(),
      from = input$group_your_project,
      to = input$group_rate_project,
      xval = point$x
    )
    
    path_ <- "../2_output/response.fst"
    if (!file.exists(path_)) {
      out_ <- tab_
      fst::write_fst(out_, path_)
    } else {
      out_ <- dplyr::bind_rows(fst::read_fst(path_), tab_)
      fst::write_fst(out_, path_)
    }
    
    output$plot1 <- renderPlot({
      # Plot the u-shaped curve
      x <- seq(-5, 5, length.out = 100)
      y <- u_shape(x)
      plot(x, y,
           type = "l", xlim = c(-5, 5), ylim = c(-25, 0), xlab = "Transparancy", ylab = "Outcome",
           xaxt = "n", yaxt = "n", main = "Your Responses"
      )
      if (file.exists(path_)) {
        tab_ <- fst::read_fst(path_)
        resx <- tab_$xval
        resy <- u_shape(resx)
        points(resx, resy, pch = 21, bg = point_fill, col = "blue", cex = 2)
      }
      
    })
    
  })
  
  # Define the plot

  
  
}

# Run the Shiny app
shinyApp(ui, server)
