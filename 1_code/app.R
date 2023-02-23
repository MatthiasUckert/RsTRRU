library(shiny)
library(dplyr)
library(shinyWidgets)
options(shiny.autoreload = TRUE)

# Define the u-shaped curve function
u_shape <- function(x) {
  -x^2
}

filter_project_list <- function(.tab, .id, .col) {
  .tab[[.col]][.tab$id == .id]
} 

draw_curve <- function(.title) {
  x <- seq(-5, 5, length.out = 100)
  y <- u_shape(x)
  plot(x, y,
       type = "l", xlim = c(-5, 5), ylim = c(-25, 0), xlab = "Transparancy", ylab = "Outcome",
       xaxt = "n", yaxt = "n", main = .title
  )
}

draw_points <- function(.x, .y, .type = c("classify", "response")) {
  type_ <- match.arg(.type,  c("classify", "response"))
  if (type_ == "classify") {
    points(.x, .y, pch = 21, bg = "white", col = "red", cex = 4)
  } else {
    points(.x, .y, pch = 21, bg = "white", col = "blue", cex = 2)
  }
}



display_text <- function(.x) {
  dplyr::case_when(
    .x <= -5 ~ "Are you sure you study transparancy? :-D",
    .x <= -2.5 ~ "Your project studies a setting where we have limited transparancy and in turn a limited outcome",
    .x > -.5 & .x < .5 ~ "Congratulation the transparancy within your project achieves the highest outcome",
    .x < 0 ~ "Your project studies a setting where we have decent transparancy and a high outcome",
    .x <= 2.5 ~ "Your project studies a setting where we have high transparancy but only a limited outcome",
    .x < 5 ~ "Your project studies a setting where we have extremely high transparancy with adverse effects on the outcome",
    .x >= 5 ~ "Maximum transparancy but no outcome! :("
  )
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
        inputId = "rGB_rate",
        label = "Please choose the project you want to classify",
        choices = sort(unique(tab_pr$id)),
        individual = TRUE
      ),
      htmlOutput("text_rate"),
      uiOutput("link_rate"),
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
  path_ <- reactive("../2_output/response.fst")
  project <- reactiveValues(id = "")

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
  # Update the point when the slider is moved
  observeEvent(input$slider, {
    point$x <- input$slider
    point$y <- u_shape(input$slider)
    
  })
  
  # Define the plot (1)
  output$plot0 <- renderPlot({
    # Plot the u-shaped curve
    draw_curve("Classify a Project") # Draw the Curve
    draw_points(point$x, point$y, "classify") # Add the point
  })
  # Define the plot (2)
  output$plot1 <- renderPlot({
    input$save_button
    input$rGB_rate
    # Plot the u-shaped curve
    draw_curve("Your Responses") # Draw the Curve

    if (file.exists(path_())) {
      tab_ <- dplyr::filter(fst::read_fst(path_()), from == project$id)
      if (nrow(tab_) > 0) {
        draw_points(tab_$xval, u_shape(tab_$xval), "response") # Add the point
        text(tab_$xval, u_shape(tab_$xval) - 1.5, label = tab_$to, offset = 0)
      }

    }
  })
  
  # Display the coordinates of the point
  output$point_coords <- renderText({
    paste0(
      "Point coordinates: (", round(point$x, 2), ", ", round(point$y, 2), ")\n",
      display_text(point$x)
    )
  })
  
  # ToDo: 
  observeEvent(input$login, {
    has_user   <- which(tab_cr$user == input$name)
    has_user   <- ifelse(length(has_user) == 0, 0L, has_user)
    pw_correct <- ifelse(has_user == 0, FALSE, tab_cr$pw[has_user] == input$pw)
    
    if (pw_correct) {
      log_ <- "You successfully logged into project: "
      prompt_ <- tab_pr %>%
        dplyr::filter(id == tab_cr$id[has_user]) %>%
        dplyr::mutate(tmp = paste0(log_, id, "\n", title)) %>%
        dplyr::pull(tmp)
      project$id <- tab_cr$id[has_user]
    } else {
      prompt_ <- ""
      project$id <- ""
    }
    
    output$pr_title <- renderText({
      dplyr::case_when(
        has_user == 0 ~ "Wrong Username",
        !pw_correct ~ "Wrong Password",
        pw_correct ~ prompt_
      )
    })
    
  })
  
  observeEvent(input$save_button, {
    tab_ <- tibble::tibble(
      time = Sys.time(),
      from = project$id,
      to = input$rGB_rate,
      xval = point$x
    )
    
    if (!project$id == "") {
      if (!file.exists(path_())) {
        out_ <- tab_
        fst::write_fst(out_, path_())
      } else {
        out_ <- dplyr::bind_rows(fst::read_fst(path_()), tab_)
        fst::write_fst(out_, path_())
      }
    }
  })
  
  
  # Show Project Title and Link
  output$text_rate <- renderUI({
    h4(filter_project_list(tab_pr, input$rGB_rate, "title"))
    }) 
  output$link_rate <- renderUI({
    url_ <- a(input$rGB_rate, href = filter_project_list(tab_pr, input$rGB_rate, "link"))
    tagList("Link to Project:", url_)
  })
  


  
  # Define the plot

  
  
}

# Run the Shiny app
shinyApp(ui, server)
