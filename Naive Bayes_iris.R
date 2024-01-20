if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

# Load the shiny package
library(shiny)
library(DT)

# Define the UI
ui <- fluidPage(
  titlePanel("Iris Species"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("species1", "Select Species 1:",
                  choices = unique(iris$Species)),
      selectInput("species2", "Select Species 2:",
                  choices = ""),
      selectInput("x_variable", "Select X-axis variable:",
                  choices = names(iris[, 1:4])),
      selectInput("y_variable", "Select Y-axis variable:",
                  choices = names(iris[, 1:4]))
    ),
    
    mainPanel(
      plotOutput("scatterplot"),
      tableOutput("summary_stats"),
      DTOutput("data_table")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "species2",
                      choices = setdiff(unique(iris$Species), input$species1))
  })
  
  observe({
    updateSelectInput(session, "y_variable",
                      choices = setdiff(names(iris[, 1:4]), input$x_variable))
  })
  
  selected_species <- reactive({
    iris[iris$Species %in% c(input$species1, input$species2), ]
  })
  
  output$scatterplot <- renderPlot({
    # Create a color vector based on species
    colors <- ifelse(selected_species()$Species == input$species1, "red", "blue")
    
    plot(selected_species()[, input$x_variable], 
         selected_species()[, input$y_variable],
         main = paste("Scatter plot for", input$species1, "and", input$species2),
         xlab = input$x_variable,
         ylab = input$y_variable,
         col = colors,
         pch = 19
    )
    legend("topright", legend = unique(selected_species()$Species), col = c("red", "blue"), pch = 19, bty = "n")
  })
  
  output$summary_stats <- renderTable({
    summary_data <- summary(selected_species()[, c(input$x_variable, input$y_variable)])
    summary_data <- as.data.frame(t(summary_data))
    colnames(summary_data) <- c("Summary Statistic", input$x_variable, input$y_variable)
    summary_data
  })
  
  output$data_table <- renderDT({
    datatable(selected_species(), options = list(pageLength = 5))
  })
}

# Run the Shiny app
shinyApp(ui, server)