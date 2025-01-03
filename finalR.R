# Load necessary libraries
library(shiny)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(leaflet)
library(DT)
library(caret)
library(readxl)
library(xgboost)

# In-memory user database (for demonstration purposes)
user_db <- reactiveVal(data.frame(username = character(), password = character(), stringsAsFactors = FALSE))

# Define UI
ui <- fluidPage(
  # Custom CSS Styling
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5e3d1; /* Light Beige */
        color: #333; /* Dark text for contrast */
      }
      .navbar {
        background-color: #e07b6d; /* Terracotta Red */
      }
      .navbar a {
        color: white !important;
      }
      .btn-primary {
        background-color: #e07b6d; /* Terracotta Red */
        border-color: #e07b6d;
      }
      .btn-primary:hover {
        background-color: #d06053; /* Slightly darker Terracotta */
        border-color: #d06053;
      }
      .btn-secondary {
        background-color: #4a7f7f; /* Muted Teal */
        border-color: #4a7f7f;
      }
      .btn-secondary:hover {
        background-color: #3e6a6a; /* Slightly darker Muted Teal */
        border-color: #3e6a6a;
      }
      .modal-content {
        background-color: #f8f8f8;
        color: #333;
      }
      .modal-footer {
        display: flex;
        justify-content: center;
      }
      .modal-header {
        background-color: #e07b6d;
        color: white;
      }
      .modal-title {
        font-weight: bold;
      }
      .panel {
        background-color: #f5e3d1; /* Light Beige */
        border: 1px solid #e07b6d; /* Terracotta Red */
      }
      .panel-heading {
        background-color: #e07b6d;
        color: white;
      }
      .panel-body {
        background-color: #f5e3d1;
      }
      .sidebar {
        background-color: #f5e3d1; /* Light Beige */
        border-right: 1px solid #e07b6d;
      }
    "))
  ),
  
  theme = shinytheme("flatly"),
  
  navbarPage(
    "Finance Management Dashboard",
    id = "main_nav",
    
    tabPanel("Login",
             fluidRow(
               column(4, offset = 4,
                      h2("Login to Dashboard"),
                      p("Welcome to the Finance Management and Predictive Budgeting app! Use your credentials to access a suite of financial tools, including data visualization, predictive modeling, and secure storage of your data."),
                      textInput("username", "Username:"),
                      passwordInput("password", "Password:"),
                      actionButton("login_btn", "Login", class = "btn-primary btn-lg"),
                      actionButton("signup_btn", "Sign Up", class = "btn-secondary btn-lg"),
                      textOutput("login_status"),
                      hr(),
                      p("Developed by Aman Choudhary and Abhay Sharma", style = "text-align:center; font-weight:bold;")
               )
             )
    ),
    
    tabPanel("Home",
             conditionalPanel(
               condition = "output.is_logged_in",
               fluidRow(
                 column(8, offset = 2,
                        h2("Welcome to Finance Management and Predictive Budgeting!"),
                        hr(),
                        p("This application helps you manage your finances by providing tools to analyze, visualize, and predict financial data. Upload your data, visualize trends, and use predictive analytics to plan your budget effectively.", style = "font-size: 16px;"),
                        
                        h3("Overview of the Application"),
                        p("The Finance Management and Predictive Budgeting App is designed to help users manage their finances effectively by providing a suite of tools for data visualization, financial analysis, and predictive modeling. The application is built using the Shiny framework in R, integrating modern UI/UX elements and robust backend functionality for real-world utility.", style = "font-size: 14px;"),
                        
                        h4("Features:"),
                        tags$ul(
                          tags$li("User Authentication: Secure login and signup system to manage user access."),
                          tags$li("Upload Financial Data: Upload financial data in Excel format (columns: Date, Income, Expenses)."),
                          tags$li("Data Visualization: Interactive 3D scatter plots using the plotly library."),
                          tags$li("Predictive Budgeting: Predict future budgets using XGBoost."),
                          tags$li("Performance Metrics: View R-squared for predictive model accuracy."),
                          tags$li("Modern UI/UX: Styled using shinythemes and custom CSS for an intuitive interface.")
                        ),
                        
                        h4("Key Technologies:"),
                        tags$ul(
                          tags$li("Shiny Framework: For building the interactive web application."),
                          tags$li("XGBoost: For implementing the predictive model."),
                          tags$li("Plotly: For interactive visualizations."),
                          tags$li("Caret: For dataset partitioning and preprocessing."),
                          tags$li("Leaflet: Placeholder for potential map-based features.")
                        ),
                        
                        h4("Future Enhancements:"),
                        tags$ul(
                          tags$li("Persistent Database: Replace in-memory storage with a database like MySQL or MongoDB."),
                          tags$li("Enhanced Security: Implement hashed passwords and session-based authentication."),
                          tags$li("Additional Predictive Features: Introduce more financial metrics and forecasting options."),
                          tags$li("Mobile Optimization: Improve the layout for smaller screens.")
                        ),
                        
                        h4("Developed by Aman Choudhary and Abhay Sharma", style = "text-align:center; font-weight:bold;")
                 )
               )
             ),
             conditionalPanel(
               condition = "!output.is_logged_in",
               h3("Please log in to access the dashboard.", style = "text-align:center; color:red;")
             )
    ),
    
    tabPanel("Dashboard",
             conditionalPanel(
               condition = "output.is_logged_in",
               sidebarLayout(
                 sidebarPanel(
                   fileInput("file", "Upload Your Finance Data (Excel File)", accept = c(".xlsx")),
                   actionButton("process", "Process Data"),
                   sliderInput("bins", "Prediction Period (Months):", min = 1, max = 12, value = 6),
                   hr()
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Data Table", dataTableOutput("data_table")),
                     tabPanel("Visualization", plotlyOutput("three_d_plot")),
                     tabPanel("Prediction", verbatimTextOutput("predicted_budget")),
                     tabPanel("Accuracy", verbatimTextOutput("accuracy_metrics"))
                   )
                 )
               )
             ),
             conditionalPanel(
               condition = "!output.is_logged_in",
               h3("Please log in to access the dashboard.", style = "text-align:center; color:red;")
             )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  is_logged_in <- reactiveVal(FALSE)
  
  observeEvent(input$login_btn, {
    db <- user_db()
    if (nrow(db[db$username == input$username & db$password == input$password, ]) > 0) {
      is_logged_in(TRUE)
      output$login_status <- renderText("")
    } else {
      output$login_status <- renderText("Invalid username or password.")
    }
  })
  
  observeEvent(input$signup_btn, {
    showModal(modalDialog(
      title = "Sign Up",
      textInput("new_username", "Username:"),
      passwordInput("new_password", "Password:"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_signup", "Sign Up", class = "btn-secondary btn-lg")
      )
    ))
  })
  
  observeEvent(input$confirm_signup, {
    db <- user_db()
    if (input$new_username %in% db$username) {
      showModal(modalDialog(title = "Error", "Username already exists. Please choose another."))
    } else {
      user_db(rbind(db, data.frame(username = input$new_username, password = input$new_password, stringsAsFactors = FALSE)))
      showModal(modalDialog(title = "Success", "Sign up successful! You can now log in."))
    }
    removeModal()
  })
  
  output$is_logged_in <- reactive({ is_logged_in() })
  outputOptions(output, "is_logged_in", suspendWhenHidden = FALSE)
  
  finance_data <- reactiveVal(NULL)
  
  observeEvent(input$process, {
    req(input$file)
    tryCatch({
      data <- read_excel(input$file$datapath)
      if (!all(c("Date", "Income", "Expenses") %in% colnames(data))) {
        showModal(modalDialog(
          title = "Error",
          "The uploaded file must have columns: 'Date', 'Income', 'Expenses'."
        ))
        return(NULL)
      }
      finance_data(data)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        "Failed to read the uploaded file. Ensure it's a valid Excel file."
      ))
    })
  })
  
  output$data_table <- renderDataTable({
    req(finance_data())
    datatable(finance_data())
  })
  
  output$three_d_plot <- renderPlotly({
    req(finance_data())
    data <- finance_data()
    plot_ly(data, x = ~Date, y = ~Income, z = ~Expenses, type = "scatter3d", mode = "markers")
  })
  
  output$predicted_budget <- renderPrint({
    req(finance_data())
    # Placeholder for prediction logic
    "Predicted budget will be shown here."
  })
  
  output$accuracy_metrics <- renderPrint({
    req(finance_data())
    # Placeholder for accuracy metrics
    "Model accuracy metrics will be shown here."
  })
}

# Run App
shinyApp(ui, server)
