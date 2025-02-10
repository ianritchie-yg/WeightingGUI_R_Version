# app.R
# Survey Weighting Suite - Main Application
# Version 1.0.0

# 1. Package Loading -------------------------------------------------------------
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyjs)
  library(shinyWidgets)
  library(waiter)
  library(DT)
  library(plotly)
  library(tidyverse)
  library(haven)
  library(openxlsx)
  library(readxl)
  library(rmarkdown)
  library(logger)
  library(yaml)
})

# 2. Source Component Files ----------------------------------------------------
source("R/utils.R")
source("R/error_handling.R")
source("R/visualizations.R")
source("R/export.R")

# 3. Load Configuration -------------------------------------------------------
config <- yaml::read_yaml("config.yml")

# 4. Initialize Logger -------------------------------------------------------
log_info("Starting Survey Weighting Suite application")
options(shiny.maxRequestSize = config$settings$max_upload_size)

# 5. Initialize Application State --------------------------------------------
app_state <- reactiveValues(
  data = NULL,
  weights = NULL,
  diagnostics = NULL,
  error_log = character(),
  session_info = list(),
  computation_log = list()
)

# 6. UI Components ---------------------------------------------------------

# 6.1 Dashboard Tab
dashboardTab <- tabItem(
  tabName = "dashboard",
  fluidRow(
    box(
      title = "Data Overview",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      DTOutput("data_summary_table")
    ),
    box(
      title = "Weight Distribution",
      status = "info",
      solidHeader = TRUE,
      width = 6,
      plotlyOutput("weight_dist_plot")
    ),
    box(
      title = "Key Metrics",
      status = "warning",
      solidHeader = TRUE,
      width = 6,
      tableOutput("key_metrics_table")
    )
  )
)

# 6.2 Data Management Tab
dataTab <- tabItem(
  tabName = "data",
  fluidRow(
    box(
      title = "Data Import",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      fileInput("data_file", "Choose Data File",
                accept = c(
                  ".csv", ".xlsx", ".sav", ".rds",
                  "text/csv",
                  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                  "application/x-spss-sav"
                )
      ),
      checkboxInput("header", "First Row as Header", TRUE),
      selectInput("encoding", "File Encoding",
                 choices = c("UTF-8", "ISO-8859-1", "UTF-16"),
                 selected = "UTF-8")
    )
  ),
  fluidRow(
    box(
      title = "Data Preview",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      DTOutput("data_preview")
    )
  )
)

# 6.3 Weighting Tab
weightingTab <- tabItem(
  tabName = "weighting",
  fluidRow(
    box(
      title = "Weighting Method",
      status = "primary",
      solidHeader = TRUE,
      width = 6,
      selectInput("weighting_method",
                 "Select Weighting Method",
                 choices = c(
                   "Post-stratification" = "post_strat",
                   "Raking" = "raking",
                   "Calibration" = "calibration",
                   "IPW" = "ipw"
                 )
      ),
      uiOutput("method_parameters")
    ),
    box(
      title = "Variable Selection",
      status = "info",
      solidHeader = TRUE,
      width = 6,
      uiOutput("variable_selection")
    )
  ),
  fluidRow(
    box(
      title = "Execution",
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      actionButton("compute_weights",
                  "Compute Weights",
                  class = "btn-primary"),
      downloadButton("download_weights",
                    "Download Weights",
                    class = "btn-success")
    )
  )
)

# 6.4 Diagnostics Tab
diagnosticsTab <- tabItem(
  tabName = "diagnostics",
  fluidRow(
    box(
      title = "Weight Distribution",
      status = "primary",
      solidHeader = TRUE,
      width = 6,
      plotlyOutput("weight_distribution")
    ),
    box(
      title = "Convergence Plot",
      status = "info",
      solidHeader = TRUE,
      width = 6,
      plotlyOutput("convergence_plot")
    )
  ),
  fluidRow(
    box(
      title = "Summary Statistics",
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      tableOutput("summary_stats")
    )
  )
)

# 6.5 Export Tab
exportTab <- tabItem(
  tabName = "export",
  fluidRow(
    box(
      title = "Export Options",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      selectInput("export_format",
                 "Select Format",
                 choices = c(
                   "CSV" = "csv",
                   "Excel" = "xlsx",
                   "SPSS" = "sav",
                   "R Data" = "rds"
                 )),
      checkboxInput("include_diagnostics",
                   "Include Diagnostics",
                   value = TRUE),
      downloadButton("download_data",
                    "Download Results",
                    class = "btn-primary"),
      downloadButton("download_report",
                    "Download Report",
                    class = "btn-info")
    )
  )
)

# 6.6 Settings Tab
settingsTab <- tabItem(
  tabName = "settings",
  fluidRow(
    box(
      title = "Application Settings",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      selectInput("theme",
                 "Select Theme",
                 choices = c(
                   "Sunset Dark" = "sunset",
                   "Ocean Dark" = "ocean",
                   "Forest Dark" = "forest"
                 )),
      numericInput("max_iterations",
                  "Maximum Iterations",
                  value = 100,
                  min = 10,
                  max = 1000),
      numericInput("convergence_threshold",
                  "Convergence Threshold",
                  value = 1e-6,
                  min = 1e-10,
                  max = 1e-2)
    )
  )
)

# 7. Main UI Definition -----------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = config$app$name,
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Management", tabName = "data", icon = icon("database")),
      menuItem("Weighting", tabName = "weighting", icon = icon("balance-scale")),
      menuItem("Diagnostics", tabName = "diagnostics", icon = icon("chart-line")),
      menuItem("Export", tabName = "export", icon = icon("download")),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    use_waiter(),
    
    # Custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$script(src = "custom.js")
    ),
    
    # Tabs
    tabItems(
      dashboardTab,
      dataTab,
      weightingTab,
      diagnosticsTab,
      exportTab,
      settingsTab
    )
  )
)

# 8. Server Logic ---------------------------------------------------------
server <- function(input, output, session) {
  # Initialize computation engine
  computation_engine <- ComputationEngine$new()
  
  # Data Loading and Validation
  observeEvent(input$data_file, {
    show_waiter(
      html = spin_flower(),
      color = config$theme$waiter_color
    )
    
    tryCatch({
      file_path <- input$data_file$datapath
      file_ext <- tools::file_ext(input$data_file$name)
      
      data <- load_data(
        file_path = file_path,
        file_type = file_ext,
        header = input$header,
        encoding = input$encoding
      )
      
      # Validate and clean data
      validation_result <- validate_data(data)
      if (!validation_result$valid) {
        throw_error(validation_result$message)
      }
      
      # Update app state
      app_state$data <- data
      app_state$data_summary <- generate_data_summary(data)
      
      # Update UI elements
      updateSelectInput(session, "weight_vars",
                       choices = names(data),
                       selected = NULL)
      
      # Show success notification
      showNotification("Data loaded successfully", type = "success")
      
    }, error = function(e) {
      log_error(e$message)
      showNotification(
        paste("Error loading data:", e$message),
        type = "error"
      )
    }, finally = {
      hide_waiter()
    })
  })
  
  # Generate Data Preview
  output$data_preview <- renderDT({
    req(app_state$data)
    datatable(
      head(app_state$data, 100),
      options = list(
        scrollX = TRUE,
        scrollY = "300px",
        pageLength = 10
      )
    )
  })
  
  # Update Method Parameters UI
  output$method_parameters <- renderUI({
    req(input$weighting_method)
    generate_method_parameters_ui(input$weighting_method)
  })
  
  # Weight Computation
  observeEvent(input$compute_weights, {
    req(app_state$data)
    
    show_waiter(
      html = spin_flower(),
      color = config$theme$waiter_color
    )
    
    tryCatch({
      # Collect parameters
      params <- collect_parameters(input, app_state$method_params)
      
      # Execute weighting
      results <- computation_engine$execute_weighting(
        data = app_state$data,
        method = input$weighting_method,
        params = params
      )
      
      if (!is.null(results)) {
        app_state$weights <- results$weights
        app_state$diagnostics <- results$diagnostics
        
        # Update visualizations
        update_visualizations()
        
        # Show success notification
        showNotification("Weights computed successfully", type = "success")
      }
      
    }, error = function(e) {
      log_error(e$message)
      showNotification(
        paste("Error computing weights:", e$message),
        type = "error"
      )
    }, finally = {
      hide_waiter()
    })
  })

  # Visualization Outputs
  output$weight_distribution <- renderPlotly({
    req(app_state$weights)
    create_weight_distribution_plot(app_state$weights)
  })
  
  output$convergence_plot <- renderPlotly({
    req(app_state$diagnostics)
    create_convergence_plot(app_state$diagnostics$convergence)
  })
  
  output$summary_stats <- renderTable({
    req(app_state$weights, app_state$diagnostics)
    generate_summary_statistics(
      weights = app_state$weights,
      diagnostics = app_state$diagnostics
    )
  })
  
  # Export Handlers
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(
        "weighted_data_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".",
        input$export_format
      )
    },
    content = function(file) {
      export_results(
        data = app_state$data,
        weights = app_state$weights,
        diagnostics = app_state$diagnostics,
        format = input$export_format,
        include_diagnostics = input$include_diagnostics,
        filename = file
      )
    }
  )
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("weighting_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      # Create temporary Rmd file
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
      
      # Render the report
      rmarkdown::render(
        input = tempReport,
        output_file = file,
        params = list(
          data = app_state$data,
          weights = app_state$weights,
          diagnostics = app_state$diagnostics,
          method = input$weighting_method,
          parameters = app_state$method_params
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # Settings Handlers
  observeEvent(input$theme, {
    updateTheme(input$theme)
  })
  
  # Error Handling
  observe({
    if (!is.null(app_state$last_error)) {
      showNotification(
        app_state$last_error$message,
        type = "error",
        duration = NULL
      )
    }
  })
  
  # Session Cleanup
  session$onSessionEnded(function() {
    # Clean up temporary files
    cleanup_temp_files()
    
    # Log session end
    log_info("Session ended")
  })
}

# 9. Helper Functions -----------------------------------------------------

#' Update application theme
#' @param theme_name Character string specifying the theme
updateTheme <- function(theme_name) {
  theme_settings <- switch(theme_name,
    "sunset" = list(
      bg_color = "#1a1a2e",
      text_color = "#e94560",
      accent_color = "#16213e"
    ),
    "ocean" = list(
      bg_color = "#1b262c",
      text_color = "#3282b8",
      accent_color = "#0f4c75"
    ),
    "forest" = list(
      bg_color = "#2d3436",
      text_color = "#6ab04c",
      accent_color = "#130f40"
    )
  )
  
  # Apply theme settings
  shinyjs::runjs(sprintf(
    "document.documentElement.style.setProperty('--bg-color', '%s');
     document.documentElement.style.setProperty('--text-color', '%s');
     document.documentElement.style.setProperty('--accent-color', '%s');",
    theme_settings$bg_color,
    theme_settings$text_color,
    theme_settings$accent_color
  ))
}

#' Clean up temporary files
cleanup_temp_files <- function() {
  temp_files <- list.files(
    path = tempdir(),
    pattern = "^weighting_.*\\.(csv|xlsx|sav|rds)$",
    full.names = TRUE
  )
  unlink(temp_files)
}

# 10. Initialize and Run Application --------------------------------------

# Initialize environment
initialize_environment <- function() {
  # Create necessary directories if they don't exist
  dirs <- c("data", "logs", "output", "temp", "www")
  sapply(dirs, dir.create, showWarnings = FALSE, recursive = TRUE)
  
  # Initialize logging
  log_file <- file.path("logs", format(Sys.time(), "app_%Y%m%d_%H%M%S.log"))
  setup_logging(log_file)
  
  # Set options
  options(
    shiny.maxRequestSize = config$settings$max_upload_size,
    shiny.fullstacktrace = config$app$debug,
    shiny.trace = config$app$debug
  )
  
  TRUE
}

# Run the application
if (initialize_environment()) {
  log_info("Starting application")
  shinyApp(ui = ui, server = server)
} else {
  stop("Failed to initialize application environment")
}