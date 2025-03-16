library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(fontawesome)
library(flexdashboard)

ui <- dashboardPage(
  dashboardHeader(
    titleWidth = "100%",   # Make the header span the entire width
    title = tags$div(
      style = "width: 100%; display: flex; align-items: center; justify-content: space-between;",
      
      # Left side: your image
      uiOutput("headerImage"),
      
      # Right side: your text
      tags$span(
        "Tax-Modeling-Toolkit",
        style = "white-space: nowrap; font-size: 18px; margin-right: 20px;"
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About Model", tabName = "aboutModel", icon = icon("info")),
      menuItem("Direct Taxes", icon = icon("file-invoice"),
               menuSubItem("PIT Module", tabName = "pitModule", icon = icon("chart-pie")),
               menuSubItem("CIT Module", tabName = "citModule", icon = icon("industry"))
      ),
      menuItem("Indirect Taxes", icon = icon("shopping-bag"),
               menuSubItem("VAT Module", tabName = "vatModule", icon = icon("shopping-cart")),
               menuSubItem("Import Taxes Module", tabName = "customsModule", icon = icon("truck"))
               #menuSubItem("Excise Module", tabName = "exciseModule", icon = icon("beer"))
      )
      # menuItem("Revenue Forecasting", icon = icon("chart-line"),
      #          menuSubItem("Short-Term", tabName = "shortTerm", icon = icon("calendar-alt")),
      #          menuSubItem("Medium-Term", tabName = "mediumTerm", icon = icon("calendar-check"))
      # ),
      # menuItem("Revenue Monitoring", icon = icon("magnifying-glass-chart"),
      #          menuSubItem("Revenue-Monitoring", tabName = "revMon", icon = icon("gauge"))
      # )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "aboutModel",
        h3("About the Model"),
        p("This is a centralized interface for managing and activating different tax modeling modules."),
        br()
      ),
      tabItem(
        tabName = "pitModule",
        h3("PIT Module"),
        p("The PIT Module provides simulations and analysis for personal income tax policies. The gold standard for estimating the fiscal impact of new and existing tax policy measures is to leverage microsimulation models, which offer powerful insights into the fiscal and distributional impacts of tax policies and their interactions with other government programs. Unlike standard economics models that typically use a 'representative agent' framework, microsimulation models fully account for individual-level heterogeneity as observed in micro data sets. This detailed analysis at the individual level is crucial for understanding specific behaviors that may be important for the policy under consideration. Additionally, microsimulation models enable the estimation of the cost/benefit of a reform down to the individual level, which standard economics models cannot usually achieve. Policymakers can calculate the cost/benefit and identify winners/losers of a reform at any level of aggregation, providing a rich perspective and informed decision-making."),
        br(),
        actionButton("activatePIT", "Activate PIT Module", icon = icon("play"), class = "btn-primary", style = "float: right;")
      ),
      tabItem(
        tabName = "citModule",
        h3("CIT Module"),
        p("The CIT Module provides simulations and analysis for corporate income tax policies."),
        br(),
        actionButton("activateCIT", "Activate CIT Module", icon = icon("play"), class = "btn-primary", style = "float: right;")
      ),
      tabItem(
        tabName = "vatModule",
        h3("VAT Module"),
        p("The VAT Module provides simulations and analysis for value-added tax policies."),
        br(),
        actionButton("activateVAT", "Activate VAT Module", icon = icon("play"), class = "btn-primary", style = "float: right;")
      ),
      tabItem(
        tabName = "customsModule",
        h3("Import Tax Module"),
        p("The Import Tax Module provides simulations and analysis for customs policies."),
        br(),
        actionButton("activateCustoms", "Activate Customs Module", icon = icon("play"), class = "btn-primary", style = "float: right;")
      ),
      tabItem(
        tabName = "exciseModule",
        h3("Excise Module"),
        p("The Excise Module provides simulations and analysis for excise tax policies."),
        br(),
        actionButton("activateExcise", "Activate Excise Module", icon = icon("play"), class = "btn-primary", style = "float: right;")
      ),
      tabItem(
        tabName = "shortTerm",
        h3("Short-Term Forecasting"),
        p("This module uses different statistical models which can be used for modeling time series. Exponential smoothing (ETS) and autoregressive integrated moving average (ARIMA) (Hyndman & Athanasopoulos, 2016) are considered two of the most frequently used models in time series forecasting that allow for a complementary approach to the problem.

          The ETS forecasting model starts from the assumption that a certain regularity in the change of observations and their random fluctuations is present in the series, whereby the alignment method gives rise to the so-called 'smoothed series', showing the basic tendency of the time series that is further used for modeling.

          The predictions of ARIMA forecasting models assume that future circumstances in the time series will be similar to past circumstances. Due to this feature, these models are widely used when modeling a great number of economic series that entail periodic variations.
          
          
          "),
        
        br(),
        actionButton("activateShortTerm", "Activate Short-Term Forecast Module", icon = icon("play"), class = "btn-primary", style = "float: right;")
      ),
      tabItem(
        tabName = "mediumTerm",
        h3("Medium-Term Forecast"),
        p("This section provides tools and analysis for medium-term economic forecasting.")
      ),
      tabItem(
        tabName = "revMon",
        h3("Revenue Monitoring"),
        p("This section provides tools and analysis for monitoring revenue performance."),
        br(),
        actionButton("activateRevenueMonitoring", "Activate Revenue Monitoring", icon = icon("play"), class = "btn-primary", style = "float: right;")
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$headerImage <- renderUI({
    img_data <- base64enc::dataURI(file = "Modules/img/WB_pic.png", mime = "image/png")
    tags$img(src = img_data, height = "40px", style = "float:left; margin-right:20px;")
  })
  
  run_shiny_app <- function(script_name, port = NULL) {
    if (file.exists(script_name)) {
      print(paste("Launching Shiny app:", script_name))
      port <- if (is.null(port)) sample(8000:9000, 1) else port
      command <- sprintf(
        "R -e \"shiny::runApp('%s', port = %d, launch.browser = TRUE)\"",
        script_name, port
      )
      system(command, wait = FALSE, intern = FALSE)
    } else {
      showModal(modalDialog(
        title = "Error",
        paste("Script not found:", script_name),
        easyClose = TRUE
      ))
    }
  }
  
  observeEvent(input$activatePIT, {
    showModal(modalDialog(
      title = "Activating PIT Module",
      "The PIT Module is now being activated in a new browser tab. Please wait...",
      easyClose = TRUE
    ))
    run_shiny_app("Modules/PIT-Module.R")
  })
  
  observeEvent(input$activateCIT, {
    showModal(modalDialog(
      title = "Activating CIT Module",
      "The CIT Module is now being activated in a new browser tab. Please wait...",
      easyClose = TRUE
    ))
    run_shiny_app("Modules/CIT-Module.R")
  })
  
  observeEvent(input$activateVAT, {
    showModal(modalDialog(
      title = "Activating VAT Module",
      "The VAT Module is now being activated in a new browser tab. Please wait...",
      easyClose = TRUE
    ))
    run_shiny_app("Modules/VAT-Module.R")
  })
  
  observeEvent(input$activateCustoms, {
    showModal(modalDialog(
      title = "Activating Customs Module",
      "The Customs Module is now being activated in a new browser tab. Please wait...",
      easyClose = TRUE
    ))
    run_shiny_app("Modules/Customs-Module.R")
  })
  
  observeEvent(input$activateExcise, {
    showModal(modalDialog(
      title = "Activating Excise Module",
      "The Excise Module is now being activated in a new browser tab. Please wait...",
      easyClose = TRUE
    ))
    run_shiny_app("Modules/Excise-Module.R")
  })
  observeEvent(input$activateShortTerm, {
    showModal(modalDialog(
      title = "Activate Short-Term Forecast Module",
      "The Short-Term Forecast Module is now being activated in a new browser tab. Please wait...",
      easyClose = TRUE
    ))
    run_shiny_app("Modules/ShortTermForecast-Module.R")
  })
  
  observeEvent(input$activateRevenueMonitoring, {
    showModal(modalDialog(
      title = "Activating Revenue Monitoring",
      "The Revenue Monitoring module is now being activated in a new browser tab. Please wait...",
      easyClose = TRUE
    ))
    run_shiny_app("Modules/Revenue-Monitoring.R")
  })
}

shinyApp(ui = ui, server = server)
