library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(data.table)
library(readxl)
library(fontawesome)
library(flexdashboard)
library(tidyverse)
library(plyr)
library(shinycssloaders)
library(future)
library(promises)
library(plotly)
library(stringr)
library(reshape2)
library(base64enc)
library(parallel)
library(purrr)
library(tidyr)
library(RColorBrewer)
library(Hmisc)
library(openxlsx)

# Define custom colors
gc(TRUE)
options(future.globals.maxSize = 10 * 1024^3)
options(scipen = 999)

# I. UI --------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",  
      uiOutput("headerImage"),  
      tags$span("Customs-Module", 
                style = "flex-grow: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; font-size: 15px;") 
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input", tabName = "input", icon = icon("file-excel")),
      menuItem("Simulation Parameters", icon = icon("list-alt"),
               menuSubItem("Policy Parameters", tabName = "Chapters_description", icon = icon("edit"))
      ),
      menuItem("Results", icon = icon("magnifying-glass-chart"),
               menuSubItem("Revenue Impact", tabName = "RevenueImpactSimulation", icon = icon("gauge")),
               menuSubItem("Tax Expenditures", tabName = "MainResultsTE", icon = icon("wallet"))
      ),
      menuItem("Visualizations", tabName = "CustomsDuties-charts", icon = icon("chart-simple"),
               menuSubItem("Dashboards", tabName = "Customs_Revenues", icon = icon("chart-column"))
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "input",
              fluidRow(
                column(6,
                       h4("Data Input"),
                       selectInput("inputType", "Data Source",
                                   choices = c("Manual", "Excel File"),
                                   selected = "Excel File"),
                       conditionalPanel(
                         condition = "input.inputType == 'Excel File'",
                         fileInput("fileInput", "Upload Excel File", accept = c(".xlsx")),
                         checkboxInput("hasHeader", "Header", TRUE)
                       ),
                       actionButton("importExcel", "Import Excel Data")
                )
              )
      ),
      tabItem(tabName = "Chapters_description",
              fluidRow(
                column(3,
                       sliderInput("SimulationYear", "Setting Simulation Year",
                                   min = 2023, max = 2027, step = 1, value = 2023, width = "100%", round = 0, sep = ""), 
                       uiOutput("Chapters_description"),
                       uiOutput("Chapter_Select"),
                       uiOutput("HS_codeSelect"),
                       actionButton("addValuesValue", "Add to Table", style = "float: left;"),
                       actionButton("clearValuesTable", "Clear Table", style = "float: left;")
                ),
                column(3,
                       numericInput("default_CustomsRate_MFN", "MFN", value = 0, min = 0, step = 0.01),
                       numericInput("default_CustomsRate_TR", "TR", value = 0, min = 0, step = 0.01),
                       numericInput("default_CustomsRate_CEFTA", "CEFTA", value = 0, min = 0, step = 0.01),
                       numericInput("default_CustomsRate_MSA", "MSA", value = 0, min = 0, step = 0.01),
                       #numericInput("default_Effective_Customs_rate", "Effective_Customs_rate", value = 0, min = 0, step = 0.01)
                       numericInput("default_Effective_Customs_rate", "Effective Customs Rate", value = 0, min = 0, step = 0.01)
                ),
                column(3,
                       switchInput("toggleSimulationRates", "Toggle Tax Expenditures", value = FALSE, onLabel = "On", offLabel = "Off"),
                       
                )
              ),
              div(h4("Selected Simulations Parameters"), style = "text-align: center;"),
              fluidRow(
                column(12,
                       DTOutput("customs_simulation_parameters_updated"),
                       actionButton("calc_Sim_Button", "Run Simulation", style = "float: right;"),
                       actionButton("savecustoms_simulation_parameters_updated", "Save Data", style = "float: right;")
                )
              )
      ),
      tabItem(
        tabName = "RevenueImpactSimulation",
        fluidRow(
          column(12, uiOutput("headerTitle")),  
          column(12, DTOutput("CUSTOMS_SUMMARY_TABLES"))
        )
      ),
      tabItem(
        tabName = "MainResultsTE",
        fluidRow(
          column(12, uiOutput("headerTitle_TE")),  
          column(12, DTOutput("TE_TABLES"))
        )
      ),
      tabItem(
        tabName = "Customs_Revenues",
        fluidRow(
          column(6,
                 selectInput("chartSelectCustoms_Revenues", "Select Chart",
                             choices = c("Revenue_Impact", "Tax_Expenditures_Charts"),
                             selected = "Revenue_Impact")
          )
        ),
        fluidRow(
          infoBoxOutput("infoBox1", width = 6),
          infoBoxOutput("infoBox2", width = 6)
        ),
        fluidRow(
          column(12, uiOutput("additionalCharts"))
        )
      )
    )
  )
)

# II. Server ---------------------------------------------------------------------
server <- function(input, output, session) {
  

  updateCharts <- function() {
    cat("Charts have been updated.\n")
  }
  

  observe({
    assign("SimulationYear", input$SimulationYear, envir = .GlobalEnv)
  })
  
  
  # Render the image dynamically using Base64 encoding
  output$headerImage <- renderUI({
    img_data <- base64enc::dataURI(file = "img/WB_pic.png", mime = "image/png")
    tags$img(src = img_data, height = "40px", style = "float:left; margin-right:20px;")
  })
  
 
  # Reactive data frame to store Excel data
  excelData <- reactiveVal(NULL)
  
  observeEvent(input$importExcel, {
    req(input$fileInput)
    inFile <- input$fileInput
    if (!is.null(inFile)) {
      data <- read_excel(inFile$datapath, col_names = input$hasHeader)
      if (!all(c("Chapters_description", "HS_code", "Effective_Customs_rate", "Description_Chapters", "SupplementaryUnit", "CustomsRate_MFN", "CustomsRate_CEFTA", "CustomsRate_MSA", "CustomsRate_TR") %in% colnames(data))) {
        showModal(modalDialog(
          title = "Error",
          "The Excel file must contain the columns: 'Chapters_description', 'HS_code', 'Effective_Customs_rate', 'Description_Chapters', 'SupplementaryUnit', 'CustomsRate_MFN', 'CustomsRate_CEFTA', 'CustomsRate_MSA', 'CustomsRate_TR' ",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      data <- data %>%
        mutate(
          CustomsRate_MFN = as.numeric(gsub("[^0-9.]", "", CustomsRate_MFN)),
          Year = as.numeric(gsub("[^0-9.]", "", Year))
        )
      excelData(data)
      assign("customs_simulation_parameters_raw", excelData(), envir = .GlobalEnv)
      cat("Excel data imported successfully\n")
    }
  })
  
  customs_simulation_parameters_updated <- reactiveVal(data.table(
    Chapters_description = character(),
    Description_Chapters = character(),
    HS_code = character(),
    CustomsRate_MFN = numeric(),
    CustomsRate_TR = numeric(),
    CustomsRate_CEFTA =  numeric(),
    CustomsRate_MSA = numeric(),
    Effective_Customs_rate = numeric()
  ))
  
  output$Chapters_description <- renderUI({
    if (!is.null(excelData())) {
      selectInput("Chapters_description", "Chapter Selection", choices = unique(excelData()$Chapters_description))
    } else {
      selectInput("Chapters_description", "Chapter Selection", choices = NULL)
    }
  })
  
  output$Chapter_Select <- renderUI({
    req(input$Chapters_description)
    Chapters_description <- input$Chapters_description
    if (!is.null(Chapters_description) && !is.null(excelData())) {
      selectInput("Chapter_Select", "Tariff Number Selection", choices = unique(excelData()[excelData()$Chapters_description == Chapters_description,]$Description_Chapters))
    } else {
      selectInput("Chapter_Select", "Tariff Number Selection", choices = NULL)
    }
  })
  
  output$HS_codeSelect <- renderUI({
    req(input$Chapter_Select)
    Description_Chapters <- input$Chapter_Select
    if (!is.null(Description_Chapters) && !is.null(excelData())) {
      selectInput("HS_codeSelect", "Selected Tariff line", choices = unique(excelData()[excelData()$Description_Chapters == Description_Chapters,]$HS_code))
    } else {
      selectInput("HS_codeSelect", "Selected Tariff line", choices = NULL)
    }
  })
  
  observeEvent(input$HS_codeSelect, {
    selected_class <- input$HS_codeSelect
    cat("Selected HS_code: ", selected_class, "\n")
    if (!is.null(selected_class) && !is.null(excelData())) {
      selected_row <- excelData() %>% filter(HS_code == selected_class)
      cat("Selected row:\n")
      print(selected_row)
      if (nrow(selected_row) == 1) {
        updateNumericInput(session, "default_CustomsRate_MFN", value = selected_row$CustomsRate_MFN)
        updateNumericInput(session, "default_CustomsRate_TR", value = selected_row$CustomsRate_TR)
        updateNumericInput(session, "default_CustomsRate_CEFTA", value = selected_row$CustomsRate_CEFTA)
        updateNumericInput(session, "default_CustomsRate_MSA", value = selected_row$CustomsRate_MSA)
        updateNumericInput(session, "default_Effective_Customs_rate", value = selected_row$Effective_Customs_rate)
        cat("Numeric inputs updated with selected row values\n")
      } else {
        cat("No matching row found or multiple rows returned\n")
      }
    }
  })
  
  observeEvent(input$addValuesValue, {
    req(input$HS_codeSelect)
    newEntry <- data.table(
      Chapters_description = input$Chapters_description,
      Description_Chapters = input$Chapter_Select,
      HS_code = input$HS_codeSelect,
      CustomsRate_MFN = if (input$toggleSimulationRates) input$sim_Customs_Rates else input$default_CustomsRate_MFN,
      CustomsRate_TR = if (input$toggleSimulationRates) input$SimulationCustomsRate_TR else input$default_CustomsRate_TR,
      CustomsRate_CEFTA = if (input$toggleSimulationRates) input$sim_CustomsRate_CEFTA else input$default_CustomsRate_CEFTA,
      CustomsRate_MSA = if (input$toggleSimulationRates) input$SimulationCustomsRate_MSA else input$default_CustomsRate_MSA,
      Effective_Customs_rate = if (input$toggleSimulationRates) input$Effective_Customs_rate else input$default_Effective_Customs_rate
    )
    customs_simulation_parameters_updated(rbind(customs_simulation_parameters_updated(), newEntry))
    cat("New entry added to customs_simulation_parameters_updated:\n")
    print(newEntry)
  })
  
  observeEvent(input$clearValuesTable, {
    customs_simulation_parameters_updated(data.table(
      Chapters_description = character(),
      Description_Chapters = character(),
      HS_code = character(),
      CustomsRate_MFN = numeric(),
      CustomsRate_TR = numeric(),
      CustomsRate_CEFTA =  numeric(),
      CustomsRate_MSA = numeric(),
      Effective_Customs_rate = numeric()
    ))
    cat("customs_simulation_parameters_updated table cleared\n")
  })
  
  observeEvent(input$savecustoms_simulation_parameters_updated, {
    assign("ValueTableUpdate", customs_simulation_parameters_updated(), envir = .GlobalEnv)
    cat("CIT simulation parameters saved to GlobalEnv as ValueTableUpdate\n")
    
    customs_simulation_parameters_updated_copy <- get("customs_simulation_parameters_raw", envir = .GlobalEnv)
    
    if (input$toggleSimulationRates) {
      customs_simulation_parameters_updated_copy$CustomsRate_MFN[customs_simulation_parameters_updated_copy$TaxExpenditure == "Yes"] <- input$sim_Customs_Rates
      customs_simulation_parameters_updated_copy$CustomsRate_TR[customs_simulation_parameters_updated_copy$TaxExpenditure == "Yes"] <- input$SimulationCustomsRate_TR
      customs_simulation_parameters_updated_copy$CustomsRate_CEFTA[customs_simulation_parameters_updated_copy$TaxExpenditure == "Yes"] <- input$sim_CustomsRate_CEFTA
      customs_simulation_parameters_updated_copy$CustomsRate_MSA[customs_simulation_parameters_updated_copy$TaxExpenditure == "Yes"] <- input$SimulationCustomsRate_MSA
      customs_simulation_parameters_updated_copy$Effective_Customs_rate[customs_simulation_parameters_updated_copy$TaxExpenditure == "Yes"] <- input$SimulationEffective_Customs_rate
      cat("Simulation rates updated in customs_simulation_parameters_updated_copy\n")
    }
    
    citRateData <- get("ValueTableUpdate", envir = .GlobalEnv)
    if (nrow(citRateData) > 0) {
      for (i in 1:nrow(citRateData)) {
        row <- citRateData[i, ]
        customs_simulation_parameters_updated_copy[customs_simulation_parameters_updated_copy$Chapters_description == row$Chapters_description & customs_simulation_parameters_updated_copy$Description_Chapters == row$Description_Chapters & customs_simulation_parameters_updated_copy$HS_code == row$HS_code, 
                                                   c("CustomsRate_MFN", "CustomsRate_TR","CustomsRate_CEFTA","CustomsRate_MSA","Effective_Customs_rate"
                                                   )] <- list(row$CustomsRate_MFN, row$CustomsRate_TR,
                                                              row$CustomsRate_CEFTA, row$CustomsRate_MSA,
                                                              row$Effective_Customs_rate
                                                   )
      }
    }
    
    assign("customs_simulation_parameters_updated", customs_simulation_parameters_updated_copy, envir = .GlobalEnv)
    cat("customs_simulation_parameters_updated assigned to GlobalEnv\n")
  })
  
  observe({
    toggleState("sim_Customs_Rates", input$toggleSimulationRates)
    
    if (!is.null(input$toggleSimulationRates) && length(input$toggleSimulationRates) > 0 && input$toggleSimulationRates) {
      assign("CustomsRate_MFN", input$sim_Customs_Rates, envir = .GlobalEnv)
      assign("CustomsRate_TR", input$SimulationCustomsRate_TR, envir = .GlobalEnv)
      assign("CustomsRate_CEFTA", input$sim_CustomsRate_CEFTA, envir = .GlobalEnv)
      assign("CustomsRate_MSA", input$SimulationCustomsRate_MSA, envir = .GlobalEnv)
      assign("Effective_Customs_rate", input$SimulationEffective_Customs_rate, envir = .GlobalEnv)
      cat("Simulation rates assigned to GlobalEnv\n")
    } else {
      if (exists("CustomsRate_MFN", envir = .GlobalEnv)) {
        rm("CustomsRate_MFN", envir = .GlobalEnv)
      }
      if (exists("CustomsRate_TR", envir = .GlobalEnv)) {
        rm("CustomsRate_TR", envir = .GlobalEnv)
      }
      if (exists("Regime", envir = .GlobalEnv)) {
        rm("Regime", envir = .GlobalEnv)
      }
      cat("Simulation rates removed from GlobalEnv\n")
    }
  })
  
  output$customs_simulation_parameters_updated <- renderDT({
    datatable(customs_simulation_parameters_updated(), options = list(dom = 't', paging = FALSE), editable = TRUE)
  })
  

  output$customs_simulation_parameters_updated <- renderDT({
    datatable(customs_simulation_parameters_updated(), options = list(dom = 't', paging = FALSE), editable = TRUE)
  })
  
  reactive_simulation_results <- reactiveVal()
  
  observeEvent(input$calc_Sim_Button, {
    if (nrow(customs_simulation_parameters_updated()) == 0 && is.null(excelData())) {
      showModal(modalDialog(
        title = "Error",
        "No parameters have been added to the table or imported from the Excel file. Please select parameters, add them to the table or import from Excel before running the simulation.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    showModal(modalDialog(
      title = "Running Simulation...",
      "Please wait while the simulation is running...",
      easyClose = FALSE,
      footer = NULL
    ))
    
    future({
      source("Scripts/Customs/Functions.R")
      source("Scripts/Customs/TaxCalculator.R")
      source("Scripts/Customs/Calc-Customs-Revenues.R")
      
      
      if (input$toggleSimulationRates) {
        source("Scripts/Customs/Calc-TaxExpenditures.R")
        summary_TE_SIM <- get("summary_TE_SIM", envir = .GlobalEnv)
      } else {
        summary_TE_SIM <- NULL
      }
      
      list(
        Customs_summary = get("Customs_summary", envir = .GlobalEnv),
        summary_TE_SIM = summary_TE_SIM
      )
    }) %...>% (function(results) {
      removeModal()
      showModal(modalDialog(
        title = "Success",
        "Simulation is done!",
        easyClose = TRUE,
        footer = NULL
      ))
      
      reactive_simulation_results(results)
      updateCharts()
    }) %...!% (function(e) {
      removeModal()
      showModal(modalDialog(
        title = "Error",
        paste("Error during calculation:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
 
  output$CUSTOMS_SUMMARY_TABLES <- renderDT({
    req(reactive_simulation_results())
    datatable(
      reactive_simulation_results()$Customs_summary,
      extensions = 'Buttons',
      options = list(
        pageLength = 15,
        dom = 'Blfrtip',  
        buttons = list(
          list(
            extend = 'copyHtml5',
            text = 'Copy',
            filename = 'Customs_Projections',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          ),
          list(
            extend = 'csvHtml5',
            text = 'CSV',
            filename = 'Customs_Projections',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          ),
          list(
            extend = 'print',
            text = 'Print',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          )
        ),
        autoWidth = TRUE,
        escape = FALSE,
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      ),
      rownames = FALSE
    )
  })
  

  output$headerTitle <- renderUI({
    tags$h4("Customs Projections", style = "text-align: center; font-weight: bold;")
  })
  
  
  
  output$TE_TABLES <- renderDT({
    req(input$toggleSimulationRates)
    req(reactive_simulation_results()$summary_TE_SIM)
    
    te_summary_selected <- reactive_simulation_results()$summary_TE_SIM
    
    datatable(
      te_summary_selected,
      caption = tags$caption(
        paste("Tax Expenditures in EUR MIL,", base_year),
        class = "table-caption-bold"
      ),
      extensions = 'Buttons',
      options = list(
        pageLength = 15,
        dom = 'Blfrtip',  
        buttons = list(
          list(
            extend = 'copyHtml5',
            text = 'Copy',
            filename = 'Tax_Expenditures',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          ),
          list(
            extend = 'csvHtml5',
            text = 'CSV',
            filename = 'Tax_Expenditures',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          ),
          list(
            extend = 'excelHtml5',
            text = 'Excel',
            filename = 'Tax_Expenditures',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          ),
          list(
            extend = 'print',
            text = 'Print',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          )
        ),
        autoWidth = TRUE,
        escape = FALSE,
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      ),
      rownames = FALSE
    )
  })
  
  # Move the header title outside the DataTable, so it only appears on the UI but not in exports.
  output$headerTitle_TE <- renderUI({
    tags$h4("Tax Expenditures", style = "text-align: center; font-weight: bold;")
  })
  
  
  
  
  updateCharts <- function() {
    chart_type <- isolate(input$chartSelectCustoms_Revenues)
    cat("Selected chart type:", chart_type, "\n")
    
    if (chart_type == "Tax_Expenditures_Charts" && !input$toggleSimulationRates) {
      cat("Toggle is OFF, doing nothing for Tax Expenditures Charts\n")
      return()  
    }
    
    if (chart_type == "Tax_Expenditures_Charts" && input$toggleSimulationRates) {
      cat("Preparing Tax Expenditures Charts because toggle is ON\n")
      source("Scripts/Customs/Charts-TaxExpenditures.R")
      charts_te <- Tax_Expenditures_Charts(ProjectionTE, CustomsDuties_TE_Sections, CustomsDuties_TE_MTN, CustomsDuties_TE_agg_countries_tbl,
                                           range(forecast_horizon), SimulationYear)
      
      ## new test
      
      # # Conditionally render infoBox1
      output$infoBox1 <- renderInfoBox({
        req(input$toggleSimulationRates)  
        cat("Rendering infoBox1\n")
        GDP_share_TE <- summary_TE_SIM %>%
          filter(Description == "Tax Expenditures(without FTA) as % of GDP")
        
        selected_value <- GDP_share_TE$Value 
        title_text <- "Total Tax Expenditures as PCT of GDP"
        infoBox(
          title_text,
          paste0(selected_value, " %"),
          icon = icon("hand-holding-usd"),
          color = "orange"
        )
      })
      # Conditionally render infoBox2
      output$infoBox2 <- renderInfoBox({
        req(input$toggleSimulationRates)  
        cat("Rendering infoBox2\n")
      
        
        TE_NOMINAL <- summary_TE_SIM %>%
          filter(Description == "Tax Expenditures(without FTA)")
        
        selected_value <- round(TE_NOMINAL$Value, 2)
        title_text <- "Total Tax Expenditures "
        infoBox(
          title_text,
          paste0(selected_value, " EUR MIL"),
          icon = icon("chart-pie"),
          color = "light-blue"
        )
      })
      
      
      
      ### new test
      output$additionalCharts <- renderUI({
        tagList(
          fluidRow(
            column(6, plotlyOutput("te_agg_plt", height = "400px")),
            column(6, plotlyOutput("Sections_HS", height = "400px"))
          ),
          fluidRow(
            column(6, plotlyOutput("Chapters_HS", height = "400px")),
            column(6, plotlyOutput("ProductGroups_MTN", height = "400px"))
          )
        )
      })
      
      output$te_agg_plt <- renderPlotly({ charts_te$te_agg_plt })
      output$Sections_HS <- renderPlotly({ charts_te$Sections_HS })
      output$Chapters_HS <- renderPlotly({ charts_te$Chapters_HS })
      output$ProductGroups_MTN <- renderPlotly({ charts_te$ProductGroups_MTN })
      
    } else if (chart_type == "Revenue_Impact") {
      cat("Preparing Revenue Impact charts\n")
      if (!exists("merged_Customs_BU_SIM", envir = .GlobalEnv)) {
        cat("Warning: merged_Customs_BU_SIM not found in the global environment.\n")
        return()
      }
      
      source("Scripts/Customs/Charts-Customs_Revenues.R")
      charts <- Revenue_Impact(merged_Customs_BU_SIM, TypeRev_customs_data, Chapter_customs_data_agg, MTN_customs_data_long, forecast_horizon)
      
      output$infoBox1 <- renderInfoBox({
        result1 <- merged_Customs_BU_SIM %>%
          filter(year == SimulationYear) %>%
          pull(calc_customs_duties_bu) %>%
          as.numeric()
        infoBox(
          "Baseline Customs revenues",
          value = paste(round(result1, 1), "(EUR MIL)"),
          icon = icon("coins"),
          color = "orange"
        )
      })
      
      output$infoBox2 <- renderInfoBox({
        result1 <- merged_Customs_BU_SIM %>%
          filter(year == SimulationYear) %>%
          pull(calc_customs_duties_sim) %>%
          as.numeric()
        infoBox(
          "Simulation Customs revenues",
          value = paste(round(result1, 1), "(EUR MIL)"),
          icon = icon("chart-line"),
          color = "light-blue"
        )
      })
      output$additionalCharts <- renderUI({
        tagList(
          fluidRow(
            column(6, plotlyOutput("Customs_RevenuesTotal_plt", height = "400px")),
            column(6, plotlyOutput("Sections_Customs_plt", height = "400px"))
          ),
          fluidRow(
            column(6, plotlyOutput("Chapter_Customs_plt", height = "400px")),
            column(6, plotlyOutput("MTN_Customs_plt", height = "400px"))
          )
        )
      })
      
      output$Customs_RevenuesTotal_plt <- renderPlotly({ charts$Customs_RevenuesTotal_plt })
      output$Sections_Customs_plt <- renderPlotly({ charts$Sections_Customs_plt })
      output$Chapter_Customs_plt <- renderPlotly({ charts$Chapter_Customs_plt })
      output$MTN_Customs_plt <- renderPlotly({ charts$MTN_Customs_plt })
    }
  }
  
  observeEvent(input$chartSelectCustoms_Revenues, {
    updateCharts()
  })
  

}

shinyApp(ui = ui, server = server)
