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
      tags$span("Import Taxes-Module",
                style = "flex-grow: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; font-size: 12px;") 
    )
  ),
  title = "Import Taxes Module",
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input", tabName = "input", icon = icon("file-excel")),
      menuItem("Simulation Parameters", icon = icon("list-alt"),
               menuSubItem("Policy Parameters", tabName = "Chapters_description", icon = icon("edit"))
      ),
      menuItem("Revenue Impact", icon = icon("magnifying-glass-chart"),
               menuSubItem("Customs duties", tabName = "RevenueImpactSimulation", icon = icon("bars-progress")),
               menuSubItem("Excise ", tabName = "RevenueImpactSimulation_Excise", icon = icon("square-poll-vertical")),
               menuSubItem("VAT", tabName = "RevenueImpactSimulation_VAT", icon = icon("square-poll-horizontal")),
               menuSubItem("Total Import Taxes", tabName = "RevenueImpactSimulation_TotalRevenues", icon = icon("calculator"))
      ),
               
      menuItem("Tax Expenditures", icon = icon("gauge"),  
               menuSubItem("Customs Duties", tabName = "MainResultsTE", icon = icon("wallet")),
               menuSubItem("Excise", tabName = "MainResultsTE_Excise", icon = icon("wallet"))
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
                       numericInput("default_Value", "Customs Value", value = 0, min = 0, step = 0.01),
                       numericInput("default_Quantity", "Quantity", value = 0, min = 0, step = 0.01),
                       numericInput("default_Netweight", "Netweight", value = 0, min = 0, step = 0.01),
                       uiOutput("default_SupplementaryUnit")
                ),
                column(3,
                       numericInput("default_CustomsRate_MFN", "MFN", value = 0, min = 0, step = 0.01),
                       numericInput("default_CustomsRate_TR", "TR", value = 0, min = 0, step = 0.01),
                       numericInput("default_CustomsRate_CEFTA", "CEFTA", value = 0, min = 0, step = 0.01),
                       numericInput("default_CustomsRate_MSA", "MSA", value = 0, min = 0, step = 0.01),
                       numericInput("default_ExciseRate", "Excise", value = 0, min = 0, step = 0.01),
                       numericInput("default_VAT", "VAT", value = 0, min = 0, step = 0.01)
                ),
                  column(3,
                  numericInput("default_Effective_Customs_rate", "ECR (Effective Customs Rate)", value = 0, min = 0, step = 0.01),
                  numericInput("default_Effective_Excise_rate", "EER (Effective Excise Rate)", value = 0, min = 0, step = 0.01),
                  numericInput("default_Effective_VAT_rate", "EVR (Effective VAT Rate)", value = 0, min = 0, step = 0.01)
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
         # column(12, uiOutput(" Customs Projections")), 
         
          column(12, DTOutput("CUSTOMS_SUMMARY_TABLES"))
        )
      ),
      tabItem(
        tabName = "RevenueImpactSimulation_Excise",
        fluidRow(
          column(12, uiOutput("headerTitle")),  
          column(12, DTOutput("EXCISE_SUMMARY_TABLES"))
        )
      ),
      tabItem(
        tabName = "RevenueImpactSimulation_VAT",
        fluidRow(
          column(12, uiOutput("headerTitle")),  
          column(12, DTOutput("VAT_SUMMARY_TABLES"))
        )
      ),
      tabItem(
        tabName = "RevenueImpactSimulation_TotalRevenues",
        fluidRow(
          column(12, uiOutput("headerTitle")),  
          column(12, DTOutput("TOTAL_REVENUES_SUMMARY_TABLES"))
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
        tabName = "MainResultsTE_Excise",
        fluidRow(
          column(12, uiOutput("headerTitle_TE")),  
          column(12, DTOutput("TE_TABLES_Excise"))
        )
      ),
      tabItem(
        tabName = "Customs_Revenues",
        fluidRow(
          column(6,
                 selectInput("chartSelectCustoms_Revenues", "Select Chart",
                             choices = c("Revenue_Impact_Customs", "Revenue_Impact_Excise","Revenue_Impact_VAT","Revenue_import_tax_total",
                                         "Tax_Expenditures_Charts_Customs","Tax_Expenditures_Charts_Excise"),
                             selected = "Revenue_Impact_Customs")
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

  output$default_SupplementaryUnit <- renderUI({
    req(input$HS_codeSelect)  # Ensure HS_codeSelect is selected
    selected_row <- excelData() %>% filter(HS_code == input$HS_codeSelect)
    if (nrow(selected_row) == 1) {
      textInput(
        "default_SupplementaryUnit",
        "Supplementary Unit",
        value = as.character(selected_row$SupplementaryUnit),
        width = "100%"
      )
    } else {
      textInput(
        "default_SupplementaryUnit",
        "Supplementary Unit",
        value = "NA",
        width = "100%"
      )
    }
  })
  
  shinyjs::disable("default_Value")
  shinyjs::disable("default_Quantity")
  shinyjs::disable("default_Netweight")
  shinyjs::disable("default_SupplementaryUnit")
  shinyjs::disable("default_CustomsRate_MFN")
  shinyjs::disable("default_CustomsRate_TR")
  shinyjs::disable("default_CustomsRate_CEFTA")
  shinyjs::disable("default_CustomsRate_MSA")
  shinyjs::disable("default_ExciseRate")
  shinyjs::disable("default_VAT")
  
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
      # Read the Excel file
      data <- read_excel(inFile$datapath, col_names = input$hasHeader)
      
      # Ensure proper data types without breaking dropdowns
      data <- data %>%
        mutate(
          Chapters_description = as.character(Chapters_description),
          HS_code = as.character(HS_code),
          Description_Chapters = as.character(Description_Chapters),
          SupplementaryUnit = as.character(SupplementaryUnit),
          Effective_Customs_rate = as.numeric(Effective_Customs_rate),
          CustomsRate_MFN = as.numeric(CustomsRate_MFN),
          CustomsRate_CEFTA = as.numeric(CustomsRate_CEFTA),
          CustomsRate_MSA = as.numeric(CustomsRate_MSA),
          CustomsRate_TR = as.numeric(CustomsRate_TR),
          ExciseRate = as.numeric(ExciseRate),
          VAT_Rate = as.numeric(VAT_Rate)
        )
      
      # Validate the required columns
      if (!all(c("Chapters_description", "HS_code", "Effective_Customs_rate", 
                 "Description_Chapters", "SupplementaryUnit", "CustomsRate_MFN", 
                 "CustomsRate_CEFTA", "CustomsRate_MSA", "CustomsRate_TR", 
                 "ExciseRate", "VAT_Rate", "Effective_Excise_rate", 
                 "Effective_VAT_rate") %in% colnames(data))) {
        showModal(modalDialog(
          title = "Error",
          "The Excel file must contain all required columns.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      
      # Update the reactive data
      excelData(data)
      assign("customs_simulation_parameters_raw", excelData(), envir = .GlobalEnv)
      cat("Excel data imported and cleaned successfully\n")
    }
  })
  
 customs_simulation_parameters_updated <- reactiveVal(data.table(
                                                                Chapters_description = character(),
                                                                Description_Chapters = character(),
                                                                HS_code = character(),
                                                                Effective_Customs_rate = numeric(),
                                                                Effective_Excise_rate = numeric(),
                                                                Effective_VAT_rate = numeric()
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
        # Update relevant numeric inputs with values from the selected row
        updateNumericInput(session, "default_Value", value = selected_row$Value)
        updateNumericInput(session, "default_Quantity", value = selected_row$Quantity)
        updateNumericInput(session, "default_Netweight", value = selected_row$Netweight)
        updateNumericInput(session, "default_CustomsRate_MFN", value = selected_row$CustomsRate_MFN)
        updateNumericInput(session, "default_CustomsRate_TR", value = selected_row$CustomsRate_TR)
        updateNumericInput(session, "default_CustomsRate_CEFTA", value = selected_row$CustomsRate_CEFTA)
        updateNumericInput(session, "default_CustomsRate_MSA", value = selected_row$CustomsRate_MSA)
        updateNumericInput(session, "default_ExciseRate", value = selected_row$ExciseRate)
        updateNumericInput(session, "default_VAT", value = selected_row$VAT_Rate)
        updateNumericInput(session, "default_Effective_Customs_rate", value = selected_row$Effective_Customs_rate)
        updateNumericInput(session, "default_Effective_Excise_rate", value = selected_row$Effective_Excise_rate)
        updateNumericInput(session, "default_Effective_VAT_rate", value = selected_row$Effective_VAT_rate)
        
        cat("Numeric inputs updated with selected row values\n")
      } else {
        cat("No matching row found or multiple rows returned\n")
      }
    }
  })
  

  observeEvent(input$addValuesValue, {
    req(input$HS_codeSelect)  # Ensure an HS_code is selected
    
    # Create a new entry with the required fields
    newEntry <- data.table(
      Chapters_description = input$Chapters_description,
      Description_Chapters = input$Chapter_Select,
      HS_code = input$HS_codeSelect,
      Effective_Customs_rate = if (input$toggleSimulationRates) input$Effective_Customs_rate else input$default_Effective_Customs_rate,
      Effective_Excise_rate = if (input$toggleSimulationRates) input$Effective_Excise_rate else input$default_Effective_Excise_rate,
      Effective_VAT_rate = if (input$toggleSimulationRates) input$Effective_VAT_rate else input$default_Effective_VAT_rate
    )
    
    # Ensure column alignment with the existing table
    updatedTable <- customs_simulation_parameters_updated()
    
    # Ensure new entry has the same columns as the existing table
    # Add missing columns with NA values
    for (col in setdiff(names(updatedTable), names(newEntry))) {
      newEntry[[col]] <- NA
    }
    
    # Drop extra columns in newEntry
    newEntry <- newEntry[, names(updatedTable), with = FALSE]
    
    # Verify final column alignment
    if (!identical(names(newEntry), names(updatedTable))) {
      stop("Mismatch in columns: new entry has different columns than the existing table.")
    }
    
    # Append the new entry to the existing table
    customs_simulation_parameters_updated(rbind(updatedTable, newEntry, fill = TRUE))
    
    # Debugging output
    cat("New entry added to customs_simulation_parameters_updated:\n")
    print(newEntry)
  })

  observeEvent(input$clearValuesTable, {
    customs_simulation_parameters_updated(data.table(
      Chapters_description = character(),
      Description_Chapters = character(),
      HS_code = character(),
      Effective_Customs_rate = numeric(),
      Effective_Excise_rate =  numeric(),
      Effective_VAT_rate = numeric()
    ))
    cat("customs_simulation_parameters_updated table cleared\n")
  })
  
  observeEvent(input$savecustoms_simulation_parameters_updated, {
    assign("ValueTableUpdate", customs_simulation_parameters_updated(), envir = .GlobalEnv)
    cat("Customs simulation parameters saved to GlobalEnv as ValueTableUpdate\n")
    
    customs_simulation_parameters_updated_copy <- get("customs_simulation_parameters_raw", envir = .GlobalEnv)
    
    if (input$toggleSimulationRates) {
      customs_simulation_parameters_updated_copy$Effective_Customs_rate[customs_simulation_parameters_updated_copy$TaxExpenditure == "Yes"] <- input$SimulationEffective_Customs_rate
      customs_simulation_parameters_updated_copy$Effective_Excise_rate[customs_simulation_parameters_updated_copy$TaxExpenditure == "Yes"] <- input$SimulationEffective_Customs_rate
      customs_simulation_parameters_updated_copy$Effective_VAT_rate[customs_simulation_parameters_updated_copy$TaxExpenditure == "Yes"] <- input$SimulationEffective_VAT_rate
      cat("Simulation rates updated in customs_simulation_parameters_updated_copy\n")
    }
    
    citRateData <- get("ValueTableUpdate", envir = .GlobalEnv)
    if (nrow(citRateData) > 0) {
      for (i in 1:nrow(citRateData)) {
        row <- citRateData[i, ]
        customs_simulation_parameters_updated_copy[customs_simulation_parameters_updated_copy$Chapters_description == row$Chapters_description & customs_simulation_parameters_updated_copy$Description_Chapters == row$Description_Chapters & customs_simulation_parameters_updated_copy$HS_code == row$HS_code, 
                                                   c(
                                                     #"CustomsRate_MFN", "CustomsRate_TR","CustomsRate_CEFTA","CustomsRate_MSA","ExciseRate","VAT_Rate","Value","Quantity","Netweight"
                                                     "Effective_Customs_rate","Effective_Excise_rate","Effective_VAT_rate"
                                                   )] <- list(
                                                              row$Effective_Customs_rate,
                                                              row$Effective_Excise_rate,
                                                              row$Effective_VAT_rate
                                                   )
      }
    }
    
    assign("customs_simulation_parameters_updated", customs_simulation_parameters_updated_copy, envir = .GlobalEnv)
    cat("customs_simulation_parameters_updated assigned to GlobalEnv\n")
  })
  
  observe({
    toggleState("sim_Customs_Rates", input$toggleSimulationRates)
    
    if (!is.null(input$toggleSimulationRates) && length(input$toggleSimulationRates) > 0 && input$toggleSimulationRates) {
      assign("Effective_Customs_rate", input$SimulationEffective_Customs_rate, envir = .GlobalEnv)
      assign("Effective_Excise_rate", input$SimulationEffective_Excise_rate, envir = .GlobalEnv)
      assign("Effective_VAT_rate", input$SimulationEffective_VAT_rate, envir = .GlobalEnv)
      cat("Simulation rates assigned to GlobalEnv\n")
    } else {
      if (exists("Effective_Customs_rate", envir = .GlobalEnv)) {
        rm("Effective_Customs_rate", envir = .GlobalEnv)
      }
      if (exists("Effective_Excise_rate", envir = .GlobalEnv)) {
        rm("Effective_Excise_rate", envir = .GlobalEnv)
      }
      if (exists("Effective_VAT_rate", envir = .GlobalEnv)) {
        rm("Effective_VAT_rate", envir = .GlobalEnv)
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
      # source("Scripts/Customs/Functions.R")
      # source("Scripts/Customs/TaxCalculator.R")
      # source("Scripts/Customs/Calc-Customs-Revenues.R")
      # source("Scripts/Customs/Calc-Excise-Revenues.R")
      # source("Scripts/Customs/Calc-VAT-Revenues.R")
      # source("Scripts/Customs/Calc-Import-Duties.R")
      
      source(paste0(path1, "/Scripts/Customs/Functions.R"))
      source(paste0(path1, "/Scripts/Customs/TaxCalculator.R"))
      source(paste0(path1, "/Scripts/Customs/Calc-Customs-Revenues.R"))
      source(paste0(path1, "/Scripts/Customs/Calc-Excise-Revenues.R"))
      source(paste0(path1, "/Scripts/Customs/Calc-VAT-Revenues.R"))
      source(paste0(path1, "/Scripts/Customs/Calc-Import-Duties.R"))
      
      
      if (input$toggleSimulationRates) {
        #source("Scripts/Customs/Calc-TaxExpenditures.R")
        source(paste0(path1, "/Scripts/Customs/Calc-TaxExpenditures.R"))
        
        summary_TE_SIM <- get("summary_TE_SIM", envir = .GlobalEnv)
        summary_TE_SIM_Excise <- get("summary_TE_SIM_Excise", envir = .GlobalEnv)
      } else {
        summary_TE_SIM <- NULL
        summary_TE_SIM_Excise<-NULL
      }
  # Declaring re-active tables for GUI
      list(
        Customs_summary = get("Customs_summary", envir = .GlobalEnv),
        Excise_summary = get("Excise_summary", envir = .GlobalEnv),
        VAT_summary = get("VAT_summary", envir = .GlobalEnv),
        Total_import_summary = get("Total_import_summary", envir = .GlobalEnv),
        summary_TE_SIM = summary_TE_SIM,
        summary_TE_SIM_Excise = summary_TE_SIM_Excise
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
  
 # Tables
 # Table Customs summary
  output$CUSTOMS_SUMMARY_TABLES <- renderDT({
    req(reactive_simulation_results())
    datatable(
      reactive_simulation_results()$Customs_summary,
      caption = tags$caption(paste("Customs Duties Projections,", min(forecast_horizon), "-", max(forecast_horizon)), class = "table-caption-bold"),
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
  
  # output$headerTitle <- renderUI({
  #   tags$h4("Customs Projections", style = "text-align: center; font-weight: bold;")
  # })

  

  
  # Excise Summary
  output$EXCISE_SUMMARY_TABLES <- renderDT({
    req(reactive_simulation_results())
    datatable(
      reactive_simulation_results()$Excise_summary,
      caption = tags$caption(paste("Excise Projections,", min(forecast_horizon), "-", max(forecast_horizon)), class = "table-caption-bold"),
      extensions = 'Buttons',
      options = list(
        pageLength = 15,
        dom = 'Blfrtip',  
        buttons = list(
          list(
            extend = 'copyHtml5',
            text = 'Copy',
            filename = 'Excise_Projections',
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
            filename = 'Excise_Projections',
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
  
  
  # output$headerTitle <- renderUI({
  #   tags$h4("Excise Projections", style = "text-align: center; font-weight: bold;")
  # })
  
  # IMPORT VAT
  
  # IMPORT VAT Summary
  output$VAT_SUMMARY_TABLES <- renderDT({
    req(reactive_simulation_results())
    datatable(
      reactive_simulation_results()$VAT_summary,
      caption = tags$caption(paste("Import VAT Projections,", min(forecast_horizon), "-", max(forecast_horizon)), class = "table-caption-bold"),
      extensions = 'Buttons',
      options = list(
        pageLength = 15,
        dom = 'Blfrtip',  
        buttons = list(
          list(
            extend = 'copyHtml5',
            text = 'Copy',
            filename = 'Import_VAT_Projections',
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
            filename = 'Import_VAT_Projections',
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
  
  
  # output$headerTitle <- renderUI({
  #   tags$h4("Import VAT Projections", style = "text-align: center; font-weight: bold;")
  # })
  # Total Import Revenues
  # IMPORT VAT Summary
  
  
  
  output$TOTAL_REVENUES_SUMMARY_TABLES <- renderDT({
    req(reactive_simulation_results())
    datatable(
      reactive_simulation_results()$Total_import_summary,
      caption = tags$caption(paste("Total Import Taxes,", min(forecast_horizon), "-", max(forecast_horizon)), class = "table-caption-bold"),
      extensions = 'Buttons',
      options = list(
        pageLength = 15,
        dom = 'Blfrtip',  
        buttons = list(
          list(
            extend = 'copyHtml5',
            text = 'Copy',
            filename = 'Total_Revenues_Projections',
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
            filename = 'Total_Revenues_Projections',
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
  
  
  # output$headerTitle <- renderUI({
  #   tags$h4("Total Import Duties Projections", style = "text-align: center; font-weight: bold;")
  # })
  # 
  
  
  
  # TE Table
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
  # output$headerTitle_TE <- renderUI({
  #   tags$h4("Tax Expenditures", style = "text-align: center; font-weight: bold;")
  # })

  # TE EXCISE
  # TE Table
  output$TE_TABLES_Excise <- renderDT({
    req(input$toggleSimulationRates)
    req(reactive_simulation_results()$summary_TE_SIM_Excise)
    
    te_summary_selected <- reactive_simulation_results()$summary_TE_SIM_Excise
    
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
            filename = 'Tax_Expenditures_Excise',
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
            filename = 'Tax_Expenditures_Excise',
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
  # output$headerTitle_TE <- renderUI({
  #   tags$h4("Tax Expenditures Excise", style = "text-align: center; font-weight: bold;")
  # })

  
  
  # Dashboards&Charts
        updateCharts <- function() {
        chart_type <- isolate(input$chartSelectCustoms_Revenues)
        cat("Selected chart type:", chart_type, "\n")
        
        if (chart_type == "Tax_Expenditures_Charts_Customs" && !input$toggleSimulationRates) {
          cat("Toggle is OFF, doing nothing for Tax Expenditures Charts\n")
          return()
        } else if (chart_type == "Tax_Expenditures_Charts_Customs" && input$toggleSimulationRates) {
          cat("Preparing Tax Expenditures Charts because toggle is ON\n")
          #source("Scripts/Customs/Charts-TaxExpenditures_Customs.R")
          source(paste0(path1, "/Scripts/Customs/Charts-TaxExpenditures_Customs.R"))
          
          charts_te <- TE_Charts_Customs_fun(
            ProjectionTE_customs,CustomsDuties_TE_agg_countries_tbl_agg,
            CustomsDuties_TE_Chapters,CustomsDuties_TE_MTN,
            range(forecast_horizon), 
            SimulationYear
          )
          
          output$infoBox1 <- renderInfoBox({
            req(input$toggleSimulationRates)
            GDP_share_TE <- summary_TE_SIM %>%
              filter(Description == "Tax Expenditures(without FTA) as % of GDP")
            selected_value <- GDP_share_TE$Value
            infoBox(
              "Total Tax Expenditures as PCT of GDP",
              paste0(selected_value, " %"),
              icon = icon("hand-holding-usd"),
              color = "orange"
            )
          })
          
          output$infoBox2 <- renderInfoBox({
            req(input$toggleSimulationRates)
            TE_NOMINAL <- summary_TE_SIM %>%
              filter(Description == "Tax Expenditures(without FTA)")
            selected_value <- round(TE_NOMINAL$Value, 2)
            infoBox(
              "Total Tax Expenditures",
              paste0(selected_value, " EUR MIL"),
              icon = icon("chart-pie"),
              color = "light-blue"
            )
          })
          
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
        } 
        # Ovde test
        else if (chart_type == "Tax_Expenditures_Charts_Excise" && input$toggleSimulationRates) {
          cat("Preparing Tax Expenditures Charts because toggle is ON\n")
          #source("Scripts/Customs/Charts-TaxExpenditures_Excise.R")
          source(paste0(path1, "/Scripts/Customs/Charts-TaxExpenditures_Excise.R"))
          charts_te <- TE_Charts_Excise_fun(
            ProjectionTE_Excise,summary_TE_SIM_Excise,Legal_Fuels_Cars_Products,
            range(forecast_horizon), 
            #forecast_horizon,
            SimulationYear
          )
          
          output$infoBox1 <- renderInfoBox({
            req(input$toggleSimulationRates)
            GDP_share_TE <- summary_TE_SIM_Excise %>%
              filter(Description == "Tax Expenditures as % of GDP")
            selected_value <- GDP_share_TE$value
            infoBox(
              "Total Tax Expenditures as PCT of GDP",
              paste0(selected_value, " %"),
              icon = icon("hand-holding-usd"),
              color = "orange"
            )
          })
          
          output$infoBox2 <- renderInfoBox({
            req(input$toggleSimulationRates)
            TE_NOMINAL <- summary_TE_SIM_Excise %>%
              filter(Description == "Tax Expenditures")
            selected_value <- round(TE_NOMINAL$value, 2)
            infoBox(
              "Total Tax Expenditures",
              paste0(selected_value, " EUR MIL"),
              icon = icon("chart-pie"),
              color = "light-blue"
            )
          })
          
          output$additionalCharts <- renderUI({
            tagList(
              fluidRow(
                column(6, plotlyOutput("te_agg_excise_plt", height = "400px")),
                column(6, plotlyOutput("TE_excise_product_plt", height = "400px"))
              ),
              fluidRow(
                column(6, plotlyOutput("TE_excise_product_plt1", height = "400px")),
                column(6, plotlyOutput("TE_excise_pie_plt", height = "400px"))
              )
            )
          })
          
          output$te_agg_excise_plt <- renderPlotly({ charts_te$te_agg_excise_plt })
          output$TE_excise_product_plt <- renderPlotly({ charts_te$TE_excise_product_plt })
          output$TE_excise_product_plt1 <- renderPlotly({ charts_te$TE_excise_product_plt1 })
          output$TE_excise_pie_plt <- renderPlotly({ charts_te$TE_excise_pie_plt })
        }
        
        # End test
        else if (chart_type == "Revenue_Impact_Customs") {
          cat("Preparing Revenue Impact charts\n")
          if (!exists("merged_Customs_BU_SIM", envir = .GlobalEnv)) {
            cat("Warning: merged_Customs_BU_SIM not found in the global environment.\n")
            return()
          }
          # source("Scripts/Customs/Charts-Customs_Revenues.R")
          source(paste0(path1, "/Scripts/Customs/Charts-Customs_Revenues.R"))
          charts <- Revenue_Impact(
            merged_Customs_BU_SIM, 
            TypeRev_customs_data, 
            Chapter_customs_data_agg, 
            MTN_customs_data_long, 
            forecast_horizon
          )
          
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
        else if (chart_type == "Revenue_Impact_Excise") {
          cat("Preparing Revenue Impact charts\n")
          if (!exists("merged_Customs_BU_SIM", envir = .GlobalEnv)) {
            cat("Warning: merged_Customs_BU_SIM not found in the global environment.\n")
            return()
          }
          
          #source("Scripts/Customs/Charts-Excise_Revenues.R")
          source(paste0(path1, "/Scripts/Customs/Charts-Excise_Revenues.R"))
          charts <- Revenue_Impact_Excise_fun(
            merged_Customs_BU_SIM,
            TypeRev_Excise_data,
            Chapter_Excise_data_agg,
            MTN_Excise_data_agg,
            forecast_horizon
          )
          
          output$infoBox1 <- renderInfoBox({
            result1 <- merged_Customs_BU_SIM %>%
              filter(year == SimulationYear) %>%
              pull(calc_excise_bu) %>%
              as.numeric()
            infoBox(
              "Baseline Excise revenues",
              value = paste(round(result1, 1), "(EUR MIL)"),
              icon = icon("coins"),
              color = "orange"
            )
          })
          
          output$infoBox2 <- renderInfoBox({
            result1 <- merged_Customs_BU_SIM %>%
              filter(year == SimulationYear) %>%
              pull(calc_excise_sim) %>%
              as.numeric()
            infoBox(
              "Simulation Excise revenues",
              value = paste(round(result1, 1), "(EUR MIL)"),
              icon = icon("chart-line"),
              color = "light-blue"
            )
          })
          
          output$additionalCharts <- renderUI({
            tagList(
              fluidRow(
                column(6, plotlyOutput("Excise_RevenuesTotal_plt", height = "400px")),
                column(6, plotlyOutput("Sections_Excise_plt", height = "400px"))
              ),
              fluidRow(
                column(6, plotlyOutput("Chapter_Excise_plt", height = "400px")),
                column(6, plotlyOutput("MTN_Excise_plt", height = "400px"))
              )
            )
          })
          
          output$Excise_RevenuesTotal_plt <- renderPlotly({ charts$Excise_RevenuesTotal_plt })
          output$Sections_Excise_plt <- renderPlotly({ charts$Sections_Excise_plt })
          output$Chapter_Excise_plt <- renderPlotly({ charts$Chapter_Excise_plt })
          output$MTN_Excise_plt <- renderPlotly({ charts$MTN_Excise_plt })
        } 
        else if (chart_type == "Revenue_import_tax_total") {
          cat("Preparing Revenue Impact charts\n")
          if (!exists("merged_Customs_BU_SIM", envir = .GlobalEnv)) {
            cat("Warning: merged_Customs_BU_SIM not found in the global environment.\n")
            return()
          }
          
          #source("Scripts/Customs/Charts-Import_Duties.R")
          source(paste0(path1, "/Scripts/Customs/Charts-Import_Duties.R"))
          charts <- Total_Import_duties_fun(
            merged_Customs_BU_SIM,
            TypeRev_import_duties_data,
            Chapter_import_duties_agg,
            MTN_import_duties_agg,
            forecast_horizon
          )
          
          output$infoBox1 <- renderInfoBox({
            result1 <- merged_Customs_BU_SIM %>%
              filter(year == SimulationYear) %>%
              summarise(total_sum = sum(calc_customs_duties_bu + calc_excise_bu + calc_vat_bu, na.rm = TRUE)) %>%
              pull(total_sum)%>%
              #pull(calc_excise_bu) %>%
              as.numeric()
            infoBox(
              "Baseline Total Import Tax Revenue",
              value = paste(round(result1, 1), "(EUR MIL)"),
              icon = icon("coins"),
              color = "orange"
            )
          })
          
          output$infoBox2 <- renderInfoBox({
            result1 <- merged_Customs_BU_SIM %>%
              filter(year == SimulationYear) %>%
              summarise(total_sum = sum(calc_customs_duties_sim + calc_excise_sim + calc_vat_sim, na.rm = TRUE)) %>%
              pull(total_sum)%>%
              #pull(calc_excise_sim) %>%
              as.numeric()
            infoBox(
              "Simulation Total Import Tax Revenue",
              value = paste(round(result1, 1), "(EUR MIL)"),
              icon = icon("chart-line"),
              color = "light-blue"
            )
          })
          
          output$additionalCharts <- renderUI({
            tagList(
              fluidRow(
                column(6, plotlyOutput("Import_duties_plt", height = "400px")),
                column(6, plotlyOutput("Sections_import_duties_plt", height = "400px"))
              ),
              fluidRow(
                column(6, plotlyOutput("Chapter_import_duties_plt", height = "400px")),
                column(6, plotlyOutput("MTN_import_duties_plt", height = "400px"))
              )
            )
          })
          
          output$Import_duties_plt <- renderPlotly({ charts$Import_duties_plt })
          output$Sections_import_duties_plt <- renderPlotly({ charts$Sections_import_duties_plt })
          output$Chapter_import_duties_plt <- renderPlotly({ charts$Chapter_import_duties_plt })
          output$MTN_import_duties_plt <- renderPlotly({ charts$MTN_import_duties_plt })
        } 
        else if (chart_type == "Revenue_Impact_VAT") {
          cat("Preparing Revenue Impact VAT charts\n")
          
          if (!exists("merged_Customs_BU_SIM", envir = .GlobalEnv)) {
            cat("Warning: merged_Customs_BU_SIM not found in the global environment.\n")
            return()
          }
          
          #source("Scripts/Customs/Charts-VAT_Revenues.R")
          source(paste0(path1, "/Scripts/Customs/Charts-VAT_Revenues.R"))
          
          
          charts <- Revenue_Impact_VAT_fun(
            merged_Customs_BU_SIM, 
            TypeRev_VAT_data, 
            Chapter_VAT_data_agg, 
            MTN_VAT_data_agg, 
            forecast_horizon
          )
          
          output$infoBox1 <- renderInfoBox({
            result1 <- merged_Customs_BU_SIM %>%
              filter(year == SimulationYear) %>%
              pull(calc_vat_bu) %>%
              as.numeric()
            infoBox(
              "Baseline import VAT revenues",
              value = paste(round(result1, 1), "(EUR MIL)"),
              icon = icon("coins"),
              color = "orange"
            )
          })
          
          output$infoBox2 <- renderInfoBox({
            result1 <- merged_Customs_BU_SIM %>%
              filter(year == SimulationYear) %>%
              pull(calc_vat_sim) %>%
              as.numeric()
            infoBox(
              "Simulation import VAT revenues",
              value = paste(round(result1, 1), "(EUR MIL)"),
              icon = icon("chart-line"),
              color = "light-blue"
            )
          })
          
          output$additionalCharts <- renderUI({
            tagList(
              fluidRow(
                column(6, plotlyOutput("VAT_RevenuesTotal_plt", height = "400px")),
                column(6, plotlyOutput("Sections_VAT_plt", height = "400px"))
              ),
              fluidRow(
                column(6, plotlyOutput("Chapter_VAT_plt", height = "400px")),
                column(6, plotlyOutput("MTN_VAT_plt", height = "400px"))
              )
            )
          })
          
          output$VAT_RevenuesTotal_plt <- renderPlotly({ charts$VAT_RevenuesTotal_plt })
          output$Sections_VAT_plt <- renderPlotly({ charts$Sections_VAT_plt })
          output$Chapter_VAT_plt <- renderPlotly({ charts$Chapter_VAT_plt })
          output$MTN_VAT_plt <- renderPlotly({ charts$MTN_VAT_plt })
        }
      }
    
      
      observeEvent(input$chartSelectCustoms_Revenues, {
        updateCharts()
      })
      
      }


shinyApp(ui = ui, server = server)