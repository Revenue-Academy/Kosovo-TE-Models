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
library(kableExtra)
library(stringr)
library(reshape2)
library(base64enc)
library(sm)
library(ks)
library(ineq)
library(IC2)
library(rccmisc)


options(scipen = 999)

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",  
      uiOutput("headerImage"),  
      tags$span("VAT-Module", 
                style = "flex-grow: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; font-size: 18px;") 
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input", tabName = "input", icon = icon("file-excel")),
      menuItem("Simulation", icon = icon("list-alt"),
               menuSubItem("Policy Parameters", tabName = "SettingTaxableProp_SIM", icon = icon("edit"))
      ),
      menuItem("Revenue Impact", icon = icon("magnifying-glass-chart"),
               menuSubItem("VAT Revenues", tabName = "RevenueImpactSimulation", icon = icon("bars-progress"))
      ),
      menuItem("Tax Expenditures", icon = icon("chart-line"),
               menuSubItem("Tax Expenditures", tabName = "TaxExpenditures", icon = icon("wallet"))
      ),
      menuItem("Visualizations", tabName = "VAT_charts", icon = icon("chart-simple"),
               menuSubItem("Dashboards", tabName = "VAT_Revenues", icon = icon("chart-column"))
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
# Input tab
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
      # Taxable Proportion Fine Tuning - Simulation
      tabItem(tabName = "SettingTaxableProp_SIM",
              #h4("Setting Simulation Parameters"),
              fluidRow(
                column(3,
                       sliderInput("SimulationYear", "Setting Simulation Year",
                                   min = 2022, max = 2027, step = 1, value = 2022, width = "100%", round = 0, sep = ""), 
                       sliderInput("benchmark_tax_rate", "VAT Benchmark Rate",
                                   min = 0, max = 1, step = 0.01, value = 0.18, width = "100%", round = 3, sep = ""), 
                       uiOutput("productIndustryNameSelect_SIM"),
                       uiOutput("productIndustryCodeSelect_SIM"),
                       actionButton("addVATRateValue_CPA_SIM", "Add to Table", style = "float: left;"),
                       actionButton("clearVATRateTable_CPA_SIM", "Clear Table", style = "float: left;")
                ),
                column(3,
                       numericInput("preferentialVATRate1_CPA_SIM", "Preferential VAT Rate 1", value = 0, min = 0, max = 1, step = 0.01),
                      # numericInput("preferentialVATRate2_CPA_SIM", "Preferential VAT Rate 2", value = 0, min = 0, max = 1, step = 0.01),
                       numericInput("standardVATRate_CPA_SIM", "Standard VAT Rate", value = 0, min = 0, max = 1, step = 0.01),
                #),
                       
                       # Radio button 
                       radioButtons(
                         "tax_category", 
                         "Include HBS data:",
                         choices = list("On" = "On", "Off" = "Off"),
                         selected = "On"
                       ),
                       tags$script(HTML("shinyjs.disable('tax_category')")), # Permanently disable the input
              ),
                
                column(3,
                       numericInput("ProportionExempted_CPA_SIM", "Proportion Exempted", value = 0, min = 0, max = 1, step = 0.01),
                       numericInput("ProportionPreferentialRate1_CPA_SIM", "Proportion Preferential Rate 1", value = 0, min = 0, max = 1, step = 0.01),
                      # numericInput("ProportionPreferentialRate2_CPA_SIM", "Proportion Preferential Rate 2", value = 0, min = 0, max = 1, step = 0.01),
                       numericInput("ProportionStandardRate_CPA_SIM", "Proportion Standard Rate", value = 0, min = 0, max = 1, step = 0.01)
                ),
                column(3,
                       switchInput("toggleSimulationRates_CPA_SIM", "Toggle Simulation Rates", value = FALSE, onLabel = "On", offLabel = "Off"),
                       numericInput("simPreferentialRate1_CPA_SIM", "Simulation Preferential Rate 1", value = 0.08, min = 0, max = 1, step = 0.01),
                       #numericInput("simPreferentialRate2_CPA_SIM", "Simulation Preferential Rate 2", value = 0.10, min = 0, max = 1, step = 0.01),
                       numericInput("simStandardRate_CPA_SIM", "Simulation Standard Rate", value = 0.18, min = 0, max = 1, step = 0.01)
                )
              ),
              div(h4("Selected Simulations Parameters"), style = "text-align: center;"),
              fluidRow(
                column(12,
                       DTOutput("vatRateTableUpdate_CPA_SIM"),
                       actionButton("calc_Sim_Button", "Run Simulation", style = "float: right;"),
                       actionButton("savevatRateTableUpdate_CPA_SIM", "Save Table", style = "float: right;")
                )
              )
      ),
      tabItem(
        tabName = "RevenueImpactSimulation",
        fluidRow(
          column(12, uiOutput("headerTitle")),  
          column(12, DTOutput("VAT_SUMMARY_TABLES"))
        )
      ),
      tabItem(
        tabName = "TaxExpenditures",
        fluidRow(
          column(12, uiOutput("headerTitle")),  
          column(12, DTOutput("TE_SUMMARY_TABLES"))
        )
      ),
      tabItem(tabName = "VAT_Revenues",
              fluidRow(
                column(6,
                       selectInput("chartSelectVAT_Revenues", "Select Chart",
                                   choices = c(
                                               "Fiscal Impact",
                                               "Sectoral Breakdown of VAT Revenues",
                                               "Tax Expenditures",
                                               "VAT-GAP Metrics"
                                   ),
                                   selected = "Fiscal Impact")
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

# II. Define the main server function-------------------------------------------

mainServer <- function(input, output, session) {
  
  
  # Render the image dynamically using Base64 encoding
  output$headerImage <- renderUI({
    img_data <- base64enc::dataURI(file = "img/WB_pic.png", mime = "image/png")
    tags$img(src = img_data, height = "40px", style = "float:left; margin-right:20px;")
  })
  
  
  observeEvent(input$chartSelectVAT_Revenues, {
    cat("Dropdown selection changed: Triggering chart updates...\n")
    updateCharts()
  })
  
  
  observe({
    assign("SimulationYear", input$SimulationYear, envir = .GlobalEnv)
  })
  
  observe({
    assign("benchmark_tax_rate", input$benchmark_tax_rate, envir = .GlobalEnv)
  })
  
  # Reactive data frame to store Excel data
  excelData <- reactiveVal(NULL)
  
  observeEvent(input$importExcel, {
    req(input$fileInput)
    inFile <- input$fileInput
    if (!is.null(inFile)) {
      data <- read_excel(inFile$datapath, col_names = input$hasHeader)
      if (!all(c("PRODUCT_INDUSTRY_CODE","PRODUCT_INDUSTRY_NAME","Current_Policy_Exempt","Current_Policy_Reduced_Rate","Current_Policy_Fully_Taxable","PreferentialVATRate_1",
                 #"PreferentialVATRate_2",
                 "StandardVATRate",             
                 "ProportionExempted","ProportionPreferentialRate1",
                 #"ProportionPreferentialRate2",
                 "ProportionStandardRate") %in% colnames(data))) {
        showModal(modalDialog(
          title = "Error",
          "The Excel file must contain the columns: 'PRODUCT_INDUSTRY_CODE', 'PRODUCT_INDUSTRY_NAME', 'Current_Policy_Exempt', 
                        'PRODUCT_INDUSTRY_CODE','PRODUCT_INDUSTRY_NAME','Current_Policy_Exempt','Current_Policy_Reduced_Rate','Current_Policy_Fully_Taxable','PreferentialVATRate_1','StandardVATRate'             
                        'ProportionExempted','ProportionPreferentialRate1',
          
          'ProportionStandardRate' .",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      excelData(data)
      assign("CPA_TAXABLE_PROPORTIONS_BU", excelData(), envir = .GlobalEnv)
    }
  })
  reactive_simulation_results <- reactiveVal()
  
  # OD OVDE 
  
  observeEvent(input$calc_Sim_Button, {
    # 1) Show "Running..." modal
    showModal(modalDialog(
      title = "Running Simulation...",
      "Please wait while the simulation is running...",
      easyClose = FALSE,
      footer = NULL
    ))
    
    # 2) Asynchronous future block
    future({
      # Source your scripts
      source("Scripts/VAT/VAT-DataTransformation.R")
      source("Scripts/VAT/TaxCalculator_BU.R")
      source("Scripts/VAT/TaxCalculator_SIM.R")
      source("Scripts/VAT/Forecast-VAT.R")
      source("Scripts/VAT/ChartsPreparation-VAT.R")
      
      # Return whatever objects you need
      list(
        forecast_combined_agg_tbl_wide = get("forecast_combined_agg_tbl_wide", envir = .GlobalEnv),
        forecast_combined_cpa_selected = get("forecast_combined_cpa_selected", envir = .GlobalEnv),
        forecast_TE_tbl                = get("forecast_TE_tbl", envir = .GlobalEnv),
        #forecast_combined_te_selected_sim = get("forecast_combined_te_selected_sim", envir = .GlobalEnv),
        forecast_combined_te_selected= get("forecast_combined_te_selected", envir = .GlobalEnv),
        forecast_TE_tbl_combined=get("forecast_TE_tbl_combined", envir = .GlobalEnv)
      )
      
    }) %...>% (function(results) {
      # 3) Remove the "Running..." modal
      removeModal()
      
      # 4) Assign returned objects if needed
      assign("forecast_combined_agg_tbl_wide", results$forecast_combined_agg_tbl_wide, envir = .GlobalEnv)
      assign("forecast_combined_cpa_selected", results$forecast_combined_cpa_selected, envir = .GlobalEnv)
      assign("forecast_TE_tbl", results$forecast_TE_tbl, envir = .GlobalEnv)
      #assign("forecast_combined_te_selected_sim", results$forecast_combined_te_selected_sim, envir = .GlobalEnv)
      assign("forecast_combined_te_selected", results$forecast_combined_te_selected, envir = .GlobalEnv)
      assign("forecast_TE_tbl_combined", results$forecast_TE_tbl_combined, envir = .GlobalEnv)
      
      
      # 5) Show success modal
      showModal(modalDialog(
        title = "Success",
        "Simulation and chart preparation are complete!",
        easyClose = TRUE
      ))
      
      # -------------------------------------------------------------------------
      # 6) Revert BOTH rates & proportions for the currently selected code
      #    using the CURRENT_POLICY columns for the proportions
      # -------------------------------------------------------------------------
      if (!is.null(input$productIndustryCodeSelect_SIM) &&
          exists("CPA_TAXABLE_PROPORTIONS_BU", envir = .GlobalEnv)) {
        
        base_data <- get("CPA_TAXABLE_PROPORTIONS_BU", envir = .GlobalEnv)
        selected_code <- isolate(input$productIndustryCodeSelect_SIM)
        
        row_idx <- which(base_data$PRODUCT_INDUSTRY_CODE == selected_code)
        if (length(row_idx) > 0) {
          # Use the first row if multiple
          selected_row <- base_data[row_idx[1], ]
          
          #
          # RATES
          #
          updateNumericInput(session, "preferentialVATRate1_CPA_SIM",
                             value = ifelse(is.na(selected_row$PreferentialVATRate_1),
                                            0, selected_row$PreferentialVATRate_1))
          
          # updateNumericInput(session, "preferentialVATRate2_CPA_SIM",
          #                    value = ifelse(is.na(selected_row$PreferentialVATRate_2),
          #                                   0, selected_row$PreferentialVATRate_2))
          
          updateNumericInput(session, "standardVATRate_CPA_SIM",
                             value = ifelse(is.na(selected_row$StandardVATRate),
                                            0, selected_row$StandardVATRate))
          
          #
          # PROPORTIONS
          # Mapped to CURRENT_POLICY_... columns:
          #
          # - ProportionExempted_CPA_SIM  <-  Current_Policy_Exempt
          # - ProportionPreferentialRate1_CPA_SIM <- Current_Policy_Reduced_Rate
          # - ProportionPreferentialRate2_CPA_SIM <- 0 or also Current_Policy_Reduced_Rate
          # - ProportionStandardRate_CPA_SIM      <- Current_Policy_Fully_Taxable
          #
          
          updateNumericInput(session, "ProportionExempted_CPA_SIM",
                             value = ifelse(is.na(selected_row$Current_Policy_Exempt),
                                            0, selected_row$Current_Policy_Exempt))
          
          updateNumericInput(session, "ProportionPreferentialRate1_CPA_SIM",
                             value = ifelse(is.na(selected_row$Current_Policy_Reduced_Rate),
                                            0, selected_row$Current_Policy_Reduced_Rate))
          
          # Option 1: always 0
          # updateNumericInput(session, "ProportionPreferentialRate2_CPA_SIM",
          #                    value = 0)
          
          # Option 2 (alternative): if you prefer the same as Current_Policy_Reduced_Rate:
          # updateNumericInput(session, "ProportionPreferentialRate2_CPA_SIM",
          #   value = ifelse(is.na(selected_row$Current_Policy_Reduced_Rate),
          #                  0, selected_row$Current_Policy_Reduced_Rate))
          
          updateNumericInput(session, "ProportionStandardRate_CPA_SIM",
                             value = ifelse(is.na(selected_row$Current_Policy_Fully_Taxable),
                                            0, selected_row$Current_Policy_Fully_Taxable))
        }
      }
      
    }) %...!% (function(e) {
      # 7) If there's an error, remove the modal & show the message
      removeModal()
      showModal(modalDialog(
        title = "Error",
        paste("Error during simulation:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  
  
  
  # 2. Simulation -----------------------------------------------------------
  cpaData_SIM <- reactiveVal({
    if (exists("CPA_TAXABLE_PROPORTIONS_BU", envir = .GlobalEnv)) {
      get("CPA_TAXABLE_PROPORTIONS_BU", envir = .GlobalEnv)
    } else {
      # Default empty data frame with expected column names
      data.frame(
        PRODUCT_INDUSTRY_CODE = character(),
        PRODUCT_INDUSTRY_NAME = character(),
        Current_Policy_Exempt = numeric(),
        Current_Policy_Reduced_Rate = numeric(),
        Current_Policy_Fully_Taxable = numeric(),
        PreferentialVATRate_1 = numeric(),
        #PreferentialVATRate_2 = numeric(),
        StandardVATRate = numeric(),
        ProportionExempted = numeric(),
        ProportionPreferentialRate1 = numeric(),
        #ProportionPreferentialRate2 = numeric(),
        ProportionStandardRate = numeric(),
        Simulated_Policy_Exempt = numeric(),
        Simulated_Policy_Reduced_Rate = numeric(),
        Simulated_Policy_Fully_Taxable = numeric(),
        stringsAsFactors = FALSE
      )
    }
  })
  
  # Reactive values to store VAT rate values
  VAT_Rate_Values_CPA_SIM <- reactiveVal(data.table(
    PRODUCT_INDUSTRY_CODE = character(),
    PRODUCT_INDUSTRY_NAME = character(),
    PreferentialVATRate_1 = numeric(),
    #PreferentialVATRate_2 = numeric(),
    StandardVATRate = numeric(),
    Current_Policy_Exempt = numeric(),
    Current_Policy_Reduced_Rate1 = numeric(),
    #Current_Policy_Reduced_Rate2 = numeric(),
    Current_Policy_Fully_Taxable = numeric(),
    ProportionExempted = numeric(),
    ProportionPreferentialRate1 = numeric(),
    #ProportionPreferentialRate2 = numeric(),
    ProportionStandardRate = numeric()
  ))
  
  
  output$productIndustryCodeSelect_SIM <- renderUI({
    req(input$productIndustryNameSelect_SIM)
    selected_name <- input$productIndustryNameSelect_SIM
    if (!is.null(selected_name) && !is.null(cpaData_SIM())) {
      unique_codes <- unique(cpaData_SIM()[cpaData_SIM()$PRODUCT_INDUSTRY_NAME == selected_name,]$PRODUCT_INDUSTRY_CODE)
      selectInput("productIndustryCodeSelect_SIM", "Select Product Industry Code", choices = unique_codes)
    } else {
      selectInput("productIndustryCodeSelect_SIM", "Select Product Industry Code", choices = NULL)
    }
  })
  
  output$productIndustryNameSelect_SIM <- renderUI({
    if (!is.null(cpaData_SIM())) {
      selectInput("productIndustryNameSelect_SIM", "Select Product Industry Name", choices = unique(cpaData_SIM()$PRODUCT_INDUSTRY_NAME))
    } else {
      selectInput("productIndustryNameSelect_SIM", "Select Product Industry Name", choices = NULL)
    }
  })
  
  
  
  observeEvent(input$productIndustryCodeSelect_SIM, {
    req(input$productIndustryCodeSelect_SIM)  # Ensure selection is not NULL
    
    selected_code <- input$productIndustryCodeSelect_SIM
    
    if (!is.null(selected_code) && !is.null(cpaData_SIM())) {
      # Retrieve the selected row
      selected_row <- cpaData_SIM()[cpaData_SIM()$PRODUCT_INDUSTRY_CODE == selected_code, ]
      
      if (nrow(selected_row) >= 1) {
        selected_row <- selected_row[1, ]  # Use the first row if duplicates exist
        
        # Debugging: Print the selected row
        print("Selected row:")
        print(selected_row)
        
        # Update Rates
        if (!is.na(selected_row$PreferentialVATRate_1)) {
          updateNumericInput(session, "preferentialVATRate1_CPA_SIM", value = selected_row$PreferentialVATRate_1)
        } else {
          updateNumericInput(session, "preferentialVATRate1_CPA_SIM", value = 0)
        }
        
        # if (!is.na(selected_row$PreferentialVATRate_2)) {
        #   updateNumericInput(session, "preferentialVATRate2_CPA_SIM", value = selected_row$PreferentialVATRate_2)
        # } else {
        #   updateNumericInput(session, "preferentialVATRate2_CPA_SIM", value = 0)
        # }
        
        if (!is.na(selected_row$StandardVATRate)) {
          updateNumericInput(session, "standardVATRate_CPA_SIM", value = selected_row$StandardVATRate)
        } else {
          updateNumericInput(session, "standardVATRate_CPA_SIM", value = 0)
        }
        
        # Update Proportions
        if (!is.na(selected_row$ProportionExempted)) {
          updateNumericInput(session, "ProportionExempted_CPA_SIM", value = selected_row$ProportionExempted)
        } else {
          updateNumericInput(session, "ProportionExempted_CPA_SIM", value = 0)
        }
        
        if (!is.na(selected_row$ProportionPreferentialRate1)) {
          updateNumericInput(session, "ProportionPreferentialRate1_CPA_SIM", value = selected_row$ProportionPreferentialRate1)
        } else {
          updateNumericInput(session, "ProportionPreferentialRate1_CPA_SIM", value = 0)
        }
        
        # if (!is.na(selected_row$ProportionPreferentialRate2)) {
        #   updateNumericInput(session, "ProportionPreferentialRate2_CPA_SIM", value = selected_row$ProportionPreferentialRate2)
        # } else {
        #   updateNumericInput(session, "ProportionPreferentialRate2_CPA_SIM", value = 0)
        # }
        
        if (!is.na(selected_row$ProportionStandardRate)) {
          updateNumericInput(session, "ProportionStandardRate_CPA_SIM", value = selected_row$ProportionStandardRate)
        } else {
          updateNumericInput(session, "ProportionStandardRate_CPA_SIM", value = 0)
        }
        
      } else {
        cat("No matching row found. Resetting all inputs.\n")
        
        # Reset Rates
        updateNumericInput(session, "preferentialVATRate1_CPA_SIM", value = 0)
       # updateNumericInput(session, "preferentialVATRate2_CPA_SIM", value = 0)
        updateNumericInput(session, "standardVATRate_CPA_SIM", value = 0)
        
        # Reset Proportions
        updateNumericInput(session, "ProportionExempted_CPA_SIM", value = 0)
        updateNumericInput(session, "ProportionPreferentialRate1_CPA_SIM", value = 0)
        #updateNumericInput(session, "ProportionPreferentialRate2_CPA_SIM", value = 0)
        updateNumericInput(session, "ProportionStandardRate_CPA_SIM", value = 0)
      }
    } else {
      cat("Invalid selection or no data available. Resetting all inputs.\n")
      
      # Reset Rates
      updateNumericInput(session, "preferentialVATRate1_CPA_SIM", value = 0)
     # updateNumericInput(session, "preferentialVATRate2_CPA_SIM", value = 0)
      updateNumericInput(session, "standardVATRate_CPA_SIM", value = 0)
      
      # Reset Proportions
      updateNumericInput(session, "ProportionExempted_CPA_SIM", value = 0)
      updateNumericInput(session, "ProportionPreferentialRate1_CPA_SIM", value = 0)
      #updateNumericInput(session, "ProportionPreferentialRate2_CPA_SIM", value = 0)
      updateNumericInput(session, "ProportionStandardRate_CPA_SIM", value = 0)
    }
  })
  
  
  
  observeEvent(input$addVATRateValue_CPA_SIM, {
    req(input$productIndustryCodeSelect_SIM)
    
    newEntry_SIM <- data.table(
      PRODUCT_INDUSTRY_CODE = input$productIndustryCodeSelect_SIM,
      PRODUCT_INDUSTRY_NAME = input$productIndustryNameSelect_SIM,
      Current_Policy_Exempt = input$ProportionExempted_CPA_SIM,
      Current_Policy_Reduced_Rate1 = input$ProportionPreferentialRate1_CPA_SIM,
      #Current_Policy_Reduced_Rate2 = input$ProportionPreferentialRate2_CPA_SIM,
      Current_Policy_Fully_Taxable = input$ProportionStandardRate_CPA_SIM,
      PreferentialVATRate_1 = input$preferentialVATRate1_CPA_SIM,
     # PreferentialVATRate_2 = input$preferentialVATRate2_CPA_SIM,
      StandardVATRate = input$standardVATRate_CPA_SIM,
      ProportionExempted = input$ProportionExempted_CPA_SIM,
      ProportionPreferentialRate1 = input$ProportionPreferentialRate1_CPA_SIM,
      #ProportionPreferentialRate2 = input$ProportionPreferentialRate2_CPA_SIM,
      ProportionStandardRate = input$ProportionStandardRate_CPA_SIM
    )
    VAT_Rate_Values_CPA_SIM(rbind(VAT_Rate_Values_CPA_SIM(), newEntry_SIM))
  })
  
  observeEvent(input$clearVATRateTable_CPA_SIM, {
    VAT_Rate_Values_CPA_SIM(data.table(
      PRODUCT_INDUSTRY_CODE = character(),
      PRODUCT_INDUSTRY_NAME = character(),
      PreferentialVATRate_1 = numeric(),
      #PreferentialVATRate_2 = numeric(),
      StandardVATRate = numeric(),
      Current_Policy_Exempt = numeric(),
      Current_Policy_Reduced_Rate1 = numeric(),
      #Current_Policy_Reduced_Rate2 = numeric(),
      Current_Policy_Fully_Taxable = numeric(),
      ProportionExempted = numeric(),
      ProportionPreferentialRate1 = numeric(),
      #ProportionPreferentialRate2 = numeric(),
      ProportionStandardRate = numeric()
    ))
  })
  
  # OD OVDE 
  observeEvent(input$savevatRateTableUpdate_CPA_SIM, {
    # --------------------------------------------------------------------------
    # 1) Ensure the base data exists
    # --------------------------------------------------------------------------
    if (!exists("CPA_TAXABLE_PROPORTIONS_BU", envir = .GlobalEnv)) {
      showModal(modalDialog(
        title = "Error",
        "Base data (CPA_TAXABLE_PROPORTIONS_BU) not found. Cannot proceed.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Typically, you start from base data for the SIM
    CPA_TAXABLE_PROPORTIONS_SIM <- get("CPA_TAXABLE_PROPORTIONS_BU", envir = .GlobalEnv)
    
    # --------------------------------------------------------------------------
    # 2) Retrieve user-updated rows (including proportions) from the reactive
    # --------------------------------------------------------------------------
    vatRateData_CPA_SIM <- VAT_Rate_Values_CPA_SIM()  # e.g. reactiveVal
    
    # If no rows, copy base to SIM
    if (is.null(vatRateData_CPA_SIM) || nrow(vatRateData_CPA_SIM) == 0) {
      assign("CPA_TAXABLE_PROPORTIONS_SIM", CPA_TAXABLE_PROPORTIONS_SIM, envir = .GlobalEnv)
      showModal(modalDialog(
        title = "No Updates",
        "The simulation will run under the business-as-usual scenario as no parameters have been added.",
        easyClose = TRUE
      ))
      return()
    }
    
    # --------------------------------------------------------------------------
    # 3) Merge user changes (both RATES and PROPORTIONS) into the SIM dataset
    # --------------------------------------------------------------------------
    for (i in seq_len(nrow(vatRateData_CPA_SIM))) {
      row_data <- vatRateData_CPA_SIM[i, ]
      
      idx <- which(
        CPA_TAXABLE_PROPORTIONS_SIM$PRODUCT_INDUSTRY_CODE == row_data$PRODUCT_INDUSTRY_CODE &
          CPA_TAXABLE_PROPORTIONS_SIM$PRODUCT_INDUSTRY_NAME == row_data$PRODUCT_INDUSTRY_NAME
      )
      if (length(idx) > 0) {
        # RATES
        CPA_TAXABLE_PROPORTIONS_SIM$PreferentialVATRate_1[idx] <- row_data$PreferentialVATRate_1
        #CPA_TAXABLE_PROPORTIONS_SIM$PreferentialVATRate_2[idx] <- row_data$PreferentialVATRate_2
        CPA_TAXABLE_PROPORTIONS_SIM$StandardVATRate[idx]       <- row_data$StandardVATRate
        
        # PROPORTIONS
        CPA_TAXABLE_PROPORTIONS_SIM$ProportionExempted[idx]          <- row_data$ProportionExempted
        CPA_TAXABLE_PROPORTIONS_SIM$ProportionPreferentialRate1[idx] <- row_data$ProportionPreferentialRate1
        #CPA_TAXABLE_PROPORTIONS_SIM$ProportionPreferentialRate2[idx] <- row_data$ProportionPreferentialRate2
        CPA_TAXABLE_PROPORTIONS_SIM$ProportionStandardRate[idx]      <- row_data$ProportionStandardRate
      }
    }
    
    assign("CPA_TAXABLE_PROPORTIONS_SIM", CPA_TAXABLE_PROPORTIONS_SIM, envir = .GlobalEnv)
    
    showModal(modalDialog(
      title = "Success",
      paste("Saved", nrow(vatRateData_CPA_SIM), "row(s) to CPA_TAXABLE_PROPORTIONS_SIM."),
      easyClose = TRUE
    ))
    
    # --------------------------------------------------------------------------
    # 4) DO NOT reset the UI. Let the user see the changed values they just saved.
    # --------------------------------------------------------------------------
  })
 
  # TEST NEW
  observeEvent(input$productIndustryCodeSelect_SIM, {
    req(input$productIndustryCodeSelect_SIM)  # Must have a selection
    
    # Which product code did the user select?
    selected_code <- input$productIndustryCodeSelect_SIM
    
    # Make sure the base data exists in the global environment
    if (!exists("CPA_TAXABLE_PROPORTIONS_BU", envir = .GlobalEnv)) {
      cat("Error: CPA_TAXABLE_PROPORTIONS_BU not found.\n")
      
      # Reset all numeric inputs to 0 if base data is missing
      updateNumericInput(session, "preferentialVATRate1_CPA_SIM", value = 0)
      #updateNumericInput(session, "preferentialVATRate2_CPA_SIM", value = 0)
      updateNumericInput(session, "standardVATRate_CPA_SIM",      value = 0)
      updateNumericInput(session, "ProportionExempted_CPA_SIM",  value = 0)
      updateNumericInput(session, "ProportionPreferentialRate1_CPA_SIM", value = 0)
      #updateNumericInput(session, "ProportionPreferentialRate2_CPA_SIM", value = 0)
      updateNumericInput(session, "ProportionStandardRate_CPA_SIM",      value = 0)
      return()
    }
    
    # Load the unmodified base data
    base_data <- get("CPA_TAXABLE_PROPORTIONS_BU", envir = .GlobalEnv)
    
    # Find the row for this specific PRODUCT_INDUSTRY_CODE
    selected_row <- base_data[base_data$PRODUCT_INDUSTRY_CODE == selected_code, ]
    
    if (nrow(selected_row) < 1) {
      cat("Warning: No matching row in CPA_TAXABLE_PROPORTIONS_BU.\n")
      
      # Reset to zero if no matching row found
      updateNumericInput(session, "preferentialVATRate1_CPA_SIM", value = 0)
      #updateNumericInput(session, "preferentialVATRate2_CPA_SIM", value = 0)
      updateNumericInput(session, "standardVATRate_CPA_SIM",      value = 0)
      updateNumericInput(session, "ProportionExempted_CPA_SIM",  value = 0)
      updateNumericInput(session, "ProportionPreferentialRate1_CPA_SIM", value = 0)
      #updateNumericInput(session, "ProportionPreferentialRate2_CPA_SIM", value = 0)
      updateNumericInput(session, "ProportionStandardRate_CPA_SIM",      value = 0)
      
    } else {
      # If duplicates, use the first row
      selected_row <- selected_row[1, ]
      
      #
      # 1) VAT Rate Inputs
      #
      updateNumericInput(session, "preferentialVATRate1_CPA_SIM",
                         value = ifelse(
                           is.na(selected_row$PreferentialVATRate_1),
                           0,
                           selected_row$PreferentialVATRate_1
                         )
      )
      # updateNumericInput(session, "preferentialVATRate2_CPA_SIM",
      #                    value = ifelse(
      #                      is.na(selected_row$PreferentialVATRate_2),
      #                      0,
      #                      selected_row$PreferentialVATRate_2
      #                    )
      # )
      updateNumericInput(session, "standardVATRate_CPA_SIM",
                         value = ifelse(
                           is.na(selected_row$StandardVATRate),
                           0,
                           selected_row$StandardVATRate
                         )
      )
      
      #
      # 2) Proportion Inputs
      #
      # If you actually want these from ProportionExempted, etc., just swap the columns below.
      # This example uses the Current_Policy_* columns to mimic your screenshot behavior.
      
      updateNumericInput(session, "ProportionExempted_CPA_SIM",
                         value = ifelse(
                           is.na(selected_row$Current_Policy_Exempt),
                           0,
                           selected_row$Current_Policy_Exempt
                         )
      )
      
      updateNumericInput(session, "ProportionPreferentialRate1_CPA_SIM",
                         value = ifelse(
                           is.na(selected_row$Current_Policy_Reduced_Rate),
                           0,
                           selected_row$Current_Policy_Reduced_Rate
                         )
      )
      
      # Hard-code 0 or point to a different column if you have one:
      # updateNumericInput(session, "ProportionPreferentialRate2_CPA_SIM",
      #                    value = 0
      # )
      
      updateNumericInput(session, "ProportionStandardRate_CPA_SIM",
                         value = ifelse(
                           is.na(selected_row$Current_Policy_Fully_Taxable),
                           0,
                           selected_row$Current_Policy_Fully_Taxable
                         )
      )
    }
  })
  
  
  # do ovde
  
  
  
  
  
  
  output$vatRateTableUpdate_CPA_SIM <- renderDT({
    # Select only the columns to display in the GUI
    table_to_display <- VAT_Rate_Values_CPA_SIM()[, .(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      PreferentialVATRate_1,
      #PreferentialVATRate_2,
      StandardVATRate,
      ProportionExempted,
      ProportionPreferentialRate1,
      #ProportionPreferentialRate2,
      ProportionStandardRate
    )]
    
    # Set shorter column titles
    colnames(table_to_display) <- c(
      "Industry Code", " Industry Description", 
      "Pref. Rate 1","Standard Rate",
      "Exempted", "Pref. Prop. 1","Standard Prop."
    )
    
    # Render the subsetted table with shorter column names
    datatable(table_to_display, options = list(dom = 't', paging = FALSE), editable = TRUE)
  })
  
  
  output$cpaTableUpdate_SIM <- renderDT({
    datatable(cpaData_SIM(), options = list(dom = 't', paging = FALSE), editable = TRUE)
  })
  
  
  output$debugTable <- renderDT({
    datatable(cpaData_SIM(), options = list(dom = 't', paging = FALSE))
  })
  
  
  # Tables ------------------------------------------------------------------
  
  # VAT-Fiscal Impact -------------------------------------------------------
  output$VAT_SUMMARY_TABLES <- renderDT({
    # Check if the reactive simulation results exist and contain data
    req(reactive_simulation_results())
    vat_summary <- reactive_simulation_results()$vat_summary
    
    # If the vat_summary data is NULL or empty, display a placeholder table
    if (is.null(vat_summary) || nrow(vat_summary) == 0) {
      datatable(
        data.frame(Message = "Data is not available yet. Please run the simulation."),
        options = list(
          dom = "t",  # Show only the table without controls
          paging = FALSE  # Disable pagination
        ),
        rownames = FALSE
      )
    } else {
      # Render the table with the actual data
      datatable(
        vat_summary,
        caption = tags$caption(
          paste("VAT Revenue Projections,", min(forecast_horizon), "-", max(forecast_horizon)),
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
              filename = 'VAT_Projections',
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
              filename = 'VAT_Projections',
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
    }
  })
  
  
  # 2. ----------------------------------------------------------------------
  
  output$TE_SUMMARY_TABLES <- renderDT({
    # Check if the reactive simulation results exist and contain data
    req(reactive_simulation_results())
    te_summary <- reactive_simulation_results()$te_summary
    
    # If the vat_summary data is NULL or empty, display a placeholder table
    if (is.null(te_summary) || nrow(te_summary) == 0) {
      datatable(
        data.frame(Message = "Data is not available yet. Please run the simulation."),
        options = list(
          dom = "t",  # Show only the table without controls
          paging = FALSE  # Disable pagination
        ),
        rownames = FALSE
      )
    } else {
      # Render the table with the actual data
      datatable(
        te_summary,
        caption = tags$caption(
          paste("Tax Expenditures Projections,", min(forecast_horizon), "-", max(forecast_horizon)),
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
              filename = 'VAT_Projections',
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
              filename = 'VAT_Projections',
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
    }
  })
  
  # Charts  -----------------------------------------------------------------
  
  # Define the updateCharts function
  updateCharts <- function(chart_type = NULL) {
    if (is.null(chart_type)) {
      cat("Error: chart_type is missing.\n")
      return()
    }
    
    cat("Selected chart type:", chart_type, "\n")
    
    if (chart_type == "VAT-GAP Metrics") {
      cat("Preparing Revenue Impact VAT charts\n")
      
      # # Check if required data exists in the global environment
      if (!exists("vat_gap_metrics_tbl", envir = .GlobalEnv) ||
          !exists("vat_gap_metrics_tbl_treemap", envir = .GlobalEnv) ||
          !exists("vat_sectors_pie_tbl", envir = .GlobalEnv) ||
          !exists("vat_sectors_normalized", envir = .GlobalEnv)) {
        cat("Warning: One or more required datasets for VAT Metrics are missing.\n")
        return()
      }
      
      # Load the chart preparation script
      tryCatch({
        source("Scripts/VAT/Charts-VAT_GAP_METRICS.R")
      }, error = function(e) {
        cat("Error sourcing Charts-VAT_GAP_METRICS.R:", e$message, "\n")
        return()
      })
      
      # Generate the charts
      charts <- NULL
      tryCatch({
        charts <- VAT_metrics_fun(
          vat_gap_metrics_tbl,vat_gap_metrics_tbl_treemap,vat_sectors_pie_tbl,vat_sectors_normalized,forecast_combined_agg,SimulationYear
        )
      }, error = function(e) {
        cat("Error in VAT_metrics_fun:", e$message, "\n")
        return()
      })
      
      if (is.null(charts)) {
        cat("Error: Charts object is NULL.\n")
        return()
      }
      
      # Render InfoBoxes
      output$infoBox1 <- renderInfoBox({
        result1 <- if (exists("MainResultsVATFinal_BU", envir = .GlobalEnv)) {
          MainResultsVATFinal_BU$Values[8]
        } else {
          NA
        }
        infoBox(
          #"Total VAT GAP", paste("in",SimulationYear),
          #paste("Total VAT GAP in",SimulationYear),
          paste("Total VAT GAP in 2022"),
          value = if (!is.na(result1)) paste(round(result1, 1), "(in percentage)") else "Data Missing",
          icon = icon("chart-pie"),
          color = "orange"
        )
      })
      
      output$infoBox2 <- renderInfoBox({
        result2 <- if (exists("MainResultsVATFinal_BU", envir = .GlobalEnv)) {
          MainResultsVATFinal_BU$Values[10]
        } else {
          NA
        }
        infoBox(
          #"Compliance Gap",
          #paste("Compliance Gap in",SimulationYear),
          paste("Compliance Gap in 2022"),
          value = if (!is.na(result2)) paste(round(result2, 1), "(in percentage)") else "Data Missing",
          icon = icon("chart-pie"),
          color = "light-blue"
        )
      })
      
      # Render UI for charts
      output$additionalCharts <- renderUI({
        tagList(
          fluidRow(
            column(6, plotlyOutput("vat_rev_tbl_plt", height = "400px")),
            column(6, plotlyOutput("vat_gap_tbl_plt", height = "400px"))
          ),
          fluidRow(
            column(6, plotlyOutput("VAT_GAP_plt", height = "400px")),
            column(6, plotlyOutput("vat_str_plt", height = "400px"))
          )
        )
      })
      # Render each chart with Plotly
      output$vat_rev_tbl_plt <- renderPlotly({ charts$vat_rev_tbl_plt })
      output$vat_gap_tbl_plt <- renderPlotly({ charts$vat_gap_tbl_plt })
      output$VAT_GAP_plt <- renderPlotly({ charts$VAT_GAP_plt })
      output$vat_str_plt <- renderPlotly({ charts$vat_str_plt })
      
    } else if (chart_type == "Sectoral Breakdown of VAT Revenues") {
      cat("Preparing Tax Expenditures Charts\n")
      tryCatch({
        source("Scripts/VAT/Charts-VAT_Sectors_Revenues.R")
      }, error = function(e) {
        cat("Error sourcing Charts-VAT_Sectors_Revenues.R:", e$message, "\n")
        return()
      })
      
      # Generate charts for Tax Expenditures
      VAT_sectors_charts <- NULL
      tryCatch({
        VAT_sectors_charts <- VAT_sectors_revenues_fun(
          forecast_combined_cpa_selected,
          SimulationYear
        )
      }, error = function(e) {
        cat("Error in VAT_sectors_revenues_fun:", e$message, "\n")
        return()
      })
      
      if (is.null(VAT_sectors_charts)) {
        cat("Error: VAT sectors charts object is NULL.\n")
        return()
      }
      output$infoBox1 <- renderInfoBox({
        FiscalImpact<- forecast_combined_agg %>%
          dplyr::filter(
            year == SimulationYear,
            Descriptions == "Calibrated VAT",
            scenario %in% c("Baseline", "Simulation")) %>%
          pivot_wider(names_from = scenario, values_from = value) %>%
          dplyr::mutate(difference = Simulation - Baseline) %>%
          dplyr::select(difference)
        
        #Results$Simulation$Simulated_Change_in_Revenues.LCU
        infoBox(
          #"Fiscal Impact (In EUR thousand)",
          paste("Fiscal Impact (In EUR thousand) in",SimulationYear),
          #paste0(round(FiscalImpact$difference),1),
          round(FiscalImpact$difference,1),
          icon = icon("chart-simple"),
          color = "orange"
        )
      })
      output$infoBox2 <- renderInfoBox({
        # Calculate the Fiscal Impact and the difference
        FiscalImpact <- forecast_combined_agg %>%
          dplyr::filter(
            year == SimulationYear,
            Descriptions == "Calibrated VAT",
            scenario %in% c("Baseline", "Simulation")
          ) %>%
          pivot_wider(names_from = scenario, values_from = value) %>%
          dplyr::mutate(difference = Simulation - Baseline)
        
        # Calculate the Share of Revenues (Baseline)
        ShareOfRevenuesVAT <- FiscalImpact$Baseline
        
        # Calculate Participation Percentage
        ParticipationPercentage <- (FiscalImpact$difference / ShareOfRevenuesVAT) * 100
        
        # Render the info box
        infoBox(
          #"As percentage of VAT revenues",
          paste("As percentage of VAT revenues in",SimulationYear),
          round(ParticipationPercentage, 1),
          icon = icon("chart-pie"),
          color = "blue"
        )
      })
      
      
      # Render UI for  charts
      output$additionalCharts <- renderUI({
        tagList(
          fluidRow(
            column(6, plotlyOutput("vat_nace_total_plt", height = "400px")),
            column(6, plotlyOutput("vat_nace_hh_plt", height = "400px"))
          ),
          fluidRow(
            column(6, plotlyOutput("vat_nace_businesses_plt", height = "400px")),
            column(6, plotlyOutput("vat_nace_gov_plt", height = "400px"))
          )
        )
      })
      
      
      
      # Render each chart with Plotly
      output$vat_nace_total_plt <- renderPlotly({ VAT_sectors_charts$vat_nace_total_plt })
      output$vat_nace_hh_plt <- renderPlotly({ VAT_sectors_charts$vat_nace_hh_plt })
      output$vat_nace_businesses_plt <- renderPlotly({ VAT_sectors_charts$vat_nace_businesses_plt })
      output$vat_nace_gov_plt <- renderPlotly({ VAT_sectors_charts$vat_nace_gov_plt })
    } 
    # OD OVDE 
    else if (chart_type == "Fiscal Impact") {
      cat("Preparing Tax Expenditures Charts\n")
      tryCatch({
        source("Scripts/VAT/Charts-VAT_AggregateRevenues.R")
      }, error = function(e) {
        cat("Error sourcing Charts-VAT_AggregateRevenues.R:", e$message, "\n")
        return()
      })
      
      # Generate charts for Tax Expenditures
      VAT_sectors_charts <- NULL
      tryCatch({
        VAT_sectors_charts <- VAT_aggregation_revenues_fun(
          forecast_combined_agg,forecast_combined_agg_tbl_wide,forecast_sim_cpa,vat_sectors_pie_tbl,
          SimulationYear
        )
      }, error = function(e) {
        cat("Error in VAT_aggregation_revenues_fun:", e$message, "\n")
        return()
      })
      
      if (is.null(VAT_sectors_charts)) {
        cat("Error: VAT sectors charts object is NULL.\n")
        return()
      }
      output$infoBox1 <- renderInfoBox({
        FiscalImpact<- forecast_combined_agg %>%
          dplyr::filter(
            year == SimulationYear,
            Descriptions == "Calibrated VAT",
            scenario %in% c("Baseline", "Simulation")) %>%
          pivot_wider(names_from = scenario, values_from = value) %>%
          dplyr::mutate(difference = Simulation - Baseline) %>%
          dplyr::select(difference)
        
        #Results$Simulation$Simulated_Change_in_Revenues.LCU
        infoBox(
          #"Fiscal Impact (In EUR thousand)",
          paste("Fiscal Impact (In EUR thousand) in",SimulationYear),
          #paste0(round(FiscalImpact$difference),1),
          round(FiscalImpact$difference,1),
          icon = icon("chart-simple"),
          color = "orange"
        )
      })
      output$infoBox2 <- renderInfoBox({
        # Calculate the Fiscal Impact and the difference
        FiscalImpact <- forecast_combined_agg %>%
          dplyr::filter(
            year == SimulationYear,
            Descriptions == "Calibrated VAT",
            scenario %in% c("Baseline", "Simulation")
          ) %>%
          pivot_wider(names_from = scenario, values_from = value) %>%
          dplyr::mutate(difference = Simulation - Baseline)
        
        # Calculate the Share of Revenues (Baseline)
        ShareOfRevenuesVAT <- FiscalImpact$Baseline
        
        # Calculate Participation Percentage
        ParticipationPercentage <- (FiscalImpact$difference / ShareOfRevenuesVAT) * 100
        
        # Render the info box
        infoBox(
          #"As percentage of VAT revenues",
          paste("As percentage of VAT revenues in",SimulationYear),
          round(ParticipationPercentage, 1),
          icon = icon("chart-pie"),
          color = "blue"
        )
      })
      
      
      # Render UI for  charts
      output$additionalCharts <- renderUI({
        tagList(
          fluidRow(
            column(6, plotlyOutput("vat_revenue_plt", height = "400px")),
            column(6, plotlyOutput("vat_revenue_gdp_plt", height = "400px"))
          ),
          fluidRow(
            column(6, plotlyOutput("vat_structure_plt", height = "400px")),
            column(6, plotlyOutput("vat_treemap_sectors_plt", height = "400px"))
          )
        )
      })
      
      # Render each chart with Plotly
      output$vat_revenue_plt <- renderPlotly({ VAT_sectors_charts$vat_revenue_plt })
      output$vat_revenue_gdp_plt <- renderPlotly({ VAT_sectors_charts$vat_revenue_gdp_plt })
      output$vat_structure_plt <- renderPlotly({ VAT_sectors_charts$vat_structure_plt })
      output$vat_treemap_sectors_plt <- renderPlotly({ VAT_sectors_charts$vat_treemap_sectors_plt })
    }
    
    else if (chart_type == "Tax Expenditures") {
      cat("Preparing Tax Expenditures Charts\n")
      tryCatch({
        source("Scripts/VAT/Charts-TaxExpenditures.R")
      }, error = function(e) {
        cat("Error sourcing Charts-TaxExpenditures.R:", e$message, "\n")
        return()
      })
      output$infoBox1 <- renderInfoBox({
        
       te_infobox_lcu <-forecast_TE_tbl_combined %>%
          filter(year == SimulationYear) %>%
          filter(scenario == "Simulation") %>%
          pull(`Tax Expenditures (In EUR thousand)`)
        
        infoBox(
                  #"Tax Expenditures (In EUR thousand)",
                  paste("Tax Expenditures (In EUR thousand) in",SimulationYear),
                  paste0(te_infobox_lcu),
                  icon = icon("chart-line"),
                  color = "red"
        )
      })
      
      output$infoBox2 <- renderInfoBox({
        
        te_infobox_pct<-forecast_TE_tbl_combined %>%
          filter(year == SimulationYear) %>%
          filter(scenario == "Simulation") %>%
          pull(`Tax Expenditures (Pct of GDP)`)
        
        infoBox(
          #"Tax Expenditures (As a Percentage of GDP)",
          paste("Tax Expenditures (As a Percentage of GDP) in",SimulationYear),
          paste0(te_infobox_pct),
          icon = icon("chart-line"),
          color = "blue"
        )
      })
      # Generate charts for Tax Expenditures
      VAT_TE_charts <- NULL
      tryCatch({
        VAT_TE_charts <- VAT_TE_fun(
                                    VAT_TE_tbl,forecast_combined_te_selected,forecast_TE_tbl_combined,SimulationYear
        )
      }, error = function(e) {
        cat("Error in VAT_TE_fun:", e$message, "\n")
        return()
      })
      
      if (is.null(VAT_TE_charts)) {
        cat("Error: VAT_TE_charts object is NULL.\n")
        return()
      }
      
      # Render UI for Tax Expenditures charts
      output$additionalCharts <- renderUI({
        tagList(
          fluidRow(
            column(6, plotlyOutput("vat_TE_plt", height = "400px")),
            column(6, plotlyOutput("vat_te_tbl_plt", height = "400px"))
          ),
          fluidRow(
            column(6, plotlyOutput("vat_CPA_plt", height = "400px")),
            column(6, plotlyOutput("vat_TE_GDP_plt", height = "400px"))
          )
        )
      })
      
      # Render each chart with Plotly
      output$vat_TE_plt <- renderPlotly({ VAT_TE_charts$vat_TE_plt })
      output$vat_te_tbl_plt <- renderPlotly({ VAT_TE_charts$vat_te_tbl_plt })
      output$vat_CPA_plt <- renderPlotly({ VAT_TE_charts$vat_CPA_plt })
      output$vat_TE_GDP_plt <- renderPlotly({ VAT_TE_charts$vat_TE_GDP_plt })
    }
    
    
  }
  
  # Observer for Dropdown Selection
  observeEvent(input$chartSelectVAT_Revenues, {
    cat("Dropdown selection changed: Triggering chart updates...\n")
    chart_type <- isolate(input$chartSelectVAT_Revenues)
    updateCharts(chart_type)
  })
  
  # Observer for calc_Sim_Button
  observeEvent(input$calc_Sim_Button, {
    cat("calc_Sim_Button pressed: Triggering chart updates...\n")
    chart_type <- isolate(input$chartSelectVAT_Revenues)
    updateCharts(chart_type)
  })
  
  
  observeEvent(input$calc_Sim_Button, {
    # Assume forecast_combined_agg_tbl_wide is computed in this block
    reactive_simulation_results(list(
      vat_summary = forecast_combined_agg_tbl_wide,
      te_summary = forecast_TE_tbl
    ))
  })
  
  # Observer for toggleSimulationRates_CPA_SIM
  observe({
    if (input$toggleSimulationRates_CPA_SIM) {
      # Toggle is ON
      cat("Toggle is ON: Assigning values to the global environment and updating data.\n")
      
      # Assign the numeric input values to the global environment
      assign("simPreferentialRate1_CPA_SIM", input$simPreferentialRate1_CPA_SIM, envir = .GlobalEnv)
      assign("simPreferentialRate2_CPA_SIM", input$simPreferentialRate2_CPA_SIM, envir = .GlobalEnv)
      assign("simStandardRate_CPA_SIM", input$simStandardRate_CPA_SIM, envir = .GlobalEnv)
      
      # Update the CPA_TAXABLE_PROPORTIONS_SIM data frame
      if (exists("CPA_TAXABLE_PROPORTIONS_SIM", envir = .GlobalEnv)) {
        CPA_TAXABLE_PROPORTIONS_SIM <- get("CPA_TAXABLE_PROPORTIONS_SIM", envir = .GlobalEnv)
        
        # Assign the rates to the entire data frame
        CPA_TAXABLE_PROPORTIONS_SIM$PreferentialVATRate_1 <- simPreferentialRate1_CPA_SIM
        #CPA_TAXABLE_PROPORTIONS_SIM$PreferentialVATRate_2 <- simPreferentialRate2_CPA_SIM
        CPA_TAXABLE_PROPORTIONS_SIM$StandardVATRate <- simStandardRate_CPA_SIM
        
        # Add logic for Description == "Imputed rents of owner-occupied dwellings"
        index <- which(CPA_TAXABLE_PROPORTIONS_SIM$PRODUCT_INDUSTRY_NAME == "Imputed rents of owner-occupied dwellings")
        if (length(index) > 0) {
          CPA_TAXABLE_PROPORTIONS_SIM$PreferentialVATRate_1[index] <- 0
          #CPA_TAXABLE_PROPORTIONS_SIM$PreferentialVATRate_2[index] <- 0
          CPA_TAXABLE_PROPORTIONS_SIM$StandardVATRate[index] <- 0
          cat("Specific rates updated for PRODUCT_INDUSTRY_NAME == 'Imputed rents of owner-occupied dwellings'.\n")
        } else {
          cat("Description == 'Imputed rents of owner-occupied dwellings' not found in CPA_TAXABLE_PROPORTIONS_SIM.\n")
        }
        
        # Assign the updated data frame back to the global environment
        assign("CPA_TAXABLE_PROPORTIONS_SIM", CPA_TAXABLE_PROPORTIONS_SIM, envir = .GlobalEnv)
        cat("Data updated successfully in CPA_TAXABLE_PROPORTIONS_SIM.\n")
      } else {
        cat("CPA_TAXABLE_PROPORTIONS_SIM does not exist in the global environment.\n")
      }
    } else {
      # Toggle is OFF
      cat("Toggle is OFF: Removing values from the global environment.\n")
      
      # Remove the variables from the global environment if they exist
      rm(list = c("simPreferentialRate1_CPA_SIM", "simPreferentialRate2_CPA_SIM", "simStandardRate_CPA_SIM"), envir = .GlobalEnv)
    }
  })
  
}
# # Run the application
shinyApp(ui = ui, server = mainServer)