library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(ineq)
library(IC2)
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
library(sfo)
library(sf)


# Define custom colors
gc(TRUE)
options(future.globals.maxSize = 10 * 1024^3)
options(scipen = 999)

# I. UI --------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",  # Align image and text
      uiOutput("headerImage"),  # Output slot for the image in the header
      tags$span("CIT Module", style = "flex-grow: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;")
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input", tabName = "input", icon = icon("file-excel")),
      menuItem("Simulation Parameters", icon = icon("list-alt"),
               menuSubItem("Policy Parameters", tabName = "PolicyParameters", icon = icon("edit"))
      ),
      menuItem("Results", icon = icon("magnifying-glass-chart"),
               menuSubItem("Main Results", tabName = "MainResultsSimulation", icon = icon("gauge")),
               menuSubItem("Distribution Effects", tabName = "MainDistributionTables", icon = icon("chart-column")),
               menuSubItem("Tax Contribution", tabName = "MainResultBins", icon = icon("chart-pie")),
               menuSubItem("Tax Expenditures", tabName = "MainResultsTE", icon = icon("wallet"))
      ),
      menuItem("Visualizations", tabName = "CustomsDuties-charts", icon = icon("chart-simple"),
               menuSubItem("Dashboards", tabName = "PIT_Revenues", icon = icon("chart-column"))
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
      tabItem(tabName = "PolicyParameters",
             # h4("Setting simulations parameters"),
              fluidRow(
                column(3,
                       sliderInput("SimulationYear", "Setting Simulation Year",
                                   min = 2023, max = 2027, step = 1, value = 2023, width = "100%", round = 0, sep = ""),
                       uiOutput("PolicyParameter"),
                       uiOutput("Descriptions_Select"),
                       uiOutput("LongNameSelect"),
                       actionButton("addValuesValue", "Add to Table", style = "float: left;"),
                       actionButton("clearValuesTable", "Clear Table", style = "float: left;")
                ),
                column(3,
                       numericInput("default_Year", "Initial year ", value = 0, min = 0, step = 0.01),
                       numericInput("default_Value", "Value", value = 0, min = 0, step = 0.01)
                     
                ),
               
                column(3,
                       switchInput("toggleSimulationRates", "Toggle Tax Expenditures", value = FALSE, onLabel = "On", offLabel = "Off"),
                       numericInput("sim_PIT_Rates", "Tax Benchmark", value = 0, min = 0, step = 1)
                )
              ),
              #h4("Selected simulations parameters"),
             div(h4("Selected simulations parameters"), style = "text-align: center;"),
              fluidRow(
                column(12,
                       DTOutput("cit_simulation_parameters_updated"),
                       actionButton("calc_Customs_Sim_Button", "Run Simulation", style = "float: right;"),
                       actionButton("savepit_simulation_parameters_updated", "Save Data", style = "float: right;")
                )
              )
              
      ),
      tabItem(
        tabName = "MainResultsSimulation",
        fluidRow(
          column(12,
                 DTOutput("CIT_SUMMARY_TABLES")
          )
        )
      ),
      tabItem(
        tabName = "MainResultsTE",
        fluidRow(
          column(12,
                 DTOutput("TE_TABLES")
          )
        )
      ),
      tabItem(
        tabName = "MainRedistributionEffects",
        fluidRow(
          column(12,
                 DTOutput("RE_TABLES")
          )
        )
      ),
      tabItem(
        tabName = "MainDistributionTables",
        fluidRow(
          column(12,
                 DTOutput("DIST_TABLES")
          )
        )
      ),
      tabItem(
        tabName = "MainResultBins",
        fluidRow(
          column(12,
                 DTOutput("BIN_TABLES")
          )
        )
      ),
      tabItem(
        tabName = "PIT_Revenues",
        fluidRow(
          column(6,
                 selectInput("chartSelectPIT_Revenues", "Select Chart",
                             choices = c(
                               #"Structure_Charts",
                               "Revenue_Charts",
                               "Distribution_Charts",
                               "Distribution_Charts_Small",
                               "Tax_Expenditures_Charts"
                             ),
                             selected = "Revenue_Charts")
          )
        ),
        fluidRow(
          infoBoxOutput("infoBox1", width = 6),
          infoBoxOutput("infoBox2", width = 6)
        ),
        fluidRow(
          column(12,
                 uiOutput("additionalCharts")
          )
        )
      )
    )
  )
)

# II. Server ---------------------------------------------------------------------
server <- function(input, output, session) {

  # Render the image dynamically using Base64 encoding
  output$headerImage <- renderUI({
    img_data <- base64enc::dataURI(file = "img/WB_pic.png", mime = "image/png")
    tags$img(src = img_data, height = "40px", style = "float:left; margin-right:20px;")
  })
  
  # Input from slider -------------------------------------------------------
  observeEvent(input$SimulationYear, {
    assign("SimulationYear", input$SimulationYear, envir = .GlobalEnv)
    cat("Simulation year updated:", input$SimulationYear, "\n")
  })
  
  # observeEvent(input$PersonalAllowance, {
  #   assign("PersonalAllowance", input$PersonalAllowance, envir = .GlobalEnv)
  #   cat("Personal Allowance updated:", input$PersonalAllowance, "\n")
  # })
  # 
  # observeEvent(input$HighestBaseSSC_Employment, {
  #   assign("HighestBaseSSC_Employment", input$HighestBaseSSC_Employment, envir = .GlobalEnv)
  #   cat("Highest Base SSC Employment updated:", input$HighestBaseSSC_Employment, "\n")
  # })
  # 
  # observeEvent(input$SSC_rate, {
  #   assign("SSC_rate", input$SSC_rate, envir = .GlobalEnv)
  #   cat("SSC_rate updated:", input$SSC_rate, "\n")
  # })
  # 
  # observeEvent(input$tax_regime, {
  #   assign("tax_regime", input$tax_regime, envir = .GlobalEnv)
  #   cat("tax_regime updated:", input$tax_regime, "\n")
  # })
  
  # Reactive data frame to store Excel data
  excelData <- reactiveVal(NULL)
  
  observeEvent(input$importExcel, {
    req(input$fileInput)
    inFile <- input$fileInput
    if (!is.null(inFile)) {
      data <- read_excel(inFile$datapath, col_names = input$hasHeader)
      if (!all(c("PolicyParameter", "Descriptions", "LongName", "Value", "Year") %in% colnames(data))) {
        showModal(modalDialog(
          title = "Error",
          "The Excel file must contain the columns: 'PolicyParameter', 'Descriptions', 'LongName', 'Value', 'Year'",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      data <- data %>%
        mutate(
          Value = as.numeric(gsub("[^0-9.]", "", Value)),
          Year = as.numeric(gsub("[^0-9.]", "", Year))
        )
      excelData(data)
      assign("cit_simulation_parameters_raw", excelData(), envir = .GlobalEnv)
      cat("Excel data imported successfully\n")
    }
  })
  
  cit_simulation_parameters_updated <- reactiveVal(data.table(
    PolicyParameter = character(),
    Descriptions = character(),
    LongName = character(),
    Value = numeric(),
    Year = numeric()
  ))
  
  output$PolicyParameter <- renderUI({
    if (!is.null(excelData())) {
      selectInput("PolicyParameter", "Policy Parameter Selection", choices = unique(excelData()$PolicyParameter))
    } else {
      selectInput("PolicyParameter", "Policy Parameter Selection", choices = NULL)
    }
  })
  
  output$Descriptions_Select <- renderUI({
    req(input$PolicyParameter)
    PolicyParameter <- input$PolicyParameter
    if (!is.null(PolicyParameter) && !is.null(excelData())) {
      selectInput("Descriptions_Select", "Description of parameter", choices = unique(excelData()[excelData()$PolicyParameter == PolicyParameter,]$Descriptions))
    } else {
      selectInput("Descriptions_Select", "Description of parameter", choices = NULL)
    }
  })
  
  output$LongNameSelect <- renderUI({
    req(input$Descriptions_Select)
    Descriptions <- input$Descriptions_Select
    if (!is.null(Descriptions) && !is.null(excelData())) {
      selectInput("LongNameSelect", "Selected variable", choices = unique(excelData()[excelData()$Descriptions == Descriptions,]$LongName))
    } else {
      selectInput("LongNameSelect", "Selected variable", choices = NULL)
    }
  })
  
  observeEvent(input$LongNameSelect, {
    selected_class <- input$LongNameSelect
    cat("Selected LongName: ", selected_class, "\n")
    if (!is.null(selected_class) && !is.null(excelData())) {
      selected_row <- excelData() %>% filter(LongName == selected_class)
      cat("Selected row:\n")
      print(selected_row)
      if (nrow(selected_row) == 1) {
        updateNumericInput(session, "default_Value", value = selected_row$Value)
        updateNumericInput(session, "default_Year", value = selected_row$Year)
        cat("Numeric inputs updated with selected row values\n")
      } else {
        cat("No matching row found or multiple rows returned\n")
      }
    }
  })

  observeEvent(input$addValuesValue, {
    req(input$LongNameSelect)
    newEntry <- data.table(
      PolicyParameter = input$PolicyParameter,
      Descriptions = input$Descriptions_Select,
      LongName = input$LongNameSelect,
      Value = if (input$toggleSimulationRates) input$sim_PIT_Rates else input$default_Value,
      Year = if (input$toggleSimulationRates) input$SimulationYear else input$default_Year
    )
    cit_simulation_parameters_updated(rbind(cit_simulation_parameters_updated(), newEntry))
    cat("New entry added to cit_simulation_parameters_updated:\n")
    print(newEntry)
  })
  
  observeEvent(input$clearValuesTable, {
    cit_simulation_parameters_updated(data.table(
      PolicyParameter = character(),
      Descriptions = character(),
      LongName = character(),
      Value = numeric(),
      Year = numeric()
    ))
    cat("cit_simulation_parameters_updated table cleared\n")
  })
  
  observeEvent(input$savepit_simulation_parameters_updated, {
    assign("ValueTableUpdate", cit_simulation_parameters_updated(), envir = .GlobalEnv)
    cat("CIT simulation parameters saved to GlobalEnv as ValueTableUpdate\n")
    
    pit_simulation_parameters_updated_copy <- get("cit_simulation_parameters_raw", envir = .GlobalEnv)
    
    if (input$toggleSimulationRates) {
      pit_simulation_parameters_updated_copy$Value[pit_simulation_parameters_updated_copy$PolicyParameter == "Deductions"] <- input$sim_PIT_Rates
      pit_simulation_parameters_updated_copy$Year[pit_simulation_parameters_updated_copy$PolicyParameter == "Deductions"] <- input$SimulationYear
      cat("Simulation rates updated in cit_simulation_parameters_updated_copy\n")
    }
    
    pitRateData <- get("ValueTableUpdate", envir = .GlobalEnv)
    if (nrow(pitRateData) > 0) {
      for (i in 1:nrow(pitRateData)) {
        row <- pitRateData[i, ]
        pit_simulation_parameters_updated_copy[pit_simulation_parameters_updated_copy$PolicyParameter == row$PolicyParameter & pit_simulation_parameters_updated_copy$Descriptions == row$Descriptions & pit_simulation_parameters_updated_copy$LongName == row$LongName, 
                                               c("Value", "Year")] <- list(row$Value, row$Year)
      }
    }
    
    assign("cit_simulation_parameters_updated", pit_simulation_parameters_updated_copy, envir = .GlobalEnv)
    cat("cit_simulation_parameters_updated assigned to GlobalEnv\n")
  })
  
  observe({
    toggleState("sim_PIT_Rates", input$toggleSimulationRates)
    
    if (!is.null(input$toggleSimulationRates) && length(input$toggleSimulationRates) > 0 && input$toggleSimulationRates) {
      assign("Value", input$sim_PIT_Rates, envir = .GlobalEnv)
      assign("Year", input$SimulationYear, envir = .GlobalEnv)
      cat("Simulation rates assigned to GlobalEnv\n")
    } else {
      if (exists("Value", envir = .GlobalEnv)) {
        rm("Value", envir = .GlobalEnv)
      }
      if (exists("Year", envir = .GlobalEnv)) {
        rm("Year", envir = .GlobalEnv)
      }
      if (exists("Regime", envir = .GlobalEnv)) {
        rm("Regime", envir = .GlobalEnv)
      }
      cat("Simulation rates removed from GlobalEnv\n")
    }
  })
  
  output$cit_simulation_parameters_updated <- renderDT({
    datatable(cit_simulation_parameters_updated(), options = list(dom = 't', paging = FALSE), editable = TRUE)
  })
  
  reactive_simulation_results <- reactiveVal()
  
  observeEvent(input$calc_Customs_Sim_Button, {
    if (nrow(cit_simulation_parameters_updated()) == 0 && is.null(excelData())) {
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
      source(paste0(path1, "/Scripts/CIT/Functions.R"))
      source(paste0(path1, "/Scripts/CIT/TaxCalculator.R"))
      #source(paste0(path1, "/Scripts/CIT/Calc-TaxExpenditures.R"))
      source(paste0(path1, "/Scripts/CIT/Calc-Distribution-Effects.R"))
      
      # Conditionally source TE code only if toggle is ON
      if (input$toggleSimulationRates) {
        source(paste0(path1, "/Scripts/CIT/Calc-TaxExpenditures2.R"))
      }
      
      # Return results
      list(
        cit_summary_df = get("cit_summary_df", envir = .GlobalEnv),
        #te_summary_df = get("te_summary_df", envir = .GlobalEnv),
        te_summary_df  = if (input$toggleSimulationRates) get("te_summary_df", envir = .GlobalEnv) else NULL,
        cit_decile_distribution_bu_sim = get("cit_decile_distribution_bu_sim", envir = .GlobalEnv),
        cit_result_bins_sim_sub = get("cit_result_bins_sim_sub", envir = .GlobalEnv)
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
      updateCharts()  # re-draw charts
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
  
  output$CIT_SUMMARY_TABLES <- renderDT({
    req(reactive_simulation_results())
    datatable(
      reactive_simulation_results()$cit_summary_df,
      caption = tags$caption(paste("CIT Projections,", min(forecast_horizon), "-", max(forecast_horizon)), class = "table-caption-bold"),
      extensions = 'Buttons',
      options = list(
        pageLength = 15,
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel', 'print', 'pdf'),
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      )
    )
  })
  
  output$TE_TABLES <- renderDT({
   # req(input$toggleSimulationRates)  # Ensure the table is only rendered when toggleSimulationRates is TRUE
    #req(reactive_simulation_results())  # Ensure simulation results exist
    req(input$toggleSimulationRates)  # Ensure the table is only rendered when toggleSimulationRates is TRUE
    req(reactive_simulation_results())  # Ensure simulation results exist
    
    te_summary_selected <- reactive_simulation_results()$te_summary_df %>%
      #select(year, `legal reference`, `current law`, benchmark, `tax expenditure`) %>%
      select(year, `current law`, benchmark, `tax expenditure`) #%>%
      #filter(`legal reference` != "" & !is.na(`legal reference`))
    
    datatable(
      te_summary_selected,
      caption = tags$caption(
        paste("Tax Expenditures in LCU MIL,", min(forecast_horizon), "-", max(forecast_horizon)),
        class = "table-caption-bold"
      ),
      extensions = 'Buttons',
      options = list(
        pageLength = 15,
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel', 'print', 'pdf'),
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      )
    )
  })
  
  output$RE_TABLES <- renderDT({
    req(reactive_simulation_results())
    datatable(
      reactive_simulation_results()$re_effects_final,
      caption = tags$caption(paste("Redistributive Effects,", SimulationYear), class = "table-caption-bold"),
      extensions = 'Buttons',
      options = list(
        pageLength = 15,
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel', 'print', 'pdf'),
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      )
    )
  })
  
  output$DIST_TABLES <- renderDT({
    req(reactive_simulation_results())
    datatable(
      reactive_simulation_results()$cit_decile_distribution_bu_sim,
      caption = tags$caption(paste("Distribution Tables (in LCU),", SimulationYear), class = "table-caption-bold"),
      extensions = 'Buttons',
      options = list(
        pageLength = 15,
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel', 'print', 'pdf'),
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      )
    )
  })
  # 
  output$BIN_TABLES <- renderDT({
    req(reactive_simulation_results())
    datatable(
      reactive_simulation_results()$cit_result_bins_sim_sub,
      caption = tags$caption(paste("Structure of CIT liability by income groups in MIL LCU, ", SimulationYear), class = "table-caption-bold"),
      extensions = 'Buttons',
      options = list(
        pageLength = 15,
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel', 'print', 'pdf'),
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      )
    )
  })

  updateCharts <- function() {
    cat("Updating charts after simulation\n")
    chart_type <- isolate(input$chartSelectPIT_Revenues)
    cat("Selected chart type:", chart_type, "\n")
    
    if (exists("merged_CIT_BU_SIM", envir = .GlobalEnv) && exists("forecast_horizon", envir = .GlobalEnv)) {
      merged_CIT_BU_SIM <- get("merged_CIT_BU_SIM", envir = .GlobalEnv)
      forecast_horizon <- get("forecast_horizon", envir = .GlobalEnv)
      
      if (chart_type == "Revenue_Charts") {
        cat("Preparing Revenue_Charts charts\n")
        #source("Scripts/CIT/Charts-CIT_Revenues.R")
        source(paste0(path1, "/Scripts/CIT/Charts-CIT_Revenues.R"))
        
        charts <- Revenue_Charts_fun(merged_CIT_BU_SIM,
                                 nace_cit_big_corporations_summary_long,
                                 nace_cit_small_corporations_summary_long,
                                 SimulationYear, 
                                 range(forecast_horizon))

        output$infoBox1 <- renderInfoBox({
          cat("Rendering infoBox1\n")
          infoBox(
            # title_text,
            # paste0(round(gross_income_infobox,0), " (in LCU)"),
            title = " ",   # Remove title
            #value = paste0(round(gross_income_infobox,0), " (in LCU)"),
            icon = icon("chart-area"),
            color = "orange"
          )
        })
        
        
        output$infoBox2 <- renderInfoBox({
          cat("Rendering infoBox1\n")
          infoBox(
            #title_text,
            #paste0(selected_value, " (in BIL LCU)"),
            title = " ",   # Remove title
            value = NULL,   # Remove value text
            icon = icon("industry"),
            color = "light-blue"
          )
        })
        
        output$additionalCharts <- renderUI({
          tagList(
            fluidRow(
              column(6, plotlyOutput("CIT_big_corporations_rev_plt", height = "400px")),
              column(6, plotlyOutput("CIT_small_corporations_rev_plt", height = "400px"))
            ),
            fluidRow(
              column(6, plotlyOutput("CIT_nace_big_corporations_plt", height = "400px")),
              column(6, plotlyOutput("CIT_nace_small_corporations_plt", height = "400px"))
            )
          )
        })
        
        output$CIT_big_corporations_rev_plt <- renderPlotly({ charts$CIT_big_corporations_rev_plt })
        output$CIT_small_corporations_rev_plt <- renderPlotly({ charts$CIT_small_corporations_rev_plt })
        output$CIT_nace_big_corporations_plt <- renderPlotly({ charts$CIT_nace_big_corporations_plt })
        output$CIT_nace_small_corporations_plt <- renderPlotly({ charts$CIT_nace_small_corporations_plt })
        
      } else if (chart_type == "Distribution_Charts_Small") {
        cat("Preparing Distribution_Charts charts\n")
        #source("Scripts/CIT/Charts-Distribution.R")
        source(paste0(path1, "/Scripts/CIT/Charts-Distribution_Small.R"))
        
        Distribution_Charts <- Distribution_ChartsSmall_fun(
          cit_centile_distribution_bu_sim_small,
          cit_result_bins_bu_small,
          cit_result_bins_sim_small,
          SimulationYear,
          forecast_horizon
        )
        
        #  Distribution_Charts <- Structure_GrossIncome_Charts(labor_capital_type, gross_nace_tbl)
        
        output$infoBox1 <- renderInfoBox({
          cat("Rendering infoBox1\n")
          infoBox(
            title = " ",   # Remove title
            icon = icon("chart-area"),
            color = "orange"
          )
        })
        
        
        output$infoBox2 <- renderInfoBox({
          cat("Rendering infoBox1\n")
          infoBox(
            title = " ",   # Remove title
            value = NULL,   # Remove value text
            icon = icon("industry"),
            color = "light-blue"
          )
        })
        
        output$chartOutputCIT <- renderPlotly({ Distribution_Charts$labor_capital_plt })
        
        
        output$additionalCharts <- renderUI({
          tagList(
            fluidRow(
              column(6, plotlyOutput("dist_centile_groups_plt_small", height = "400px")),
              column(6, plotlyOutput("dist_decile_groups_plt_small", height = "400px"))
            ),
            fluidRow(
              column(6, plotlyOutput("cit_bins_bu_sub_plt_small", height = "400px")),
              column(6, plotlyOutput("cit_bins_sim_sub_plt_small", height = "400px"))
            )
          )
        })
        
        output$dist_centile_groups_plt_small <- renderPlotly({ Distribution_Charts$dist_centile_groups_plt_small })
        output$dist_decile_groups_plt_small <- renderPlotly({ Distribution_Charts$dist_decile_groups_plt_small })
        output$cit_bins_bu_sub_plt_small <- renderPlotly({ Distribution_Charts$cit_bins_bu_sub_plt_small })
        output$cit_bins_sim_sub_plt_small <- renderPlotly({ Distribution_Charts$cit_bins_sim_sub_plt_small })
        
        } else if (chart_type == "Distribution_Charts") {
        cat("Preparing Distribution_Charts charts\n")
        #source("Scripts/CIT/Charts-Distribution.R")
          source(paste0(path1, "/Scripts/CIT/Charts-Distribution.R"))
        
        Distribution_Charts <- Distribution_Charts_fun(
                                                      cit_centile_distribution_bu_sim,
                                                      cit_decile_distribution_bu_sim,
                                                      cit_result_bins_bu_sub,
                                                      cit_result_bins_sim_sub,
                                                      SimulationYear,
                                                      forecast_horizon
                                                      )
        
        
        
        #  Distribution_Charts <- Structure_GrossIncome_Charts(labor_capital_type, gross_nace_tbl)
        
        output$infoBox1 <- renderInfoBox({
          cat("Rendering infoBox1\n")
          infoBox(
            # title_text,
            # paste0(round(gross_income_infobox,0), " (in LCU)"),
            title = " ",   # Remove title
            #value = paste0(round(gross_income_infobox,0), " (in LCU)"),
            icon = icon("chart-area"),
            color = "orange"
          )
        })
        
        
        output$infoBox2 <- renderInfoBox({
          cat("Rendering infoBox1\n")
          infoBox(
            #title_text,
            #paste0(selected_value, " (in BIL LCU)"),
            title = " ",   # Remove title
            value = NULL,   # Remove value text
            icon = icon("industry"),
            color = "light-blue"
          )
        })
        
        
        # OVDE DA SE PROVERI POVTORNO
        
        output$chartOutputCIT <- renderPlotly({ Distribution_Charts$labor_capital_plt })
        
        # # Conditionally render the charts
        # output$chartOutputCIT <- renderPlotly({
        #   # req(input$toggleSimulationRates)  # Ensure the chart is only rendered when toggleSimulationRates is TRUE
        #   Distribution_Charts$dist_centile_groups_plt
        # })
        # 
        # 
        
        output$additionalCharts <- renderUI({
          tagList(
            fluidRow(
              column(6, plotlyOutput("dist_centile_groups_plt", height = "400px")),
              column(6, plotlyOutput("dist_decile_groups_plt", height = "400px"))
            ),
            fluidRow(
              column(6, plotlyOutput("cit_bins_bu_sub_plt", height = "400px")),
              column(6, plotlyOutput("cit_bins_sim_sub_plt", height = "400px"))
            )
          )
        })
        
        output$dist_centile_groups_plt <- renderPlotly({ Distribution_Charts$dist_centile_groups_plt })
        output$dist_decile_groups_plt <- renderPlotly({ Distribution_Charts$dist_decile_groups_plt })
        output$cit_bins_bu_sub_plt <- renderPlotly({ Distribution_Charts$cit_bins_bu_sub_plt })
        output$cit_bins_sim_sub_plt <- renderPlotly({ Distribution_Charts$cit_bins_sim_sub_plt })
        
       } 
      #else if (chart_type == "Tax_Expenditures_Charts") {
#         cat("Preparing Tax_Expenditures_Charts charts\n")
#         #source("Scripts/CIT/Charts-TaxExpenditures.R")
#         source(paste0(path1, "/Scripts/CIT/Charts-TaxExpenditures.R"))
#         
#         
#         charts_te <- Tax_Expenditures_Charts_fun(
#                                              te_summary_df,
#                                              company_type_cit_summary_te,
#                                              nace_cit_summary_tbl,
#                                              nace_cit_summary_te_deciles,
#                                              forecast_horizon,
#                                              SimulationYear)
#       
#         output$infoBox1 <- renderInfoBox({
#           cat("Rendering infoBox1\n")
#           infoBox(
#             # title_text,
#             # paste0(round(gross_income_infobox,0), " (in LCU)"),
#             title = " ",   # Remove title
#             #value = paste0(round(gross_income_infobox,0), " (in LCU)"),
#             icon = icon("chart-area"),
#             color = "orange"
#           )
#         })
#         
#         
#         output$infoBox2 <- renderInfoBox({
#           cat("Rendering infoBox1\n")
#           infoBox(
#             #title_text,
#             #paste0(selected_value, " (in BIL LCU)"),
#             title = " ",   # Remove title
#             value = NULL,   # Remove value text
#             icon = icon("industry"),
#             color = "light-blue"
#           )
#         })
#         
#         # Conditionally render the charts
#         output$chartOutputCIT <- renderPlotly({
#          # req(input$toggleSimulationRates)  # Ensure the chart is only rendered when toggleSimulationRates is TRUE
#           charts_te$te_agg_plt
#         })
#         
#         output$additionalCharts <- renderUI({
#           #req(input$toggleSimulationRates)  # Ensure the additional charts are only rendered when toggleSimulationRates is TRUE
#           tagList(
#             fluidRow(
#               column(6, plotlyOutput("te_agg_plt", height = "400px")),
#               column(6, plotlyOutput("te_type_companies_plt", height = "400px"))
#             ),
#             fluidRow(
#               column(6, plotlyOutput("treemap_nace_type_plt", height = "400px")),
#               column(6, plotlyOutput("te_decile_groups_plt", height = "400px"))
#             )
#           )
#         })
#         
#         output$te_agg_plt <- renderPlotly({ charts_te$te_agg_plt })
#         output$te_type_companies_plt <- renderPlotly({ charts_te$te_type_companies_plt })
#         output$treemap_nace_type_plt <- renderPlotly({ charts_te$treemap_nace_type_plt })
#         output$te_decile_groups_plt <- renderPlotly({ charts_te$te_decile_groups_plt })
#       }
#       
#     } else {
#       cat("Error: merged_CIT_BU_SIM or forecast_horizon not found in the global environment\n")
#     }
#   }
#   
#   observeEvent(input$chartSelectPIT_Revenues, {
#     updateCharts()
#   })
# }

        else if (chart_type == "Tax_Expenditures_Charts") {
          
          if (!input$toggleSimulationRates) {
            cat("Tax expenditures are disabled (toggle OFF). No charts to display.\n")
            # Optionally show an empty UI or a "please turn on toggle" message
            output$additionalCharts <- renderUI({
              h4("Tax Expenditures are disabled. Turn on the toggle to see charts.")
            })
            
          } else {
            cat("Preparing Tax_Expenditures_Charts charts\n")
            #source(paste0(path1, "/Scripts/CIT/Calc-TaxExpenditures2.R"))
            source(paste0(path1, "/Scripts/CIT/Charts-TaxExpenditures.R"))
            charts_te <- Tax_Expenditures_Charts_fun(
              te_summary_df,
              company_type_cit_summary_te,
              nace_cit_summary_tbl,
              te_summary_df_type,
              forecast_horizon,
              SimulationYear
            )
            
            output$infoBox1 <- renderInfoBox({
              infoBox(
                title = " ",  
                icon  = icon("chart-area"),
                color = "orange"
              )
            })
            
            output$infoBox2 <- renderInfoBox({
              infoBox(
                title = " ",  
                icon  = icon("industry"),
                color = "light-blue"
              )
            })
            
            # Conditionally render the charts
            output$chartOutputCIT <- renderPlotly({
              # req(input$toggleSimulationRates)  # Ensure the chart is only rendered when toggleSimulationRates is TRUE
              charts_te$te_agg_plt
            })
            
            
            
            output$additionalCharts <- renderUI({
              #req(input$toggleSimulationRates)  # Ensure the additional charts are only rendered when toggleSimulationRates is TRUE
              tagList(
                fluidRow(
                  column(6, plotlyOutput("te_agg_plt", height = "400px")),
                  column(6, plotlyOutput("te_type_companies_plt", height = "400px"))
                ),
                fluidRow(
                  column(6, plotlyOutput("treemap_nace_type_plt", height = "400px")),
                  column(6, plotlyOutput("te_type_plt", height = "400px"))
                )
              )
            })
            
            output$te_agg_plt <- renderPlotly({ charts_te$te_agg_plt })
            output$te_type_companies_plt <- renderPlotly({ charts_te$te_type_companies_plt })
            output$treemap_nace_type_plt <- renderPlotly({ charts_te$treemap_nace_type_plt })
            output$te_type_plt <- renderPlotly({ charts_te$te_type_plt })
          }
        }
        
        } else {
          cat("Error: merged_PIT_BU_SIM or forecast_horizon not found in the global environment\n")
        }
    }
    
    observeEvent(input$chartSelectPIT_Revenues, {
      updateCharts()
    })
  }
  
        
        
shinyApp(ui = ui, server = server)
