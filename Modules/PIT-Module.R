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
      tags$span("PIT Module", style = "flex-grow: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;")
    )
  ),
  title = "PIT Module",
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
                       # We keep the switchInput to toggle TE, but remove the numericInput for sim_PIT_Rates
                       switchInput("toggleSimulationRates", "Toggle Tax Expenditures", value = FALSE, onLabel = "On", offLabel = "Off")
                )
              ),
              div(h4("Selected simulations parameters"), style = "text-align: center;"),
              fluidRow(
                column(12,
                       DTOutput("pit_simulation_parameters_updated"),
                       actionButton("calc_Customs_Sim_Button", "Run Simulation", style = "float: right;"),
                       actionButton("savepit_simulation_parameters_updated", "Save Data", style = "float: right;")
                )
              )
      ),
      tabItem(
        tabName = "MainResultsSimulation",
        fluidRow(
          column(12,
                 DTOutput("PIT_SUMMARY_TABLES")
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
                               "Structure_Charts",
                               "Revenue_Charts",
                               "Distribution_Charts",
                               "Tax_Expenditures_Charts"
                             ),
                             selected = "Structure_Charts")
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
      assign("pit_simulation_parameters_raw", excelData(), envir = .GlobalEnv)
      cat("Excel data imported successfully\n")
    }
  })
  
  pit_simulation_parameters_updated <- reactiveVal(data.table(
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
      selectInput("Descriptions_Select", "Description of parameter", 
                  choices = unique(excelData()[excelData()$PolicyParameter == PolicyParameter, ]$Descriptions))
    } else {
      selectInput("Descriptions_Select", "Description of parameter", choices = NULL)
    }
  })
  
  output$LongNameSelect <- renderUI({
    req(input$Descriptions_Select)
    Descriptions <- input$Descriptions_Select
    if (!is.null(Descriptions) && !is.null(excelData())) {
      selectInput("LongNameSelect", "Selected variable", 
                  choices = unique(excelData()[excelData()$Descriptions == Descriptions, ]$LongName))
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
  
  # -- Removed references to sim_PIT_Rates here --
  observeEvent(input$addValuesValue, {
    req(input$LongNameSelect)
    newEntry <- data.table(
      PolicyParameter = input$PolicyParameter,
      Descriptions    = input$Descriptions_Select,
      LongName        = input$LongNameSelect,
      Value           = input$default_Value,
      Year            = input$default_Year
    )
    pit_simulation_parameters_updated(rbind(pit_simulation_parameters_updated(), newEntry))
    cat("New entry added to pit_simulation_parameters_updated:\n")
    print(newEntry)
  })
  
  observeEvent(input$clearValuesTable, {
    pit_simulation_parameters_updated(data.table(
      PolicyParameter = character(),
      Descriptions = character(),
      LongName = character(),
      Value = numeric(),
      Year = numeric()
    ))
    cat("pit_simulation_parameters_updated table cleared\n")
  })
  
  observeEvent(input$savepit_simulation_parameters_updated, {
    assign("ValueTableUpdate", pit_simulation_parameters_updated(), envir = .GlobalEnv)
    cat("PIT simulation parameters saved to GlobalEnv as ValueTableUpdate\n")
    
    pit_simulation_parameters_updated_copy <- get("pit_simulation_parameters_raw", envir = .GlobalEnv)
    
    # -- Removed block that assigned pit_simulation_parameters_updated_copy$Value = input$sim_PIT_Rates if toggle is ON --
    
    pitRateData <- get("ValueTableUpdate", envir = .GlobalEnv)
    if (nrow(pitRateData) > 0) {
      for (i in 1:nrow(pitRateData)) {
        row <- pitRateData[i, ]
        pit_simulation_parameters_updated_copy[
          pit_simulation_parameters_updated_copy$PolicyParameter == row$PolicyParameter & 
            pit_simulation_parameters_updated_copy$Descriptions     == row$Descriptions &
            pit_simulation_parameters_updated_copy$LongName         == row$LongName, 
          c("Value", "Year")
        ] <- list(row$Value, row$Year)
      }
    }
    
    assign("pit_simulation_parameters_updated", pit_simulation_parameters_updated_copy, envir = .GlobalEnv)
    cat("pit_simulation_parameters_updated assigned to GlobalEnv\n")
  })
  
  # -- Removed block that used toggleState("sim_PIT_Rates", ...) and assigned sim_PIT_Rates to .GlobalEnv --
  
  output$pit_simulation_parameters_updated <- renderDT({
    datatable(pit_simulation_parameters_updated(), options = list(dom = 't', paging = FALSE), editable = TRUE)
  })
  
  reactive_simulation_results <- reactiveVal()
  
  observeEvent(input$calc_Customs_Sim_Button, {
    if (nrow(pit_simulation_parameters_updated()) == 0 && is.null(excelData())) {
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
      source(paste0(path1, "/Scripts/PIT/Functions.R"))
      source(paste0(path1, "/Scripts/PIT/TaxCalculator.R"))
      source(paste0(path1, "/Scripts/PIT/Calc-Structure.R"))
      source(paste0(path1, "/Scripts/PIT/Calc-Distribution-Effects.R"))
      
      # Conditionally source TE code only if toggle is ON
      if (input$toggleSimulationRates) {
        source(paste0(path1, "/Scripts/PIT/Calc-TaxExpenditures.R"))
      }
      
      # Return results
      list(
        pit_summary_df = get("pit_summary_df", envir = .GlobalEnv),
        te_summary_df  = if (input$toggleSimulationRates) get("te_summary_df", envir = .GlobalEnv) else NULL,
        pit_decile_distribution_bu_sim = get("pit_decile_distribution_bu_sim", envir = .GlobalEnv),
        pit_result_bins_sim_sub        = get("pit_result_bins_sim_sub", envir = .GlobalEnv)
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
  
  output$PIT_SUMMARY_TABLES <- renderDT({
    req(reactive_simulation_results())
    datatable(
      reactive_simulation_results()$pit_summary_df,
      caption = tags$caption(paste("PIT Projections,", min(forecast_horizon), "-", max(forecast_horizon)), class = "table-caption-bold"),
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
    req(input$toggleSimulationRates)  # Ensure the table is only rendered when toggleSimulationRates is TRUE
    req(reactive_simulation_results())  # Ensure simulation results exist
    
    te_summary_selected <- reactive_simulation_results()$te_summary_df %>%
      select(year, `current law`, benchmark, `tax expenditure`)
    datatable(
      te_summary_selected,
      caption = tags$caption(
        paste("Tax Expenditures in LCU thousand,", min(forecast_horizon), "-", max(forecast_horizon)),
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
      caption = tags$caption(paste("Redistributive Effects,", simulation_year), class = "table-caption-bold"),
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
      reactive_simulation_results()$pit_decile_distribution_bu_sim,
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
  
  output$BIN_TABLES <- renderDT({
    req(reactive_simulation_results())
    datatable(
      reactive_simulation_results()$pit_result_bins_sim_sub,
      caption = tags$caption(paste("Structure of PIT liability by income groups in MIL LCU, ", simulation_year), class = "table-caption-bold"),
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
    
    if (exists("merged_PIT_BU_SIM", envir = .GlobalEnv) && exists("forecast_horizon", envir = .GlobalEnv)) {
      merged_PIT_BU_SIM <- get("merged_PIT_BU_SIM", envir = .GlobalEnv)
      forecast_horizon  <- get("forecast_horizon", envir = .GlobalEnv)
      
      if (chart_type == "Revenue_Charts") {
        cat("Preparing Revenue_Charts charts\n")
        source(paste0(path1, "/Scripts/PIT/Charts-PIT_Revenues.R"))
        
        charts <- Revenue_Charts(merged_PIT_BU_SIM,
                                 pit_gender_summary,
                                 pit_nace_summary,
                                 SimulationYear,
                                 range(forecast_horizon))
        
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
            value = NULL,  # Remove value text
            icon = icon("industry"),
            color = "light-blue"
          )
        })
        
        output$additionalCharts <- renderUI({
          tagList(
            fluidRow(
              column(6, plotlyOutput("PIT_RevenuesTotal_plt", height = "400px")),
              column(6, plotlyOutput("pit_gender_summary_plt", height = "400px"))
            ),
            fluidRow(
              column(6, plotlyOutput("treemap_nace_pit_bu_type_plt", height = "400px")),
              column(6, plotlyOutput("treemap_nace_pit_sim_type_plt", height = "400px"))
            )
          )
        })
        
        output$PIT_RevenuesTotal_plt     <- renderPlotly({ charts$PIT_RevenuesTotal_plt })
        output$pit_gender_summary_plt    <- renderPlotly({ charts$pit_gender_summary_plt })
        output$treemap_nace_pit_bu_type_plt <- renderPlotly({ charts$treemap_nace_pit_bu_type_plt })
        output$treemap_nace_pit_sim_type_plt <- renderPlotly({ charts$treemap_nace_pit_sim_type_plt })
        
      } else if (chart_type == "Structure_Charts") {
        cat("Preparing Structure_Charts charts\n")
        source(paste0(path1, "/Scripts/PIT/Charts-StructureGrossIncome.R"))
        Charts_structure <- Structure_GrossIncome_Charts_fun(
          gross_income_BU_SIM,
          labor_capital_type,
          gross_nace_tbl,
          forecast_horizon
        )
        
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
            value = NULL,  # Remove value text
            icon = icon("industry"),
            color = "light-blue"
          )
        })
        
        output$chartOutputPIT <- renderPlotly({ Charts_structure$labor_capital_plt })
        
        output$additionalCharts <- renderUI({
          tagList(
            fluidRow(
              column(6, plotlyOutput("gross_inc_plt", height = "400px")),
              column(6, plotlyOutput("labor_capital_type_plt", height = "400px"))
            ),
            fluidRow(
              column(6, plotlyOutput("pit_bins_bu_sub_plt", height = "400px")),
              column(6, plotlyOutput("treemap_nace_type_plt", height = "400px"))
            )
          )
        })
        
        output$gross_inc_plt           <- renderPlotly({ Charts_structure$gross_inc_plt })
        output$labor_capital_type_plt  <- renderPlotly({ Charts_structure$labor_capital_type_plt })
        output$pit_bins_bu_sub_plt     <- renderPlotly({ Charts_structure$treemap_labor_capital_type_plt })
        output$treemap_nace_type_plt   <- renderPlotly({ Charts_structure$treemap_nace_type_plt })
        
      } else if (chart_type == "Distribution_Charts") {
        cat("Preparing Distribution_Charts charts\n")
        source(paste0(path1, "/Scripts/PIT/Charts-Distribution.R"))
        
        Distribution_Charts <- Distribution_Charts_fun(
          pit_centile_distribution_bu_sim,
          pit_decile_distribution_bu_sim_raw,
          pit_result_bins_bu_sub,
          pit_result_bins_sim_sub,
          SimulationYear
        )
        
        output$infoBox1 <- renderInfoBox({
          cat("Rendering infoBox1\n")
          infoBox(
            title = " ",
            icon  = icon("chart-area"),
            color = "orange"
          )
        })
        
        output$infoBox2 <- renderInfoBox({
          cat("Rendering infoBox1\n")
          infoBox(
            title = " ",
            value = NULL,
            icon  = icon("industry"),
            color = "light-blue"
          )
        })
        
        output$chartOutputPIT <- renderPlotly({ Distribution_Charts$labor_capital_plt })
        
        output$additionalCharts <- renderUI({
          tagList(
            fluidRow(
              column(6, plotlyOutput("dist_centile_groups_plt", height = "400px")),
              column(6, plotlyOutput("dist_decile_groups_plt", height = "400px"))
            ),
            fluidRow(
              column(6, plotlyOutput("pit_bins_bu_sub_plt", height = "400px")),
              column(6, plotlyOutput("pit_bins_sim_sub_plt", height = "400px"))
            )
          )
        })
        
        output$dist_centile_groups_plt <- renderPlotly({ Distribution_Charts$dist_centile_groups_plt })
        output$dist_decile_groups_plt  <- renderPlotly({ Distribution_Charts$dist_decile_groups_plt })
        output$pit_bins_bu_sub_plt     <- renderPlotly({ Distribution_Charts$pit_bins_bu_sub_plt })
        output$pit_bins_sim_sub_plt    <- renderPlotly({ Distribution_Charts$pit_bins_sim_sub_plt })
        
      } else if (chart_type == "Tax_Expenditures_Charts") {
        
        if (!input$toggleSimulationRates) {
          cat("Tax expenditures are disabled (toggle OFF). No charts to display.\n")
          output$additionalCharts <- renderUI({
            h4("Tax Expenditures are disabled. Turn on the toggle to see charts.")
          })
          
        } else {
          cat("Preparing Tax_Expenditures_Charts charts\n")
          source(paste0(path1, "/Scripts/PIT/Calc-TaxExpenditures.R"))
          source(paste0(path1, "/Scripts/PIT/Charts-TaxExpenditures.R"))
          charts_te <- Tax_Expenditures_Charts(
            te_agg,
            te_agg_type,
            nace_pit_summary_tbl,
            decile_pit_summary_tbl,
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
          
          output$chartOutputPIT <- renderPlotly({
            charts_te$te_agg_plt
          })
          
          output$additionalCharts <- renderUI({
            tagList(
              fluidRow(
                column(6, plotlyOutput("te_agg_plt", height = "400px")),
                column(6, plotlyOutput("treemap_nace_type_plt", height = "400px"))
              ),
              fluidRow(
                column(6, plotlyOutput("te_gender_groups_plt", height = "400px")),
                column(6, plotlyOutput("te_decile_groups_plt", height = "400px"))
              )
            )
          })
          
          output$te_agg_plt            <- renderPlotly({ charts_te$te_agg_plt })
          output$treemap_nace_type_plt <- renderPlotly({ charts_te$treemap_nace_type_plt })
          output$te_gender_groups_plt  <- renderPlotly({ charts_te$te_gender_groups_plt })
          output$te_decile_groups_plt  <- renderPlotly({ charts_te$te_decile_groups_plt })
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
