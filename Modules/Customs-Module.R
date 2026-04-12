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

gc(TRUE)
options(future.globals.maxSize = 10 * 1024^3)
options(scipen = 999)

# I. UI --------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      uiOutput("headerImage"),
      tags$span(
        "Customs Taxes Module",
        style = "flex-grow: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; font-size: 11px;"
      )
    )
  ),
  title = "Customs Taxes Module",
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_tabs",
      menuItem("Input", tabName = "input", icon = icon("file-excel")),
      menuItem(
        "Simulation Parameters", icon = icon("list-alt"),
        menuSubItem("Policy Parameters", tabName = "Chapters_description", icon = icon("edit"))
      ),
      menuItem(
        "Revenue Impact", icon = icon("magnifying-glass-chart"),
        menuSubItem("Customs duties", tabName = "RevenueImpactSimulation", icon = icon("bars-progress")),
        menuSubItem("Excise ", tabName = "RevenueImpactSimulation_Excise", icon = icon("square-poll-vertical")),
        menuSubItem("VAT", tabName = "RevenueImpactSimulation_VAT", icon = icon("square-poll-horizontal")),
        menuSubItem("Total Import Taxes", tabName = "RevenueImpactSimulation_TotalRevenues", icon = icon("calculator"))
      ),
      menuItem(
        "Tax Expenditures", icon = icon("gauge"),
        menuSubItem("Customs Duties", tabName = "MainResultsTE", icon = icon("wallet")),
        menuSubItem("Excise", tabName = "MainResultsTE_Excise", icon = icon("wallet"))
      ),
      menuItem(
        "Visualizations", icon = icon("chart-simple"),
        menuSubItem("Revenues", tabName = "Customs_Revenues", icon = icon("chart-column")),
        menuSubItem("Tax Expenditures", tabName = "Tax_Expenditures_Charts", icon = icon("chart-pie"))
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "input",
        fluidRow(
          column(
            6,
            h4("Data Input"),
            selectInput(
              "inputType", "Data Source",
              choices = c("Manual", "Excel File"),
              selected = "Excel File"
            ),
            conditionalPanel(
              condition = "input.inputType == 'Excel File'",
              fileInput("fileInput", "Upload Excel File", accept = c(".xlsx")),
              checkboxInput("hasHeader", "Header", TRUE)
            ),
            actionButton("importExcel", "Import Excel Data")
          )
        )
      ),
      tabItem(
        tabName = "Chapters_description",
        fluidRow(
          column(
            3,
            sliderInput(
              "SimulationYear", "Setting Simulation Year",
              min = 2023, max = 2028, step = 1, value = 2023,
              width = "100%", round = 0, sep = ""
            ),
            uiOutput("yearSelectUI"),
            uiOutput("Chapters_description"),
            uiOutput("Chapter_Select"),
            uiOutput("HS_codeSelect"),
            actionButton("addValuesValue", "Add to Table", style = "float: left; margin-right: 6px;"),
            actionButton("clearValuesTable", "Clear Table", style = "float: left; margin-right: 6px;"),
            actionButton("removeLastRow", "Remove Last Row", style = "float: left;")
          ),
          column(
            3,
            numericInput("default_Value", "Customs Value", value = 0, min = 0, step = 0.01),
            numericInput("default_Quantity", "Quantity", value = 0, min = 0, step = 0.01),
            numericInput("default_Netweight", "Netweight", value = 0, min = 0, step = 0.01),
            uiOutput("default_SupplementaryUnit")
          ),
          column(
            3,
            numericInput("default_CustomsRate_MFN", "MFN", value = 0, min = 0, step = 0.01),
            numericInput("default_CustomsRate_TR", "TR", value = 0, min = 0, step = 0.01),
            numericInput("default_CustomsRate_CEFTA", "CEFTA", value = 0, min = 0, step = 0.01),
            numericInput("default_CustomsRate_MSA", "MSA", value = 0, min = 0, step = 0.01),
            numericInput("default_ExciseRate", "Excise", value = 0, min = 0, step = 0.01),
            numericInput("default_VAT", "VAT", value = 0, min = 0, step = 0.01)
          ),
          column(
            3,
            numericInput("default_Effective_Customs_rate", "ECR (Effective Customs Rate)", value = 0, min = 0, step = 0.01),
            numericInput("default_Effective_Excise_rate", "EER (Effective Excise Rate)", value = 0, min = 0, step = 0.01),
            numericInput("default_Effective_VAT_rate", "EVR (Effective VAT Rate)", value = 0, min = 0, step = 0.01),
            switchInput("toggleSimulationRates", "Toggle Tax Expenditures", value = FALSE, onLabel = "On", offLabel = "Off"),
            br(),
            switchInput("toggleYears", "Toggle NextYears", value = FALSE, onLabel = "On", offLabel = "Off")
          )
        ),
        div(h4("Selected Simulations Parameters"), style = "text-align: center;"),
        fluidRow(
          column(
            12,
            DTOutput("customs_simulation_parameters_updated"),
            br(),
            htmlOutput("simulationComment"),
            actionButton("calc_Sim_Button", "Run Simulation", style = "float: right;"),
            actionButton("savecustoms_simulation_parameters_updated", "Save Data", style = "float: right; margin-right: 6px;")
          )
        )
      ),
      tabItem(
        tabName = "RevenueImpactSimulation",
        fluidRow(
          column(12, DTOutput("CUSTOMS_SUMMARY_TABLES"))
        )
      ),
      tabItem(
        tabName = "RevenueImpactSimulation_Excise",
        fluidRow(
          column(12, DTOutput("EXCISE_SUMMARY_TABLES"))
        )
      ),
      tabItem(
        tabName = "RevenueImpactSimulation_VAT",
        fluidRow(
          column(12, DTOutput("VAT_SUMMARY_TABLES"))
        )
      ),
      tabItem(
        tabName = "RevenueImpactSimulation_TotalRevenues",
        fluidRow(
          column(12, DTOutput("TOTAL_REVENUES_SUMMARY_TABLES"))
        )
      ),
      tabItem(
        tabName = "MainResultsTE",
        fluidRow(
          column(12, DTOutput("TE_TABLES"))
        )
      ),
      tabItem(
        tabName = "MainResultsTE_Excise",
        fluidRow(
          column(12, DTOutput("TE_TABLES_Excise"))
        )
      ),
      tabItem(
        tabName = "Customs_Revenues",
        fluidRow(
          column(
            6,
            selectInput(
              "chartSelectRevenues", "Select Chart",
              choices = c(
                "Revenue_Impact_Customs",
                "Revenue_Impact_Excise",
                "Revenue_Impact_VAT",
                "Revenue_import_tax_total"
              ),
              selected = "Revenue_Impact_Customs"
            )
          )
        ),
        fluidRow(
          column(6, uiOutput("infoBoxRev1")),
          column(6, uiOutput("infoBoxRev2"))
        ),
        fluidRow(
          column(12, uiOutput("additionalChartsRev"))
        )
      ),
      tabItem(
        tabName = "Tax_Expenditures_Charts",
        uiOutput("taxExpTabUI")
      )
    )
  )
)

# II. Server ----------------------------------------------------------------
server <- function(input, output, session) {
  
  # III. Helpers -------------------------------------------------------------
  clean_import_data <- function(data) {
    names(data) <- as.character(names(data))
    
    if ("HS_code1" %in% names(data)) {
      data <- data %>% select(-HS_code1)
    }
    
    required_cols <- c(
      "Chapters_description", "Description_Chapters", "HS_code",
      "SupplementaryUnit", "CustomsRate_MFN", "ExciseRate",
      "Excise_Description", "VAT_Rate", "CustomsRate_CEFTA",
      "CustomsRate_MSA", "CustomsRate_TR", "TypeOfProducts",
      "Value", "Quantity", "Netweight", "VAT_Revenue",
      "Effective_Customs_rate", "Effective_Excise_rate",
      "Effective_VAT_rate", "Year", "Category", "Group", "ExciseBase"
    )
    
    for (nm in setdiff(required_cols, names(data))) {
      data[[nm]] <- NA
    }
    
    data <- data %>%
      mutate(
        Chapters_description   = as.character(Chapters_description),
        Description_Chapters   = as.character(Description_Chapters),
        HS_code                = as.character(HS_code),
        SupplementaryUnit      = as.character(SupplementaryUnit),
        Excise_Description     = as.character(Excise_Description),
        TypeOfProducts         = as.character(TypeOfProducts),
        Category               = as.character(Category),
        Group                  = as.character(Group),
        Year                   = suppressWarnings(as.integer(Year)),
        CustomsRate_MFN        = suppressWarnings(as.numeric(CustomsRate_MFN)),
        ExciseRate             = suppressWarnings(as.numeric(ExciseRate)),
        VAT_Rate               = suppressWarnings(as.numeric(VAT_Rate)),
        CustomsRate_CEFTA      = suppressWarnings(as.numeric(CustomsRate_CEFTA)),
        CustomsRate_MSA        = suppressWarnings(as.numeric(CustomsRate_MSA)),
        CustomsRate_TR         = suppressWarnings(as.numeric(CustomsRate_TR)),
        Value                  = suppressWarnings(as.numeric(Value)),
        Quantity               = suppressWarnings(as.numeric(Quantity)),
        Netweight              = suppressWarnings(as.numeric(Netweight)),
        VAT_Revenue            = suppressWarnings(as.numeric(VAT_Revenue)),
        Effective_Customs_rate = suppressWarnings(as.numeric(Effective_Customs_rate)),
        Effective_Excise_rate  = suppressWarnings(as.numeric(Effective_Excise_rate)),
        Effective_VAT_rate     = suppressWarnings(as.numeric(Effective_VAT_rate)),
        ExciseBase             = suppressWarnings(as.numeric(ExciseBase))
      )
    
    data
  }
  
  first_or_default <- function(x, default = 0) {
    if (length(x) == 0 || all(is.na(x))) return(default)
    val <- x[1]
    if (is.na(val)) default else val
  }
  
  first_or_text <- function(x, default = "NA") {
    if (length(x) == 0 || all(is.na(x))) return(default)
    val <- as.character(x[1])
    if (is.na(val) || identical(val, "NA")) default else val
  }
  
  empty_sim_table <- function() {
    data.table(
      Year = integer(),
      Chapters_description = character(),
      Description_Chapters = character(),
      HS_code = character(),
      Effective_Customs_rate = numeric(),
      Effective_Excise_rate = numeric(),
      Effective_VAT_rate = numeric()
    )
  }
  
  show_center_message <- function(title = "Message", message = "", easyClose = TRUE) {
    showModal(
      modalDialog(
        title = title,
        message,
        easyClose = easyClose,
        footer = modalButton("OK")
      )
    )
  }
  
  make_info_box <- function(title, value, icon_name, color_name) {
    infoBox(
      title = title,
      value = value,
      icon = icon(icon_name),
      color = color_name,
      width = 12,
      fill = TRUE
    )
  }
  
  clear_visual_outputs <- function() {
    output$infoBoxRev1 <- renderUI({ NULL })
    output$infoBoxRev2 <- renderUI({ NULL })
    output$additionalChartsRev <- renderUI({ NULL })
    
    output$infoBoxTE1 <- renderUI({ NULL })
    output$infoBoxTE2 <- renderUI({ NULL })
    output$additionalChartsTE <- renderUI({ NULL })
  }
  
  get_forecast_years <- reactive({
    req(excelData())
    observed_years <- sort(unique(na.omit(excelData()$Year)))
    if (length(observed_years) == 0) return(integer(0))
    
    base_year_local <- min(observed_years, na.rm = TRUE)
    forecast_end <- max(
      max(observed_years, na.rm = TRUE),
      base_year_local + 5L,
      input$SimulationYear
    )
    
    seq(base_year_local, forecast_end)
  })
  
  build_missing_future_row <- function(raw_dt, sim_row) {
    candidates <- raw_dt[
      HS_code == sim_row$HS_code &
        Chapters_description == sim_row$Chapters_description &
        Description_Chapters == sim_row$Description_Chapters
    ]
    
    if (nrow(candidates) == 0) {
      candidates <- raw_dt[HS_code == sim_row$HS_code]
    }
    
    if (nrow(candidates) == 0) {
      return(NULL)
    }
    
    template_row <- candidates[order(-Year)][1]
    template_row$Year <- as.integer(sim_row$Year)
    template_row$Effective_Customs_rate <- as.numeric(sim_row$Effective_Customs_rate)
    template_row$Effective_Excise_rate <- as.numeric(sim_row$Effective_Excise_rate)
    template_row$Effective_VAT_rate <- as.numeric(sim_row$Effective_VAT_rate)
    
    template_row
  }
  
  prepare_simulation_dataset <- function() {
    req(excelData())
    
    sim_tbl <- copy(customs_simulation_parameters_updated())
    raw_copy <- as.data.table(copy(get("customs_simulation_parameters_raw", envir = .GlobalEnv)))
    
    if (nrow(sim_tbl) > 0) {
      for (i in seq_len(nrow(sim_tbl))) {
        row_i <- sim_tbl[i, ]
        
        idx <- which(
          raw_copy$Year == row_i$Year &
            raw_copy$Chapters_description == row_i$Chapters_description &
            raw_copy$Description_Chapters == row_i$Description_Chapters &
            raw_copy$HS_code == row_i$HS_code
        )
        
        if (length(idx) > 0) {
          raw_copy$Effective_Customs_rate[idx] <- row_i$Effective_Customs_rate
          raw_copy$Effective_Excise_rate[idx] <- row_i$Effective_Excise_rate
          raw_copy$Effective_VAT_rate[idx] <- row_i$Effective_VAT_rate
        } else {
          new_row <- build_missing_future_row(raw_copy, row_i)
          if (!is.null(new_row)) {
            raw_copy <- rbind(raw_copy, new_row, fill = TRUE)
          }
        }
      }
    }
    
    setorder(raw_copy, Year, Chapters_description, Description_Chapters, HS_code)
    assign("ValueTableUpdate", sim_tbl, envir = .GlobalEnv)
    assign("customs_simulation_parameters_updated", raw_copy, envir = .GlobalEnv)
    
    invisible(raw_copy)
  }
  
  simulationComment <- reactiveVal("")
  excelData <- reactiveVal(NULL)
  reactive_simulation_results <- reactiveVal(NULL)
  
  selectedData <- reactive({
    req(excelData(), input$yearSelect)
    excelData() %>% filter(Year == input$yearSelect)
  })
  
  selectedRow <- reactive({
    req(excelData(), input$yearSelect)
    req(input$Chapters_description, input$Chapter_Select, input$HS_codeSelect)
    
    out <- excelData() %>%
      filter(
        Year == input$yearSelect,
        Chapters_description == input$Chapters_description,
        Description_Chapters == input$Chapter_Select,
        HS_code == input$HS_codeSelect
      )
    
    if (nrow(out) == 0) return(NULL)
    out[1, , drop = FALSE]
  })
  
  output$simulationComment <- renderUI({
    txt <- simulationComment()
    if (is.null(txt) || txt == "") return(NULL)
    
    tags$div(
      style = "color:#b22222; font-weight:bold; margin-top:8px;",
      txt
    )
  })
  
  output$headerImage <- renderUI({
    img_data <- base64enc::dataURI(file = "img/WB_pic.png", mime = "image/png")
    tags$img(src = img_data, height = "40px", style = "float:left; margin-right:20px;")
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
  
  observe({
    assign("SimulationYear", input$SimulationYear, envir = .GlobalEnv)
  })
  
  # IV. Import Excel ---------------------------------------------------------
  observeEvent(input$importExcel, {
    req(input$fileInput)
    
    inFile <- input$fileInput
    data <- read_excel(inFile$datapath, col_names = input$hasHeader)
    data <- clean_import_data(data)
    
    required_check <- c(
      "Chapters_description", "Description_Chapters", "HS_code",
      "SupplementaryUnit", "CustomsRate_MFN", "CustomsRate_CEFTA",
      "CustomsRate_MSA", "CustomsRate_TR", "ExciseRate", "VAT_Rate",
      "Effective_Customs_rate", "Effective_Excise_rate",
      "Effective_VAT_rate", "Year"
    )
    
    if (!all(required_check %in% colnames(data))) {
      show_center_message("Error", "The Excel file must contain all required columns, including Year.")
      return()
    }
    
    if (all(is.na(data$Year))) {
      show_center_message("Error", "Column 'Year' exists, but all values are missing.")
      return()
    }
    
    excelData(data)
    assign("customs_simulation_parameters_raw", data, envir = .GlobalEnv)
    
    forecast_years_now <- {
      observed_years <- sort(unique(na.omit(data$Year)))
      base_year_local <- min(observed_years, na.rm = TRUE)
      seq(base_year_local, max(max(observed_years, na.rm = TRUE), base_year_local + 5L))
    }
    
    updateSliderInput(
      session, "SimulationYear",
      min = min(forecast_years_now),
      max = max(forecast_years_now),
      value = min(forecast_years_now)
    )
    
    customs_simulation_parameters_updated(empty_sim_table())
    simulationComment("")
  })
  
  # V. Dynamic dropdowns -----------------------------------------------------
  output$yearSelectUI <- renderUI({
    req(excelData())
    years_available <- sort(unique(na.omit(excelData()$Year)))
    selectInput("yearSelect", "Year Selection", choices = years_available, selected = min(years_available))
  })
  
  output$Chapters_description <- renderUI({
    req(selectedData())
    choices <- selectedData() %>%
      pull(Chapters_description) %>%
      unique() %>%
      sort()
    selectInput("Chapters_description", "Chapter Selection", choices = choices)
  })
  
  output$Chapter_Select <- renderUI({
    req(selectedData(), input$Chapters_description)
    choices <- selectedData() %>%
      filter(Chapters_description == input$Chapters_description) %>%
      pull(Description_Chapters) %>%
      unique() %>%
      sort()
    selectInput("Chapter_Select", "Tariff Number Selection", choices = choices)
  })
  
  output$HS_codeSelect <- renderUI({
    req(selectedData(), input$Chapters_description, input$Chapter_Select)
    choices <- selectedData() %>%
      filter(
        Chapters_description == input$Chapters_description,
        Description_Chapters == input$Chapter_Select
      ) %>%
      pull(HS_code) %>%
      unique() %>%
      sort()
    selectInput("HS_codeSelect", "Selected Tariff line", choices = choices)
  })
  
  output$default_SupplementaryUnit <- renderUI({
    row <- selectedRow()
    textInput(
      "default_SupplementaryUnit",
      "Supplementary Unit",
      value = if (is.null(row)) "NA" else first_or_text(row$SupplementaryUnit, "NA"),
      width = "100%"
    )
  })
  
  observeEvent(
    list(input$yearSelect, input$Chapters_description, input$Chapter_Select, input$HS_codeSelect),
    {
      row <- selectedRow()
      
      if (is.null(row)) {
        updateNumericInput(session, "default_Value", value = 0)
        updateNumericInput(session, "default_Quantity", value = 0)
        updateNumericInput(session, "default_Netweight", value = 0)
        updateNumericInput(session, "default_CustomsRate_MFN", value = 0)
        updateNumericInput(session, "default_CustomsRate_TR", value = 0)
        updateNumericInput(session, "default_CustomsRate_CEFTA", value = 0)
        updateNumericInput(session, "default_CustomsRate_MSA", value = 0)
        updateNumericInput(session, "default_ExciseRate", value = 0)
        updateNumericInput(session, "default_VAT", value = 0)
        updateNumericInput(session, "default_Effective_Customs_rate", value = 0)
        updateNumericInput(session, "default_Effective_Excise_rate", value = 0)
        updateNumericInput(session, "default_Effective_VAT_rate", value = 0)
        return()
      }
      
      updateNumericInput(session, "default_Value", value = first_or_default(row$Value, 0))
      updateNumericInput(session, "default_Quantity", value = first_or_default(row$Quantity, 0))
      updateNumericInput(session, "default_Netweight", value = first_or_default(row$Netweight, 0))
      updateNumericInput(session, "default_CustomsRate_MFN", value = first_or_default(row$CustomsRate_MFN, 0))
      updateNumericInput(session, "default_CustomsRate_TR", value = first_or_default(row$CustomsRate_TR, 0))
      updateNumericInput(session, "default_CustomsRate_CEFTA", value = first_or_default(row$CustomsRate_CEFTA, 0))
      updateNumericInput(session, "default_CustomsRate_MSA", value = first_or_default(row$CustomsRate_MSA, 0))
      updateNumericInput(session, "default_ExciseRate", value = first_or_default(row$ExciseRate, 0))
      updateNumericInput(session, "default_VAT", value = first_or_default(row$VAT_Rate, 0))
      updateNumericInput(session, "default_Effective_Customs_rate", value = first_or_default(row$Effective_Customs_rate, 0))
      updateNumericInput(session, "default_Effective_Excise_rate", value = first_or_default(row$Effective_Excise_rate, 0))
      updateNumericInput(session, "default_Effective_VAT_rate", value = first_or_default(row$Effective_VAT_rate, 0))
      
      simulationComment("")
    },
    ignoreInit = TRUE
  )
  
  # VI. Parameters table -----------------------------------------------------
  customs_simulation_parameters_updated <- reactiveVal(empty_sim_table())
  
  observeEvent(input$addValuesValue, {
    req(input$Chapters_description, input$Chapter_Select, input$HS_codeSelect, excelData())
    
    row0 <- selectedRow()
    if (is.null(row0)) {
      show_center_message("Error", "No valid tariff line is currently selected.")
      return()
    }
    
    new_ecr <- as.numeric(input$default_Effective_Customs_rate)
    new_eer <- as.numeric(input$default_Effective_Excise_rate)
    new_evr <- as.numeric(input$default_Effective_VAT_rate)
    
    simulationComment("")
    forecast_years_now <- get_forecast_years()
    
    if (isTRUE(input$toggleYears)) {
      years_to_apply <- forecast_years_now[forecast_years_now >= input$SimulationYear]
    } else {
      years_to_apply <- as.integer(input$yearSelect)
    }
    
    if (length(years_to_apply) == 0) {
      show_center_message("Error", "No valid years found for simulation.")
      return()
    }
    
    newEntries <- rbindlist(lapply(years_to_apply, function(yy) {
      data.table(
        Year = as.integer(yy),
        Chapters_description = input$Chapters_description,
        Description_Chapters = input$Chapter_Select,
        HS_code = input$HS_codeSelect,
        Effective_Customs_rate = new_ecr,
        Effective_Excise_rate = new_eer,
        Effective_VAT_rate = new_evr
      )
    }))
    
    updatedTable <- copy(customs_simulation_parameters_updated())
    
    if (nrow(updatedTable) > 0) {
      dup_check <- merge(
        updatedTable,
        newEntries,
        by = c("Year", "Chapters_description", "Description_Chapters", "HS_code"),
        all = FALSE
      )
      
      if (nrow(dup_check) > 0) {
        show_center_message("Duplicate Parameter", "The selected parameter already exists in the simulation table.")
        return()
      }
    }
    
    updatedTable <- rbind(updatedTable, newEntries, fill = TRUE)
    setorder(updatedTable, Year, Chapters_description, Description_Chapters, HS_code)
    
    customs_simulation_parameters_updated(updatedTable)
    simulationComment("")
  })
  
  observeEvent(input$clearValuesTable, {
    customs_simulation_parameters_updated(empty_sim_table())
    simulationComment("")
  })
  
  observeEvent(input$removeLastRow, {
    tbl <- copy(customs_simulation_parameters_updated())
    
    if (nrow(tbl) == 0) {
      show_center_message("Message", "The simulation table is already empty.")
      return()
    }
    
    tbl <- tbl[-nrow(tbl)]
    customs_simulation_parameters_updated(tbl)
    simulationComment("")
  })
  
  output$customs_simulation_parameters_updated <- renderDT({
    datatable(
      customs_simulation_parameters_updated(),
      options = list(dom = "t", paging = FALSE),
      editable = TRUE,
      rownames = FALSE
    )
  }, server = TRUE)
  
  # VII. Save updated parameters --------------------------------------------
  observeEvent(input$savecustoms_simulation_parameters_updated, {
    req(excelData())
    prepare_simulation_dataset()
    show_center_message("Success", "Simulation parameters saved successfully.")
  })
  
  # VIII. Toggle and tab logic ----------------------------------------------
  observeEvent(input$sidebar_tabs, {
    if (identical(input$sidebar_tabs, "Tax_Expenditures_Charts") && !isTRUE(input$toggleSimulationRates)) {
      show_center_message(
        title = "Tax Expenditures Visualization",
        message = "This tab is available only when the 'Toggle Tax Expenditures' option is switched ON."
      )
      updateTabItems(session, "sidebar_tabs", selected = "Customs_Revenues")
    }
    
    if (input$sidebar_tabs %in% c("MainResultsTE", "MainResultsTE_Excise") && !isTRUE(input$toggleSimulationRates)) {
      show_center_message(
        title = "Tax Expenditures Tables",
        message = "These tables are available only when the 'Toggle Tax Expenditures' option is switched ON."
      )
      updateTabItems(session, "sidebar_tabs", selected = "RevenueImpactSimulation")
    }
    
    if (input$sidebar_tabs %in% c("Customs_Revenues", "Tax_Expenditures_Charts")) {
      clear_visual_outputs()
      updateCharts()
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$toggleSimulationRates, {
    clear_visual_outputs()
    
    if (!isTRUE(input$toggleSimulationRates) && identical(input$sidebar_tabs, "Tax_Expenditures_Charts")) {
      updateTabItems(session, "sidebar_tabs", selected = "Customs_Revenues")
    } else {
      updateCharts()
    }
  }, ignoreInit = TRUE)
  
  output$taxExpTabUI <- renderUI({
    if (!isTRUE(input$toggleSimulationRates)) {
      return(
        fluidRow(
          column(
            12,
            box(
              width = 12,
              title = "Tax Expenditures",
              status = "warning",
              solidHeader = TRUE,
              p("This visualization tab is available only when the 'Toggle Tax Expenditures' option is switched ON.")
            )
          )
        )
      )
    }
    
    if (is.null(reactive_simulation_results())) {
      return(
        fluidRow(
          column(
            12,
            box(
              width = 12,
              title = "Tax Expenditures",
              status = "primary",
              solidHeader = TRUE,
              p("Toggle is ON. Please run the simulation first to display tax expenditure charts.")
            )
          )
        )
      )
    }
    
    tagList(
      fluidRow(
        column(
          6,
          selectInput(
            "chartSelectTaxExpenditures", "Select Chart",
            choices = c(
              "Tax_Expenditures_Charts_Customs",
              "Tax_Expenditures_Charts_Excise"
            ),
            selected = "Tax_Expenditures_Charts_Customs"
          )
        )
      ),
      fluidRow(
        column(6, uiOutput("infoBoxTE1")),
        column(6, uiOutput("infoBoxTE2"))
      ),
      fluidRow(
        column(12, uiOutput("additionalChartsTE"))
      )
    )
  })
  
  # IX. Run simulation -------------------------------------------------------
  observeEvent(input$calc_Sim_Button, {
    req(excelData())
    
    prepare_simulation_dataset()
    
    showModal(modalDialog(
      title = "Running Simulation...",
      "Please wait while the simulation is running...",
      easyClose = FALSE,
      footer = NULL
    ))
    
    future({
      source(paste0(path1, "/Scripts/Customs/TaxCalculator.R"))
      source(paste0(path1, "/Scripts/Customs/Calc_aggregate_data.R"))
      
      if (input$toggleSimulationRates) {
        source(paste0(path1, "/Scripts/Customs/Calc-TaxExpenditures.R"))
        summary_TE_SIM <- get("summary_TE_SIM", envir = .GlobalEnv)
        summary_TE_SIM_Excise <- get("summary_TE_SIM_Excise", envir = .GlobalEnv)
      } else {
        summary_TE_SIM <- NULL
        summary_TE_SIM_Excise <- NULL
      }
      
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
        footer = modalButton("OK")
      ))
      
      reactive_simulation_results(results)
      clear_visual_outputs()
      updateCharts()
      
    }) %...!% (function(e) {
      removeModal()
      showModal(modalDialog(
        title = "Error",
        paste("Error during calculation:", e$message),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
  })
  
  # X. Tables ----------------------------------------------------------------
  output$CUSTOMS_SUMMARY_TABLES <- renderDT({
    req(reactive_simulation_results())
    fh <- get_forecast_years()
    
    datatable(
      reactive_simulation_results()$Customs_summary,
      caption = tags$caption(
        paste("Customs Duties Projections,", min(fh), "-", max(fh)),
        class = "table-caption-bold"
      ),
      extensions = "Buttons",
      options = list(
        pageLength = 15,
        dom = "Blfrtip",
        buttons = c("copy", "csv", "print"),
        autoWidth = TRUE,
        escape = FALSE,
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      ),
      rownames = FALSE
    )
  })
  
  output$EXCISE_SUMMARY_TABLES <- renderDT({
    req(reactive_simulation_results())
    fh <- get_forecast_years()
    
    datatable(
      reactive_simulation_results()$Excise_summary,
      caption = tags$caption(
        paste("Excise Projections,", min(fh), "-", max(fh)),
        class = "table-caption-bold"
      ),
      extensions = "Buttons",
      options = list(
        pageLength = 15,
        dom = "Blfrtip",
        buttons = c("copy", "csv", "print"),
        autoWidth = TRUE,
        escape = FALSE,
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      ),
      rownames = FALSE
    )
  })
  
  output$VAT_SUMMARY_TABLES <- renderDT({
    req(reactive_simulation_results())
    fh <- get_forecast_years()
    
    datatable(
      reactive_simulation_results()$VAT_summary,
      caption = tags$caption(
        paste("Import VAT Projections,", min(fh), "-", max(fh)),
        class = "table-caption-bold"
      ),
      extensions = "Buttons",
      options = list(
        pageLength = 15,
        dom = "Blfrtip",
        buttons = c("copy", "csv", "print"),
        autoWidth = TRUE,
        escape = FALSE,
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      ),
      rownames = FALSE
    )
  })
  
  output$TOTAL_REVENUES_SUMMARY_TABLES <- renderDT({
    req(reactive_simulation_results())
    fh <- get_forecast_years()
    
    datatable(
      reactive_simulation_results()$Total_import_summary,
      caption = tags$caption(
        paste("Total Import Taxes,", min(fh), "-", max(fh)),
        class = "table-caption-bold"
      ),
      extensions = "Buttons",
      options = list(
        pageLength = 15,
        dom = "Blfrtip",
        buttons = c("copy", "csv", "print"),
        autoWidth = TRUE,
        escape = FALSE,
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      ),
      rownames = FALSE
    )
  })
  
  output$TE_TABLES <- renderDT({
    req(input$toggleSimulationRates)
    req(reactive_simulation_results())
    req(reactive_simulation_results()$summary_TE_SIM)
    fh <- get_forecast_years()
    base_year_local <- min(fh)
    
    datatable(
      reactive_simulation_results()$summary_TE_SIM,
      caption = tags$caption(
        #paste("Tax Expenditures in EUR MIL,", base_year_local),
        paste("Tax Expenditures in EUR MIL"),
        class = "table-caption-bold"
      ),
      extensions = "Buttons",
      options = list(
        pageLength = 15,
        dom = "Blfrtip",
        buttons = c("copy", "csv", "excel", "print"),
        autoWidth = TRUE,
        escape = FALSE,
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      ),
      rownames = FALSE
    )
  })
  
  output$TE_TABLES_Excise <- renderDT({
    req(input$toggleSimulationRates)
    req(reactive_simulation_results())
    req(reactive_simulation_results()$summary_TE_SIM_Excise)
    fh <- get_forecast_years()
    base_year_local <- min(fh)
    
    datatable(
      reactive_simulation_results()$summary_TE_SIM_Excise,
      caption = tags$caption(
        #paste("Tax Expenditures in EUR MIL,", base_year_local),
        paste("Tax Expenditures in EUR MIL"),
        class = "table-caption-bold"
      ),
      extensions = "Buttons",
      options = list(
        pageLength = 15,
        dom = "Blfrtip",
        buttons = c("copy", "csv", "excel", "print"),
        autoWidth = TRUE,
        escape = FALSE,
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      ),
      rownames = FALSE
    )
  })
  
  # XI. Charts ----------------------------------------------------------------
  updateCharts <- function() {
    req(excelData())
    
    fh <- get_forecast_years()
    sim_year <- input$SimulationYear
    active_tab <- input$sidebar_tabs
    
    if (is.null(fh) || length(fh) == 0) {
      clear_visual_outputs()
      return()
    }
    
    if (identical(active_tab, "Tax_Expenditures_Charts")) {
      if (!isTRUE(input$toggleSimulationRates)) {
        clear_visual_outputs()
        return()
      }
      
      res <- reactive_simulation_results()
      if (is.null(res)) {
        clear_visual_outputs()
        return()
      }
      
      chart_type <- input$chartSelectTaxExpenditures
      if (is.null(chart_type) || length(chart_type) == 0) {
        clear_visual_outputs()
        return()
      }
      
      if (chart_type == "Tax_Expenditures_Charts_Customs") {
        if (!exists("ProjectionTE_customs", envir = .GlobalEnv) ||
            !exists("CustomsDuties_TE_agg_countries_tbl_agg", envir = .GlobalEnv) ||
            !exists("CustomsDuties_TE_Chapters", envir = .GlobalEnv) ||
            !exists("CustomsDuties_TE_MTN", envir = .GlobalEnv) ||
            is.null(res$summary_TE_SIM)) {
          clear_visual_outputs()
          return()
        }
        
        source(paste0(path1, "/Scripts/Customs/Charts-TaxExpenditures_Customs.R"))
        
        charts_te <- TE_Charts_Customs_fun(
          get("ProjectionTE_customs", envir = .GlobalEnv),
          get("CustomsDuties_TE_agg_countries_tbl_agg", envir = .GlobalEnv),
          get("CustomsDuties_TE_Chapters", envir = .GlobalEnv),
          get("CustomsDuties_TE_MTN", envir = .GlobalEnv),
          range(fh),
          sim_year
        )
        
        # output$infoBoxTE1 <- renderUI({
        #   GDP_share_TE <- res$summary_TE_SIM %>%
        #     filter(Description == "Tax Expenditures(without FTA) as % of GDP")
        #   selected_value <- GDP_share_TE$Value
        #   make_info_box(
        #    # "Total Tax Expenditures as PCT of GDP",
        #     #paste0(selected_value, " %"),
        #     "hand-holding-usd",
        #     "orange"
        #   )
        # })
        # 
        # output$infoBoxTE2 <- renderUI({
        #   TE_NOMINAL <- res$summary_TE_SIM %>%
        #     filter(Description == "Tax Expenditures(without FTA)")
        #   selected_value <- round(TE_NOMINAL$Value, 2)
        #   make_info_box(
        #     #"Total Tax Expenditures",
        #     #paste0(selected_value, " EUR MIL"),
        #     "chart-pie",
        #     "light-blue"
        #   )
        # })
        
        output$infoBoxTE1 <- renderUI({
          infoBox(
            title = "",
            value = "",
            icon = icon("hand-holding-usd"),
            color = "orange",
            width = 12,
            fill = TRUE
          )
        })
        
        output$infoBoxTE2 <- renderUI({
          infoBox(
            title = "",
            value = "",
            icon = icon("chart-pie"),
            color = "light-blue",
            width = 12,
            fill = TRUE
          )
        })
        
        output$additionalChartsTE <- renderUI({
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
      
      if (chart_type == "Tax_Expenditures_Charts_Excise") {
        if (!exists("ProjectionTE_Excise", envir = .GlobalEnv) ||
            !exists("Legal_Fuels_Cars_Products", envir = .GlobalEnv) ||
            is.null(res$summary_TE_SIM_Excise)) {
          clear_visual_outputs()
          return()
        }
        
        source(paste0(path1, "/Scripts/Customs/Charts-TaxExpenditures_Excise.R"))
        
        charts_te <- TE_Charts_Excise_fun(
          get("ProjectionTE_Excise", envir = .GlobalEnv),
          res$summary_TE_SIM_Excise,
          get("Legal_Fuels_Cars_Products", envir = .GlobalEnv),
          range(fh),
          sim_year
        )
        
        # output$infoBoxTE1 <- renderUI({
        #   GDP_share_TE <- res$summary_TE_SIM_Excise %>%
        #     filter(Description == "Tax Expenditures as % of GDP")
        #   selected_value <- GDP_share_TE$value
        #   make_info_box(
        #     #"Total Tax Expenditures as PCT of GDP",
        #     #paste0(selected_value, " %"),
        #     "hand-holding-usd",
        #     "orange"
        #   )
        # })
        # 
        # output$infoBoxTE2 <- renderUI({
        #   TE_NOMINAL <- res$summary_TE_SIM_Excise %>%
        #     filter(Description == "Tax Expenditures")
        #   selected_value <- round(TE_NOMINAL$value, 2)
        #   make_info_box(
        #     #"Total Tax Expenditures",
        #     #paste0(selected_value, " EUR MIL"),
        #     "chart-pie",
        #     "light-blue"
        #   )
        # })
        
        output$infoBoxTE1 <- renderUI({
          infoBox(
            title = "",
            value = "",
            icon = icon("hand-holding-usd"),
            color = "orange",
            width = 12,
            fill = TRUE
          )
        })
        
        output$infoBoxTE2 <- renderUI({
          infoBox(
            title = "",
            value = "",
            icon = icon("chart-pie"),
            color = "light-blue",
            width = 12,
            fill = TRUE
          )
        })
        
        
        output$additionalChartsTE <- renderUI({
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
      
      return()
    }
    
    chart_type <- input$chartSelectRevenues
    if (is.null(chart_type) || length(chart_type) == 0) {
      clear_visual_outputs()
      return()
    }
    
    if (!exists("merged_Customs_BU_SIM", envir = .GlobalEnv)) {
      clear_visual_outputs()
      return()
    }
    
    if (chart_type == "Revenue_Impact_Customs") {
      source(paste0(path1, "/Scripts/Customs/Charts-Customs_Revenues.R"))
      
      charts <- Revenue_Impact(
        merged_Customs_BU_SIM,
        TypeRev_customs_data,
        Chapter_customs_data_agg,
        MTN_customs_data_long,
        fh
      )
      
      output$infoBoxRev1 <- renderUI({
        result1 <- merged_Customs_BU_SIM %>%
          filter(year == sim_year) %>%
          pull(calc_customs_duties_bu) %>%
          as.numeric()
        make_info_box(
          "Baseline Customs revenues",
          paste(round(result1, 1), "(EUR MIL)"),
          "coins",
          "orange"
        )
      })
      
      output$infoBoxRev2 <- renderUI({
        result1 <- merged_Customs_BU_SIM %>%
          filter(year == sim_year) %>%
          pull(calc_customs_duties_sim) %>%
          as.numeric()
        make_info_box(
          "Simulation Customs revenues",
          paste(round(result1, 1), "(EUR MIL)"),
          "chart-line",
          "light-blue"
        )
      })
      
      output$additionalChartsRev <- renderUI({
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
    
    if (chart_type == "Revenue_Impact_Excise") {
      source(paste0(path1, "/Scripts/Customs/Charts-Excise_Revenues.R"))
      
      charts <- Revenue_Impact_Excise_fun(
        merged_Customs_BU_SIM,
        TypeRev_Excise_data,
        Chapter_Excise_data_agg,
        MTN_Excise_data_agg,
        fh
      )
      
      output$infoBoxRev1 <- renderUI({
        result1 <- merged_Customs_BU_SIM %>%
          filter(year == sim_year) %>%
          pull(calc_excise_bu) %>%
          as.numeric()
        make_info_box(
          "Baseline Excise revenues",
          paste(round(result1, 1), "(EUR MIL)"),
          "coins",
          "orange"
        )
      })
      
      output$infoBoxRev2 <- renderUI({
        result1 <- merged_Customs_BU_SIM %>%
          filter(year == sim_year) %>%
          pull(calc_excise_sim) %>%
          as.numeric()
        make_info_box(
          "Simulation Excise revenues",
          paste(round(result1, 1), "(EUR MIL)"),
          "chart-line",
          "light-blue"
        )
      })
      
      output$additionalChartsRev <- renderUI({
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
    
    if (chart_type == "Revenue_import_tax_total") {
      source(paste0(path1, "/Scripts/Customs/Charts-Import_Duties.R"))
      
      charts <- Total_Import_duties_fun(
        merged_Customs_BU_SIM,
        TypeRev_import_duties_data,
        Chapter_import_duties_agg,
        MTN_import_duties_agg,
        fh
      )
      
      output$infoBoxRev1 <- renderUI({
        result1 <- merged_Customs_BU_SIM %>%
          filter(year == sim_year) %>%
          summarise(total_sum = sum(calc_customs_duties_bu + calc_excise_bu + calc_vat_bu, na.rm = TRUE)) %>%
          pull(total_sum) %>%
          as.numeric()
        make_info_box(
          "Baseline Total Import Tax Revenue",
          paste(round(result1, 1), "(EUR MIL)"),
          "coins",
          "orange"
        )
      })
      
      output$infoBoxRev2 <- renderUI({
        result1 <- merged_Customs_BU_SIM %>%
          filter(year == sim_year) %>%
          summarise(total_sum = sum(calc_customs_duties_sim + calc_excise_sim + calc_vat_sim, na.rm = TRUE)) %>%
          pull(total_sum) %>%
          as.numeric()
        make_info_box(
          "Simulation Total Import Tax Revenue",
          paste(round(result1, 1), "(EUR MIL)"),
          "chart-line",
          "light-blue"
        )
      })
      
      output$additionalChartsRev <- renderUI({
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
    
    if (chart_type == "Revenue_Impact_VAT") {
      source(paste0(path1, "/Scripts/Customs/Charts-VAT_Revenues.R"))
      
      charts <- Revenue_Impact_VAT_fun(
        merged_Customs_BU_SIM,
        TypeRev_VAT_data,
        Chapter_VAT_data_agg,
        MTN_VAT_data_agg,
        fh
      )
      
      output$infoBoxRev1 <- renderUI({
        result1 <- merged_Customs_BU_SIM %>%
          filter(year == sim_year) %>%
          pull(calc_vat_bu) %>%
          as.numeric()
        make_info_box(
          "Baseline import VAT revenues",
          paste(round(result1, 1), "(EUR MIL)"),
          "coins",
          "orange"
        )
      })
      
      output$infoBoxRev2 <- renderUI({
        result1 <- merged_Customs_BU_SIM %>%
          filter(year == sim_year) %>%
          pull(calc_vat_sim) %>%
          as.numeric()
        make_info_box(
          "Simulation import VAT revenues",
          paste(round(result1, 1), "(EUR MIL)"),
          "chart-line",
          "light-blue"
        )
      })
      
      output$additionalChartsRev <- renderUI({
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
  
  observeEvent(input$chartSelectRevenues, {
    if (identical(input$sidebar_tabs, "Customs_Revenues")) {
      clear_visual_outputs()
      updateCharts()
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$chartSelectTaxExpenditures, {
    if (identical(input$sidebar_tabs, "Tax_Expenditures_Charts")) {
      clear_visual_outputs()
      updateCharts()
    }
  }, ignoreInit = TRUE)
}

shinyApp(ui = ui, server = server)