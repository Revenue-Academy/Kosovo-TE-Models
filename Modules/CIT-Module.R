library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(data.table)
library(readxl)
library(plotly)
library(base64enc)
library(dplyr)
library(tidyr)

options(scipen = 999)


# ============================================================
# CIT STARTUP: LOAD THE ENTIRE SAVED .Rdata WORKSPACE
# ============================================================
# The module runs in its own R process when started by the Toolkit.
# Restore EVERY saved object into THAT process's .GlobalEnv before any
# module initialization or simulation code executes. No name filtering.
# Override the chosen file with Sys.setenv(CIT_RDATA_FILE="C:/.../my.Rdata")
# in RStudio before launching the Toolkit, if the auto-detected file is wrong.

.cit_source_folder <- local({
  source_files <- unlist(lapply(sys.frames(), function(fr) {
    f <- get0("ofile", envir = fr, inherits = FALSE, ifnotfound = NULL)
    if (is.character(f) && length(f) == 1L && !is.na(f) && nzchar(f)) f
    else character(0)
  }), use.names = FALSE)
  source_files <- source_files[grepl("^CIT-Module.*\\.R$",
                                     basename(source_files), ignore.case = TRUE)]
  if (length(source_files)) {
    normalizePath(dirname(tail(source_files, 1L)), winslash = "/", mustWork = TRUE)
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  }
})

.cit_workspace_dirs <- unique(normalizePath(c(
  .cit_source_folder,
  file.path(.cit_source_folder, "Data", "CIT"),
  file.path(.cit_source_folder, "Data"),
  dirname(.cit_source_folder),
  file.path(dirname(.cit_source_folder), "Data", "CIT"),
  file.path(dirname(.cit_source_folder), "Data"),
  dirname(dirname(.cit_source_folder)),
  file.path(dirname(dirname(.cit_source_folder)), "Data", "CIT"),
  getwd(),
  file.path(getwd(), "Data", "CIT")
), winslash = "/", mustWork = FALSE))
.cit_workspace_dirs <- .cit_workspace_dirs[dir.exists(.cit_workspace_dirs)]

.cit_find_workspace <- function() {
  configured <- Sys.getenv("CIT_RDATA_FILE", unset = "")
  if (nzchar(configured)) {
    candidates <- unique(c(configured,
                           file.path(.cit_workspace_dirs, configured)))
    matches <- candidates[file.exists(candidates) & !dir.exists(candidates)]
    if (!length(matches)) stop(
      "CIT_RDATA_FILE does not point to an existing file: ", configured,
      "\nTried:\n", paste(candidates, collapse = "\n"), call. = FALSE
    )
    return(normalizePath(matches[1L], winslash = "/", mustWork = TRUE))
  }

  # Prefer the exact filename used by the working PIT module: .Rdata
  exact <- unlist(lapply(.cit_workspace_dirs, function(d) {
    file.path(d, c(".Rdata", ".RData", ".rdata", ".rda"))
  }), use.names = FALSE)
  exact <- exact[file.exists(exact) & !dir.exists(exact)]
  if (length(exact))
    return(normalizePath(exact[1L], winslash = "/", mustWork = TRUE))

  # Also allow a named workspace such as CIT-inputs.RData, but do not
  # silently select among multiple unrelated workspaces.
  found <- unique(unlist(lapply(.cit_workspace_dirs, function(d) {
    list.files(d, pattern = "\\.([Rr][Dd][Aa][Tt][Aa]|[Rr][Dd][Aa])$",
               all.files = TRUE, full.names = TRUE)
  }), use.names = FALSE))
  named <- found[grepl("CIT", basename(found), ignore.case = TRUE)]
  if (length(named) == 1L)
    return(normalizePath(named[1L], winslash = "/", mustWork = TRUE))
  if (length(found) == 1L)
    return(normalizePath(found[1L], winslash = "/", mustWork = TRUE))
  if (!length(found)) stop(
    "CIT: no .Rdata workspace was found. Put .Rdata beside CIT-Module.R ",
    "or in Data/CIT, or set CIT_RDATA_FILE to the full filename.\n",
    "Folders checked:\n", paste(.cit_workspace_dirs, collapse = "\n"),
    call. = FALSE
  )
  stop(
    "CIT: multiple candidate .Rdata workspaces exist. Set CIT_RDATA_FILE ",
    "to the correct full path. Files:\n",
    paste(found, collapse = "\n"), call. = FALSE
  )
}

.cit_workspace_file <- .cit_find_workspace()
message("CIT: loading ALL saved objects from ", .cit_workspace_file)
.cit_loaded_workspace_names <- tryCatch(
  load(.cit_workspace_file, envir = .GlobalEnv),
  error = function(e) stop(
    "CIT: failed to load .Rdata file: ", .cit_workspace_file,
    "\n", conditionMessage(e), call. = FALSE
  )
)
message("CIT: restored ", length(.cit_loaded_workspace_names),
        " objects (no filtering).")
message("CIT: objects in workspace: ",
        paste(.cit_loaded_workspace_names, collapse = ", "))

# Rdata files created on another machine can contain a stale path1.
# Restore the actual model root that contains Scripts/CIT, if found.
.cit_root_candidates <- unique(normalizePath(c(
  .cit_source_folder, dirname(.cit_source_folder),
  dirname(dirname(.cit_source_folder)), getwd(), dirname(getwd())
), winslash = "/", mustWork = FALSE))
.cit_script_roots <- .cit_root_candidates[vapply(
  .cit_root_candidates,
  function(d) dir.exists(file.path(d, "Scripts", "CIT")), logical(1)
)]
if (length(.cit_script_roots)) {
  path1 <- .cit_script_roots[1L]
  assign("path1", path1, envir = .GlobalEnv)
  message("CIT: calculation script root = ", path1)
} else {
  message("CIT warning: Scripts/CIT not found near the module. ",
          "Any path1 loaded from .Rdata will be used.")
}

# Step1 Derive forecast horizon safely from scenario_years_cit ----

if (!exists("scenario_years_cit", envir = .GlobalEnv, inherits = FALSE))
  stop("'scenario_years_cit' not found in GlobalEnv. Run the CIT data-prep script first.")

shiny_years_raw <- sort(unique(suppressWarnings(
  as.integer(get("scenario_years_cit", envir = .GlobalEnv))
)))

if (length(shiny_years_raw) == 0L || anyNA(shiny_years_raw))
  stop("scenario_years_cit must contain at least one valid integer year.")

shiny_scenarios_raw           <- paste0("t", seq_along(shiny_years_raw) - 1L)
shiny_scenario_lookup_cit     <- data.table(year     = shiny_years_raw,
                                            scenario = shiny_scenarios_raw)
shiny_simulation_year_min     <- min(shiny_years_raw)
shiny_simulation_year_max     <- max(shiny_years_raw)
shiny_simulation_year_default <- shiny_simulation_year_min

cat("CIT Shiny forecast horizon:", shiny_simulation_year_min,
    "-", shiny_simulation_year_max, "\n")
cat("Scenario labels:", paste(shiny_scenarios_raw, collapse = ", "), "\n")


# Step2 Reusable TE disabled message ----

te_disabled_message <- function() {
  tags$div(
    class = "te-disabled-panel",
    tags$p(icon("toggle-off")),
    tags$p(tags$strong("Tax Expenditure results are not available.")),
    tags$p(
      "To activate this section, go to ",
      tags$strong("Simulation Parameters"),
      ", enable the ", tags$em("Toggle Tax Expenditures"),
      " switch, and click ", tags$strong("Run Simulation"), "."
    )
  )
}


# Step3 UI ----

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    titleWidth = 280,
    title = tags$div(
      class = "header-flex",
      uiOutput("headerImage"),
      tags$span("CIT Module", class = "header-title")
    )
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Input", tabName = "input", icon = icon("file-excel")),
      menuItem(
        "Simulation Parameters", icon = icon("list-alt"), startExpanded = TRUE,
        menuSubItem("Policy Parameters", tabName = "PolicyParameters",
                    icon = icon("pen-to-square"))
      ),
      menuItem(
        "Results", icon = icon("magnifying-glass-chart"),
        menuSubItem("Revenue Impact",       tabName = "MainResultsSimulation",
                    icon = icon("gauge")),
        menuSubItem("Distribution Effects", tabName = "MainDistributionTables",
                    icon = icon("chart-column")),
        menuSubItem("Tax Contribution",     tabName = "MainResultBins",
                    icon = icon("chart-pie")),
        menuSubItem("Tax Expenditures",     tabName = "MainResultsTE",
                    icon = icon("wallet"))
      ),
      menuItem(
        "Visualizations", icon = icon("chart-simple"),
        menuSubItem("Dashboards", tabName = "PIT_Revenues",
                    icon = icon("chart-column"))
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tags$head(tags$style(HTML("
      body, .content-wrapper, .right-side, .content {
        font-family: 'Segoe UI', Arial, sans-serif;
        background-color: #f4f7fb !important;
      }
      .skin-blue .main-header .logo {
        background-color: #1f4e79 !important;
        font-weight: 600; font-size: 18px;
      }
      .skin-blue .main-header .navbar { background-color: #2f6c99 !important; }
      .skin-blue .main-sidebar,
      .skin-blue .treeview-menu       { background-color: #1e2d3b !important; }
      .skin-blue .sidebar-menu > li > a { color: #dbe6ef !important; }
      .skin-blue .sidebar-menu > li.active > a,
      .skin-blue .sidebar-menu > li.menu-open > a,
      .skin-blue .sidebar-menu > li:hover > a {
        background: #243849 !important; color: #ffffff !important;
      }
      .skin-blue .treeview-menu > li > a { color: #c8d4df !important; }
      .skin-blue .treeview-menu > li.active > a,
      .skin-blue .treeview-menu > li > a:hover {
        color: #ffffff !important; background: #2f3f4f !important;
      }
      .content { padding: 18px; }
      .box {
        border-radius: 10px; border-top: 3px solid #3c8dbc;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        background-color: #f4f7fb !important; margin-bottom: 22px;
      }
      .box-header.with-border, .box-body { background-color: #f4f7fb !important; }
      .header-flex  { display: flex; align-items: center; gap: 12px; }
      .header-title { font-size: 22px; font-weight: 600; white-space: nowrap; }
      .subtle-note  { color: #6c7a89; font-size: 13px; line-height: 1.5; margin-top: 12px; }
      .button-row, .right-button-row {
        display: flex; gap: 10px; flex-wrap: wrap; margin-top: 15px;
      }
      .btn         { border-radius: 6px !important; box-shadow: none !important; }
      .btn-primary { background-color: #2f6c99 !important; border-color: #2f6c99 !important; }
      .btn-success { background-color: #2a9d8f !important; border-color: #2a9d8f !important; }
      .btn-warning {
        background-color: #e9a03b !important; border-color: #e9a03b !important;
        color: #ffffff !important;
      }
      .form-control, .selectize-input {
        border-radius: 6px !important; background-color: #ffffff !important;
        border-color: #ccd6e0 !important;
      }
      .table-caption-bold { font-weight: 700; color: #22313f; }
      .te-disabled-panel {
        padding: 30px; text-align: center;
        color: #6c7a89; font-size: 15px; margin-top: 10px;
      }
      .te-disabled-panel .fa { font-size: 36px; color: #b0bec5; margin-bottom: 12px; }
      .progress-overlay {
        position: fixed; top: 0; left: 0;
        width: 100%; height: 100%;
        background: rgba(0,0,0,0.35);
        z-index: 99999; display: none;
      }
      .progress-center-box {
        position: absolute; top: 50%; left: 50%;
        transform: translate(-50%, -50%);
        width: 440px; max-width: 90%;
        background: #ffffff; border-radius: 12px;
        padding: 28px 28px 22px 28px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.20);
        text-align: center;
      }
      .progress-center-title {
        font-size: 20px; font-weight: 700;
        color: #1f2d3d; margin-bottom: 10px;
      }
      .progress-center-text {
        font-size: 14px; color: #5c6b7a; margin-bottom: 14px;
      }
      .progress { height: 18px; margin-bottom: 0; border-radius: 10px; overflow: hidden; }
      .progress-bar { background-color: #2f6c99 !important; }
    "))),
    
    # Progress overlay
    div(
      id = "citProgressOverlay", class = "progress-overlay",
      div(
        class = "progress-center-box",
        div(class = "progress-center-title", "Running CIT Simulation"),
        div(id = "citProgressText", class = "progress-center-text",
            "Preparing simulation..."),
        div(
          class = "progress",
          div(id = "citProgressBar",
              class = "progress-bar progress-bar-striped active",
              role = "progressbar", style = "width: 0%;")
        )
      )
    ),
    
    tabItems(
      
      # ---- Input ----
      tabItem(
        tabName = "input",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = FALSE,
            title = tags$span(icon("file-excel"), " Data Input"),
            column(
              width = 6,
              selectInput("inputType", "Data Source",
                          choices  = c("Manual", "Excel File"),
                          selected = "Excel File"),
              conditionalPanel(
                condition = "input.inputType == 'Excel File'",
                fileInput("fileInput", "Upload Excel File", accept = ".xlsx"),
                checkboxInput("hasHeader", "Header", TRUE)
              ),
              actionButton("importExcel", "Import Excel Data",
                           icon = icon("file-import"), class = "btn-primary")
            )
          )
        )
      ),
      
      # ---- Policy Parameters ----
      tabItem(
        tabName = "PolicyParameters",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = FALSE,
            title = tags$span(icon("sliders"), " Simulation Settings"),
            column(
              width = 4,
              sliderInput("SimulationYear", "Setting Simulation Year",
                          min   = shiny_simulation_year_min,
                          max   = shiny_simulation_year_max,
                          step  = 1,
                          value = shiny_simulation_year_default,
                          width = "100%", round = 0, sep = ""),
              uiOutput("PolicyParameter"),
              uiOutput("Descriptions_Select"),
              uiOutput("LongNameSelect"),
              div(
                class = "button-row",
                actionButton("addValuesValue",   "Add to Table",
                             icon = icon("plus"),   class = "btn-success"),
                actionButton("removeLastRow",    "Remove last row",
                             icon = icon("minus"),  class = "btn-primary"),
                actionButton("clearValuesTable", "Clear Table",
                             icon = icon("eraser"), class = "btn-warning")
              )
            ),
            column(
              width = 4,
              uiOutput("YearSelect"),
              numericInput("default_Value", "Value", value = 0, min = 0, step = 0.01)
            ),
            column(
              width = 4,
              br(),
              prettySwitch("toggleSimulationRates", "Toggle Tax Expenditures",
                           value = FALSE, status = "primary", fill = TRUE),
              p(class = "subtle-note",
                "When enabled, the simulation also calculates tax-expenditure results and charts."),
              tags$hr(),
              prettySwitch("apply_next_years", "Apply on next years",
                           value = FALSE, status = "primary", fill = TRUE),
              p(class = "subtle-note",
                paste0("When enabled, the selected CIT parameter value is applied from ",
                       "the simulation year through the remaining forecast years."))
            )
          )
        ),
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = FALSE,
            title = tags$span(icon("table"), " Selected Simulation Parameters"),
            DTOutput("cit_simulation_parameters_updated_tbl"),
            div(
              class = "right-button-row",
              actionButton("savepit_simulation_parameters_updated", "Save Data",
                           icon = icon("floppy-disk"), class = "btn-primary"),
              actionButton("calc_Customs_Sim_Button", "Run Simulation",
                           icon = icon("play"), class = "btn-success")
            )
          )
        )
      ),
      
      # ---- Revenue Impact ----
      tabItem(
        tabName = "MainResultsSimulation",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = FALSE,
              title = tags$span(icon("coins"), " CIT Revenue Impact"),
              DTOutput("CIT_SUMMARY_TABLES"))
        )
      ),
      
      # ---- Distribution Effects ----
      tabItem(
        tabName = "MainDistributionTables",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = FALSE,
              title = tags$span(icon("chart-column"), " Distribution Effects"),
              DTOutput("DIST_TABLES"))
        )
      ),
      
      # ---- Tax Contribution ----
      tabItem(
        tabName = "MainResultBins",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = FALSE,
              title = tags$span(icon("chart-pie"), " Tax Contribution by Income Group"),
              DTOutput("BIN_TABLES"))
        )
      ),
      
      # ---- Tax Expenditures ----
      tabItem(
        tabName = "MainResultsTE",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = FALSE,
            title = tags$span(icon("wallet"), " Tax Expenditures"),
            conditionalPanel(
              condition = "input.toggleSimulationRates == false",
              tags$div(
                class = "te-disabled-panel",
                tags$p(icon("toggle-off")),
                tags$p(tags$strong("Tax Expenditure results are not available.")),
                tags$p(
                  "To activate this section, go to ",
                  tags$strong("Simulation Parameters"),
                  ", enable the ", tags$em("Toggle Tax Expenditures"),
                  " switch, and click ", tags$strong("Run Simulation"), "."
                )
              )
            ),
            conditionalPanel(
              condition = "input.toggleSimulationRates == true",
              DTOutput("TE_TABLES")
            )
          )
        )
      ),
      
      # ---- Dashboards ----
      # Distribution_Charts removed - ETR and decile charts moved to Revenue_Charts
      tabItem(
        tabName = "PIT_Revenues",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = FALSE,
            title = tags$span(icon("chart-column"), " Dashboard Visualizations"),
            fluidRow(
              column(6,
                     selectInput("chartSelectPIT_Revenues", "Select Chart",
                                 choices  = c("Revenue_Charts",
                                              "Tax_Expenditures_Charts"),
                                 selected = "Revenue_Charts"))
            ),
            fluidRow(
              infoBoxOutput("infoBox1", width = 6),
              infoBoxOutput("infoBox2", width = 6)
            ),
            fluidRow(column(12, uiOutput("additionalCharts")))
          )
        )
      )
    )
  )
)


# Step4 Server ----

server <- function(input, output, session) {
  
  # Progress helpers ----
  cit_show_progress <- function(value = 0, text = "Running CIT simulation...") {
    shinyjs::html(id = "citProgressText", html = text, add = FALSE)
    shinyjs::runjs(sprintf(
      "$('#citProgressBar').css('width','%s%%').attr('aria-valuenow','%s');",
      round(value * 100), round(value * 100)
    ))
    shinyjs::show("citProgressOverlay")
  }
  
  cit_update_progress <- function(value, text) {
    shinyjs::html(id = "citProgressText", html = text, add = FALSE)
    shinyjs::runjs(sprintf(
      "$('#citProgressBar').css('width','%s%%').attr('aria-valuenow','%s');",
      round(value * 100), round(value * 100)
    ))
  }
  
  cit_hide_progress <- function() shinyjs::hide("citProgressOverlay")
  
  
  # Header image ----
  output$headerImage <- renderUI({
    candidates <- c(
      file.path("img", "WB_pic.png"),
      file.path("www", "WB_pic.png"),
      if (exists("path1", envir = .GlobalEnv, inherits = FALSE))
        file.path(get("path1", envir = .GlobalEnv), "img", "WB_pic.png")
      else NA_character_
    )
    candidates <- candidates[!is.na(candidates) & file.exists(candidates)]
    if (length(candidates) == 0L) return(NULL)
    img_data <- base64enc::dataURI(file = candidates[1L], mime = "image/png")
    tags$img(src = img_data, height = "40px",
             style = "float:left; margin-right:20px;")
  })
  
  
  # Reactives ----
  excelData                   <- reactiveVal(NULL)
  reactive_simulation_results <- reactiveVal(NULL)
  
  empty_override_table <- function() {
    data.table(
      PolicyParameter = character(), Descriptions = character(),
      LongName        = character(), Parameters   = character(),
      Year            = numeric(),   Value        = numeric()
    )
  }
  
  cit_sim_params_updated_rv <- reactiveVal(empty_override_table())
  
  observeEvent(input$SimulationYear, {
    assign("SimulationYear", as.integer(input$SimulationYear), envir = .GlobalEnv)
    cat("Simulation year updated:", input$SimulationYear, "\n")
  })
  
  
  # Import Excel ----
  observeEvent(input$importExcel, {
    req(input$fileInput)
    data <- read_excel(input$fileInput$datapath, col_names = input$hasHeader)
    required_cols <- c("PolicyParameter", "Descriptions", "LongName",
                       "Value", "Year", "Parameters")
    missing_cols  <- setdiff(required_cols, colnames(data))
    if (length(missing_cols) > 0L) {
      showModal(modalDialog(
        title = "Import error",
        paste("The Excel file is missing columns:",
              paste(missing_cols, collapse = ", ")),
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    data <- as.data.table(data)
    data[, Value := suppressWarnings(
      as.numeric(gsub("[^0-9.-]", "", as.character(Value))))]
    data[, Year  := suppressWarnings(
      as.numeric(gsub("[^0-9.]",  "", as.character(Year))))]
    excelData(data)
    cit_sim_params_updated_rv(empty_override_table())
    assign("cit_simulation_parameters_raw", copy(data), envir = .GlobalEnv)
    showNotification("Excel data imported successfully.",
                     type = "message", duration = 4)
    cat("Imported rows:", nrow(data),
        "| Years:", paste(sort(unique(data$Year)), collapse = ", "), "\n")
  })
  
  
  # Dynamic UI ----
  output$PolicyParameter <- renderUI({
    selectInput("PolicyParameter", "Policy Parameter Selection",
                choices = if (is.null(excelData())) NULL
                else unique(excelData()$PolicyParameter))
  })
  
  output$Descriptions_Select <- renderUI({
    req(excelData(), input$PolicyParameter)
    choices <- unique(
      excelData()[PolicyParameter == input$PolicyParameter, ]$Descriptions)
    selectInput("Descriptions_Select", "Description of parameter",
                choices = choices)
  })
  
  output$LongNameSelect <- renderUI({
    req(excelData(), input$Descriptions_Select)
    choices <- unique(
      excelData()[Descriptions == input$Descriptions_Select, ]$LongName)
    selectInput("LongNameSelect", "Selected variable", choices = choices)
  })
  
  output$YearSelect <- renderUI({
    if (!is.null(excelData())) {
      years <- sort(unique(excelData()$Year))
      selectInput("YearSelect", "Apply override to year",
                  choices = years, selected = years[1])
    } else {
      selectInput("YearSelect", "Apply override to year", choices = NULL)
    }
  })
  
  # Auto-fill value
  observeEvent(list(input$LongNameSelect, input$YearSelect), {
    req(input$LongNameSelect, input$YearSelect, excelData())
    row <- excelData()[LongName == input$LongNameSelect &
                         Year == as.numeric(input$YearSelect)]
    if (nrow(row) >= 1L)
      updateNumericInput(session, "default_Value", value = row$Value[1L])
  })
  
  
  # Add override ----
  observeEvent(input$addValuesValue, {
    req(input$LongNameSelect, input$YearSelect, excelData())
    
    param_name    <- excelData()[LongName == input$LongNameSelect,
                                 unique(Parameters)][1L]
    selected_year <- as.numeric(input$YearSelect)
    
    available_years <- sort(unique(
      excelData()[Parameters == param_name, Year]
    ))
    
    years_to_apply <- if (isTRUE(input$apply_next_years)) {
      available_years[available_years >= selected_year]
    } else {
      selected_year
    }
    
    new_entries <- rbindlist(lapply(years_to_apply, function(yr) {
      data.table(
        PolicyParameter = input$PolicyParameter,
        Descriptions    = input$Descriptions_Select,
        LongName        = input$LongNameSelect,
        Parameters      = param_name,
        Year            = yr,
        Value           = input$default_Value
      )
    }), use.names = TRUE, fill = TRUE)
    
    current <- cit_sim_params_updated_rv()
    current <- current[!(Parameters == param_name & Year %in% years_to_apply)]
    cit_sim_params_updated_rv(rbind(current, new_entries))
    
    cat("Override added:", param_name,
        "| Years:", paste(years_to_apply, collapse = ", "),
        "| Value:", input$default_Value, "\n")
  })
  
  
  # Remove last row ----
  observeEvent(input$removeLastRow, {
    cc <- cit_sim_params_updated_rv()
    if (nrow(cc) == 0L) {
      showNotification("Table is already empty.", type = "warning")
      return()
    }
    cit_sim_params_updated_rv(cc[-nrow(cc)])
  })
  
  
  # Clear overrides ----
  observeEvent(input$clearValuesTable, {
    cit_sim_params_updated_rv(empty_override_table())
    cat("Override table cleared\n")
  })
  
  
  # Render override table ----
  output$cit_simulation_parameters_updated_tbl <- renderDT({
    datatable(cit_sim_params_updated_rv(),
              options = list(dom = "t", paging = FALSE,
                             ordering = TRUE, autoWidth = TRUE),
              rownames = FALSE)
  })
  
  
  # Save overrides ----
  observeEvent(input$savepit_simulation_parameters_updated, {
    tryCatch({
      raw_params <- get("cit_simulation_parameters_raw", envir = .GlobalEnv)
      updated    <- copy(as.data.table(raw_params))
      overrides  <- cit_sim_params_updated_rv()
      
      if (nrow(overrides) > 0L) {
        for (i in seq_len(nrow(overrides))) {
          row <- overrides[i]
          idx <- which(updated$Parameters == row$Parameters &
                         updated$Year == row$Year)
          if (length(idx) > 0L) {
            updated[idx, Value := row$Value]
          } else {
            updated <- rbind(updated,
                             row[, names(updated), with = FALSE],
                             fill = TRUE)
          }
        }
      }
      
      assign("cit_simulation_parameters_updated", updated, envir = .GlobalEnv)
      showNotification("Parameters saved successfully.",
                       type = "message", duration = 5)
      cat("Saved. Rows:", nrow(updated),
          "| Years:", paste(sort(unique(updated$Year)), collapse = ", "), "\n")
    }, error = function(e) {
      showModal(modalDialog(title = "Save error", conditionMessage(e),
                            easyClose = TRUE, footer = NULL))
    })
  })
  
  
  # Script helpers ----
  source_cit_script <- function(script_name) {
    script_path <- file.path(path1, "Scripts", "CIT", script_name)
    if (!file.exists(script_path))
      stop(paste0("Script not found: ", script_path), call. = FALSE)
    tryCatch(
      source(script_path, local = .GlobalEnv, echo = FALSE, print.eval = FALSE),
      error = function(e)
        stop(paste0("Error in [", script_name, "]: ", conditionMessage(e)),
             call. = FALSE)
    )
    invisible(TRUE)
  }
  
  collect_object <- function(object_name) {
    if (!exists(object_name, envir = .GlobalEnv, inherits = FALSE))
      stop(paste0("Required object '", object_name,
                  "' was not created by the calculation scripts."), call. = FALSE)
    get(object_name, envir = .GlobalEnv, inherits = FALSE)
  }
  
  
  # Run simulation ----
  observeEvent(input$calc_Customs_Sim_Button, {
    
    if (!exists("cit_simulation_parameters_updated", envir = .GlobalEnv)) {
      showModal(modalDialog(
        title = "Input error",
        "Please import parameters and click 'Save Data' before running the simulation.",
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    
    te_enabled        <- isTRUE(input$toggleSimulationRates)
    calculation_error <- NULL
    results           <- NULL
    
    cit_show_progress(0.05, "Preparing simulation parameters...")
    
    tryCatch({
      assign("SimulationYear", as.integer(input$SimulationYear),
             envir = .GlobalEnv)
      
      if (te_enabled) {
        
        # Flow B - Toggle ON
        cit_update_progress(0.30, "Running Tax Calculator...")
        source_cit_script("TaxCalculator.R")
        
        cit_update_progress(0.60, "Computing distribution effects...")
        source_cit_script("Calc-Distribution-Effects.R")
        
        cit_update_progress(0.70, "Computing tax expenditures...")
        source_cit_script("Calc-TaxExpenditures.R")
        
        cit_update_progress(0.90, "Collecting results...")
        
        results <- list(
          simulation_year                        = as.integer(input$SimulationYear),
          cit_summary_df_big                     = collect_object("cit_summary_df_big"),
          te_summary_df                          = collect_object("te_summary_df"),
          cit_decile_distribution_bu_sim         = collect_object("cit_decile_distribution_bu_sim"),
          cit_result_bins_sim_sub                = collect_object("cit_result_bins_sim_sub"),
          re_effects_final_big                   = collect_object("re_effects_final_big"),
          forecast_horizon                       = collect_object("forecast_horizon"),
          merged_CIT_BU_SIM                      = collect_object("merged_CIT_BU_SIM"),
          nace_cit_big_corporations_summary_long = collect_object("nace_cit_big_corporations_summary_long"),
          cit_centile_distribution_bu_sim        = collect_object("cit_centile_distribution_bu_sim"),
          cit_result_bins_bu_sub                 = collect_object("cit_result_bins_bu_sub"),
          te_summary_df_type                     = collect_object("te_summary_df_type"),
          company_type_cit_summary_te            = collect_object("company_type_cit_summary_te"),
          nace_cit_summary_tbl                   = collect_object("nace_cit_summary_tbl")
        )
        
      } else {
        
        # Flow A - Toggle OFF
        cit_update_progress(0.30, "Running Tax Calculator...")
        source_cit_script("TaxCalculator.R")
        
        cit_update_progress(0.65, "Computing distribution effects...")
        source_cit_script("Calc-Distribution-Effects.R")
        
        cit_update_progress(0.90, "Collecting results...")
        
        results <- list(
          simulation_year                        = as.integer(input$SimulationYear),
          cit_summary_df_big                     = collect_object("cit_summary_df_big"),
          te_summary_df                          = NULL,
          cit_decile_distribution_bu_sim         = collect_object("cit_decile_distribution_bu_sim"),
          cit_result_bins_sim_sub                = collect_object("cit_result_bins_sim_sub"),
          re_effects_final_big                   = collect_object("re_effects_final_big"),
          forecast_horizon                       = collect_object("forecast_horizon"),
          merged_CIT_BU_SIM                      = collect_object("merged_CIT_BU_SIM"),
          nace_cit_big_corporations_summary_long = collect_object("nace_cit_big_corporations_summary_long"),
          cit_centile_distribution_bu_sim        = collect_object("cit_centile_distribution_bu_sim"),
          cit_result_bins_bu_sub                 = collect_object("cit_result_bins_bu_sub"),
          te_summary_df_type                     = NULL,
          company_type_cit_summary_te            = NULL,
          nace_cit_summary_tbl                   = NULL
        )
      }
    }, error = function(e) {
      calculation_error <<- conditionMessage(e)
    })
    
    cit_hide_progress()
    
    if (!is.null(calculation_error)) {
      showModal(modalDialog(title = "Calculation error", calculation_error,
                            easyClose = TRUE, footer = NULL))
      return()
    }
    
    reactive_simulation_results(results)
    showModal(modalDialog(title = "Success", "Simulation is done.",
                          easyClose = TRUE, footer = NULL))
    tryCatch(updateCharts(), error = function(e) {
      showModal(modalDialog(
        title = "Chart error",
        paste0("Simulation completed but chart creation failed: ",
               conditionMessage(e)),
        easyClose = TRUE, footer = NULL
      ))
    })
  })
  
  
  # Render tables ----
  
  output$CIT_SUMMARY_TABLES <- renderDT({
    req(reactive_simulation_results())
    results <- reactive_simulation_results()
    fh      <- results$forecast_horizon
    datatable(
      results$cit_summary_df_big,
      caption = tags$caption(
        paste("CIT Projections,", min(fh), "-", max(fh)),
        class = "table-caption-bold"
      ),
      extensions = "Buttons",
      options = list(
        pageLength = 15, dom = "Blfrtip",
        buttons    = list(
          list(extend = "copyHtml5", text = "Copy",  filename = "CIT_Projections"),
          list(extend = "csvHtml5",  text = "CSV",   filename = "CIT_Projections"),
          list(extend = "print",     text = "Print")
        ),
        autoWidth = TRUE, escape = FALSE,
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      ),
      rownames = FALSE
    )
  })
  
  output$TE_TABLES <- renderDT({
    if (!isTRUE(input$toggleSimulationRates)) return(NULL)
    req(reactive_simulation_results())
    results <- reactive_simulation_results()
    req(results$te_summary_df_type)
    
    selected_year <- as.character(results$simulation_year)
    te_year <- as.data.table(results$te_summary_df_type)[
      as.character(year) == selected_year,
      .(type, `tax expenditure`)
    ]
    
    validate(need(
      nrow(te_year) > 0L,
      paste("No tax-expenditure-by-type results found for", selected_year)
    ))
    
    # Values in te_summary_df_type are already expressed in thousands.
    total_row <- data.table(
      type = "Total",
      `tax expenditure` = sum(te_year$`tax expenditure`, na.rm = TRUE)
    )
    te_year <- rbind(te_year, total_row, use.names = TRUE)
    setnames(te_year,
             old = c("type", "tax expenditure"),
             new = c("Tax-expenditure provision", "Tax expenditure (thousand LCU)"))
    te_year[, `Tax expenditure (thousand LCU)` :=
              round(`Tax expenditure (thousand LCU)`, 1)]
    
    datatable(
      te_year,
      caption = tags$caption(
        paste("Tax Expenditures by Provision (thousand LCU),", selected_year),
        class = "table-caption-bold"
      ),
      extensions = "Buttons",
      options = list(
        pageLength = 10, dom = "Blfrtip",
        buttons    = list(
          list(extend = "copyHtml5", text = "Copy", filename = "TE_By_Provision"),
          list(extend = "csvHtml5",  text = "CSV",  filename = "TE_By_Provision"),
          list(extend = "print",     text = "Print")
        ),
        autoWidth = TRUE, escape = FALSE,
        paging = FALSE, searching = FALSE, info = FALSE,
        ordering = FALSE
      ),
      rownames = FALSE
    )
  })
  
  output$DIST_TABLES <- renderDT({
    req(reactive_simulation_results())
    results <- reactive_simulation_results()
    datatable(
      results$cit_decile_distribution_bu_sim,
      caption = tags$caption(
        paste("Distribution Tables (in LCU),", results$simulation_year),
        class = "table-caption-bold"
      ),
      extensions = "Buttons",
      options = list(
        pageLength = 15, dom = "Blfrtip",
        buttons    = list(
          list(extend = "copyHtml5", text = "Copy",  filename = "Distribution_Table"),
          list(extend = "csvHtml5",  text = "CSV",   filename = "Distribution_Table"),
          list(extend = "print",     text = "Print")
        ),
        autoWidth = TRUE, escape = FALSE,
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      ),
      rownames = FALSE
    )
  })
  
  output$BIN_TABLES <- renderDT({
    req(reactive_simulation_results())
    results <- reactive_simulation_results()
    datatable(
      results$cit_result_bins_sim_sub,
      caption = tags$caption(
        paste("Structure of CIT liability by income groups in MIL LCU,",
              results$simulation_year),
        class = "table-caption-bold"
      ),
      extensions = "Buttons",
      options = list(
        pageLength = 15, dom = "Blfrtip",
        buttons    = list(
          list(extend = "copyHtml5", text = "Copy",  filename = "Bin_Table"),
          list(extend = "csvHtml5",  text = "CSV",   filename = "Bin_Table"),
          list(extend = "print",     text = "Print")
        ),
        autoWidth = TRUE, escape = FALSE,
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      ),
      rownames = FALSE
    )
  })
  
  
  # Build charts ----
  
  updateCharts <- function() {
    results <- reactive_simulation_results()
    if (is.null(results)) return(invisible(NULL))
    
    chart_type       <- isolate(input$chartSelectPIT_Revenues)
    forecast_horizon <- results$forecast_horizon
    re_effects       <- results$re_effects_final_big
    sy               <- results$simulation_year
    
    if (identical(chart_type, "Revenue_Charts")) {
      
      source(file.path(path1, "Scripts", "CIT", "Charts-CIT_Revenues.R"),
             local = .GlobalEnv, echo = FALSE)
      
      charts <- Revenue_Charts_fun(
        results$merged_CIT_BU_SIM,
        results$nace_cit_big_corporations_summary_long,
        results$cit_centile_distribution_bu_sim,
        results$cit_decile_distribution_bu_sim,
        sy,
        range(forecast_horizon)
      )
      
      output$infoBox1 <- renderInfoBox({
        infoBox(title = paste("ETR - Business as usual", sy),
                value = round(re_effects$`Business as usual`[1], 4),
                icon  = icon("chart-area"), color = "orange")
      })
      output$infoBox2 <- renderInfoBox({
        infoBox(title = paste("ETR - Simulation", sy),
                value = round(re_effects$Simulation[1], 4),
                icon  = icon("industry"), color = "light-blue")
      })
      
      output$additionalCharts <- renderUI({
        tagList(
          fluidRow(
            column(6, plotlyOutput("CIT_big_corporations_rev_plt", height = "400px")),
            column(6, plotlyOutput("CIT_etr_centile_plt",          height = "400px"))
          ),
          fluidRow(
            column(6, plotlyOutput("CIT_nace_big_corporations_plt", height = "400px")),
            column(6, plotlyOutput("CIT_decile_groups_plt",         height = "400px"))
          )
        )
      })
      
      output$CIT_big_corporations_rev_plt  <- renderPlotly({ charts$CIT_big_corporations_rev_plt })
      output$CIT_etr_centile_plt           <- renderPlotly({ charts$CIT_etr_centile_plt })
      output$CIT_nace_big_corporations_plt <- renderPlotly({ charts$CIT_nace_big_corporations_plt })
      output$CIT_decile_groups_plt         <- renderPlotly({ charts$CIT_decile_groups_plt })
      
    } else if (identical(chart_type, "Tax_Expenditures_Charts")) {
      
      if (!isTRUE(input$toggleSimulationRates)) {
        output$infoBox1 <- renderInfoBox({
          infoBox(title = "Tax Expenditures", value = "Disabled",
                  icon = icon("toggle-off"), color = "light-blue")
        })
        output$infoBox2 <- renderInfoBox({
          infoBox(title = "Action required", value = "Enable toggle & rerun",
                  icon = icon("circle-info"), color = "light-blue")
        })
        output$additionalCharts <- renderUI({ te_disabled_message() })
        return(invisible(NULL))
      }
      
      if (is.null(results$nace_cit_summary_tbl)) {
        output$infoBox1 <- renderInfoBox({
          infoBox(title = "Tax Expenditures", value = "Not computed",
                  icon = icon("circle-exclamation"), color = "orange")
        })
        output$infoBox2 <- renderInfoBox({
          infoBox(title = "Action required", value = "Rerun simulation",
                  icon = icon("play"), color = "orange")
        })
        output$additionalCharts <- renderUI({
          tags$div(
            class = "te-disabled-panel",
            tags$p(icon("circle-exclamation",
                        style = "font-size:36px;color:#e9a03b;")),
            tags$p(tags$strong("Tax Expenditure charts are not yet computed.")),
            tags$p("Click ", tags$strong("Run Simulation"),
                   " with the toggle enabled to compute results.")
          )
        })
        return(invisible(NULL))
      }
      
      source(file.path(path1, "Scripts", "CIT", "Charts-TaxExpenditures.R"),
             local = .GlobalEnv, echo = FALSE)
      
      charts_te <- Tax_Expenditures_Charts_fun(
        results$te_summary_df,
        results$company_type_cit_summary_te,
        results$nace_cit_summary_tbl,
        results$te_summary_df_type,
        forecast_horizon, sy
      )
      
      output$infoBox1 <- renderInfoBox({
        infoBox(title = " ", icon = icon("chart-area"), color = "orange")
      })
      output$infoBox2 <- renderInfoBox({
        infoBox(title = " ", icon = icon("industry"), color = "light-blue")
      })
      
      output$additionalCharts <- renderUI({
        tagList(
          fluidRow(
            column(6, plotlyOutput("te_agg_plt",            height = "400px")),
            column(6, plotlyOutput("te_type_companies_plt", height = "400px"))
          ),
          fluidRow(
            column(6, plotlyOutput("treemap_nace_type_plt", height = "400px")),
            column(6, plotlyOutput("te_type_plt",           height = "400px"))
          )
        )
      })
      
      output$te_agg_plt            <- renderPlotly({ charts_te$te_agg_plt })
      output$te_type_companies_plt <- renderPlotly({ charts_te$te_type_companies_plt })
      output$treemap_nace_type_plt <- renderPlotly({ charts_te$treemap_nace_type_plt })
      output$te_type_plt           <- renderPlotly({ charts_te$te_type_plt })
    }
    
    invisible(TRUE)
  }
  
  
  # Chart selector observer ----
  observeEvent(
    input$chartSelectPIT_Revenues,
    {
      req(reactive_simulation_results())
      tryCatch(updateCharts(), error = function(e) {
        showModal(modalDialog(title = "Chart error", conditionMessage(e),
                              easyClose = TRUE, footer = NULL))
      })
    },
    ignoreInit = TRUE
  )
  
  
  # Stop app on session end ----
  session$onSessionEnded(function() {
    if (interactive()) try(shiny::stopApp(), silent = TRUE)
  })
}


# Step5 Run app ----

shinyApp(ui = ui, server = server)
