
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

options(scipen = 999)

# ============================================================
# STARTUP: LOAD THE ENTIRE SAVED R WORKSPACE FROM .Rdata
# ============================================================
# load() restores EVERY object saved in the selected .Rdata file, without
# selecting or filtering names. This executes in PIT's OWN R process,
# including when PIT is launched through callr::r_bg().
# The filename .Rdata is literal, not a list of required objects.

.pit_source_folder <- local({
  paths <- unlist(lapply(sys.frames(), function(fr) {
    f <- get0("ofile", envir = fr, inherits = FALSE, ifnotfound = NULL)
    if (is.character(f) && length(f) == 1L && !is.na(f) && nzchar(f)) f
    else character(0)
  }), use.names = FALSE)
  paths <- paths[grepl("^PIT-Module.*\\.R$", basename(paths), ignore.case = TRUE)]
  if (length(paths)) {
    normalizePath(dirname(tail(paths, 1L)), winslash = "/", mustWork = TRUE)
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  }
})

.pit_workspace_folders <- unique(normalizePath(c(
  .pit_source_folder,
  file.path(.pit_source_folder, "Data", "PIT"),
  file.path(.pit_source_folder, "Data"),
  dirname(.pit_source_folder),
  file.path(dirname(.pit_source_folder), "Data", "PIT"),
  file.path(dirname(.pit_source_folder), "Data"),
  dirname(dirname(.pit_source_folder)),
  file.path(dirname(dirname(.pit_source_folder)), "Data", "PIT"),
  getwd(),
  file.path(getwd(), "Data", "PIT")
), winslash = "/", mustWork = FALSE))
.pit_workspace_folders <- .pit_workspace_folders[dir.exists(.pit_workspace_folders)]

.pit_find_workspace <- function() {
  configured <- Sys.getenv("PIT_RDATA_FILE", unset = "")
  if (nzchar(configured)) {
    trials <- unique(c(configured, file.path(.pit_workspace_folders, configured)))
    matches <- trials[file.exists(trials) & !dir.exists(trials)]
    if (!length(matches)) stop(
      "PIT_RDATA_FILE points to a missing file: ", configured,
      "\nWorking directory: ", getwd(), call. = FALSE
    )
    return(normalizePath(matches[1L], winslash = "/", mustWork = TRUE))
  }

  # First look for exactly '.Rdata', as requested, nearest to PIT-Module.R.
  exact <- unlist(lapply(.pit_workspace_folders, function(d) {
    file.path(d, c(".Rdata", ".RData"))
  }), use.names = FALSE)
  exact <- exact[file.exists(exact) & !dir.exists(exact)]
  if (length(exact)) return(normalizePath(exact[1L], winslash = "/", mustWork = TRUE))

  # If the workspace has a non-hidden filename such as PIT-inputs.RData,
  # accept it only when there is a unique file or a unique PIT-named file.
  found <- unique(unlist(lapply(.pit_workspace_folders, function(d) {
    list.files(d, pattern = "\\.([Rr][Dd][Aa][Tt][Aa]|[Rr][Dd][Aa])$",
               all.files = TRUE, full.names = TRUE)
  }), use.names = FALSE))
  if (!length(found)) stop(
    "PIT cannot find the saved .Rdata workspace.\n",
    "Put .Rdata beside PIT-Module.R or in Data/PIT, or set ",
    "PIT_RDATA_FILE to its complete path.\nLooked in:\n",
    paste(.pit_workspace_folders, collapse = "\n"), call. = FALSE
  )
  pit_named <- found[grepl("pit", basename(found), ignore.case = TRUE)]
  if (length(pit_named) == 1L)
    return(normalizePath(pit_named, winslash = "/", mustWork = TRUE))
  if (length(found) == 1L)
    return(normalizePath(found, winslash = "/", mustWork = TRUE))
  stop(
    "More than one possible .Rdata file was found. Set PIT_RDATA_FILE ",
    "to the full path of the correct file. Candidates:\n",
    paste(found, collapse = "\n"), call. = FALSE
  )
}

.pit_workspace_file <- .pit_find_workspace()
message("PIT: loading ALL saved objects from: ", .pit_workspace_file)
.pit_loaded_workspace_names <- tryCatch(
  load(.pit_workspace_file, envir = .GlobalEnv),
  error = function(e) stop(
    "PIT cannot load full .Rdata file: ", .pit_workspace_file,
    "\n", conditionMessage(e), call. = FALSE
  )
)
message("PIT: successfully loaded ", length(.pit_loaded_workspace_names),
        " workspace objects (NO name filtering).")
message("PIT: loaded object names: ", paste(.pit_loaded_workspace_names, collapse = ", "))

# Some saved workspaces contain a path1 from the computer on which they
# were created. Prefer the actual model folder if it has Scripts/PIT.
.pit_root_candidates <- unique(normalizePath(c(
  .pit_source_folder, dirname(.pit_source_folder),
  dirname(dirname(.pit_source_folder)), getwd(), dirname(getwd())
), winslash = "/", mustWork = FALSE))
.pit_script_roots <- .pit_root_candidates[vapply(.pit_root_candidates,
  function(d) dir.exists(file.path(d, "Scripts", "PIT")), logical(1))]
if (length(.pit_script_roots)) {
  path1 <- .pit_script_roots[1L]
  assign("path1", path1, envir = .GlobalEnv)
  message("PIT: calculation scripts root = ", path1)
} else {
  message("PIT warning: no Scripts/PIT folder found near the app; ",
          "the saved path1, if any, will be used.")
}




# Step1 Validate PIT years ----

if (!exists("scenario_years_pit", envir = .GlobalEnv, inherits = FALSE)) {
  stop(
    paste0(
      "Object 'scenario_years_pit' is absent AFTER loading the entire .Rdata. ",
      "Loaded file: ", .pit_workspace_file, ". ",
      "Objects in that file: ", paste(.pit_loaded_workspace_names, collapse = ", ")
    )
  )
}

shiny_years_raw <- suppressWarnings(
  as.integer(get("scenario_years_pit", envir = .GlobalEnv))
)

if (length(shiny_years_raw) == 0L || anyNA(shiny_years_raw))
  stop("scenario_years_pit must contain at least one valid year.")

if (anyDuplicated(shiny_years_raw))
  stop("scenario_years_pit contains duplicated years.")


# Step2 Prepare PIT scenarios ----

if (exists("scenarios_pit", envir = .GlobalEnv, inherits = FALSE)) {
  shiny_scenarios_raw <- as.character(
    get("scenarios_pit", envir = .GlobalEnv)
  )
} else {
  shiny_scenarios_raw <- paste0("t", seq_along(shiny_years_raw) - 1L)
}

if (length(shiny_scenarios_raw) != length(shiny_years_raw))
  stop("scenario_years_pit and scenarios_pit must contain the same number of entries.")

if (anyNA(shiny_scenarios_raw) || anyDuplicated(shiny_scenarios_raw))
  stop("scenarios_pit contains missing or duplicated scenario labels.")

shiny_scenario_lookup_pit <- data.table(
  year     = shiny_years_raw,
  scenario = shiny_scenarios_raw
)
setorder(shiny_scenario_lookup_pit, year)

shiny_scenario_years_pit      <- shiny_scenario_lookup_pit$year
shiny_scenarios_pit           <- shiny_scenario_lookup_pit$scenario
shiny_simulation_year_min     <- min(shiny_scenario_years_pit)
shiny_simulation_year_max     <- max(shiny_scenario_years_pit)
shiny_simulation_year_default <- shiny_simulation_year_min
shiny_base_year_pit           <- shiny_simulation_year_min
shiny_end_year_pit            <- shiny_simulation_year_max

cat("PIT Shiny forecast horizon:", shiny_base_year_pit, "-", shiny_end_year_pit, "\n")


# Step3 Normalize PIT baseline years ----

normalize_pit_baseline_year <- function(object_name, expected_year) {
  
  if (!exists(object_name, envir = .GlobalEnv, inherits = FALSE))
    stop(paste0("Required baseline object '", object_name,
                "' is missing after full workspace load from: ", .pit_workspace_file,
                ". The Rdata file contains: ",
                paste(.pit_loaded_workspace_names, collapse = ", ")))
  
  baseline_data <- as.data.table(
    copy(get(object_name, envir = .GlobalEnv, inherits = FALSE))
  )
  
  if (!"Year" %in% names(baseline_data))
    stop(paste0(object_name, " does not contain a Year column."))
  
  if (nrow(baseline_data) == 0L)
    stop(paste0(object_name, " contains no rows."))
  
  original_year  <- baseline_data$Year
  converted_year <- suppressWarnings(as.integer(as.character(original_year)))
  
  invalid_rows <- which(
    is.na(converted_year) & !is.na(original_year) &
      trimws(as.character(original_year)) != ""
  )
  
  if (length(invalid_rows) > 0L)
    stop(paste0(object_name, " contains nonnumeric Year values in rows: ",
                paste(head(invalid_rows, 20L), collapse = ", ")))
  
  baseline_data[, Year := converted_year]
  
  negative_years <- sort(unique(baseline_data[!is.na(Year) & Year < 0L, Year]))
  if (length(negative_years) > 0L)
    stop(paste0(object_name, " contains invalid negative Year values: ",
                paste(negative_years, collapse = ", ")))
  
  positive_years <- sort(unique(baseline_data[!is.na(Year) & Year > 0L, Year]))
  
  if (length(positive_years) == 0L)
    stop(paste0(object_name, " contains no valid positive Year values."))
  
  if (length(positive_years) > 1L)
    stop(paste0(object_name, " contains multiple years: ",
                paste(positive_years, collapse = ", "),
                ". Only one baseline year is allowed."))
  
  if (positive_years[1L] != as.integer(expected_year)) {
    message(object_name, " data year (", positive_years[1L], ") differs from ",
            "growth factor start year (", expected_year, "). ",
            "Overwriting Year with ", expected_year, ".")
    baseline_data[, Year := as.integer(expected_year)]
  }
  
  
  repaired_rows <- which(is.na(baseline_data$Year) | baseline_data$Year == 0L)
  if (length(repaired_rows) > 0L)
    baseline_data[is.na(Year) | Year == 0L, Year := as.integer(expected_year)]
  
  final_years <- sort(unique(baseline_data$Year))
  if (length(final_years) != 1L || final_years[1L] != as.integer(expected_year))
    stop(paste0(object_name, " contains invalid years after normalization: ",
                paste(final_years, collapse = ", ")))
  
  assign(object_name, baseline_data, envir = .GlobalEnv)
  invisible(baseline_data)
}

normalize_all_pit_baseline_data <- function() {
  baseline_objects <- c("pit_raw_pd", "pit_raw_qs", "pit_raw_wm",
                        "pit_raw_wr", "pit_raw_is")
  for (nm in baseline_objects)
    normalize_pit_baseline_year(nm, shiny_base_year_pit)
  invisible(TRUE)
}


# Step4 Reusable TE disabled message ----

te_disabled_message <- function() {
  tags$div(
    style = paste0(
      "padding: 30px; text-align: center; ",
      "color: #6c7a89; font-size: 15px; margin-top: 10px;"
    ),
    tags$p(icon("toggle-off", style = "font-size: 36px; color: #b0bec5;")),
    tags$p(tags$strong("Tax Expenditure results are not available.")),
    tags$p(
      "To activate this section, go to ",
      tags$strong("Simulation Parameters"),
      ", enable the ",
      tags$em("Toggle Tax Expenditures"),
      " switch, and click ",
      tags$strong("Run Simulation"), "."
    )
  )
}


# Step5 UI ----

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    titleWidth = 280,
    title = tags$div(
      class = "header-flex",
      uiOutput("headerImage"),
      tags$span("PIT Module", class = "header-title")
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
        menuSubItem("Main Results",     tabName = "MainResultsSimulation", icon = icon("gauge")),
        menuSubItem("Tax Expenditures", tabName = "MainResultsTE",         icon = icon("wallet"))
      ),
      menuItem(
        "Visualizations", icon = icon("chart-simple"),
        menuSubItem("Dashboards", tabName = "PIT_Revenues", icon = icon("chart-column"))
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        body, .content-wrapper, .right-side, .content {
          font-family: 'Segoe UI', Arial, sans-serif;
          background-color: #f4f7fb !important;
        }
        .skin-blue .main-header .logo {
          background-color: #1f4e79 !important; font-weight: 600; font-size: 18px;
        }
        .skin-blue .main-header .navbar  { background-color: #2f6c99 !important; }
        .skin-blue .main-sidebar,
        .skin-blue .treeview-menu        { background-color: #1e2d3b !important; }
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
        .te-disabled-panel .fa {
          font-size: 36px; color: #b0bec5; margin-bottom: 12px;
        }
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
        .progress {
          height: 18px; margin-bottom: 0;
          border-radius: 10px; overflow: hidden;
        }
        .progress-bar { background-color: #2f6c99 !important; }
      "))
    ),
    
    div(
      id    = "pitProgressOverlay",
      class = "progress-overlay",
      div(
        class = "progress-center-box",
        div(class = "progress-center-title", "Running PIT Simulation"),
        div(id = "pitProgressText", class = "progress-center-text",
            "Preparing simulation..."),
        div(
          class = "progress",
          div(
            id    = "pitProgressBar",
            class = "progress-bar progress-bar-striped active",
            role  = "progressbar",
            style = "width: 0%;"
          )
        )
      )
    ),
    
    tabItems(
      
      # ---- Input ----
      tabItem(
        tabName = "input",
        fluidRow(
          box(
            width = 12, title = tags$span(icon("file-excel"), " Data Input"),
            status = "primary", solidHeader = FALSE,
            column(
              width = 6,
              selectInput("inputType", "Data Source",
                          choices = c("Manual", "Excel File"), selected = "Excel File"),
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
            width = 12, title = tags$span(icon("sliders"), " Simulation Settings"),
            status = "primary", solidHeader = FALSE,
            column(
              width = 4,
              sliderInput("SimulationYear", "Setting Simulation Year",
                          min = shiny_simulation_year_min,
                          max = shiny_simulation_year_max,
                          step = 1, value = shiny_simulation_year_default,
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
              numericInput("default_Year", "Initial year",
                           value = shiny_simulation_year_default,
                           min = shiny_simulation_year_min,
                           max = shiny_simulation_year_max, step = 1),
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
                paste0("When enabled, the selected PIT parameter value is applied from ",
                       "the simulation year through the remaining forecast years."))
            )
          )
        ),
        fluidRow(
          box(
            width = 12, title = tags$span(icon("table"), " Selected Simulation Parameters"),
            status = "primary", solidHeader = FALSE,
            DTOutput("pit_simulation_parameters_updated"),
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
      
      # ---- Main Results ----
      tabItem(
        tabName = "MainResultsSimulation",
        fluidRow(
          box(width = 12, title = tags$span(icon("coins"), " Aggregate Fiscal Impact"),
              status = "primary", solidHeader = FALSE,
              DTOutput("PIT_SUMMARY_AGG_TABLES"))
        ),
        fluidRow(
          box(
            width = 12,
            #title = tags$span(icon("layer-group"), " Distributional Fiscal Impact by Income Group"),
            title = tags$span(
              icon("layer-group"),
              " PIT Distribution by Gross Income"
            ),
            status = "primary",
            solidHeader = FALSE,
            fluidRow(
              column(
                width = 6,
                radioButtons(
                  inputId = "pit_distribution_group",
                  label   = "Income grouping",
                  choices = c(
                    "Deciles (10 groups)"       = "decile",
                    "Percentiles (100 groups)" = "percentile"
                  ),
                  selected = "decile",
                  inline   = TRUE
                )
              ),
              column(
                width = 6,
                tags$p(
                  class = "subtle-note",
                  paste0(
                    "Income groups are defined from current-law taxpayer income for the selected ",
                    "simulation-year snapshot. D01/P001 are the lowest-income groups; ",
                    "D10/P100 are the highest-income groups."
                  )
                )
              )
            ),
            DTOutput("PIT_DISTRIBUTION_TABLE")
          )
        ),
        fluidRow(
          box(width = 12, title = tags$span(icon("table"), " Disaggregated Fiscal Impact"),
              status = "primary", solidHeader = FALSE,
              DTOutput("PIT_SUMMARY_TABLES"))
        )
      ),
      
      # ---- Tax Expenditures Results Tab ----
      tabItem(
        tabName = "MainResultsTE",
        fluidRow(
          box(
            width = 12,
            title = tags$span(icon("chart-line"), " Tax Expenditures"),
            status = "primary", solidHeader = FALSE,
            conditionalPanel(
              condition = "input.toggleSimulationRates == false",
              tags$div(
                class = "te-disabled-panel",
                tags$p(icon("toggle-off")),
                tags$p(tags$strong("Tax Expenditure results are not available.")),
                tags$p(
                  "To activate this section, go to ",
                  tags$strong("Simulation Parameters"),
                  ", enable the ",
                  tags$em("Toggle Tax Expenditures"),
                  " switch, and click ",
                  tags$strong("Run Simulation"), "."
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
      tabItem(
        tabName = "PIT_Revenues",
        fluidRow(
          box(
            width = 12, title = tags$span(icon("chart-column"), " Dashboard Visualizations"),
            status = "primary", solidHeader = FALSE,
            fluidRow(
              column(6,
                     selectInput("chartSelectPIT_Revenues", "Select Chart",
                                 choices  = c("Revenue_Charts", "Tax_Expenditures_Charts"),
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


# Step6 Server ----

server <- function(input, output, session) {
  
  # Step6a Progress overlay helpers ----
  
  pit_show_progress <- function(value = 0, text = "Running PIT simulation...") {
    shinyjs::html(id = "pitProgressText", html = text, add = FALSE)
    shinyjs::runjs(sprintf(
      "$('#pitProgressBar').css('width','%s%%').attr('aria-valuenow','%s');",
      round(value * 100), round(value * 100)
    ))
    shinyjs::show("pitProgressOverlay")
  }
  
  pit_update_progress <- function(value, text) {
    shinyjs::html(id = "pitProgressText", html = text, add = FALSE)
    shinyjs::runjs(sprintf(
      "$('#pitProgressBar').css('width','%s%%').attr('aria-valuenow','%s');",
      round(value * 100), round(value * 100)
    ))
  }
  
  pit_hide_progress <- function() {
    shinyjs::hide("pitProgressOverlay")
  }
  
  
  # Step6b Header image ----
  
  output$headerImage <- renderUI({
    logo_candidates <- c(
      file.path("img", "WB_pic.png"),
      file.path("www", "WB_pic.png"),
      if (exists("path1", envir = .GlobalEnv, inherits = FALSE))
        file.path(get("path1", envir = .GlobalEnv), "img", "WB_pic.png")
      else NA_character_
    )
    logo_candidates <- logo_candidates[!is.na(logo_candidates) & file.exists(logo_candidates)]
    if (length(logo_candidates) == 0L) return(NULL)
    img_data <- base64enc::dataURI(file = logo_candidates[1L], mime = "image/png")
    tags$img(src = img_data, height = "40px", style = "float:left; margin-right:20px;")
  })
  
  excelData                   <- reactiveVal(NULL)
  reactive_simulation_results <- reactiveVal(NULL)
  
  empty_parameter_changes <- function() {
    data.table(
      PolicyParameter = character(), Descriptions = character(),
      LongName = character(), Parameters = character(),
      Value = numeric(), Year = integer()
    )
  }
  
  pit_simulation_parameter_changes <- reactiveVal(empty_parameter_changes())
  
  observeEvent(input$SimulationYear, {
    selected_year <- as.integer(input$SimulationYear)
    assign("SimulationYear", selected_year, envir = .GlobalEnv)
    updateNumericInput(session, "default_Year", value = selected_year,
                       min = shiny_simulation_year_min,
                       max = shiny_simulation_year_max, step = 1)
  })
  
  
  # Step7 Import parameter file ----
  
  observeEvent(input$importExcel, {
    req(input$fileInput)
    
    data <- read_excel(input$fileInput$datapath, col_names = input$hasHeader)
    
    required_columns <- c("PolicyParameter", "Descriptions", "LongName",
                          "Parameters", "Value", "Year")
    missing_columns  <- setdiff(required_columns, names(data))
    
    if (length(missing_columns) > 0L) {
      showModal(modalDialog(
        title = "Import error",
        paste0("The Excel file is missing these columns: ",
               paste(missing_columns, collapse = ", ")),
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    
    data <- as.data.table(data)
    data[, Value := suppressWarnings(
      as.numeric(gsub("[^0-9eE+.-]", "", as.character(Value)))
    )]
    data[, Year := suppressWarnings(
      as.integer(gsub("[^0-9-]", "", as.character(Year)))
    )]
    
    if (anyNA(data$Value) || anyNA(data$Year)) {
      showModal(modalDialog(
        title = "Import error",
        "The imported parameter table contains invalid Value or Year entries.",
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    
    duplicates <- data[, .N, by = .(Parameters, Year)][N > 1L]
    if (nrow(duplicates) > 0L) {
      showModal(modalDialog(
        title = "Import error",
        paste0("Duplicate Parameters + Year combinations: ",
               paste(paste0(duplicates$Parameters, " / ", duplicates$Year), collapse = ", ")),
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    
    excelData(copy(data))
    pit_simulation_parameter_changes(empty_parameter_changes())
    assign("pit_simulation_parameters_raw", copy(data), envir = .GlobalEnv)
    showNotification("Excel parameter data imported successfully.",
                     type = "message", duration = 4)
  })
  
  
  # Step8 Parameter selectors ----
  
  output$PolicyParameter <- renderUI({
    data <- excelData()
    selectInput("PolicyParameter", "Policy Parameter Selection",
                choices = if (is.null(data)) NULL else unique(data$PolicyParameter))
  })
  
  output$Descriptions_Select <- renderUI({
    req(excelData(), input$PolicyParameter)
    data <- as.data.table(copy(excelData()))
    selectInput("Descriptions_Select", "Description of parameter",
                choices = data[PolicyParameter == input$PolicyParameter, unique(Descriptions)])
  })
  
  output$LongNameSelect <- renderUI({
    req(excelData(), input$PolicyParameter, input$Descriptions_Select)
    data <- as.data.table(copy(excelData()))
    selectInput("LongNameSelect", "Selected variable",
                choices = data[
                  PolicyParameter == input$PolicyParameter &
                    Descriptions  == input$Descriptions_Select,
                  unique(LongName)
                ])
  })
  
  observeEvent(
    list(input$PolicyParameter, input$Descriptions_Select,
         input$LongNameSelect,  input$default_Year),
    {
      req(excelData(), input$PolicyParameter, input$Descriptions_Select,
          input$LongNameSelect, input$default_Year)
      data <- as.data.table(copy(excelData()))
      row  <- data[
        PolicyParameter == input$PolicyParameter &
          Descriptions == input$Descriptions_Select &
          LongName     == input$LongNameSelect &
          Year         == as.integer(input$default_Year)
      ]
      if (nrow(row) == 1L)
        updateNumericInput(session, "default_Value", value = row$Value[1L])
    },
    ignoreInit = FALSE
  )
  
  
  # Step9 Manage parameter changes ----
  
  observeEvent(input$addValuesValue, {
    req(excelData(), input$PolicyParameter, input$Descriptions_Select,
        input$LongNameSelect, input$SimulationYear, input$default_Value)
    
    data          <- as.data.table(copy(excelData()))
    selected_year <- as.integer(input$SimulationYear)
    
    parameter_rows <- data[
      PolicyParameter == input$PolicyParameter &
        Descriptions == input$Descriptions_Select &
        LongName     == input$LongNameSelect
    ]
    
    if (nrow(parameter_rows) == 0L) {
      showNotification("The selected parameter was not found.", type = "error")
      return()
    }
    
    available_years <- sort(unique(as.integer(parameter_rows$Year)))
    
    if (!(selected_year %in% available_years)) {
      showNotification(
        paste0("The parameter is not available for ", selected_year, "."),
        type = "error"
      )
      return()
    }
    
    years_to_apply <- if (isTRUE(input$apply_next_years))
      available_years[available_years >= selected_year]
    else
      selected_year
    
    base_row <- parameter_rows[Year == selected_year][1L]
    
    new_entries <- rbindlist(
      lapply(years_to_apply, function(yr) {
        data.table(
          PolicyParameter = base_row$PolicyParameter[1L],
          Descriptions    = base_row$Descriptions[1L],
          LongName        = base_row$LongName[1L],
          Parameters      = base_row$Parameters[1L],
          Value           = as.numeric(input$default_Value),
          Year            = as.integer(yr)
        )
      }),
      use.names = TRUE, fill = TRUE
    )
    
    current_changes <- as.data.table(copy(pit_simulation_parameter_changes()))
    current_changes <- current_changes[
      !(Parameters == base_row$Parameters[1L] & Year %in% years_to_apply)
    ]
    
    updated_changes <- rbindlist(list(current_changes, new_entries),
                                 use.names = TRUE, fill = TRUE)
    setorder(updated_changes, Year, Parameters)
    pit_simulation_parameter_changes(updated_changes)
  })
  
  observeEvent(input$removeLastRow, {
    cc <- as.data.table(copy(pit_simulation_parameter_changes()))
    if (nrow(cc) == 0L) {
      showNotification("The table is already empty.", type = "warning")
      return()
    }
    pit_simulation_parameter_changes(cc[-nrow(cc)])
  })
  
  observeEvent(input$clearValuesTable, {
    pit_simulation_parameter_changes(empty_parameter_changes())
  })
  
  output$pit_simulation_parameters_updated <- renderDT({
    changes <- as.data.table(copy(pit_simulation_parameter_changes()))
    datatable(
      changes[, .(PolicyParameter, Descriptions, LongName, Value, Year)],
      options = list(dom = "t", paging = FALSE, ordering = TRUE, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  
  # Step10 Build complete parameter table ----
  
  build_updated_parameter_table <- function() {
    if (!exists("pit_simulation_parameters_raw", envir = .GlobalEnv, inherits = FALSE))
      stop("pit_simulation_parameters_raw was not found.")
    
    updated_parameters <- as.data.table(
      copy(get("pit_simulation_parameters_raw", envir = .GlobalEnv))
    )
    
    updated_parameters[, Year  := as.integer(Year)]
    updated_parameters[, Value := as.numeric(Value)]
    
    changes <- as.data.table(copy(pit_simulation_parameter_changes()))
    
    if (nrow(changes) > 0L) {
      changes[, Year  := as.integer(Year)]
      changes[, Value := as.numeric(Value)]
      
      missing_matches <- changes[!updated_parameters, on = .(Parameters, Year)]
      if (nrow(missing_matches) > 0L)
        stop(paste0("These changes were not found in the raw table: ",
                    paste(paste0(missing_matches$Parameters, " / ", missing_matches$Year),
                          collapse = ", ")))
      
      updated_parameters[changes, on = .(Parameters, Year), Value := i.Value]
    }
    
    assign("ValueTableUpdate",                  copy(changes),            envir = .GlobalEnv)
    assign("pit_simulation_parameters_updated", copy(updated_parameters), envir = .GlobalEnv)
    assign("SimulationYear", as.integer(input$SimulationYear),            envir = .GlobalEnv)
    
    invisible(updated_parameters)
  }
  
  observeEvent(input$savepit_simulation_parameters_updated, {
    tryCatch(
      {
        build_updated_parameter_table()
        showNotification("Updated policy parameters were saved successfully.",
                         type = "message", duration = 5)
      },
      error = function(e) {
        showModal(modalDialog(title = "Save error", conditionMessage(e),
                              easyClose = TRUE, footer = NULL))
      }
    )
  })
  
  
  # Step11 Source one script safely ----
  
  source_calculation_script <- function(script_name) {
    script_path <- file.path(path1, "Scripts", "PIT", script_name)
    if (!file.exists(script_path))
      stop(paste0("Script not found: ", script_path), call. = FALSE)
    tryCatch(
      source(script_path, local = .GlobalEnv, echo = FALSE, print.eval = FALSE),
      error = function(e)
        stop(paste0("Error in [", script_name, "]: ", conditionMessage(e)), call. = FALSE)
    )
    invisible(TRUE)
  }
  
  collect_required_object <- function(object_name) {
    if (!exists(object_name, envir = .GlobalEnv, inherits = FALSE))
      stop(paste0("Required object '", object_name,
                  "' was not created by the calculation scripts."), call. = FALSE)
    get(object_name, envir = .GlobalEnv, inherits = FALSE)
  }
  
  
  # Step12 Run simulation synchronously with progress overlay ----
  #
  # TWO DISTINCT PROGRESS FLOWS:
  #
  # Flow A Ã¢ÂÂ Toggle OFF (detailed steps visible):
  #   5%  Preparing simulation parameters
  #   15% Running TaxCalculator PD
  #   30% Running TaxCalculator QS
  #   45% Running TaxCalculator WM
  #   60% Running TaxCalculator WR
  #   72% Running TaxCalculator IS
  #   82% Aggregating results
  #   96% Collecting results
  #   Done
  #
  # Flow B Ã¢ÂÂ Toggle ON (only 3 steps visible):
  #   5%  Preparing simulation parameters
  #       [all calculator + aggregate scripts run silently here]
  #   70% Computing tax expenditures...
  #   90% Aggregating results
  #   Done
  
  observeEvent(input$calc_Customs_Sim_Button, {
    
    if (is.null(excelData()) &&
        !exists("pit_simulation_parameters_raw", envir = .GlobalEnv, inherits = FALSE)) {
      showModal(modalDialog(
        title = "Input error",
        "Import the Excel parameter file before running the simulation.",
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    
    calculation_error   <- NULL
    calculation_results <- NULL
    
    # Capture toggle ONCE before any calculation starts.
    te_enabled <- isTRUE(input$toggleSimulationRates)
    
    # Show the first progress step immediately.
    pit_show_progress(0.05, "Preparing simulation parameters...")
    
    tryCatch(
      {
        build_updated_parameter_table()
        normalize_all_pit_baseline_data()
        assign("SimulationYear", as.integer(input$SimulationYear), envir = .GlobalEnv)
        
        if (te_enabled) {
          
          # ----------------------------------------------------------
          # Flow B Ã¢ÂÂ Toggle ON
          # All 5 calculators + Aggregate run silently between 5%-70%.
          # Only 3 labels are shown to the user:
          #   5%  Preparing simulation parameters   (already shown above)
          #   70% Computing tax expenditures...
          #   90% Aggregating results
          # ----------------------------------------------------------
          
          # Run all calculator scripts silently (no progress update)
          source_calculation_script("TaxCalculator_PD.R")
          source_calculation_script("TaxCalculator_QS.R")
          source_calculation_script("TaxCalculator_WM.R")
          source_calculation_script("TaxCalculator_WR.R")
          source_calculation_script("TaxCalculator_IS.R")
          source_calculation_script("Aggregate-Results.R")
          
          # Step visible to the user Ã¢ÂÂ 70%
          pit_update_progress(0.70, "Computing tax expenditures...")
          source_calculation_script("Calc-TaxExpenditures.R")
          
          if (!exists("pd_pit_results_te", envir = .GlobalEnv, inherits = FALSE))
            stop(paste0(
              "Calc-TaxExpenditures.R did not create 'pd_pit_results_te'. ",
              "Check the TE script."
            ), call. = FALSE)
          
          pd_te <- get("pd_pit_results_te", envir = .GlobalEnv, inherits = FALSE)
          
          required_te_keys <- c(
            "pit_summary_pd_te",
            "te_split_combined",
            "nace_pit_summary_te",
            "taxpayer_pit_summary_te"
          )
          
          missing_te_keys <- setdiff(required_te_keys, names(pd_te))
          if (length(missing_te_keys) > 0L)
            stop(paste0(
              "pd_pit_results_te is missing these keys: ",
              paste(missing_te_keys, collapse = ", ")
            ), call. = FALSE)
          
          te_summary_df_out <- copy(pd_te$pit_summary_pd_te)
          
          data.table::setnames(
            te_summary_df_out,
            old = c(
              "Current law (LCU Mil)",
              "Benchmark (LCU Mil)",
              "Tax Expenditure (LCU Mil)"
            ),
            new = c(
              "current law",
              "benchmark",
              "tax expenditure"
            ),
            skip_absent = TRUE
          )
          
          # Step visible to the user Ã¢ÂÂ 90%
          pit_update_progress(0.90, "Aggregating results...")
          
          calculation_results <- list(
            simulation_year      = as.integer(input$SimulationYear),
            pit_summary_agg_df   = collect_required_object("pit_summary_agg_df"),
            pit_summary_df       = collect_required_object("pit_summary_df"),
            pit_section_summary  = collect_required_object("pit_section_summary"),
            pit_by_taxpayer_type = collect_required_object("pit_by_taxpayer_type"),
            pit_etr_by_section   = collect_required_object("pit_etr_by_section"),
            pit_by_decile        = collect_required_object("pit_by_decile"),
            pit_by_percentile    = collect_required_object("pit_by_percentile"),
            forecast_horizon     = collect_required_object("forecast_horizon"),
            te_summary_df        = te_summary_df_out,
            pd_pit_results_te    = pd_te
          )
          
        } else {
          
          # ----------------------------------------------------------
          # Flow A Ã¢ÂÂ Toggle OFF
          # Each calculator step shown individually.
          # ----------------------------------------------------------
          
          pit_update_progress(0.15, "Running TaxCalculator PD...")
          source_calculation_script("TaxCalculator_PD.R")
          
          pit_update_progress(0.30, "Running TaxCalculator QS...")
          source_calculation_script("TaxCalculator_QS.R")
          
          pit_update_progress(0.45, "Running TaxCalculator WM...")
          source_calculation_script("TaxCalculator_WM.R")
          
          pit_update_progress(0.60, "Running TaxCalculator WR...")
          source_calculation_script("TaxCalculator_WR.R")
          
          pit_update_progress(0.72, "Running TaxCalculator IS...")
          source_calculation_script("TaxCalculator_IS.R")
          
          pit_update_progress(0.82, "Aggregating results...")
          source_calculation_script("Aggregate-Results.R")
          
          pit_update_progress(0.96, "Collecting results...")
          
          calculation_results <- list(
            simulation_year      = as.integer(input$SimulationYear),
            pit_summary_agg_df   = collect_required_object("pit_summary_agg_df"),
            pit_summary_df       = collect_required_object("pit_summary_df"),
            pit_section_summary  = collect_required_object("pit_section_summary"),
            pit_by_taxpayer_type = collect_required_object("pit_by_taxpayer_type"),
            pit_etr_by_section   = collect_required_object("pit_etr_by_section"),
            pit_by_decile        = collect_required_object("pit_by_decile"),
            pit_by_percentile    = collect_required_object("pit_by_percentile"),
            forecast_horizon     = collect_required_object("forecast_horizon"),
            te_summary_df        = NULL,
            pd_pit_results_te    = NULL
          )
        }
      },
      error = function(e) {
        calculation_error <<- conditionMessage(e)
      }
    )
    
    pit_hide_progress()
    
    if (!is.null(calculation_error)) {
      showModal(modalDialog(
        title = "Calculation error",
        calculation_error,
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    
    reactive_simulation_results(calculation_results)
    
    showModal(modalDialog(
      title = "Success", "Simulation is done.",
      easyClose = TRUE, footer = NULL
    ))
    
    tryCatch(
      updateCharts(),
      error = function(e) {
        showModal(modalDialog(
          title = "Chart error",
          paste0("The simulation completed, but chart creation failed: ",
                 conditionMessage(e)),
          easyClose = TRUE, footer = NULL
        ))
      }
    )
  })
  
  
  # Step13 Render result tables ----
  
  output$PIT_SUMMARY_AGG_TABLES <- renderDT({
    req(reactive_simulation_results())
    results <- reactive_simulation_results()
    fh      <- results$forecast_horizon
    datatable(
      results$pit_summary_agg_df,
      caption = tags$caption(
        paste("Aggregate Fiscal Impact (LCU Mil),", min(fh), "-", max(fh)),
        class = "table-caption-bold"
      ),
      extensions = "Buttons",
      options = list(
        pageLength = 15, dom = "Blfrtip",
        buttons    = c("copy", "csv", "excel", "print", "pdf"),
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      ),
      rownames = FALSE
    )
  })
  
  
  #==============================================================================
  # Distributional results: Gross Income Deciles / Percentiles
  #==============================================================================
  
  output$PIT_DISTRIBUTION_TABLE <- renderDT({
    
    req(
      reactive_simulation_results()
    )
    
    req(
      input$pit_distribution_group
    )
    
    
    results <- reactive_simulation_results()
    
    
    #--------------------------------------------------------------------------
    # Select Decile or Percentile table
    #--------------------------------------------------------------------------
    
    if (
      identical(
        input$pit_distribution_group,
        "percentile"
      )
    ) {
      
      req(
        results$pit_by_percentile
      )
      
      distribution_dt <- data.table::as.data.table(
        data.table::copy(
          results$pit_by_percentile
        )
      )
      
      
      caption_text <- paste0(
        "PIT Distribution by Gross Income Percentile - ",
        results$simulation_year
      )
      
      
      page_length <- 25L
      
      
    } else {
      
      req(
        results$pit_by_decile
      )
      
      distribution_dt <- data.table::as.data.table(
        data.table::copy(
          results$pit_by_decile
        )
      )
      
      
      caption_text <- paste0(
        "PIT Distribution by Gross Income Decile - ",
        results$simulation_year
      )
      
      
      page_length <- 10L
    }
    
    
    #--------------------------------------------------------------------------
    # Table
    #--------------------------------------------------------------------------
    
    datatable(
      
      distribution_dt,
      
      
      # Caption
      caption = tags$caption(
        caption_text,
        class = "table-caption-bold"
      ),
      
      
      # Export buttons
      extensions = "Buttons",
      
      
      options = list(
        
        pageLength = page_length,
        
        dom = "Blfrtip",
        
        scrollX = TRUE,
        
        autoWidth = TRUE,
        
        
        buttons = c(
          "copy",
          "csv",
          "excel",
          "print",
          "pdf"
        ),
        
        
        lengthMenu = list(
          c(
            10,
            25,
            50,
            100,
            -1
          ),
          c(
            10,
            25,
            50,
            100,
            "All"
          )
        ),
        
        
        columnDefs = list(
          
          list(
            className = "dt-center",
            targets   = "_all"
          )
          
        )
      ),
      
      
      rownames = FALSE
      
    ) %>%
      
      
      #==========================================================================
    # Gross Income - 2 decimal places
    #==========================================================================
    
    formatRound(
      
      columns = c(
        "Gross Income - Current Law (LCU Mil)",
        "Gross Income - Simulation (LCU Mil)"
      ),
      
      digits = 2
      
    ) %>%
      
      
      #==========================================================================
    # PIT - 2 decimal places
    #==========================================================================
    
    formatRound(
      
      columns = c(
        "PIT - Current Law (LCU Mil)",
        "PIT - Simulation (LCU Mil)",
        "PIT Fiscal Impact (LCU Mil)"
      ),
      
      digits = 2
      
    ) %>%
      
      
      #==========================================================================
    # ETR - 2 decimal places
    #==========================================================================
    
    formatRound(
      
      columns = c(
        "ETR - Current Law (%)",
        "ETR - Simulation (%)",
        "ETR Change (pp)"
      ),
      
      digits = 2
      
    ) %>%
      
      
      #==========================================================================
    # Taxpayer count - no decimals
    #==========================================================================
    
    formatRound(
      
      columns = "Taxpayers",
      
      digits = 0
      
    )
    
  })
  output$PIT_SUMMARY_TABLES <- renderDT({
    req(reactive_simulation_results())
    results <- reactive_simulation_results()
    fh      <- results$forecast_horizon
    datatable(
      results$pit_summary_df,
      caption = tags$caption(
        paste("Disaggregated Fiscal Impact by Module (LCU Mil),", min(fh), "-", max(fh)),
        class = "table-caption-bold"
      ),
      extensions = "Buttons",
      options = list(
        pageLength = 15, dom = "Blfrtip",
        buttons    = c("copy", "csv", "excel", "print", "pdf"),
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      ),
      rownames = FALSE
    )
  })
  
  output$TE_TABLES <- renderDT({
    if (!isTRUE(input$toggleSimulationRates)) return(NULL)
    req(reactive_simulation_results())
    results <- reactive_simulation_results()
    req(results$pd_pit_results_te)
    fh <- results$forecast_horizon
    
    # Pivot te_split_combined wide: one row per year, one column per incentive
    te_long <- data.table::as.data.table(
      data.table::copy(results$pd_pit_results_te$te_split_combined)
    )
    
    te_wide <- data.table::dcast(
      te_long,
      year ~ incentive,
      value.var      = "te_lcu",
      fun.aggregate  = sum
    )
    
    # Add Total column
    incentive_cols <- setdiff(names(te_wide), "year")
    te_wide[, Total := rowSums(.SD, na.rm = TRUE), .SDcols = incentive_cols]
    
    # Convert LCU to LCU Thousands
    numeric_cols <- setdiff(names(te_wide), "year")
    te_wide[
      ,
      (numeric_cols) := lapply(.SD, function(x) round(x / 1e3, 3)),
      .SDcols = numeric_cols
    ]
    
    data.table::setorder(te_wide, year)
    
    datatable(
      te_wide,
      caption = tags$caption(
        paste("Tax Expenditures by Incentive Type (LCU Thousands),", min(fh), "-", max(fh)),
        class = "table-caption-bold"
      ),
      extensions = "Buttons",
      options = list(
        pageLength = 15,
        dom        = "Blfrtip",
        buttons    = c("copy", "csv", "excel", "print", "pdf"),
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All")),
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = numeric_cols, digits = 3)
  })
  
  
  # Step14 Build charts ----
  
  updateCharts <- function() {
    results <- reactive_simulation_results()
    if (is.null(results)) return(invisible(NULL))
    
    chart_type <- isolate(input$chartSelectPIT_Revenues)
    
    # ---- Revenue_Charts ----
    if (identical(chart_type, "Revenue_Charts")) {
      
      chart_script <- file.path(path1, "Scripts", "PIT", "Charts-PIT_Revenues.R")
      if (!file.exists(chart_script))
        stop(paste0("Chart script not found: ", chart_script), call. = FALSE)
      
      tryCatch(
        source(chart_script, local = .GlobalEnv, echo = FALSE, print.eval = FALSE),
        error = function(e)
          stop(paste0("Error sourcing Charts-PIT_Revenues.R: ", conditionMessage(e)),
               call. = FALSE)
      )
      
      if (!exists("Revenue_Charts", mode = "function"))
        stop("Charts-PIT_Revenues.R did not create Revenue_Charts().", call. = FALSE)
      
      revenue_arguments <- list(
        pit_summary_df       = results$pit_summary_df,
        pit_section_summary  = results$pit_section_summary,
        pit_by_taxpayer_type = results$pit_by_taxpayer_type,
        pit_etr_by_section   = results$pit_etr_by_section,
        pit_by_decile       = results$pit_by_decile,
        SimulationYear       = results$simulation_year,
        forecast_horizon     = results$forecast_horizon
        
        
      )
      
      missing_args <- names(revenue_arguments)[
        vapply(revenue_arguments, function(x) is.null(x) || length(x) == 0L, logical(1))
      ]
      if (length(missing_args) > 0L)
        stop(paste0("Revenue_Charts() is missing inputs: ",
                    paste(missing_args, collapse = ", ")), call. = FALSE)
      
      charts <- tryCatch(
        do.call(Revenue_Charts, revenue_arguments),
        error = function(e)
          stop(paste0("Revenue_Charts() failed: ", conditionMessage(e)), call. = FALSE)
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
            column(6, plotlyOutput("PIT_RevenuesTotal_plt",      height = "400px")),
            column(6, plotlyOutput("PIT_ETRbyDecile_plt",     height = "400px"))
          ),
          fluidRow(
            column(6, plotlyOutput("PIT_RevenuesByTaxpayer_plt", height = "400px")),
            column(6, plotlyOutput("PIT_ETRbyNACE_plt",          height = "400px"))
          )
        )
      })
      
      output$PIT_RevenuesTotal_plt      <- renderPlotly({ charts$PIT_RevenuesTotal_plt })
      output$PIT_ETRbyDecile_plt     <- renderPlotly({ charts$PIT_ETRbyDecile_plt })
      output$PIT_RevenuesByTaxpayer_plt <- renderPlotly({ charts$PIT_RevenuesByTaxpayer_plt })
      output$PIT_ETRbyNACE_plt          <- renderPlotly({ charts$PIT_ETRbyNACE_plt })
      
      # ---- Tax_Expenditures_Charts ----
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
        output$additionalCharts <- renderUI({
          tags$div(
            class = "te-disabled-panel",
            tags$p(icon("toggle-off")),
            tags$p(tags$strong("Tax Expenditure charts are not available.")),
            tags$p(
              "Go to ", tags$strong("Simulation Parameters"),
              ", enable the ", tags$em("Toggle Tax Expenditures"),
              " switch, and click ", tags$strong("Run Simulation"),
              " to generate tax expenditure charts."
            )
          )
        })
        return(invisible(NULL))
      }
      
      if (is.null(results$pd_pit_results_te)) {
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
                        style = "font-size: 36px; color: #e9a03b;")),
            tags$p(tags$strong("Tax Expenditure charts are not yet computed.")),
            tags$p(
              "The toggle is enabled but the simulation has not been run with it. ",
              "Click ", tags$strong("Run Simulation"),
              " to compute tax expenditure results."
            )
          )
        })
        return(invisible(NULL))
      }
      
      tax_chart_script <- file.path(path1, "Scripts", "PIT", "Charts-TaxExpenditures.R")
      if (!file.exists(tax_chart_script))
        stop(paste0("Chart script not found: ", tax_chart_script), call. = FALSE)
      
      tryCatch(
        source(tax_chart_script, local = .GlobalEnv, echo = FALSE, print.eval = FALSE),
        error = function(e)
          stop(paste0("Error sourcing Charts-TaxExpenditures.R: ", conditionMessage(e)),
               call. = FALSE)
      )
      
      if (!exists("Tax_Expenditures_Charts", mode = "function"))
        stop("Charts-TaxExpenditures.R did not create Tax_Expenditures_Charts().", call. = FALSE)
      
      charts_te <- tryCatch(
        Tax_Expenditures_Charts(
          pd_pit_results_te = results$pd_pit_results_te,
          forecast_horizon  = results$forecast_horizon,
          SimulationYear    = results$simulation_year
        ),
        error = function(e)
          stop(paste0("Tax_Expenditures_Charts() failed: ", conditionMessage(e)), call. = FALSE)
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
            column(6, plotlyOutput("te_agg_plt",         height = "400px")),
            column(6, plotlyOutput("te_groups_plt",       height = "400px"))
          ),
          fluidRow(
            column(6, plotlyOutput("treemap_nace_te_plt", height = "400px")),
            column(6, plotlyOutput("te_taxpayer_plt",     height = "400px"))
          )
        )
      })
      
      output$te_agg_plt          <- renderPlotly({ charts_te$te_agg_plt })
      output$te_groups_plt       <- renderPlotly({ charts_te$te_groups_plt })
      output$treemap_nace_te_plt <- renderPlotly({ charts_te$treemap_nace_te_plt })
      output$te_taxpayer_plt     <- renderPlotly({ charts_te$te_taxpayer_plt })
    }
    
    invisible(TRUE)
  }
  
  
  # Step15 Chart selector observer ----
  
  observeEvent(
    input$chartSelectPIT_Revenues,
    {
      req(reactive_simulation_results())
      tryCatch(
        updateCharts(),
        error = function(e) {
          showModal(modalDialog(
            title = "Chart error", conditionMessage(e),
            easyClose = TRUE, footer = NULL
          ))
        }
      )
    },
    ignoreInit = TRUE
  )
  
  
  # Step16 Stop local app on session end ----
  
  session$onSessionEnded(function() {
    if (interactive()) try(shiny::stopApp(), silent = TRUE)
  })
}


# Step17 Run app ----

shinyApp(ui = ui, server = server)
