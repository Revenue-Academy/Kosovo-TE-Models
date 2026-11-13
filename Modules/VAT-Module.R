
# ============================================================
# VAT STARTUP: LOAD THE ENTIRE SAVED .Rdata WORKSPACE
# ============================================================


.vat_source_folder <- local({
  source_files <- unlist(lapply(sys.frames(), function(fr) {
    f <- get0("ofile", envir = fr, inherits = FALSE, ifnotfound = NULL)
    if (is.character(f) && length(f) == 1L && !is.na(f) && nzchar(f)) f
    else character(0)
  }), use.names = FALSE)
  source_files <- source_files[grepl("^VAT-Module.*\\.R$",
                                     basename(source_files), ignore.case = TRUE)]
  if (length(source_files)) {
    normalizePath(dirname(tail(source_files, 1L)), winslash = "/", mustWork = TRUE)
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  }
})

.vat_workspace_dirs <- unique(normalizePath(c(
  .vat_source_folder,
  file.path(.vat_source_folder, "Data", "VAT"),
  file.path(.vat_source_folder, "Data"),
  dirname(.vat_source_folder),
  file.path(dirname(.vat_source_folder), "Data", "VAT"),
  file.path(dirname(.vat_source_folder), "Data"),
  dirname(dirname(.vat_source_folder)),
  file.path(dirname(dirname(.vat_source_folder)), "Data", "VAT"),
  getwd(),
  file.path(getwd(), "Data", "VAT")
), winslash = "/", mustWork = FALSE))
.vat_workspace_dirs <- .vat_workspace_dirs[dir.exists(.vat_workspace_dirs)]

.vat_find_workspace <- function() {
  configured <- Sys.getenv("VAT_RDATA_FILE", unset = "")
  if (nzchar(configured)) {
    candidates <- unique(c(configured,
                           file.path(.vat_workspace_dirs, configured)))
    matches <- candidates[file.exists(candidates) & !dir.exists(candidates)]
    if (!length(matches)) stop(
      "VAT_RDATA_FILE does not point to an existing file: ", configured,
      "\nTried:\n", paste(candidates, collapse = "\n"), call. = FALSE
    )
    return(normalizePath(matches[1L], winslash = "/", mustWork = TRUE))
  }

  # Prefer the exact filename used by the working PIT module: .Rdata
  exact <- unlist(lapply(.vat_workspace_dirs, function(d) {
    file.path(d, c(".Rdata", ".RData", ".rdata", ".rda"))
  }), use.names = FALSE)
  exact <- exact[file.exists(exact) & !dir.exists(exact)]
  if (length(exact))
    return(normalizePath(exact[1L], winslash = "/", mustWork = TRUE))

  # Also allow a named workspace such as CIT-inputs.RData, but do not
  # silently select among multiple unrelated workspaces.
  found <- unique(unlist(lapply(.vat_workspace_dirs, function(d) {
    list.files(d, pattern = "\\.([Rr][Dd][Aa][Tt][Aa]|[Rr][Dd][Aa])$",
               all.files = TRUE, full.names = TRUE)
  }), use.names = FALSE))
  named <- found[grepl("VAT", basename(found), ignore.case = TRUE)]
  if (length(named) == 1L)
    return(normalizePath(named[1L], winslash = "/", mustWork = TRUE))
  if (length(found) == 1L)
    return(normalizePath(found[1L], winslash = "/", mustWork = TRUE))
  if (!length(found)) stop(
    "VAT: no .Rdata workspace was found. Put .Rdata beside VAT-Module.R ",
    "or in Data/VAT, or set VAT_RDATA_FILE to the full filename.\n",
    "Folders checked:\n", paste(.vat_workspace_dirs, collapse = "\n"),
    call. = FALSE
  )
  stop(
    "VAT: multiple candidate .Rdata workspaces exist. Set VAT_RDATA_FILE ",
    "to the correct full path. Files:\n",
    paste(found, collapse = "\n"), call. = FALSE
  )
}

.vat_workspace_file <- .vat_find_workspace()
message("VAT: loading ALL saved objects from ", .vat_workspace_file)
.vat_loaded_workspace_names <- tryCatch(
  load(.vat_workspace_file, envir = .GlobalEnv),
  error = function(e) stop(
    "VAT: failed to load .Rdata file: ", .vat_workspace_file,
    "\n", conditionMessage(e), call. = FALSE
  )
)
message("VAT: restored ", length(.vat_loaded_workspace_names),
        " objects (no filtering).")
message("VAT: objects in workspace: ",
        paste(.vat_loaded_workspace_names, collapse = ", "))

# Rdata files created on another machine can contain a stale path1.
# Restore the actual model root that contains Scripts/VAT, if found.
.vat_root_candidates <- unique(normalizePath(c(
  .vat_source_folder, dirname(.vat_source_folder),
  dirname(dirname(.vat_source_folder)), getwd(), dirname(getwd())
), winslash = "/", mustWork = FALSE))
.vat_script_roots <- .vat_root_candidates[vapply(
  .vat_root_candidates,
  function(d) dir.exists(file.path(d, "Scripts", "VAT")), logical(1)
)]
if (length(.vat_script_roots)) {
  path1 <- .vat_script_roots[1L]
  assign("path1", path1, envir = .GlobalEnv)
  message("VAT: calculation script root = ", path1)
} else {
  message("VAT warning: Scripts/VAT not found near the module. ",
          "Any path1 loaded from .Rdata will be used.")
}

options(scipen = 999)

# 1. Dynamic base year and forecast horizon ----

if (!exists("growfactors_vat", envir = .GlobalEnv)) {
  stop("growfactors_vat must be imported before starting the VAT Shiny app.")
}

if (!exists("base_year_VAT", envir = .GlobalEnv)) {
  stop("base_year_VAT must be created by VAT-Init.R before starting the VAT Shiny app.")
}

if (!exists("SUPPLY_ACTIVE", envir = .GlobalEnv) ||
    !exists("USE_ACTIVE", envir = .GlobalEnv) ||
    !exists("USE_VAT_raw", envir = .GlobalEnv)) {
  stop("VAT-Init.R must load SUPPLY_ACTIVE, USE_ACTIVE and USE_VAT_raw before starting the VAT Shiny app.")
}

growfactors_vat <- get("growfactors_vat", envir = .GlobalEnv)

if ("Year" %in% names(growfactors_vat) && !"year" %in% names(growfactors_vat)) {
  names(growfactors_vat)[names(growfactors_vat) == "Year"] <- "year"
}

if ("grow_factor" %in% names(growfactors_vat) && !"growth_factor" %in% names(growfactors_vat)) {
  names(growfactors_vat)[names(growfactors_vat) == "grow_factor"] <- "growth_factor"
}

if (!all(c("year", "growth_factor") %in% names(growfactors_vat))) {
  stop(
    paste0(
      "growfactors_vat must contain year/growth_factor (or original Year/grow_factor). Current columns: ",
      paste(names(growfactors_vat), collapse = ", ")
    )
  )
}

growfactors_vat$year <- as.integer(growfactors_vat$year)
growfactors_vat$growth_factor <- as.numeric(growfactors_vat$growth_factor)
assign("growfactors_vat", growfactors_vat, envir = .GlobalEnv)

vat_input_year <- suppressWarnings(
  as.integer(
    get("base_year_VAT", envir = .GlobalEnv)
  )
)

if (length(vat_input_year) != 1 || is.na(vat_input_year)) {
  stop("base_year_VAT is not a valid single year.")
}

# Keep only years that belong to the active SUT forecast.
# The active SUT already represents the base year, so its growth factor must be 1.
growfactors_vat <- growfactors_vat |>
  dplyr::filter(!is.na(year), year >= vat_input_year) |>
  dplyr::arrange(year) |>
  dplyr::mutate(
    growth_factor = dplyr::if_else(
      year == vat_input_year,
      1,
      growth_factor
    )
  )

assign("growfactors_vat", growfactors_vat, envir = .GlobalEnv)

forecast_horizon <- sort(
  unique(
    growfactors_vat$year
  )
)

forecast_horizon <- forecast_horizon[
  !is.na(forecast_horizon) &
    forecast_horizon >= vat_input_year
]

if (length(forecast_horizon) == 0) {
  stop(
    paste0(
      "No forecast years are available from base year ",
      vat_input_year,
      "."
    )
  )
}

if (!vat_input_year %in% forecast_horizon) {
  stop(
    paste0(
      "The detected SUT base year ",
      vat_input_year,
      " is not available in growfactors_vat. Available years: ",
      paste(sort(unique(growfactors_vat$year)), collapse = ", ")
    )
  )
}

SimulationYear <- vat_input_year
assign("SimulationYear", SimulationYear, envir = .GlobalEnv)
assign("base_year_VAT", vat_input_year, envir = .GlobalEnv)

cat("VAT Shiny base SUT year:", vat_input_year, "\n")
cat("VAT Shiny forecast horizon:", paste(forecast_horizon, collapse = ", "), "\n")


# 2. Source core scripts without year-specific input names ----

replace_vat_year_symbol <- function(x) {
  
  if (is.symbol(x)) {
    symbol_name <- as.character(x)
    
    if (grepl("^VAT_[0-9]{4}$", symbol_name)) {
      return(as.name("USE_VAT_raw"))
    }
    
    if (grepl("^sut_supply_[0-9]{4}$", symbol_name)) {
      return(as.name("SUPPLY_ACTIVE"))
    }
    
    if (grepl("^sut_use_[0-9]{4}$", symbol_name)) {
      return(as.name("USE_ACTIVE"))
    }
    
    return(x)
  }
  
  if (is.call(x)) {
    call_parts <- as.list(x)
    call_parts[] <- lapply(call_parts, replace_vat_year_symbol)
    return(as.call(call_parts))
  }
  
  if (is.pairlist(x)) {
    pair_parts <- as.list(x)
    pair_parts[] <- lapply(pair_parts, replace_vat_year_symbol)
    return(as.pairlist(pair_parts))
  }
  
  if (is.expression(x)) {
    return(
      as.expression(
        lapply(
          as.list(x),
          replace_vat_year_symbol
        )
      )
    )
  }
  
  x
}

source_vat_core_script <- function(file_name) {
  
  script_path <- paste0(
    path1,
    "/Scripts/VAT/",
    file_name
  )
  
  if (!file.exists(script_path)) {
    stop(
      paste0(
        "Script not found: ",
        script_path
      )
    )
  }
  
  parsed_script <- parse(
    file = script_path,
    keep.source = FALSE
  )
  
  parsed_script <- replace_vat_year_symbol(
    parsed_script
  )
  
  eval(
    parsed_script,
    envir = .GlobalEnv
  )
  
  invisible(TRUE)
}

clear_stale_vat_results <- function() {
  
  stale_results <- intersect(
    c(
      "forecast_combined_agg",
      "forecast_combined_agg_tbl_wide",
      "forecast_combined_cpa_selected",
      "forecast_combined_te_selected",
      "forecast_TE_tbl",
      "forecast_TE_tbl_combined",
      "MainResultsVATFinal_BU",
      "MainResultsVATFinal_SIM"
    ),
    ls(envir = .GlobalEnv)
  )
  
  if (length(stale_results) > 0) {
    rm(
      list = stale_results,
      envir = .GlobalEnv
    )
  }
  
  year_specific_inputs <- grep(
    "^(VAT|sut_supply|sut_use)_[0-9]{4}$",
    ls(envir = .GlobalEnv),
    value = TRUE
  )
  
  if (length(year_specific_inputs) > 0) {
    rm(
      list = year_specific_inputs,
      envir = .GlobalEnv
    )
  }
  
  invisible(TRUE)
}

trim_vat_forecast_to_active_years <- function() {
  
  active_years <- as.integer(forecast_horizon)
  active_min <- min(active_years)
  active_max <- max(active_years)
  
  forecast_objects <- grep(
    "^forecast_",
    ls(envir = .GlobalEnv),
    value = TRUE
  )
  
  for (object_name in forecast_objects) {
    object_value <- get(object_name, envir = .GlobalEnv)
    
    if (is.data.frame(object_value) && "year" %in% names(object_value)) {
      object_year <- suppressWarnings(as.integer(as.character(object_value$year)))
      keep <- !is.na(object_year) & object_year >= active_min & object_year <= active_max
      object_value <- object_value[keep, , drop = FALSE]
      object_value$year <- object_year[keep]
      assign(object_name, object_value, envir = .GlobalEnv)
    }
    
    if (is.data.frame(object_value) && "Year" %in% names(object_value)) {
      object_year <- suppressWarnings(as.integer(as.character(object_value$Year)))
      keep <- !is.na(object_year) & object_year >= active_min & object_year <= active_max
      object_value <- object_value[keep, , drop = FALSE]
      object_value$Year <- object_year[keep]
      assign(object_name, object_value, envir = .GlobalEnv)
    }
  }
  
  assign("forecast_horizon", active_years, envir = .GlobalEnv)
  
  invisible(TRUE)
}

# 3. Reload active Supply, Use and VAT before every simulation ----

reload_active_vat_inputs <- function() {
  
  if (!exists("simulation_file", envir = .GlobalEnv)) {
    stop(
      "simulation_file is missing. Run VAT-Init.R before starting the VAT Shiny app.",
      call. = FALSE
    )
  }
  
  active_file <- as.character(
    get("simulation_file", envir = .GlobalEnv)
  )
  
  # VAT-Init FIXED20 stores simulation_file as an absolute path.
  # This fallback also supports older VAT-Init versions where the path
  # was relative and runApp() changed the working directory.
  active_candidates <- active_file
  
  if (!file.exists(active_file)) {
    active_candidates <- c(
      active_candidates,
      file.path(getwd(), active_file),
      file.path("..", active_file),
      file.path("../..", active_file)
    )
    
    if (exists("path1", envir = .GlobalEnv)) {
      active_candidates <- c(
        active_candidates,
        file.path(
          get("path1", envir = .GlobalEnv),
          active_file
        )
      )
    }
  }
  
  active_candidates <- unique(active_candidates)
  existing_candidates <- active_candidates[file.exists(active_candidates)]
  
  if (length(existing_candidates) == 0) {
    stop(
      paste0(
        "Active SUT file not found. Checked: ",
        paste(active_candidates, collapse = " | ")
      ),
      call. = FALSE
    )
  }
  
  active_file <- normalizePath(
    existing_candidates[[1]],
    winslash = "/",
    mustWork = TRUE
  )
  
  assign("simulation_file", active_file, envir = .GlobalEnv)
  
  required_sheets <- c(
    "Supply",
    "Use",
    "VAT"
  )
  
  available_sheets <- readxl::excel_sheets(active_file)
  missing_sheets <- setdiff(required_sheets, available_sheets)
  
  if (length(missing_sheets) > 0) {
    stop(
      paste0(
        basename(active_file),
        " is missing sheet(s): ",
        paste(missing_sheets, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  assign(
    "SUPPLY_ACTIVE",
    tibble::as_tibble(
      readxl::read_excel(
        active_file,
        sheet = "Supply",
        .name_repair = "minimal"
      )
    ),
    envir = .GlobalEnv
  )
  
  assign(
    "USE_ACTIVE",
    tibble::as_tibble(
      readxl::read_excel(
        active_file,
        sheet = "Use",
        .name_repair = "minimal"
      )
    ),
    envir = .GlobalEnv
  )
  
  assign(
    "USE_VAT_raw",
    tibble::as_tibble(
      readxl::read_excel(
        active_file,
        sheet = "VAT",
        .name_repair = "minimal"
      )
    ),
    envir = .GlobalEnv
  )
  
  cat(
    "Reloaded fresh Supply, Use and VAT from:",
    basename(active_file),
    "\n"
  )
  
  invisible(TRUE)
}


# 4. Normalize active SUT/VAT industry headers ----

normalize_vat_industry_header <- function(x) {
  
  x <- stringr::str_squish(as.character(x))
  
  stringr::str_replace(
    x,
    "^([A-U])\\s*[-–—]\\s*",
    "\\1-"
  )
}

prepare_active_vat_inputs <- function() {
  
  product_codes <- LETTERS[1:21]
  
  supply_active <- get("SUPPLY_ACTIVE", envir = .GlobalEnv)
  use_active <- get("USE_ACTIVE", envir = .GlobalEnv)
  use_vat_raw <- get("USE_VAT_raw", envir = .GlobalEnv)
  
  find_industry_positions <- function(df, object_label, allow_vat_position_fallback = FALSE) {
    
    object_names <- names(df)
    object_codes <- stringr::str_extract(
      stringr::str_squish(object_names),
      "^[A-U](?=\\s*[-–—])"
    )
    
    positions <- vapply(
      product_codes,
      function(code) {
        hit <- which(object_codes == code)
        if (length(hit) == 1) hit else NA_integer_
      },
      integer(1)
    )
    
    if (all(!is.na(positions))) {
      return(positions)
    }
    
    if (isTRUE(allow_vat_position_fallback) && ncol(df) >= 24) {
      first_three <- stringr::str_to_lower(
        stringr::str_squish(object_names[1:3])
      )
      
      if (identical(first_three, c("code", "description", "etr"))) {
        return(4:24)
      }
    }
    
    stop(
      paste0(
        "Cannot identify ",
        object_label,
        " industry columns A-U. Current columns: ",
        paste(object_names, collapse = " | ")
      ),
      call. = FALSE
    )
  }
  
  supply_positions <- find_industry_positions(
    supply_active,
    "Supply"
  )
  
  use_positions <- find_industry_positions(
    use_active,
    "Use"
  )
  
  vat_positions <- find_industry_positions(
    use_vat_raw,
    "VAT",
    allow_vat_position_fallback = TRUE
  )
  
  canonical_industry_names <- normalize_vat_industry_header(
    names(use_active)[use_positions]
  )
  
  canonical_codes <- stringr::str_extract(
    canonical_industry_names,
    "^[A-U](?=-)"
  )
  
  if (!identical(as.character(canonical_codes), product_codes)) {
    stop(
      paste0(
        "Cannot standardize active Use industry columns A-U. Current columns: ",
        paste(names(use_active)[use_positions], collapse = " | ")
      ),
      call. = FALSE
    )
  }
  
  names(supply_active)[supply_positions] <- canonical_industry_names
  names(use_active)[use_positions] <- canonical_industry_names
  names(use_vat_raw)[vat_positions] <- canonical_industry_names
  
  if (!all(canonical_industry_names %in% names(use_vat_raw))) {
    stop(
      "VAT industry columns are not aligned with the active Use table after normalization.",
      call. = FALSE
    )
  }
  
  assign("SUPPLY_ACTIVE", supply_active, envir = .GlobalEnv)
  assign("USE_ACTIVE", use_active, envir = .GlobalEnv)
  assign("USE_VAT_raw", use_vat_raw, envir = .GlobalEnv)
  
  cat(
    "Active Supply/Use/VAT industry headers normalized:",
    paste(canonical_industry_names, collapse = " | "),
    "\\n"
  )
  
  invisible(TRUE)
}

reload_active_vat_inputs()
prepare_active_vat_inputs()

# Define UI ------------------------------------------------------------------
ui <- shinydashboard::dashboardPage(
  skin = "blue",
  
  shinydashboard::dashboardHeader(
    titleWidth = 280,
    title = shiny::tags$div(
      class = "header-flex",
      shiny::uiOutput("headerImage"),
      shiny::tags$span("VAT Module", class = "header-title")
    )
  ),
  
  title = "VAT Module",
  
  shinydashboard::dashboardSidebar(
    width = 280,
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Input", tabName = "input", icon = shiny::icon("file-excel")),
      shinydashboard::menuItem(
        "Simulation",
        icon = shiny::icon("sliders"),
        startExpanded = TRUE,
        shinydashboard::menuSubItem("Policy Parameters", tabName = "SettingTaxableProp_SIM", icon = shiny::icon("pen-to-square"))
      ),
      shinydashboard::menuItem(
        "Revenue Impact",
        icon = shiny::icon("magnifying-glass-chart"),
        shinydashboard::menuSubItem("VAT Revenues", tabName = "RevenueImpactSimulation", icon = shiny::icon("bars-progress"))
      ),
      shinydashboard::menuItem(
        "Tax Expenditures",
        icon = shiny::icon("chart-line"),
        shinydashboard::menuSubItem("Tax Expenditures", tabName = "TaxExpenditures", icon = shiny::icon("wallet"))
      ),
      shinydashboard::menuItem(
        "Visualizations",
        icon = shiny::icon("chart-simple"),
        shinydashboard::menuSubItem("Dashboards", tabName = "VAT_Revenues", icon = shiny::icon("chart-column"))
      )
    )
  ),
  
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        body, .content-wrapper, .right-side {
          font-family: 'Segoe UI', Arial, sans-serif;
          background-color: #f4f7fb !important;
        }
        .content-wrapper, .right-side, .content {
          background-color: #f4f7fb !important;
        }
        .skin-blue .main-header .logo {
          background-color: #1f4e79 !important;
          font-weight: 600;
          font-size: 18px;
        }
        .skin-blue .main-header .navbar {
          background-color: #2f6c99 !important;
        }
        .skin-blue .main-sidebar {
          background-color: #1e2d3b !important;
        }
        .skin-blue .sidebar-menu > li.active > a,
        .skin-blue .sidebar-menu > li.menu-open > a {
          border-left-color: #3c8dbc !important;
          background: #243849 !important;
        }
        .skin-blue .sidebar-menu > li:hover > a {
          background: #243849 !important;
        }
        .skin-blue .treeview-menu > li.active > a,
        .skin-blue .treeview-menu > li > a:hover {
          color: #ffffff !important;
          background: #2f3f4f !important;
        }
        .content {
          padding: 18px;
        }
        .box {
          border-radius: 10px;
          border-top: 3px solid #3c8dbc;
          box-shadow: 0 2px 8px rgba(0,0,0,0.08);
          background-color: #f4f7fb !important;
        }
        .box-header.with-border, .box-body {
          background-color: #f4f7fb !important;
        }
        .box-header.with-border {
          border-bottom: 1px solid #d9e2ec;
        }
        .small-box, .info-box {
          border-radius: 10px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        }
        .subtle-note {
          color: #6c7a89;
          font-size: 13px;
        }
        .header-flex {
          display: flex;
          align-items: center;
          gap: 12px;
        }
        .header-title {
          font-size: 22px;
          font-weight: 600;
          white-space: nowrap;
        }
        .btn {
          border-radius: 6px !important;
        }
        .btn-primary {
          background-color: #2f6c99 !important;
          border-color: #2f6c99 !important;
        }
        .btn-success {
          background-color: #2a9d8f !important;
          border-color: #2a9d8f !important;
        }
        .btn-warning {
          background-color: #e9a03b !important;
          border-color: #e9a03b !important;
          color: white !important;
        }
        .control-label {
          font-weight: 600;
          color: #22313f;
        }
        .form-control, .selectize-input {
          border-radius: 6px !important;
          background-color: white !important;
        }
        .compact-input label {
          font-size: 11px;
          margin-bottom: 2px;
        }
        .compact-input .form-control {
          height: 28px;
          padding: 3px 6px;
          font-size: 11px;
        }
        .policy-col {
          min-height: 430px;
        }
        .button-row {
          display: flex;
          gap: 12px;
          flex-wrap: wrap;
          margin-top: 18px;
        }
        .right-button-row {
          display: flex;
          justify-content: flex-end;
          gap: 10px;
          flex-wrap: wrap;
          margin-top: 15px;
        }
        table.dataTable thead th {
          color: #1f2d3d;
          font-weight: 700;
        }
      "))
    ),
    
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "input",
        shiny::fluidRow(
          shinydashboard::box(
            width = 12,
            title = shiny::tags$span(shiny::icon("file-excel"), " Policy Parameters Input"),
            status = "primary",
            solidHeader = FALSE,
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::h4("Import simulation policy parameters from Excel 1"),
                shiny::p(class = "subtle-note", "The product list and policy parameters used in the Simulation tab come only from this imported Excel file."),
                shiny::fileInput(
                  "fileInput",
                  "Upload Excel 1",
                  accept = c(".xlsx", ".xls")
                ),
                shiny::checkboxInput("hasHeader", "Header", TRUE),
                shiny::actionButton("importExcel", "Import Excel Data", icon = shiny::icon("file-import"), class = "btn-primary")
              ),
              shiny::column(
                width = 6,
                shiny::h4("Required columns"),
                shiny::tags$pre(
                  "PRODUCT_INDUSTRY_CODE\nPRODUCT_INDUSTRY_NAME\nCurrent_Policy_Exempt\nCurrent_Policy_Reduced_Rate\nCurrent_Policy_Rate\nCurrent_Policy_Fully_Taxable\nPreferentialVATRate_1\nStandardVATRate\nProportionExempted\nProportionPreferentialRate1\nProportionStandardRate"
                ),
                shiny::uiOutput("policyImportStatus")
              )
            )
          )
        )
      ),
      
      shinydashboard::tabItem(
        tabName = "SettingTaxableProp_SIM",
        shiny::fluidRow(
          shinydashboard::box(
            width = 12,
            title = shiny::tags$span(shiny::icon("sliders"), " Simulation Settings"),
            status = "primary",
            solidHeader = FALSE,
            shiny::fluidRow(
              shiny::column(
                width = 3,
                class = "policy-col",
                shiny::sliderInput(
                  "SimulationYear", "Simulation Year",
                  min = min(forecast_horizon), max = max(forecast_horizon),
                  step = 1, value = vat_input_year,
                  width = "100%", round = 0, sep = ""
                ),
                shiny::sliderInput(
                  "benchmark_tax_rate", "VAT Benchmark Rate",
                  min = 0, max = 1, step = 0.01, value = 0.18,
                  width = "100%", round = 3, sep = ""
                ),
                shiny::uiOutput("productIndustryCombinedSelect_SIM")
              ),
              
              shiny::column(
                width = 3,
                class = "policy-col compact-input",
                shiny::numericInput(
                  "preferentialVATRate1_CPA_SIM",
                  "Preferential VAT Rate 1",
                  value = 0, min = 0, max = 1, step = 0.01
                ),
                shiny::numericInput(
                  "standardVATRate_CPA_SIM",
                  "Standard VAT Rate",
                  value = 0, min = 0, max = 1, step = 0.01
                )
              ),
              
              shiny::column(
                width = 3,
                class = "policy-col compact-input",
                shiny::numericInput(
                  "ProportionExempted_CPA_SIM",
                  "Proportion Exempted",
                  value = 0, min = 0, max = 1, step = 0.01
                ),
                shiny::numericInput(
                  "ProportionPreferentialRate1_CPA_SIM",
                  "Proportion Preferential Rate 1",
                  value = 0, min = 0, max = 1, step = 0.01
                ),
                shiny::numericInput(
                  "ProportionStandardRate_CPA_SIM",
                  "Proportion Standard Rate",
                  value = 0, min = 0, max = 1, step = 0.01
                )
              ),
              
              shiny::column(
                width = 3,
                class = "policy-col compact-input",
                shinyWidgets::switchInput(
                  "toggleSimulationRates_CPA_SIM",
                  "Toggle Simulation Rates",
                  value = FALSE,
                  onLabel = "On",
                  offLabel = "Off"
                ),
                shiny::numericInput(
                  "simPreferentialRate1_CPA_SIM",
                  "Simulation Preferential Rate 1",
                  value = 0.08, min = 0, max = 1, step = 0.01
                ),
                shiny::numericInput(
                  "simStandardRate_CPA_SIM",
                  "Simulation Standard Rate",
                  value = 0.18, min = 0, max = 1, step = 0.01
                ),
                shiny::br(),
                shinyWidgets::prettySwitch(
                  inputId = "apply_next_years",
                  label = "Apply on next years",
                  value = FALSE,
                  status = "primary",
                  fill = TRUE
                ),
                shiny::div(
                  class = "button-row",
                  shiny::actionButton("addVATRateValue_CPA_SIM", "Add to Table", icon = shiny::icon("plus"), class = "btn-success"),
                  shiny::actionButton("clearVATRateTable_CPA_SIM", "Clear Table", icon = shiny::icon("eraser"), class = "btn-warning")
                )
              )
            )
          )
        ),
        
        shiny::fluidRow(
          shinydashboard::box(
            width = 12,
            title = shiny::tags$span(shiny::icon("table"), " Selected Simulation Parameters"),
            status = "primary",
            solidHeader = FALSE,
            shiny::p(class = "subtle-note", "Selected policy changes for the VAT simulation."),
            DT::DTOutput("vatRateTableUpdate_CPA_SIM"),
            shiny::div(
              class = "right-button-row",
              shiny::actionButton("calc_Sim_Button", "Run Simulation", icon = shiny::icon("play"), class = "btn-success")
            )
          )
        )
      ),
      
      shinydashboard::tabItem(
        tabName = "RevenueImpactSimulation",
        shiny::fluidRow(
          shinydashboard::box(
            width = 12,
            title = shiny::tags$span(shiny::icon("coins"), " VAT Revenue Summary"),
            status = "primary",
            solidHeader = FALSE,
            shiny::uiOutput("headerTitle"),
            DT::DTOutput("VAT_SUMMARY_TABLES")
          )
        )
      ),
      
      shinydashboard::tabItem(
        tabName = "TaxExpenditures",
        shiny::fluidRow(
          shinydashboard::box(
            width = 12,
            title = shiny::tags$span(shiny::icon("chart-line"), " Tax Expenditures Summary"),
            status = "primary",
            solidHeader = FALSE,
            shiny::uiOutput("headerTitle"),
            DT::DTOutput("TE_SUMMARY_TABLES")
          )
        )
      ),
      
      shinydashboard::tabItem(
        tabName = "VAT_Revenues",
        shiny::fluidRow(
          shinydashboard::box(
            width = 12,
            title = shiny::tags$span(shiny::icon("chart-column"), " Dashboard Visualizations"),
            status = "primary",
            solidHeader = FALSE,
            shiny::fluidRow(
              shiny::column(
                6,
                shiny::selectInput(
                  "chartSelectVAT_Revenues",
                  "Select Chart",
                  choices = c(
                    "Fiscal Impact",
                    "Sectoral Breakdown of VAT Revenues",
                    "Tax Expenditures",
                    "VAT-GAP Metrics"
                  ),
                  selected = "Fiscal Impact"
                )
              )
            ),
            shiny::fluidRow(
              shinydashboard::infoBoxOutput("infoBox1", width = 6),
              shinydashboard::infoBoxOutput("infoBox2", width = 6)
            ),
            shiny::fluidRow(
              shiny::column(12, shiny::uiOutput("additionalCharts"))
            )
          )
        )
      )
    )
  )
)

# II. Define the main server function-------------------------------------------

mainServer <- function(input, output, session) {
  
  
  # Render the image dynamically using Base64 encoding
  output$headerImage <- shiny::renderUI({
    img_data <- base64enc::dataURI(file = "img/WB_pic.png", mime = "image/png")
    shiny::tags$img(src = img_data, height = "40px", style = "float:left; margin-right:20px;")
  })
  
  
  shiny::observe({
    assign("SimulationYear", input$SimulationYear, envir = .GlobalEnv)
  })
  
  shiny::observe({
    assign("benchmark_tax_rate", input$benchmark_tax_rate, envir = .GlobalEnv)
  })
  
  reactive_simulation_results <- shiny::reactiveVal(NULL)
  
  # Simulation policy data ----
  # IMPORTANT: this table is populated ONLY from the manually imported Excel 1.
  # Do not populate the selector from CPA_TAXABLE_PROPORTIONS_BU.
  excelData <- shiny::reactiveVal(NULL)
  
  cpaData_SIM <- shiny::reactiveVal(
    data.frame(
      PRODUCT_INDUSTRY_CODE = character(),
      PRODUCT_INDUSTRY_NAME = character(),
      Current_Policy_Exempt = numeric(),
      Current_Policy_Reduced_Rate = numeric(),
      Current_Policy_Rate = numeric(),
      Current_Policy_Fully_Taxable = numeric(),
      PreferentialVATRate_1 = numeric(),
      StandardVATRate = numeric(),
      ProportionExempted = numeric(),
      ProportionPreferentialRate1 = numeric(),
      ProportionStandardRate = numeric(),
      stringsAsFactors = FALSE
    )
  )
  
  output$policyImportStatus <- shiny::renderUI({
    df <- excelData()
    if (is.null(df)) {
      return(shiny::tags$p(class = "subtle-note", "No policy Excel file has been imported yet."))
    }
    shiny::tags$div(
      shiny::tags$b("Imported successfully"),
      shiny::tags$br(),
      paste0(nrow(df), " policy rows loaded.")
    )
  })
  
  shiny::observeEvent(input$importExcel, {
    shiny::req(input$fileInput)
    
    required_policy_columns <- c(
      "PRODUCT_INDUSTRY_CODE",
      "PRODUCT_INDUSTRY_NAME",
      "Current_Policy_Exempt",
      "Current_Policy_Reduced_Rate",
      "Current_Policy_Rate",
      "Current_Policy_Fully_Taxable",
      "PreferentialVATRate_1",
      "StandardVATRate",
      "ProportionExempted",
      "ProportionPreferentialRate1",
      "ProportionStandardRate"
    )
    
    data <- tryCatch(
      readxl::read_excel(
        input$fileInput$datapath,
        col_names = isTRUE(input$hasHeader)
      ),
      error = function(e) {
        shiny::showModal(shiny::modalDialog(
          title = "Import Error",
          conditionMessage(e),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      }
    )
    
    if (is.null(data)) return()
    
    if (!all(required_policy_columns %in% names(data))) {
      missing_columns <- setdiff(required_policy_columns, names(data))
      shiny::showModal(shiny::modalDialog(
        title = "Invalid Policy File",
        paste0(
          "Excel 1 is missing the following required column(s): ",
          paste(missing_columns, collapse = ", ")
        ),
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    data <- data[, required_policy_columns, drop = FALSE]
    data$PRODUCT_INDUSTRY_CODE <- as.character(data$PRODUCT_INDUSTRY_CODE)
    data$PRODUCT_INDUSTRY_NAME <- as.character(data$PRODUCT_INDUSTRY_NAME)
    
    numeric_policy_columns <- c(
      "Current_Policy_Exempt",
      "Current_Policy_Reduced_Rate",
      "Current_Policy_Rate",
      "Current_Policy_Fully_Taxable",
      "PreferentialVATRate_1",
      "StandardVATRate",
      "ProportionExempted",
      "ProportionPreferentialRate1",
      "ProportionStandardRate"
    )
    
    for (nm in numeric_policy_columns) {
      data[[nm]] <- suppressWarnings(as.numeric(data[[nm]]))
    }
    
    data$PRODUCT_INDUSTRY_CODE <- trimws(data$PRODUCT_INDUSTRY_CODE)
    data$PRODUCT_INDUSTRY_NAME <- trimws(data$PRODUCT_INDUSTRY_NAME)
    
    excelData(data)
    cpaData_SIM(data)
    
    # ORIGINAL import logic: Excel 1 defines the current-law/BU policy table.
    # The simulation starts as an identical copy and changes only after the
    # user adds reform rows to Selected Simulation Parameters.
    assign("CPA_TAXABLE_PROPORTIONS_BU", data, envir = .GlobalEnv)
    assign("CPA_TAXABLE_PROPORTIONS_SIM", data, envir = .GlobalEnv)
    
    VAT_Rate_Values_CPA_SIM(empty_vat_rate_values_table())
    
    shiny::showModal(shiny::modalDialog(
      title = "Success",
      paste0("Imported ", nrow(data), " policy rows from Excel 1."),
      easyClose = TRUE
    ))
    
    shinydashboard::updateTabItems(session, "tabs", selected = "SettingTaxableProp_SIM")
  })
  
  VAT_Rate_Values_CPA_SIM <- shiny::reactiveVal(data.table::data.table(
    Year = integer(),
    PRODUCT_INDUSTRY_CODE = character(),
    PRODUCT_INDUSTRY_NAME = character(),
    PreferentialVATRate_1 = numeric(),
    StandardVATRate = numeric(),
    Current_Policy_Exempt = numeric(),
    Current_Policy_Reduced_Rate1 = numeric(),
    Current_Policy_Fully_Taxable = numeric(),
    ProportionExempted = numeric(),
    ProportionPreferentialRate1 = numeric(),
    ProportionStandardRate = numeric()
  ))
  
  empty_vat_rate_values_table <- function() {
    data.table::data.table(
      Year = integer(),
      PRODUCT_INDUSTRY_CODE = character(),
      PRODUCT_INDUSTRY_NAME = character(),
      PreferentialVATRate_1 = numeric(),
      StandardVATRate = numeric(),
      Current_Policy_Exempt = numeric(),
      Current_Policy_Reduced_Rate1 = numeric(),
      Current_Policy_Fully_Taxable = numeric(),
      ProportionExempted = numeric(),
      ProportionPreferentialRate1 = numeric(),
      ProportionStandardRate = numeric()
    )
  }
  
  selected_product_SIM <- shiny::reactive({
    sel <- input$productIndustryCombinedSelect_SIM
    if (is.null(sel) || identical(sel, "")) {
      return(list(code = NULL, name = NULL))
    }
    parts <- strsplit(sel, "\\|", fixed = FALSE)[[1]]
    list(
      code = if (length(parts) >= 1) parts[1] else NULL,
      name = if (length(parts) >= 2) parts[2] else NULL
    )
  })
  
  output$productIndustryCombinedSelect_SIM <- shiny::renderUI({
    df <- cpaData_SIM()
    if (is.null(df) || nrow(df) == 0) {
      return(shiny::selectInput(
        "productIndustryCombinedSelect_SIM",
        "Select Product Industry Code and Name",
        choices = NULL
      ))
    }
    df_unique <- unique(df[, c("PRODUCT_INDUSTRY_CODE", "PRODUCT_INDUSTRY_NAME")])
    df_unique <- df_unique[order(df_unique$PRODUCT_INDUSTRY_CODE, df_unique$PRODUCT_INDUSTRY_NAME), ]
    choice_values <- paste(df_unique$PRODUCT_INDUSTRY_CODE, df_unique$PRODUCT_INDUSTRY_NAME, sep = "|")
    choice_labels <- paste(df_unique$PRODUCT_INDUSTRY_CODE, df_unique$PRODUCT_INDUSTRY_NAME, sep = " - ")
    choices <- stats::setNames(choice_values, choice_labels)
    shiny::selectInput(
      "productIndustryCombinedSelect_SIM",
      "Select Product Industry Code and Name",
      choices = choices,
      selected = if (length(choice_values) > 0) choice_values[1] else NULL
    )
  })
  
  update_inputs_for_selected_product <- function() {
    selected <- selected_product_SIM()
    selected_code <- selected$code
    base_data <- cpaData_SIM()
    
    if (is.null(selected_code) || is.null(base_data) || nrow(base_data) == 0) {
      shiny::updateNumericInput(session, "preferentialVATRate1_CPA_SIM", value = 0)
      shiny::updateNumericInput(session, "standardVATRate_CPA_SIM", value = 0)
      shiny::updateNumericInput(session, "ProportionExempted_CPA_SIM", value = 0)
      shiny::updateNumericInput(session, "ProportionPreferentialRate1_CPA_SIM", value = 0)
      shiny::updateNumericInput(session, "ProportionStandardRate_CPA_SIM", value = 0)
      return()
    }
    
    selected_row <- base_data[base_data$PRODUCT_INDUSTRY_CODE == selected_code, , drop = FALSE]
    
    if (nrow(selected_row) < 1) return()
    
    selected_row <- selected_row[1, , drop = FALSE]
    
    shiny::updateNumericInput(session, "preferentialVATRate1_CPA_SIM", value = ifelse(is.na(selected_row$PreferentialVATRate_1), 0, selected_row$PreferentialVATRate_1))
    shiny::updateNumericInput(session, "standardVATRate_CPA_SIM", value = ifelse(is.na(selected_row$StandardVATRate), 0, selected_row$StandardVATRate))
    shiny::updateNumericInput(session, "ProportionExempted_CPA_SIM", value = ifelse(is.na(selected_row$Current_Policy_Exempt), 0, selected_row$Current_Policy_Exempt))
    shiny::updateNumericInput(session, "ProportionPreferentialRate1_CPA_SIM", value = ifelse(is.na(selected_row$Current_Policy_Reduced_Rate), 0, selected_row$Current_Policy_Reduced_Rate))
    shiny::updateNumericInput(session, "ProportionStandardRate_CPA_SIM", value = ifelse(is.na(selected_row$Current_Policy_Fully_Taxable), 0, selected_row$Current_Policy_Fully_Taxable))
  }
  
  reset_simulation_inputs_after_add <- function() {
    selected <- selected_product_SIM()
    selected_code <- selected$code
    base_data <- cpaData_SIM()
    
    if (!is.null(selected_code) && !is.null(base_data) && nrow(base_data) > 0) {
      selected_row <- base_data[base_data$PRODUCT_INDUSTRY_CODE == selected_code, , drop = FALSE]
      
      if (nrow(selected_row) > 0) {
        selected_row <- selected_row[1, , drop = FALSE]
        
        shiny::updateNumericInput(session, "preferentialVATRate1_CPA_SIM", value = ifelse(is.na(selected_row$PreferentialVATRate_1), 0, selected_row$PreferentialVATRate_1))
        shiny::updateNumericInput(session, "standardVATRate_CPA_SIM", value = ifelse(is.na(selected_row$StandardVATRate), 0, selected_row$StandardVATRate))
        shiny::updateNumericInput(session, "ProportionExempted_CPA_SIM", value = ifelse(is.na(selected_row$Current_Policy_Exempt), 0, selected_row$Current_Policy_Exempt))
        shiny::updateNumericInput(session, "ProportionPreferentialRate1_CPA_SIM", value = ifelse(is.na(selected_row$Current_Policy_Reduced_Rate), 0, selected_row$Current_Policy_Reduced_Rate))
        shiny::updateNumericInput(session, "ProportionStandardRate_CPA_SIM", value = ifelse(is.na(selected_row$Current_Policy_Fully_Taxable), 0, selected_row$Current_Policy_Fully_Taxable))
        shiny::updateNumericInput(session, "simPreferentialRate1_CPA_SIM", value = ifelse(is.na(selected_row$PreferentialVATRate_1), 0, selected_row$PreferentialVATRate_1))
        shiny::updateNumericInput(session, "simStandardRate_CPA_SIM", value = ifelse(is.na(selected_row$StandardVATRate), 0, selected_row$StandardVATRate))
      }
    }
    
    shinyWidgets::updateSwitchInput(session, "toggleSimulationRates_CPA_SIM", value = FALSE)
    shinyWidgets::updatePrettySwitch(session, "apply_next_years", value = FALSE)
  }
  
  shiny::observeEvent(input$productIndustryCombinedSelect_SIM, {
    update_inputs_for_selected_product()
  }, ignoreInit = FALSE)
  
  shiny::observeEvent(input$SimulationYear, {
    update_inputs_for_selected_product()
  }, ignoreInit = TRUE)
  
  apply_simulation_changes_automatically <- function() {
    imported_policy <- cpaData_SIM()
    
    if (is.null(imported_policy) || nrow(imported_policy) == 0) {
      stop("Import Excel 1 with the policy parameters before running the simulation.")
    }
    
    if (!exists("CPA_TAXABLE_PROPORTIONS_BU", envir = .GlobalEnv)) {
      stop("CPA_TAXABLE_PROPORTIONS_BU not found. The baseline policy must be available before running the simulation.")
    }
    
    selected_year <- suppressWarnings(as.integer(input$SimulationYear))
    if (is.na(selected_year)) selected_year <- min(forecast_horizon)
    
    # IMPORTANT: preserve the ORIGINAL simulation logic.
    # Every simulation starts from the BU/current-law policy.
    # Excel 1 supplies the GUI parameters; it does NOT become a reform by itself.
    sim_policy <- data.table::as.data.table(data.table::copy(get("CPA_TAXABLE_PROPORTIONS_BU", envir = .GlobalEnv)))
    
    vatRateData_CPA_SIM <- data.table::as.data.table(VAT_Rate_Values_CPA_SIM())
    
    # If the user did not add any reform rows, SIM must be exactly BU.
    if (is.null(vatRateData_CPA_SIM) || nrow(vatRateData_CPA_SIM) == 0) {
      assign("CPA_TAXABLE_PROPORTIONS_SIM", sim_policy, envir = .GlobalEnv)
      return(invisible(sim_policy))
    }
    
    vatRateData_CPA_SIM[, Year := suppressWarnings(as.integer(Year))]
    vatRateData_CPA_SIM <- vatRateData_CPA_SIM[Year == selected_year]
    
    # If no staged reform applies in the selected year, SIM is again exactly BU.
    if (nrow(vatRateData_CPA_SIM) == 0) {
      assign("CPA_TAXABLE_PROPORTIONS_SIM", sim_policy, envir = .GlobalEnv)
      return(invisible(sim_policy))
    }
    
    for (i in seq_len(nrow(vatRateData_CPA_SIM))) {
      row_data <- vatRateData_CPA_SIM[i]
      
      idx <- which(
        trimws(as.character(sim_policy$PRODUCT_INDUSTRY_CODE)) ==
          trimws(as.character(row_data$PRODUCT_INDUSTRY_CODE))
      )
      
      if (length(idx) == 0) {
        stop(
          paste0(
            "The selected policy code was not found in the imported Excel 1 baseline table: ",
            row_data$PRODUCT_INDUSTRY_CODE,
            "."
          )
        )
      }
      
      sim_policy$PreferentialVATRate_1[idx] <- row_data$PreferentialVATRate_1
      sim_policy$StandardVATRate[idx] <- row_data$StandardVATRate
      sim_policy$ProportionExempted[idx] <- row_data$ProportionExempted
      sim_policy$ProportionPreferentialRate1[idx] <- row_data$ProportionPreferentialRate1
      sim_policy$ProportionStandardRate[idx] <- row_data$ProportionStandardRate
    }
    
    assign("CPA_TAXABLE_PROPORTIONS_SIM", sim_policy, envir = .GlobalEnv)
    invisible(sim_policy)
  }
  
  shiny::observeEvent(input$addVATRateValue_CPA_SIM, {
    selected <- selected_product_SIM()
    shiny::req(selected$code)
    selected_year <- suppressWarnings(as.integer(input$SimulationYear))
    if (is.na(selected_year)) selected_year <- min(forecast_horizon)
    years_to_apply <- if (isTRUE(input$apply_next_years)) forecast_horizon[forecast_horizon >= selected_year] else selected_year
    if (length(years_to_apply) == 0) years_to_apply <- selected_year
    newEntry_SIM <- data.table::rbindlist(lapply(years_to_apply, function(y) {
      data.table::data.table(
        Year = suppressWarnings(as.integer(y)),
        PRODUCT_INDUSTRY_CODE = selected$code,
        PRODUCT_INDUSTRY_NAME = selected$name,
        Current_Policy_Exempt = input$ProportionExempted_CPA_SIM,
        Current_Policy_Reduced_Rate1 = input$ProportionPreferentialRate1_CPA_SIM,
        Current_Policy_Fully_Taxable = input$ProportionStandardRate_CPA_SIM,
        PreferentialVATRate_1 = if (isTRUE(input$toggleSimulationRates_CPA_SIM)) input$simPreferentialRate1_CPA_SIM else input$preferentialVATRate1_CPA_SIM,
        StandardVATRate = if (isTRUE(input$toggleSimulationRates_CPA_SIM)) input$simStandardRate_CPA_SIM else input$standardVATRate_CPA_SIM,
        ProportionExempted = input$ProportionExempted_CPA_SIM,
        ProportionPreferentialRate1 = input$ProportionPreferentialRate1_CPA_SIM,
        ProportionStandardRate = input$ProportionStandardRate_CPA_SIM
      )
    }), fill = TRUE)
    current_tbl <- VAT_Rate_Values_CPA_SIM()
    if (!is.null(current_tbl) && nrow(current_tbl) > 0) {
      current_tbl <- current_tbl[!(PRODUCT_INDUSTRY_CODE == selected$code & Year %in% years_to_apply)]
    }
    VAT_Rate_Values_CPA_SIM(data.table::rbindlist(list(current_tbl, newEntry_SIM), fill = TRUE))
    
    # After saving the reform row(s), return the upper controls to the
    # current-law/default values for the selected product. The staged row(s)
    # remain unchanged in Selected Simulation Parameters.
    reset_simulation_inputs_after_add()
  })
  
  shiny::observeEvent(input$clearVATRateTable_CPA_SIM, {
    VAT_Rate_Values_CPA_SIM(empty_vat_rate_values_table())
  })
  
  output$vatRateTableUpdate_CPA_SIM <- DT::renderDT({
    table_to_display <- VAT_Rate_Values_CPA_SIM()[, .(
      Year,
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      PreferentialVATRate_1,
      StandardVATRate,
      ProportionExempted,
      ProportionPreferentialRate1,
      ProportionStandardRate
    )]
    colnames(table_to_display) <- c(
      "Year", "Industry Code", "Industry Description",
      "Pref. Rate 1", "Standard Rate",
      "Exempted", "Pref. Prop. 1", "Standard Prop."
    )
    table_to_display <- table_to_display[order(table_to_display$Year, table_to_display$`Industry Code`), ]
    DT::datatable(table_to_display, options = list(dom = "t", paging = FALSE), editable = TRUE, rownames = FALSE)
  })
  
  # Run simulation ----
  shiny::observeEvent(input$calc_Sim_Button, {
    if (is.null(cpaData_SIM()) || nrow(cpaData_SIM()) == 0) {
      shiny::showModal(shiny::modalDialog(
        title = "Policy Parameters Missing",
        "Please import Excel 1 in the Input tab before running the simulation.",
        easyClose = TRUE
      ))
      return()
    }
    
    apply_simulation_changes_automatically()
    
    # CPA_TAXABLE_PROPORTIONS_SIM intentionally keeps the same 11 columns
    # as the manually imported Excel policy file. No Year/year column is added.
    
    # Standardize EXISTING year keys before the original model scripts run.
    # This does not add year columns and does not change any policy values.
    # It only converts numeric-looking Year/year join keys (for example "2024")
    # to integer so dplyr joins cannot fail because one table stores 2024 as
    # integer while another stores it as character.
    for (object_name in ls(envir = .GlobalEnv)) {
      object_value <- get(object_name, envir = .GlobalEnv)
      
      if (is.data.frame(object_value)) {
        object_changed <- FALSE
        
        if ("Year" %in% names(object_value)) {
          year_text <- trimws(as.character(object_value$Year))
          year_integer <- suppressWarnings(as.integer(year_text))
          year_present <- !is.na(object_value$Year) & nzchar(year_text)
          
          if (all(!year_present | !is.na(year_integer))) {
            object_value$Year <- year_integer
            object_changed <- TRUE
          }
        }
        
        if ("year" %in% names(object_value)) {
          year_text <- trimws(as.character(object_value$year))
          year_integer <- suppressWarnings(as.integer(year_text))
          year_present <- !is.na(object_value$year) & nzchar(year_text)
          
          if (all(!year_present | !is.na(year_integer))) {
            object_value$year <- year_integer
            object_changed <- TRUE
          }
        }
        
        if (object_changed) {
          assign(object_name, object_value, envir = .GlobalEnv)
        }
      }
    }
    
    assign("SimulationYear", as.integer(input$SimulationYear), envir = .GlobalEnv)
    assign("base_year_VAT", vat_input_year, envir = .GlobalEnv)
    clear_stale_vat_results()
    
    cat("Active SUT base year:", vat_input_year, "\n")
    cat("Selected simulation/reform year:", get("SimulationYear", envir = .GlobalEnv), "\n")
    
    shiny::showModal(shiny::modalDialog(
      title = "Running Simulation...",
      "Please wait while the simulation is running...",
      easyClose = FALSE,
      footer = NULL
    ))
    tryCatch({
      reload_active_vat_inputs()
      prepare_active_vat_inputs()
      
      cat("Running VAT-DataTransformation.R\n")
      tryCatch(
        source_vat_core_script("VAT-DataTransformation.R"),
        error = function(e) stop(paste0("VAT-DataTransformation.R: ", conditionMessage(e)), call. = FALSE)
      )
      
      cat("Running TaxCalculator_BU.R\n")
      tryCatch(
        source_vat_core_script("TaxCalculator_BU.R"),
        error = function(e) stop(paste0("TaxCalculator_BU.R: ", conditionMessage(e)), call. = FALSE)
      )
      
      cat("Running TaxCalculator_SIM.R\n")
      tryCatch(
        source_vat_core_script("TaxCalculator_SIM.R"),
        error = function(e) stop(paste0("TaxCalculator_SIM.R: ", conditionMessage(e)), call. = FALSE)
      )
      
      # No-reform control: if no simulation parameters were added,
      # baseline and simulation calibrated VAT must be the same.
      if (nrow(VAT_Rate_Values_CPA_SIM()) == 0) {
        bu_value <- MainResultsVATFinal_BU$Values[MainResultsVATFinal_BU$Descriptions == "Calibrated VAT"][1]
        sim_value <- MainResultsVATFinal_SIM$Values[MainResultsVATFinal_SIM$Descriptions == "Calibrated VAT"][1]
        
        if (length(bu_value) == 0 || length(sim_value) == 0 || is.na(bu_value) || is.na(sim_value)) {
          stop("No-reform validation failed: Calibrated VAT could not be read from BU/SIM results.", call. = FALSE)
        }
        
        if (!isTRUE(all.equal(as.numeric(bu_value), as.numeric(sim_value), tolerance = 1e-8))) {
          stop(
            paste0(
              "No reform was selected, but TaxCalculator_SIM.R produced a different Calibrated VAT. ",
              "BU = ", bu_value, ", SIM = ", sim_value, ". ",
              "This indicates a calculation difference inside TaxCalculator_SIM.R, not a Shiny policy-input change."
            ),
            call. = FALSE
          )
        }
      }
      
      # Re-apply the active SUT year immediately before forecasting.
      # This prevents Forecast-VAT.R from seeing earlier growth-factor years.
      active_growfactors_vat <- get("growfactors_vat", envir = .GlobalEnv) |>
        dplyr::filter(year >= vat_input_year) |>
        dplyr::arrange(year) |>
        dplyr::mutate(
          growth_factor = dplyr::if_else(year == vat_input_year, 1, growth_factor)
        )
      assign("growfactors_vat", active_growfactors_vat, envir = .GlobalEnv)
      assign("base_year_VAT", vat_input_year, envir = .GlobalEnv)
      
      cat("Running Forecast-VAT.R\n")
      tryCatch(
        source_vat_core_script("Forecast-VAT.R"),
        error = function(e) stop(paste0("Forecast-VAT.R: ", conditionMessage(e)), call. = FALSE)
      )
      
      # Defensive cleanup: no forecast object may contain a year before the
      # detected SUT base year or after the configured forecast horizon.
      trim_vat_forecast_to_active_years()
      
      if (exists("forecast_combined_agg", envir = .GlobalEnv)) {
        forecast_years_check <- sort(unique(as.integer(get("forecast_combined_agg", envir = .GlobalEnv)$year)))
        cat("Forecast years after active-year cleanup:", paste(forecast_years_check, collapse = ", "), "\n")
      }
      
      # Recalculate Tax Expenditures as % of GDP from nominal_gdp_vat.
      # Monetary model values remain unchanged internally; this only repairs the reporting ratio.
      if (exists("nominal_gdp_vat", envir = .GlobalEnv)) {
        nominal_gdp_vat <- get("nominal_gdp_vat", envir = .GlobalEnv)
        nominal_gdp_vat$year <- as.integer(nominal_gdp_vat$year)
        
        if (exists("forecast_TE_tbl", envir = .GlobalEnv)) {
          forecast_TE_tbl <- get("forecast_TE_tbl", envir = .GlobalEnv)
          forecast_TE_tbl$year <- as.integer(forecast_TE_tbl$year)
          forecast_TE_tbl <- forecast_TE_tbl |>
            dplyr::select(-dplyr::any_of(c("Tax Expenditures (Pct of GDP)", "gdp"))) |>
            dplyr::left_join(
              nominal_gdp_vat |> dplyr::select(year, gdp),
              by = "year"
            )
          
          if ("Tax Expenditures (In EUR million)" %in% names(forecast_TE_tbl)) {
            forecast_TE_tbl <- forecast_TE_tbl |>
              dplyr::mutate(`Tax Expenditures (Pct of GDP)` = round((`Tax Expenditures (In EUR million)` / gdp) * 100, 2))
          } else if ("Tax Expenditures (In EUR thousand)" %in% names(forecast_TE_tbl)) {
            forecast_TE_tbl <- forecast_TE_tbl |>
              dplyr::mutate(`Tax Expenditures (Pct of GDP)` = round((`Tax Expenditures (In EUR thousand)` / (gdp * 1000)) * 100, 2))
          }
          
          forecast_TE_tbl <- forecast_TE_tbl |> dplyr::select(-gdp)
          assign("forecast_TE_tbl", forecast_TE_tbl, envir = .GlobalEnv)
        }
        
        if (exists("forecast_TE_tbl_combined", envir = .GlobalEnv)) {
          forecast_TE_tbl_combined <- get("forecast_TE_tbl_combined", envir = .GlobalEnv)
          forecast_TE_tbl_combined$year <- as.integer(forecast_TE_tbl_combined$year)
          forecast_TE_tbl_combined <- forecast_TE_tbl_combined |>
            dplyr::select(-dplyr::any_of(c("Tax Expenditures (Pct of GDP)", "gdp"))) |>
            dplyr::left_join(
              nominal_gdp_vat |> dplyr::select(year, gdp),
              by = "year"
            )
          
          if ("Tax Expenditures (In EUR million)" %in% names(forecast_TE_tbl_combined)) {
            forecast_TE_tbl_combined <- forecast_TE_tbl_combined |>
              dplyr::mutate(`Tax Expenditures (Pct of GDP)` = round((`Tax Expenditures (In EUR million)` / gdp) * 100, 2))
          } else if ("Tax Expenditures (In EUR thousand)" %in% names(forecast_TE_tbl_combined)) {
            forecast_TE_tbl_combined <- forecast_TE_tbl_combined |>
              dplyr::mutate(`Tax Expenditures (Pct of GDP)` = round((`Tax Expenditures (In EUR thousand)` / (gdp * 1000)) * 100, 2))
          }
          
          forecast_TE_tbl_combined <- forecast_TE_tbl_combined |> dplyr::select(-gdp)
          assign("forecast_TE_tbl_combined", forecast_TE_tbl_combined, envir = .GlobalEnv)
        }
      }
      
      cat("Running ChartsPreparation-VAT.R\n")
      tryCatch(
        source_vat_core_script("ChartsPreparation-VAT.R"),
        error = function(e) stop(paste0("ChartsPreparation-VAT.R: ", conditionMessage(e)), call. = FALSE)
      )
      
      trim_vat_forecast_to_active_years()
      
      reactive_simulation_results(list(
        vat_summary = get("forecast_combined_agg_tbl_wide", envir = .GlobalEnv),
        te_summary = get("forecast_TE_tbl", envir = .GlobalEnv)
      ))
      shiny::removeModal()
      shiny::showModal(shiny::modalDialog(
        title = "Success",
        "Simulation and chart preparation are complete!",
        easyClose = TRUE
      ))
      chart_type <- shiny::isolate(input$chartSelectVAT_Revenues)
      updateCharts(chart_type)
    }, error = function(e) {
      shiny::removeModal()
      shiny::showModal(shiny::modalDialog(
        title = "Error",
        paste("Error during simulation:", conditionMessage(e)),
        easyClose = TRUE
      ))
    })
  })
  
  # Tables ------------------------------------------------------------------
  
  # VAT-Fiscal Impact -------------------------------------------------------
  output$VAT_SUMMARY_TABLES <- DT::renderDT({
    # Check if the reactive simulation results exist and contain data
    shiny::req(reactive_simulation_results())
    vat_summary <- reactive_simulation_results()$vat_summary
    
    if (!is.null(vat_summary) && "year" %in% names(vat_summary)) {
      vat_summary$year <- suppressWarnings(as.integer(as.character(vat_summary$year)))
      vat_summary <- vat_summary |>
        dplyr::filter(year %in% forecast_horizon) |>
        dplyr::arrange(year)
    }
    
    # Reporting unit: EUR million, one decimal.
    if (all(c(
      "Current Law (In EUR thousand)",
      "Simulation (In EUR thousand)",
      "Fiscal Impact (In EUR thousand)"
    ) %in% names(vat_summary))) {
      vat_summary <- vat_summary |>
        dplyr::mutate(
          `Current Law (In EUR million)` = round(`Current Law (In EUR thousand)` / 1000, 1),
          `Simulation (In EUR million)` = round(`Simulation (In EUR thousand)` / 1000, 1),
          `Fiscal Impact (In EUR million)` = round(`Fiscal Impact (In EUR thousand)` / 1000, 1)
        ) |>
        dplyr::select(
          year,
          `Current Law (In EUR million)`,
          `Simulation (In EUR million)`,
          `Fiscal Impact (In EUR million)`,
          dplyr::everything(),
          -`Current Law (In EUR thousand)`,
          -`Simulation (In EUR thousand)`,
          -`Fiscal Impact (In EUR thousand)`
        )
    }
    
    # If the vat_summary data is NULL or empty, display a placeholder table
    if (is.null(vat_summary) || nrow(vat_summary) == 0) {
      DT::datatable(
        data.frame(Message = "Data is not available yet. Please run the simulation."),
        options = list(
          dom = "t",  # Show only the table without controls
          paging = FALSE  # Disable pagination
        ),
        rownames = FALSE
      )
    } else {
      # Render the table with the actual data
      DT::datatable(
        vat_summary,
        caption = shiny::tags$caption(
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
                  body = htmlwidgets::JS("function(data, row, column, node) {
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
                  body = htmlwidgets::JS("function(data, row, column, node) {
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
                  body = htmlwidgets::JS("function(data, row, column, node) {
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
  
  output$TE_SUMMARY_TABLES <- DT::renderDT({
    # Check if the reactive simulation results exist and contain data
    shiny::req(reactive_simulation_results())
    te_summary <- reactive_simulation_results()$te_summary
    
    if (!is.null(te_summary) && "year" %in% names(te_summary)) {
      te_summary$year <- suppressWarnings(as.integer(as.character(te_summary$year)))
      te_summary <- te_summary |>
        dplyr::filter(year %in% forecast_horizon) |>
        dplyr::arrange(year)
    }
    
    # Reporting unit: EUR million, one decimal.
    if ("Tax Expenditures (In EUR thousand)" %in% names(te_summary)) {
      te_summary <- te_summary |>
        dplyr::mutate(
          `Tax Expenditures (In EUR million)` = round(`Tax Expenditures (In EUR thousand)` / 1000, 1)
        ) |>
        dplyr::select(
          year,
          `Tax Expenditures (In EUR million)`,
          dplyr::everything(),
          -`Tax Expenditures (In EUR thousand)`
        )
    }
    
    # If the vat_summary data is NULL or empty, display a placeholder table
    if (is.null(te_summary) || nrow(te_summary) == 0) {
      DT::datatable(
        data.frame(Message = "Data is not available yet. Please run the simulation."),
        options = list(
          dom = "t",  # Show only the table without controls
          paging = FALSE  # Disable pagination
        ),
        rownames = FALSE
      )
    } else {
      # Render the table with the actual data
      DT::datatable(
        te_summary,
        caption = shiny::tags$caption(
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
                  body = htmlwidgets::JS("function(data, row, column, node) {
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
                  body = htmlwidgets::JS("function(data, row, column, node) {
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
                  body = htmlwidgets::JS("function(data, row, column, node) {
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
    
    if (is.null(reactive_simulation_results())) {
      output$infoBox1 <- shinydashboard::renderInfoBox({ NULL })
      output$infoBox2 <- shinydashboard::renderInfoBox({ NULL })
      output$additionalCharts <- shiny::renderUI({
        shiny::tags$div(
          style = "padding: 20px; color: #6c7a89;",
          shiny::tags$b("Please run the VAT simulation first."),
          shiny::tags$p("After the simulation is complete, the selected dashboard charts will appear here.")
        )
      })
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
        # source("Scripts/VAT/Charts-VAT_GAP_METRICS.R")
        source(paste0(path1, "/Scripts/VAT/Charts-VAT_GAP_METRICS.R"))
        
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
      output$infoBox1 <- shinydashboard::renderInfoBox({
        result1 <- if (exists("MainResultsVATFinal_BU", envir = .GlobalEnv)) {
          MainResultsVATFinal_BU$Values[8]
        } else {
          NA
        }
        shinydashboard::infoBox(
          #"Total VAT GAP", paste("in",SimulationYear),
          #paste("Total VAT GAP in",SimulationYear),
          paste("Total VAT GAP in", SimulationYear),
          value = if (!is.na(result1)) paste(round(result1, 1), "(in percentage)") else "Data Missing",
          icon = shiny::icon("chart-pie"),
          color = "orange"
        )
      })
      
      output$infoBox2 <- shinydashboard::renderInfoBox({
        result2 <- if (exists("MainResultsVATFinal_BU", envir = .GlobalEnv)) {
          MainResultsVATFinal_BU$Values[10]
        } else {
          NA
        }
        shinydashboard::infoBox(
          #"Compliance Gap",
          #paste("Compliance Gap in",SimulationYear),
          paste("Compliance Gap in", SimulationYear),
          value = if (!is.na(result2)) paste(round(result2, 1), "(in percentage)") else "Data Missing",
          icon = shiny::icon("chart-pie"),
          color = "light-blue"
        )
      })
      
      # Render UI for charts
      output$additionalCharts <- shiny::renderUI({
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(6, plotly::plotlyOutput("vat_rev_tbl_plt", height = "400px")),
            shiny::column(6, plotly::plotlyOutput("vat_gap_tbl_plt", height = "400px"))
          ),
          shiny::fluidRow(
            shiny::column(6, plotly::plotlyOutput("VAT_GAP_plt", height = "400px")),
            shiny::column(6, plotly::plotlyOutput("vat_str_plt", height = "400px"))
          )
        )
      })
      # Render each chart with Plotly
      output$vat_rev_tbl_plt <- plotly::renderPlotly({ charts$vat_rev_tbl_plt })
      output$vat_gap_tbl_plt <- plotly::renderPlotly({ charts$vat_gap_tbl_plt })
      output$VAT_GAP_plt <- plotly::renderPlotly({ charts$VAT_GAP_plt })
      output$vat_str_plt <- plotly::renderPlotly({ charts$vat_str_plt })
      
    } else if (chart_type == "Sectoral Breakdown of VAT Revenues") {
      cat("Preparing Tax Expenditures Charts\n")
      tryCatch({
        #source("Scripts/VAT/Charts-VAT_Sectors_Revenues.R")
        source(paste0(path1, "/Scripts/VAT/Charts-VAT_Sectors_Revenues.R"))
        
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
      output$infoBox1 <- shinydashboard::renderInfoBox({
        FiscalImpact<- forecast_combined_agg |>
          dplyr::filter(
            year == SimulationYear,
            Descriptions == "Calibrated VAT",
            scenario %in% c("Baseline", "Simulation")) |>
          tidyr::pivot_wider(names_from = scenario, values_from = value) |>
          dplyr::mutate(difference = Simulation - Baseline) |>
          dplyr::select(difference)
        
        #Results$Simulation$Simulated_Change_in_Revenues.LCU
        shinydashboard::infoBox(
          #"Fiscal Impact (In EUR thousand)",
          paste("Fiscal Impact (In EUR million) in",SimulationYear),
          #paste0(round(FiscalImpact$difference),1),
          round(FiscalImpact$difference / 1000, 1),
          icon = shiny::icon("chart-simple"),
          color = "orange"
        )
      })
      output$infoBox2 <- shinydashboard::renderInfoBox({
        # Calculate the Fiscal Impact and the difference
        FiscalImpact <- forecast_combined_agg |>
          dplyr::filter(
            year == SimulationYear,
            Descriptions == "Calibrated VAT",
            scenario %in% c("Baseline", "Simulation")
          ) |>
          tidyr::pivot_wider(names_from = scenario, values_from = value) |>
          dplyr::mutate(difference = Simulation - Baseline)
        
        # Calculate the Share of Revenues (Baseline)
        ShareOfRevenuesVAT <- FiscalImpact$Baseline
        
        # Calculate Participation Percentage
        ParticipationPercentage <- (FiscalImpact$difference / ShareOfRevenuesVAT) * 100
        
        # Render the info box
        shinydashboard::infoBox(
          #"As percentage of VAT revenues",
          paste("As percentage of VAT revenues in",SimulationYear),
          round(ParticipationPercentage, 1),
          icon = shiny::icon("chart-pie"),
          color = "blue"
        )
      })
      
      
      # Render UI for  charts
      output$additionalCharts <- shiny::renderUI({
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(6, plotly::plotlyOutput("vat_nace_total_plt", height = "400px")),
            shiny::column(6, plotly::plotlyOutput("vat_nace_hh_plt", height = "400px"))
          ),
          shiny::fluidRow(
            shiny::column(6, plotly::plotlyOutput("vat_nace_businesses_plt", height = "400px")),
            shiny::column(6, plotly::plotlyOutput("vat_nace_gov_plt", height = "400px"))
          )
        )
      })
      
      
      
      # Render each chart with Plotly
      output$vat_nace_total_plt <- plotly::renderPlotly({ VAT_sectors_charts$vat_nace_total_plt })
      output$vat_nace_hh_plt <- plotly::renderPlotly({ VAT_sectors_charts$vat_nace_hh_plt })
      output$vat_nace_businesses_plt <- plotly::renderPlotly({ VAT_sectors_charts$vat_nace_businesses_plt })
      output$vat_nace_gov_plt <- plotly::renderPlotly({ VAT_sectors_charts$vat_nace_gov_plt })
    } 
    # OD OVDE 
    else if (chart_type == "Fiscal Impact") {
      cat("Preparing Tax Expenditures Charts\n")
      tryCatch({
        #source("Scripts/VAT/Charts-VAT_AggregateRevenues.R")
        source(paste0(path1, "/Scripts/VAT/Charts-VAT_AggregateRevenues.R"))
        
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
      output$infoBox1 <- shinydashboard::renderInfoBox({
        FiscalImpact<- forecast_combined_agg |>
          dplyr::filter(
            year == SimulationYear,
            Descriptions == "Calibrated VAT",
            scenario %in% c("Baseline", "Simulation")) |>
          tidyr::pivot_wider(names_from = scenario, values_from = value) |>
          dplyr::mutate(difference = Simulation - Baseline) |>
          dplyr::select(difference)
        
        #Results$Simulation$Simulated_Change_in_Revenues.LCU
        shinydashboard::infoBox(
          #"Fiscal Impact (In EUR thousand)",
          paste("Fiscal Impact (In EUR million) in",SimulationYear),
          #paste0(round(FiscalImpact$difference),1),
          round(FiscalImpact$difference / 1000, 1),
          icon = shiny::icon("chart-simple"),
          color = "orange"
        )
      })
      output$infoBox2 <- shinydashboard::renderInfoBox({
        # Calculate the Fiscal Impact and the difference
        FiscalImpact <- forecast_combined_agg |>
          dplyr::filter(
            year == SimulationYear,
            Descriptions == "Calibrated VAT",
            scenario %in% c("Baseline", "Simulation")
          ) |>
          tidyr::pivot_wider(names_from = scenario, values_from = value) |>
          dplyr::mutate(difference = Simulation - Baseline)
        
        # Calculate the Share of Revenues (Baseline)
        ShareOfRevenuesVAT <- FiscalImpact$Baseline
        
        # Calculate Participation Percentage
        ParticipationPercentage <- (FiscalImpact$difference / ShareOfRevenuesVAT) * 100
        
        # Render the info box
        shinydashboard::infoBox(
          #"As percentage of VAT revenues",
          paste("As percentage of VAT revenues in",SimulationYear),
          round(ParticipationPercentage, 1),
          icon = shiny::icon("chart-pie"),
          color = "blue"
        )
      })
      
      
      # Render UI for  charts
      output$additionalCharts <- shiny::renderUI({
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(6, plotly::plotlyOutput("vat_revenue_plt", height = "400px")),
            shiny::column(6, plotly::plotlyOutput("vat_revenue_gdp_plt", height = "400px"))
          ),
          shiny::fluidRow(
            shiny::column(6, plotly::plotlyOutput("vat_structure_plt", height = "400px")),
            shiny::column(6, plotly::plotlyOutput("vat_treemap_sectors_plt", height = "400px"))
          )
        )
      })
      
      # Render each chart with Plotly
      output$vat_revenue_plt <- plotly::renderPlotly({ VAT_sectors_charts$vat_revenue_plt })
      output$vat_revenue_gdp_plt <- plotly::renderPlotly({ VAT_sectors_charts$vat_revenue_gdp_plt })
      output$vat_structure_plt <- plotly::renderPlotly({ VAT_sectors_charts$vat_structure_plt })
      output$vat_treemap_sectors_plt <- plotly::renderPlotly({ VAT_sectors_charts$vat_treemap_sectors_plt })
    }
    
    else if (chart_type == "Tax Expenditures") {
      cat("Preparing Tax Expenditures Charts\n")
      tryCatch({
        #source("Scripts/VAT/Charts-TaxExpenditures.R")
        source(paste0(path1, "/Scripts/VAT/Charts-TaxExpenditures.R"))
        
      }, error = function(e) {
        cat("Error sourcing Charts-TaxExpenditures.R:", e$message, "\n")
        return()
      })
      output$infoBox1 <- shinydashboard::renderInfoBox({
        
        if ("Tax Expenditures (In EUR million)" %in% names(forecast_TE_tbl_combined)) {
          te_infobox_lcu <- forecast_TE_tbl_combined |>
            dplyr::filter(year == SimulationYear) |>
            dplyr::filter(scenario == "Simulation") |>
            dplyr::pull(`Tax Expenditures (In EUR million)`)
        } else {
          te_infobox_lcu <- forecast_TE_tbl_combined |>
            dplyr::filter(year == SimulationYear) |>
            dplyr::filter(scenario == "Simulation") |>
            dplyr::pull(`Tax Expenditures (In EUR thousand)`) / 1000
        }
        
        shinydashboard::infoBox(
          #"Tax Expenditures (In EUR thousand)",
          paste("Tax Expenditures (In EUR million) in",SimulationYear),
          paste0(round(te_infobox_lcu, 1)),
          icon = shiny::icon("chart-line"),
          color = "red"
        )
      })
      
      output$infoBox2 <- shinydashboard::renderInfoBox({
        
        te_infobox_pct<-forecast_TE_tbl_combined |>
          dplyr::filter(year == SimulationYear) |>
          dplyr::filter(scenario == "Simulation") |>
          dplyr::pull(`Tax Expenditures (Pct of GDP)`)
        
        shinydashboard::infoBox(
          #"Tax Expenditures (As a Percentage of GDP)",
          paste("Tax Expenditures (As a Percentage of GDP) in",SimulationYear),
          paste0(te_infobox_pct),
          icon = shiny::icon("chart-line"),
          color = "blue"
        )
      })
      # Generate charts for Tax Expenditures
      VAT_TE_charts <- NULL
      
      tryCatch({
        VAT_TE_charts <- VAT_TE_fun(
          forecast_combined_te_selected,
          forecast_TE_tbl_combined,
          SimulationYear
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
      output$additionalCharts <- shiny::renderUI({
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(6, plotly::plotlyOutput("vat_TE_plt", height = "400px")),
            shiny::column(6, plotly::plotlyOutput("vat_te_tbl_plt", height = "400px"))
          ),
          shiny::fluidRow(
            shiny::column(6, plotly::plotlyOutput("vat_CPA_plt", height = "400px")),
            shiny::column(6, plotly::plotlyOutput("vat_TE_GDP_plt", height = "400px"))
          )
        )
      })
      
      # Render each chart with Plotly
      output$vat_TE_plt <- plotly::renderPlotly({ VAT_TE_charts$vat_TE_plt })
      output$vat_te_tbl_plt <- plotly::renderPlotly({ VAT_TE_charts$vat_te_tbl_plt })
      output$vat_CPA_plt <- plotly::renderPlotly({ VAT_TE_charts$vat_CPA_plt })
      output$vat_TE_GDP_plt <- plotly::renderPlotly({ VAT_TE_charts$vat_TE_GDP_plt })
    }
    
    
  }
  
  # Observer for dashboard selection ----
  shiny::observeEvent(input$chartSelectVAT_Revenues, {
    shiny::req(reactive_simulation_results())
    chart_type <- shiny::isolate(input$chartSelectVAT_Revenues)
    updateCharts(chart_type)
  }, ignoreInit = TRUE)
  
  
}
# # Run the application
shiny::shinyApp(ui = ui, server = mainServer)