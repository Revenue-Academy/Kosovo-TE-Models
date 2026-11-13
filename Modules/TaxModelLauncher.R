# -*- coding: UTF-8 -*-
# ============================================================
# TAX MODELING TOOLKIT - MAIN PORTAL
# Synthetic-data demonstration version
# Each module file is sourced and run in a background R session.
# ============================================================

options(encoding = "UTF-8")
try({
  if (.Platform$OS.type == "windows") {
    Sys.setlocale("LC_CTYPE", "English_United States.utf8")
  } else {
    Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
  }
}, silent = TRUE)

library(shiny)
library(shinydashboard)
library(shinyjs)
library(fontawesome)
library(base64enc)
library(callr)


find_launcher_dir <- function() {
  # source() places the active file in the calling frame as `ofile`.
  source_files <- unlist(lapply(sys.frames(), function(frame) {
    get0("ofile", envir = frame, inherits = FALSE,
         ifnotfound = character(0))
  }), use.names = FALSE)
  source_files <- source_files[nzchar(source_files)]
  script_name <- "TaxModelLauncher_FULL_RDATA.R"

  # Prefer the actual file currently being sourced to an arbitrary getwd().
  if (length(source_files)) {
    matching <- source_files[basename(source_files) == script_name]
    if (length(matching)) {
      return(normalizePath(dirname(tail(matching, 1L)),
                           winslash = "/", mustWork = TRUE))
    }
    return(normalizePath(dirname(tail(source_files, 1L)),
                         winslash = "/", mustWork = TRUE))
  }

  # Support `Rscript /path/to/TaxModelLauncher_FULL_RDATA.R`.
  cli_args <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(cli_args)) {
    cli_file <- sub("^--file=", "", tail(cli_args, 1L))
    if (file.exists(cli_file) && basename(cli_file) == script_name) {
      return(normalizePath(dirname(cli_file), winslash = "/",
                           mustWork = TRUE))
    }
  }

  # RStudio Run App usually sets getwd() to the folder of this script.
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

launcher_dir <- find_launcher_dir()
model_root <- launcher_dir
modules_dir <- launcher_dir

# Ensure relative resource paths on the main portal resolve beside this file.
setwd(launcher_dir)
message("Toolkit launcher folder: ", launcher_dir)

module_config <- list(
  PIT     = list(title = "PIT Module",     script = "PIT-Module.R",     port = 5801),
  CIT     = list(title = "CIT Module",     script = "CIT-Module.R",     port = 3802),
  VAT     = list(title = "VAT Module",     script = "VAT-Module.R",     port = 3803),
  Customs = list(title = "Customs Module", script = "Customs-Module.R", port = 3804)
)

# Only launch scripts placed in the same directory as this launcher.
resolve_module_script <- function(key) {
  target <- file.path(launcher_dir, module_config[[key]]$script)
  if (!file.exists(target)) return(NULL)
  normalizePath(target, winslash = "/", mustWork = TRUE)
}

# Background process handles, one per module.
module_procs <- new.env()

# ============================================================
# Launcher helpers
# ============================================================

# TRUE if a process is already listening on the local port.
port_is_open <- function(port) {
  con <- tryCatch(
    suppressWarnings(
      socketConnection("127.0.0.1", port = port, open = "r",
                       blocking = TRUE, timeout = 1)
    ),
    error = function(e) NULL
  )
  if (is.null(con)) return(FALSE)
  close(con)
  TRUE
}

# Separate output and error logs, to avoid both processes writing to the same file.
log_path <- function(key, stream = "stderr") {
  file.path(tempdir(), paste0("module_", key, "_", stream, ".log"))
}

log_tail <- function(key, n = 18) {
  read_tail <- function(path) {
    if (!file.exists(path)) return(character(0))
    tryCatch(tail(readLines(path, warn = FALSE, encoding = "UTF-8"), n),
             error = function(e) paste("Could not read log:", conditionMessage(e)))
  }
  errors <- read_tail(log_path(key, "stderr"))
  messages <- read_tail(log_path(key, "stdout"))
  paste(c("STDERR:", if (length(errors)) errors else "(empty)",
          "", "STDOUT:", if (length(messages)) messages else "(empty)"),
        collapse = "\n")
}

# Run the Shiny module in a separate R process.
# Important: several PIT scripts deliberately look for variables in .GlobalEnv.
# Using source(..., local = new.env()) can break a module that works normally
# when launched by itself. Source into the background process's globalenv().
start_module_process <- function(key, script_path) {
  cfg <- module_config[[key]]
  module_dir <- launcher_dir  # Exact same folder for all four modules.
  # Clear logs from any previous launch, so only the new error is shown.
  unlink(c(log_path(key, "stdout"), log_path(key, "stderr")))

  # PIT-Module.R itself loads its .Rdata file into the child R process.
  # Do not pass stale objects from RStudio or load PIT data here.
  # Keep the launcher independent of RStudio's Global Environment.
  shared_objects <- list()

  # Optional, only if your independently working PIT module needs a separate
  # preparation file. Example, set in the R console BEFORE starting the portal:
  # options(tax_toolkit.pit_prep_script = "Modules/PIT-data-preparation.R")
  prep_script <- NULL
  if (identical(key, "PIT")) {
    prep_script <- getOption("tax_toolkit.pit_prep_script", NULL)
    if (!is.null(prep_script)) {
      if (!is.character(prep_script) || length(prep_script) != 1L ||
          !nzchar(prep_script)) {
        stop("Option tax_toolkit.pit_prep_script must be a single file path.")
      }
      candidates <- c(prep_script, file.path(model_root, prep_script),
                      file.path(module_dir, prep_script))
      found <- candidates[file.exists(candidates)]
      if (!length(found)) {
        stop("PIT preparation script not found: ", prep_script)
      }
      prep_script <- normalizePath(found[[1L]], winslash = "/", mustWork = TRUE)
    }
  }

  module_procs[[key]] <- callr::r_bg(
    func = function(script_path, module_dir, port, shared_objects, prep_script) {
      options(encoding = "UTF-8")
      setwd(module_dir)
      message("Module working directory: ", getwd())
      message("Module script: ", script_path)

      # Seed the child R process with available PIT data, BEFORE sourcing PIT.
      if (length(shared_objects)) {
        list2env(shared_objects, envir = globalenv())
        message("Imported prepared objects: ",
                paste(names(shared_objects), collapse = ", "))
      }

      # When explicitly configured, run PIT's preparation inside THIS process.
      if (!is.null(prep_script)) {
        message("Preparing PIT data: ", prep_script)
        source(prep_script, local = globalenv(), encoding = "UTF-8")
      }

      message("Loading module: ", script_path)
      # Source directly into the child process Global Environment because
      # PIT checks for prepared data there at simulation time.
      result <- source(script_path, local = globalenv(), encoding = "UTF-8")
      app <- result$value

      # Also support scripts that assign an app object, or define ui/server.
      if (!inherits(app, "shiny.appobj") &&
          exists("app", envir = globalenv(), inherits = FALSE)) {
        app <- get("app", envir = globalenv(), inherits = FALSE)
      }
      if (!inherits(app, "shiny.appobj") &&
          exists("ui", envir = globalenv(), inherits = FALSE) &&
          exists("server", envir = globalenv(), inherits = FALSE)) {
        app <- shiny::shinyApp(
          get("ui", envir = globalenv(), inherits = FALSE),
          get("server", envir = globalenv(), inherits = FALSE)
        )
      }
      if (!inherits(app, "shiny.appobj")) {
        stop("Module must return shinyApp(ui, server), define an app object, ",
             "or define ui and server. See module source file.")
      }
      message("Starting module on http://127.0.0.1:", port)
      shiny::runApp(app, port = port, host = "127.0.0.1", launch.browser = FALSE)
    },
    args   = list(script_path = script_path, module_dir = module_dir,
                  port = cfg$port, shared_objects = shared_objects,
                  prep_script = prep_script),
    stdout = log_path(key, "stdout"),
    stderr = log_path(key, "stderr")
  )
  invisible(TRUE)
}

# ============================================================
# Reusable UI helpers
# ============================================================

module_card <- function(title, icon_name, description, button_id, accent = "#2f6c99") {
  tags$div(
    class = "module-card",
    tags$div(
      class = "module-card-header",
      tags$span(
        class = "module-icon",
        style = paste0("background-color:", accent, ";"),
        icon(icon_name)
      ),
      tags$h3(title)
    ),
    tags$p(description, class = "module-card-text"),
    tags$div(
      class = "module-card-footer",
      actionButton(
        button_id,
        paste("Open", title),
        icon = icon("external-link-alt"),
        class = "btn btn-primary module-open-btn"
      )
    )
  )
}

synthetic_notice <- tags$div(
  class = "synthetic-notice",
  tags$h4(icon("circle-info"), " Kosovo Tax Model"),
  tags$p(
    "The Kosovo Tax Model combines bottom-up and top-down approaches to estimate the ",
    "fiscal effects of alternative policy scenarios and, above all, to cost tax ",
    "expenditures. It supports what-if analysis across the PIT, CIT, VAT and Customs ",
    "modules, allowing baseline and reform outcomes to be compared within one ",
    "framework, with tax expenditures quantified in each tax regime."
  )
)

module_page <- function(tab, title, paragraphs, features, footer, button_id, button_label) {
  tabItem(
    tabName = tab,
    fluidRow(
      column(
        width = 12,
        synthetic_notice,
        tags$div(
          class = "module-detail-box",
          tags$h2(class = "page-title", title),
          lapply(paragraphs, tags$p),
          tags$ul(class = "feature-list", lapply(features, tags$li)),
          tags$p(class = "footer-note", footer),
          tags$div(
            style = "text-align:right;",
            actionButton(button_id, button_label,
                         icon = icon("external-link-alt"), class = "btn-primary")
          )
        )
      )
    )
  )
}

# ============================================================
# UI
# ============================================================

ui <- dashboardPage(
  title = "Tax Modeling Toolkit",
  skin = "blue",
  
  dashboardHeader(
    titleWidth = 280,
    title = tags$div(class = "header-flex", uiOutput("headerImage"))
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "sidebar_tabs",
      selected = "pitModule",
      
      menuItem("About Toolkit", tabName = "aboutModel", icon = icon("circle-info")),
      
      menuItem(
        "Direct Taxes",
        icon = icon("file-invoice-dollar"),
        startExpanded = TRUE,
        menuSubItem("PIT Module", tabName = "pitModule", icon = icon("chart-pie")),
        menuSubItem("CIT Module", tabName = "citModule", icon = icon("building"))
      ),
      
      menuItem(
        "Indirect Taxes",
        icon = icon("receipt"),
        startExpanded = TRUE,
        menuSubItem("VAT Module", tabName = "vatModule", icon = icon("shopping-cart")),
        menuSubItem("Customs Module", tabName = "customsModule", icon = icon("truck"))
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$title("Tax Modeling Toolkit"),
      # Open a separate browser tab while the click is still a direct user
      # gesture. If window.open() happens later (after the R module starts),
      # browser popup blockers commonly prevent it from opening.
      # Keep the portal open and navigate the reserved tab once PIT is ready.
      tags$script(HTML("
        $(function() {
          var moduleWindows = {};
          var buttonKeys = {
            activatePIT: 'PIT', activatePITDetail: 'PIT',
            activateCIT: 'CIT', activateCITDetail: 'CIT',
            activateVAT: 'VAT', activateVATDetail: 'VAT',
            activateCustoms: 'Customs', activateCustomsDetail: 'Customs'
          };

          // The click handler fires synchronously, BEFORE Shiny starts the R
          // callback. A tab opened here is not subject to delayed popup rules.
          $(document).on('click', Object.keys(buttonKeys).map(function(id) {
            return '#' + id;
          }).join(', '), function() {
            var key = buttonKeys[this.id];
            if (!key) return;
            var tab = moduleWindows[key];
            if (tab && !tab.closed) {
              try { tab.focus(); } catch (e) {}
              return;
            }
            tab = window.open('about:blank', '_blank');
            if (tab) {
              moduleWindows[key] = tab;
              try {
                tab.document.title = key + ' Module - Starting';
                tab.document.body.innerHTML =
                  '<div style=\"font:18px Arial,sans-serif;padding:36px;\">' +
                  'Starting ' + key + ' module... Please keep this tab open.' +
                  '</div>';
              } catch (e) {}
            }
          });

          Shiny.addCustomMessageHandler('open-tax-module', function(data) {
            var tab = moduleWindows[data.key];
            if (tab && !tab.closed) {
              try {
                tab.location.replace(data.url);
                tab.focus();
                return;
              } catch (e) {}
            }
            // Fallback if a browser blocked the initial popup. The visible
            // clickable link in the portal notification always remains usable.
            var fallbackTab = window.open(data.url, '_blank');
            if (fallbackTab) moduleWindows[data.key] = fallbackTab;
          });

          Shiny.addCustomMessageHandler('tax-module-failed', function(data) {
            var tab = moduleWindows[data.key];
            if (tab && !tab.closed) {
              try {
                tab.document.body.innerHTML =
                  '<div style=\"font:18px Arial,sans-serif;padding:36px;\">' +
                  'The ' + data.key + ' module could not start.' +
                  '</div>';
                var details = tab.document.createElement('pre');
                details.style.cssText = 'white-space:pre-wrap;font:13px monospace;' +
                                        'margin:14px 36px;max-height:75vh;overflow:auto';
                details.textContent = data.details ||
                  'Check the error notification in the Toolkit portal.';
                tab.document.body.appendChild(details);
              } catch (e) {}
            }
          });
        });
      ")),
      tags$style(HTML("
        body, .content-wrapper, .right-side, .content {
          font-family: 'Segoe UI', Arial, sans-serif;
          background-color: #f4f7fb !important;
        }
        .skin-blue .main-header .logo { background-color: #1f4e79 !important; font-weight: 600; font-size: 18px; }
        .skin-blue .main-header .navbar { background-color: #2f6c99 !important; }
        .skin-blue .main-sidebar { background-color: #1e2d3b !important; }
        .skin-blue .sidebar-menu > li > a { color: #dce6ef !important; }
        .skin-blue .sidebar-menu > li.active > a,
        .skin-blue .sidebar-menu > li:hover > a { border-left-color: #3c8dbc !important; background: #243849 !important; }
        .skin-blue .treeview-menu > li > a { color: #c7d4df !important; }
        .skin-blue .treeview-menu > li.active > a,
        .skin-blue .treeview-menu > li > a:hover { color: #ffffff !important; background: #2f4154 !important; }
        .content { padding: 18px; }
        .header-flex { display: flex; align-items: center; gap: 12px; }
        .btn { border-radius: 6px !important; }
        .btn-primary { background-color: #2f6c99 !important; border-color: #2f6c99 !important; }
        .btn-primary:hover { background-color: #1f4e79 !important; border-color: #1f4e79 !important; }
        .page-title { font-size: 26px; font-weight: 700; color: #1f2d3d; margin-top: 0; margin-bottom: 10px; }
        .lead-text { font-size: 15px; line-height: 1.65; color: #34495e; }
        .synthetic-notice {
          background: #e8f2fb; border-left: 5px solid #2f6c99; border-radius: 10px;
          padding: 16px 18px; margin-bottom: 18px; box-shadow: 0 2px 8px rgba(0,0,0,0.06);
        }
        .synthetic-notice h4 { margin-top: 0; color: #1f4e79; font-weight: 700; }
        .synthetic-notice p { margin-bottom: 0; color: #34495e; line-height: 1.6; }
        .module-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); gap: 18px; margin-top: 12px; }
        .module-card {
          background: #ffffff; border-radius: 12px; padding: 20px; min-height: 250px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.08); border-top: 4px solid #3c8dbc;
          display: flex; flex-direction: column; justify-content: space-between;
        }
        .module-card-header { display: flex; align-items: center; gap: 12px; margin-bottom: 10px; }
        .module-card-header h3 { margin: 0; font-size: 20px; font-weight: 700; color: #1f2d3d; }
        .module-icon {
          width: 42px; height: 42px; border-radius: 10px; display: inline-flex;
          align-items: center; justify-content: center; color: #ffffff; font-size: 18px;
        }
        .module-card-text { color: #435466; line-height: 1.62; font-size: 14px; margin-bottom: 16px; }
        .module-card-footer { text-align: right; }
        .module-open-btn { min-width: 150px; }
        .module-detail-box {
          background: #ffffff; border-radius: 12px; padding: 20px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.08); border-top: 4px solid #2f6c99; margin-bottom: 18px;
        }
        .module-detail-box p { color: #34495e; line-height: 1.65; font-size: 15px; }
        .feature-list { margin-top: 12px; padding-left: 20px; color: #34495e; line-height: 1.7; }
        .footer-note { color: #6c7a89; font-size: 13px; margin-top: 10px; }
      "))
    ),
    
    tabItems(
      
      tabItem(
        tabName = "aboutModel",
        fluidRow(
          column(
            width = 12,
            synthetic_notice,
            # tags$div(
            #   class = "module-detail-box",
            #   tags$h2(class = "page-title", "About the Tax Modeling Toolkit"),
            #   tags$p(
            #     class = "lead-text",
            #     "The Tax Modeling Toolkit is an integrated demonstration platform for tax policy analysis. ",
            #     "It brings together four Shiny modules in one portal: Personal Income Tax, Corporate Income Tax, Value Added Tax, and Customs Duties."
            #   ),
            #   tags$p(
            #     class = "lead-text",
            #     "The toolkit is designed to illustrate how tax models can be organized, parameterized, and presented to policy teams. ",
            #     "Each module allows users to review policy parameters, define reform scenarios, run simulations, and examine fiscal outputs through tables and dashboards."
            #   ),
            #   tags$p(
            #     class = "lead-text",
            #     "All datasets used in this toolkit are synthetic. They do not represent real taxpayers, firms, households, importers, customs declarations, tax returns, survey records, or official country statistics."
            #   )
            # ),
            tags$div(
              class = "module-detail-box",
              tags$h2(class = "page-title", "About the Kosovo Tax Model"),
              tags$p(
                class = "lead-text",
                "The Kosovo Tax Model combines bottom-up and top-down approaches in one framework. ",
                "It brings together four modules: Personal Income Tax, Corporate Income Tax, ",
                "Value Added Tax, and Customs Duties."
              ),
              tags$p(
                class = "lead-text",
                "Each module lets users review policy parameters, define reform scenarios, run ",
                "simulations, and examine the results through tables and dashboards."
              ),
              tags$p(
                class = "lead-text",
                "Costing tax expenditures is a central purpose of the model. Revenue forgone is ",
                "quantified within each tax regime and shown alongside the baseline and reform results."
              )
            ),
            
      #       tags$div(
      #         class = "module-grid",
      #         module_card("PIT Module", "chart-pie",
      #                     "Simulates personal income tax reforms using synthetic microdata. It applies PIT rates, brackets, deductions, and exemptions to estimate revenue and distributional effects.",
      #                     "activatePIT", "#2f6c99"),
      #         module_card("CIT Module", "building",
      #                     "Simulates corporate income tax reforms using synthetic firm-level data. It compares current-law and reform scenarios for standard CIT, turnover regimes, and selected tax expenditures.",
      #                     "activateCIT", "#2a9d8f"),
      #         module_card("VAT Module", "shopping-cart",
      #                     "Simulates VAT policy changes using synthetic supply-use, household, and policy inputs. It estimates revenue impact, VAT gap metrics, tax expenditures, and distributional indicators.",
      #                     "activateVAT", "#e9a03b"),
      #         module_card("Customs Module", "ship",
      #                     "Simulates customs duty reforms using synthetic tariff-line import data. It covers MFN and preferential treatments, ad valorem and specific duties, import-demand responses, and customs tax expenditures.",
      #                     "activateCustoms", "#8e5ea2")
      #       )
      #     )
      #   )
      # ),
      tags$div(
        class = "module-grid",
        module_card("PIT Module", "chart-pie",
                    "Simulates personal income tax reforms using microdata. It applies PIT rates, brackets, deductions, and exemptions to estimate revenue and distributional effects, and costs the tax expenditures within the PIT regime.",
                    "activatePIT", "#2f6c99"),
        module_card("CIT Module", "building",
                    "Simulates corporate income tax reforms using firm-level data. It compares current-law and reform scenarios for standard CIT regime, and costs the tax expenditures within the CIT regime.",
                    "activateCIT", "#2a9d8f"),
        module_card("VAT Module", "shopping-cart",
                    "Simulates VAT policy changes using supply-use tables, household, and policy inputs. It estimates revenue impact, VAT gap metrics and costs the tax expenditures within the VAT regime.",
                    "activateVAT", "#e9a03b"),
        module_card("Customs Module", "truck",
                    "Simulates customs duty reforms using tariff-line import data. It covers ad valorem and specific duties simulations and costs the customs tax expenditures within the customs regime.",
                    "activateCustoms", "#8e5ea2")
      )
          )
        )
      ),
      
      module_page(
        "pitModule", "PIT Module",
        paragraphs = list(
          "The PIT Module provides simulation and analysis for personal income tax policy. It uses individual microdata to apply PIT rates, brackets, deductions, and exemptions.",
          "The module is designed to compare baseline and reform scenarios and to estimate the fiscal and distributional effects of policy changes."
        ),
        features = list(
          "Policy simulations for PIT rates, brackets, deductions, and exemptions.",
          "Revenue impact tables and dashboard visualizations.",
          "Distributional analysis."
        ),
        footer = " ",
        button_id = "activatePITDetail",
        button_label = "Open PIT Module"
      ),
      
      module_page(
        "citModule", "CIT Module",
        paragraphs = list(
          "The CIT Module provides simulation and analysis for corporate income tax policy. It uses   firm-level data to estimate current-law corporate income tax liabilities and compare them with simulated policy reforms.",
          "It is designed to support comparison between baseline and reform scenarios and to show the impact of changes across sectors, firm groups, and fiscal aggregates."
        ),
        features = list(
          " ",
          "Baseline and simulation comparisons for CIT policy parameters.",
          "Revenue impact and tax expenditure outputs."
        ),
        footer = " ",
        button_id = "activateCITDetail",
        button_label = "Open CIT Module"
      ),
      
      module_page(
        "vatModule", "VAT Module",
        paragraphs = list(
          "The VAT Module provides simulation and analysis for value added tax policy. It uses SUTs to demonstrate how VAT rates, exemptions, preferential treatment, and household-consumption information can be combined in a VAT simulation model.",
          "It compares baseline VAT outcomes with the results of simulated policy changes."
        ),
        features = list(
          "VAT policy parameters for products and sectors.",
          "Revenue impact and tax expenditure tables.",
          "Dashboards for fiscal impact, sectoral results, VAT gap metrics, and household distributional effects."
        ),
        footer = " ",
        button_id = "activateVATDetail",
        button_label = "Open VAT Module"
      ),
      
      module_page(
        "customsModule", "Customs Module",
        paragraphs = list(
          "The Customs Module provides simulation and analysis for customs duty policy. It uses synthetic tariff-line import data to demonstrate reforms to MFN rates, preferential rates, specific duties, and import-demand assumptions.",
          "Preferential rates can be recalculated automatically as a percentage of MFN rates or entered manually by the user."
        ),
        features = list(
          "Tariff-line simulation by HS code, origin country, region, and treatment.",
          "MFN and preferential ad valorem and specific customs duties.",
          "Import-demand elasticity and customs tax expenditure outputs."
        ),
        footer = " ",
        button_id = "activateCustomsDetail",
        button_label = "Open Customs Module"
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  output$headerImage <- renderUI({
    img_candidates <- c(file.path(launcher_dir, "img", "WB_pic.png"),
                        file.path(dirname(launcher_dir), "img", "WB_pic.png"))
    img_found <- img_candidates[file.exists(img_candidates)]
    if (length(img_found)) {
      img_path <- img_found[[1L]]
      img_data <- base64enc::dataURI(file = img_path, mime = "image/png")
      tags$img(src = img_data, height = "34px", style = "margin-left: 8px;")
    } else {
      tags$span("")
    }
  })
  
  pending        <- reactiveVal(NULL)
  launch_started <- reactiveVal(NULL)

  open_in_client_browser <- function(key) {
    url <- sprintf("http://127.0.0.1:%d", module_config[[key]]$port)
    # The link is also displayed as a fallback for blocked client navigation.
    showNotification(
      tags$span("Module ready: ", tags$a(href = url, target = "_blank",
                                              "open in a new tab")),
      type = "message", duration = 20
    )
    session$sendCustomMessage("open-tax-module", list(key = key, url = url))
  }

  request_module <- function(key) {
    cfg <- module_config[[key]]
    script_path <- resolve_module_script(key)

    if (is.null(script_path)) {
      showNotification(
        paste0(cfg$title, " cannot start: ", cfg$script,
               " was not found beside the launcher. Put both files in: ",
               launcher_dir),
        type = "error", duration = NULL
      )
      return(invisible(FALSE))
    }

    # A running module can be re-opened without creating a duplicate process.
    if (port_is_open(cfg$port)) {
      open_in_client_browser(key)
      return(invisible(TRUE))
    }

    existing <- module_procs[[key]]
    if (is.null(existing) || !isTRUE(tryCatch(existing$is_alive(),
                                             error = function(e) FALSE))) {
      launched <- tryCatch(start_module_process(key, script_path),
                           error = function(e) {
                             showNotification(
                               paste(cfg$title, "launch failed:", conditionMessage(e)),
                               type = "error", duration = NULL
                             )
                             FALSE
                           })
      if (!isTRUE(launched)) return(invisible(FALSE))
    }

    launch_started(Sys.time())
    pending(key)
    showNotification(
      paste(cfg$title, "is starting; the page will open when it is ready."),
      type = "message", duration = 6
    )
    invisible(TRUE)
  }

  # Check every half second without blocking the Shiny portal.
  observe({
    key <- pending()
    req(key)
    invalidateLater(500, session)

    cfg <- module_config[[key]]
    proc <- module_procs[[key]]
    elapsed <- as.numeric(difftime(Sys.time(), launch_started(), units = "secs"))

    if (port_is_open(cfg$port)) {
      pending(NULL)
      open_in_client_browser(key)
      return()
    }

    is_running <- !is.null(proc) &&
      isTRUE(tryCatch(proc$is_alive(), error = function(e) FALSE))
    if (!is_running || elapsed > 180) {
      showNotification(
        tags$div(
          tags$b(paste(cfg$title, "could not start.")),
          tags$p("The module was launched from: ", launcher_dir),
          tags$p("Check the filename/path in the error log. Relative data paths are resolved from this folder."),
          tags$pre(log_tail(key), style = "white-space: pre-wrap; font-size: 11px;"),
          tags$small(paste("Error log:", log_path(key, "stderr"))),
          tags$div(tags$small(paste("Output log:", log_path(key, "stdout"))))
        ),
        type = "error", duration = NULL
      )
      pending(NULL)
      session$sendCustomMessage("tax-module-failed",
                                list(key = key, details = log_tail(key)))
    }
  })

  observeEvent(input$activatePIT,           request_module("PIT"))
  observeEvent(input$activatePITDetail,     request_module("PIT"))
  observeEvent(input$activateCIT,           request_module("CIT"))
  observeEvent(input$activateCITDetail,     request_module("CIT"))
  observeEvent(input$activateVAT,           request_module("VAT"))
  observeEvent(input$activateVATDetail,     request_module("VAT"))
  observeEvent(input$activateCustoms,       request_module("Customs"))
  observeEvent(input$activateCustomsDetail, request_module("Customs"))
}

shinyApp(ui = ui, server = server)
