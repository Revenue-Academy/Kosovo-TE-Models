library(readxl)
library(tidyverse)

# I.Import data -------------------------------------------------------------
      # 1.Growth factor ---------------------------------------------------------

#Data_raw/SUTs
      
          #tmp_vat <- paste0(path1, "/Scripts/VAT/")
          tmp_vat <- paste0(path1)
          setwd(tmp_vat)
          getwd()



              growth_factor_sut <- read_excel("Data/VAT/Data_raw/SUTs/ExtrapoliationVAT.xlsx", 
                                                 sheet = "GrowthFactors", range = "M3:U20")%>%
                                        dplyr::select(
                  Description,
                  dplyr::starts_with("cumulative_")
                )
                
              
              library(readxl)
              ExtrapoliationVAT <- read_excel("~/Models/Kosovo-TE-Models/Data/VAT/Data_raw/SUTs/ExtrapoliationVAT.xlsx")
              
              
                 
              #View(growth_factor_sut)
      
      
            growth_factor_sectors <- read_excel("Data/VAT/Data_raw/SUTs/ExtrapoliationVAT.xlsx", 
                                            #sheet = "GrowthFactors", range = "M23:U47")%>%
                                            sheet = "GrowthFactors", range = "M24:U47")%>%
              dplyr::select(
                Description,
                dplyr::starts_with("cumulative_")
              )
              
              #View(growth_factor_sectors)
              
      
      # 2. SUTs----------------------------------------------------------------------
        # 2.1 Supply --------------------------------------------------------------
              sut_supply <- read_excel("Data/VAT/Data_raw/SUTs/ExtrapoliationVAT.xlsx", 
                                              sheet = "Supply", range = "A5:AC27")
              
              
              #View(sut_supply)
      
      
        # 2.2 Use -----------------------------------------------------------------
              sut_use <- read_excel("Data/VAT/Data_raw/SUTs/ExtrapoliationVAT.xlsx", 
                                       sheet = "Use", range = "A5:AG27")
              
              
              #View(sut_use)
              
              
              
        # 2.3 VAT --------------------------------------------------------------------
      
              
              ETR <- read_excel("Data/VAT/Data_raw/SUTs/ExtrapoliationVAT.xlsx", 
                                              sheet = "VAT", range = "C8:D29")%>%
                dplyr::filter()
            
              
              
        # 2.4 VAT target ----------------------------------------------------------
              
              
              vat_targets <- read_excel("Data/VAT/Data_raw/SUTs/ExtrapoliationVAT.xlsx", 
                                              sheet = "GrowthFactors", range = "A3:H21")%>%
                                  filter(Description=="VAT")%>%
                                  select( Description,`2024`,`2025`)
              
       
              
              
# II.Extrapolation of SUTs ---------------------------------------------------------------------
      
     
        # 1. Settings ----
        
        years <- c(2024L, 2025L)
        
        product_codes <- LETTERS[1:21]
        
        allowed_negative_margin_codes <- c("G", "H")
        
        tol <- 1e-6
        ras_tol <- 1e-10
        ras_max_iter <- 10000L
        
        
        # 2. Required objects ----
        
        required_objects <- c(
          "sut_supply",
          "sut_use",
          "growth_factor_sectors",
          "growth_factor_sut"
        )
        
        missing_objects <- required_objects[
          !vapply(
            required_objects,
            exists,
            logical(1),
            envir = .GlobalEnv
          )
        ]
        
        if (length(missing_objects) > 0) {
          stop(
            paste0(
              "Missing required object(s): ",
              paste(missing_objects, collapse = ", ")
            )
          )
        }
        
        
        # 3. Helpers ----
        
        norm_text <- function(x) {
          
          stringr::str_to_lower(
            stringr::str_squish(
              as.character(x)
            )
          )
        }
        
        
        clean_code <- function(x) {
          
          x <- stringr::str_to_upper(
            stringr::str_trim(
              as.character(x)
            )
          )
          
          x[x == ""] <- NA_character_
          
          x
        }
        
        
        clean_sut <- function(df) {
          
          df |>
            dplyr::mutate(
              Code = clean_code(Code),
              Description = stringr::str_squish(
                as.character(Description)
              )
            ) |>
            dplyr::filter(
              Code %in% product_codes
            )
        }
        
        
        near_target <- function(actual, target, tolerance = tol) {
          
          abs(actual - target) <=
            tolerance *
            pmax(
              1,
              abs(target)
            )
        }
        
        
        find_sector_col <- function(df, code) {
          
          hits <- names(df)[
            stringr::str_detect(
              names(df),
              paste0("^", code, "-")
            )
          ]
          
          if (length(hits) != 1) {
            stop(
              paste0(
                "Sector column not uniquely found: ",
                code
              )
            )
          }
          
          hits
        }
        
        
        get_macro_factor <- function(description, year) {
          
          idx <- which(
            norm_text(growth_factor_sut$Description) ==
              norm_text(description)
          )
          
          if (length(idx) != 1) {
            stop(
              paste0(
                "Growth factor not uniquely found: ",
                description
              )
            )
          }
          
          year_col <- paste0("cumulative_", year)
          
          value <- as.numeric(growth_factor_sut[[year_col]][idx])
          
          if (!is.finite(value)) {
            stop(
              paste0(
                "Invalid growth factor for ",
                description,
                " in ",
                year
              )
            )
          }
          
          value
        }
        
        
        get_sector_factor <- function(description, year) {
          
          idx <- which(
            norm_text(growth_factor_sectors$Description) ==
              norm_text(description)
          )
          
          if (length(idx) != 1) {
            stop(
              paste0(
                "Sector factor not uniquely found: ",
                description
              )
            )
          }
          
          year_col <- paste0("cumulative_", year)
          
          value <- as.numeric(growth_factor_sectors[[year_col]][idx])
          
          if (!is.finite(value)) {
            stop(
              paste0(
                "Invalid sector factor for ",
                description,
                " in ",
                year
              )
            )
          }
          
          value
        }
        
        
        # 4. RAS ----
        
        ras_balance <- function(seed, row_targets, col_targets) {
          
          m <- as.matrix(seed)
          
          storage.mode(m) <- "double"
          
          m[!is.finite(m)] <- 0
          m[m < 0] <- 0
          
          row_targets <- as.numeric(row_targets)
          col_targets <- as.numeric(col_targets)
          
          if (any(row_targets < -tol)) {
            stop("RAS contains a negative row target.")
          }
          
          if (any(col_targets < -tol)) {
            stop("RAS contains a negative column target.")
          }
          
          if (!near_target(sum(row_targets), sum(col_targets))) {
            stop("RAS row and column target totals are different.")
          }
          
          if (sum(row_targets) <= tol) {
            return(m * 0)
          }
          
          support <- outer(
            row_targets > tol,
            col_targets > tol,
            "&"
          )
          
          epsilon <- max(
            sum(m),
            1
          ) * 1e-12
          
          m[m <= 0 & support] <- epsilon
          m[!support] <- 0
          
          converged <- FALSE
          
          for (iter in seq_len(ras_max_iter)) {
            
            current_rows <- rowSums(m)
            
            if (any(row_targets > tol & current_rows <= 0)) {
              stop(
                "RAS positive row target has no support."
              )
            }
            
            row_factor <- rep(
              0,
              length(row_targets)
            )
            
            positive_rows <- row_targets > tol
            
            row_factor[positive_rows] <-
              row_targets[positive_rows] /
              current_rows[positive_rows]
            
            m <- sweep(
              m,
              1,
              row_factor,
              "*"
            )
            
            current_cols <- colSums(m)
            
            if (any(col_targets > tol & current_cols <= 0)) {
              stop(
                "RAS positive column target has no support."
              )
            }
            
            col_factor <- rep(
              0,
              length(col_targets)
            )
            
            positive_cols <- col_targets > tol
            
            col_factor[positive_cols] <-
              col_targets[positive_cols] /
              current_cols[positive_cols]
            
            m <- sweep(
              m,
              2,
              col_factor,
              "*"
            )
            
            row_error <- max(
              abs(
                rowSums(m) -
                  row_targets
              ) /
                pmax(
                  1,
                  abs(row_targets)
                )
            )
            
            col_error <- max(
              abs(
                colSums(m) -
                  col_targets
              ) /
                pmax(
                  1,
                  abs(col_targets)
                )
            )
            
            if (max(row_error, col_error) < ras_tol) {
              converged <- TRUE
              break
            }
          }
          
          if (!converged) {
            stop(
              "RAS did not converge."
            )
          }
          
          m
        }
        
        
        # 5. Benchmark SUT ----
        
        supply_base <- clean_sut(
          sut_supply
        )
        
        use_base <- clean_sut(
          sut_use
        )
        
        
        if (anyDuplicated(supply_base$Code)) {
          stop(
            "Duplicate product codes in Supply."
          )
        }
        
        
        if (anyDuplicated(use_base$Code)) {
          stop(
            "Duplicate product codes in Use."
          )
        }
        
        
        if (!setequal(supply_base$Code, product_codes)) {
          stop(
            "Supply must contain exactly product codes A-U."
          )
        }
        
        
        if (!setequal(use_base$Code, product_codes)) {
          stop(
            "Use must contain exactly product codes A-U."
          )
        }
        
        
        supply_base <- supply_base[
          match(
            product_codes,
            supply_base$Code
          ),
          ,
          drop = FALSE
        ]
        
        
        use_base <- use_base[
          match(
            product_codes,
            use_base$Code
          ),
          ,
          drop = FALSE
        ]
        
        
        # 6. Sector mapping ----
        
        sector_mapping <- tibble::tibble(
          Code = product_codes,
          
          supply_col = vapply(
            product_codes,
            function(code) {
              find_sector_col(
                supply_base,
                code
              )
            },
            character(1)
          ),
          
          use_col = vapply(
            product_codes,
            function(code) {
              find_sector_col(
                use_base,
                code
              )
            },
            character(1)
          )
        )
        
        
        sector_factor_codes <- stringr::str_extract(
          as.character(
            growth_factor_sectors$Description
          ),
          "^[A-U](?=-)"
        )
        
        
        for (year in years) {
          
          year_col <- paste0("cumulative_", year)
          
          factors <- as.numeric(
            growth_factor_sectors[[year_col]][
              match(
                sector_mapping$Code,
                sector_factor_codes
              )
            ]
          )
          
          core <- sector_mapping$Code %in%
            LETTERS[1:19]
          
          if (any(!is.finite(factors[core]) | factors[core] <= 0)) {
            stop(
              paste0(
                "Invalid A-S sector factor in ",
                year
              )
            )
          }
          
          fallback <-
            sector_mapping$Code %in%
            c("T", "U") &
            (
              !is.finite(factors) |
                factors <= 0
            )
          
          factors[fallback] <- 1
          
          sector_mapping[[year_col]] <- factors
        }
        
        
        supply_sector_cols <- sector_mapping$supply_col
        use_sector_cols <- sector_mapping$use_col
        
        
        # 7. Benchmark vectors ----
        
        base_household <- as.numeric(
          use_base[["Final consumption expenditure by households"]]
        )
        
        base_npish <- as.numeric(
          use_base[["Final consumption expenditure by NPISH"]]
        )
        
        base_government <- as.numeric(
          use_base[["Final consumption expenditure by government"]]
        )
        
        base_gfcf <- as.numeric(
          use_base[["Gross fixed capital formation"]]
        )
        
        base_inventories <- as.numeric(
          use_base[["Changes in inventories"]]
        )
        
        base_exports <- as.numeric(
          use_base[["Exports"]]
        )
        
        base_imports <- as.numeric(
          supply_base[["Total Imports"]]
        )
        
        base_taxes <- as.numeric(
          supply_base[["Taxes on product"]]
        )
        
        
        base_household[!is.finite(base_household)] <- 0
        base_npish[!is.finite(base_npish)] <- 0
        base_government[!is.finite(base_government)] <- 0
        base_gfcf[!is.finite(base_gfcf)] <- 0
        base_inventories[!is.finite(base_inventories)] <- 0
        base_exports[!is.finite(base_exports)] <- 0
        base_imports[!is.finite(base_imports)] <- 0
        base_taxes[!is.finite(base_taxes)] <- 0
        
        
        # 8. Tax/subsidy structure ----
        
        tax_sign_check <- tibble::tibble(
          Code = supply_base$Code,
          Description = supply_base$Description,
          benchmark_tax = base_taxes,
          type = dplyr::case_when(
            base_taxes > 0 ~ "Tax",
            base_taxes < 0 ~ "Subsidy",
            TRUE ~ "Zero"
          )
        )
        
        
        cat(
          "\n===== BENCHMARK TAX / SUBSIDY STRUCTURE =====\n"
        )
        
        
        print(
          tax_sign_check,
          n = Inf,
          width = Inf
        )
        
        
        # 9. Factors used ----
        
        factors_used <- dplyr::bind_rows(
          
          lapply(
            years,
            function(year) {
              
              tibble::tibble(
                year = year,
                
                household = get_macro_factor(
                  "Final consumption of households",
                  year
                ),
                
                npish = get_macro_factor(
                  "Final Consumption expenditure of NPISH",
                  year
                ),
                
                government = get_macro_factor(
                  "Final consumption of Government",
                  year
                ),
                
                gfcf = get_macro_factor(
                  "Gross fixed capital formation",
                  year
                ),
                
                inventories = get_macro_factor(
                  "Changes on inventories",
                  year
                ),
                
                exports = get_macro_factor(
                  "Exports of goods and services",
                  year
                ),
                
                imports = get_macro_factor(
                  "Imports of goods and services",
                  year
                ),
                
                taxes = get_sector_factor(
                  "Taxes on product",
                  year
                ),
                
                subsidies = get_sector_factor(
                  "Subsidies",
                  year
                )
              )
            }
          )
        )
        
        
        # 10. Containers ----
        
        sut_supply_by_year <- list()
        sut_use_by_year <- list()
        
        numeric_check_by_year <- list()
        validation_by_year <- list()
        
        tax_detail_by_year <- list()
        
        
        # 11. Extrapolation ----
        
        for (year in years) {
          
          year_col <- paste0("cumulative_", year)
          
          factor_row <- factors_used[
            factors_used$year == year,
            ,
            drop = FALSE
          ]
          
          
          household_factor <- factor_row$household[[1]]
          npish_factor <- factor_row$npish[[1]]
          government_factor <- factor_row$government[[1]]
          gfcf_factor <- factor_row$gfcf[[1]]
          inventories_factor <- factor_row$inventories[[1]]
          export_factor <- factor_row$exports[[1]]
          import_factor <- factor_row$imports[[1]]
          tax_factor <- factor_row$taxes[[1]]
          subsidy_factor <- factor_row$subsidies[[1]]
          
          
          supply_y <- supply_base
          use_y <- use_base
          
          
          # 11.1 Domestic output ----
          
          for (j in seq_len(nrow(sector_mapping))) {
            
            supply_col <- sector_mapping$supply_col[j]
            
            sector_factor <- sector_mapping[[year_col]][j]
            
            supply_y[[supply_col]] <-
              supply_base[[supply_col]] *
              sector_factor
          }
          
          
          supply_y[["Total supply at purchasers' prices domestic"]] <-
            rowSums(
              supply_y[
                ,
                supply_sector_cols,
                drop = FALSE
              ],
              na.rm = TRUE
            )
          
          
          # 11.2 Imports ----
          # Growth factor only
          
          supply_y[["Total Imports"]] <-
            base_imports *
            import_factor
          
          
          # 11.3 Basic supply ----
          
          supply_y[["Total supply at basic prices"]] <-
            supply_y[["Total supply at purchasers' prices domestic"]] +
            supply_y[["Total Imports"]]
          
          
          # 11.4 Taxes and subsidies ----
          #
          # Positive benchmark value:
          #   benchmark * Taxes on product factor
          #
          # Negative benchmark value:
          #   benchmark * Subsidies factor
          #
          # No target
          # No redistribution
          # No rescaling
          
          tax_expected_vector <- ifelse(
            base_taxes > 0,
            base_taxes * tax_factor,
            ifelse(
              base_taxes < 0,
              base_taxes * subsidy_factor,
              0
            )
          )
          
          
          supply_y[["Taxes on product"]] <-
            tax_expected_vector
          
          
          # 11.5 Households ----
          # Growth factor only
          
          household_expected_vector <-
            base_household *
            household_factor
          
          
          use_y[["Final consumption expenditure by households"]] <-
            household_expected_vector
          
          
          # 11.6 NPISH ----
          
          npish_expected_vector <-
            base_npish *
            npish_factor
          
          
          use_y[["Final consumption expenditure by NPISH"]] <-
            npish_expected_vector
          
          
          # 11.7 Government ----
          
          government_expected_vector <-
            base_government *
            government_factor
          
          
          use_y[["Final consumption expenditure by government"]] <-
            government_expected_vector
          
          
          # 11.8 GFCF ----
          
          gfcf_expected_vector <-
            base_gfcf *
            gfcf_factor
          
          
          use_y[["Gross fixed capital formation"]] <-
            gfcf_expected_vector
          
          
          # 11.9 Inventories ----
          
          inventories_expected_vector <-
            base_inventories *
            inventories_factor
          
          
          use_y[["Changes in inventories"]] <-
            inventories_expected_vector
          
          
          # 11.10 Exports ----
          # Growth factor only
          
          exports_expected_vector <-
            base_exports *
            export_factor
          
          
          use_y[["Exports"]] <-
            exports_expected_vector
          
          
          # 11.11 Final consumption ----
          
          use_y[["Final consumptin expenditure"]] <-
            use_y[["Final consumption expenditure by households"]] +
            use_y[["Final consumption expenditure by NPISH"]] +
            use_y[["Final consumption expenditure by government"]]
          
          
          # 11.12 Domestic final use ----
          
          use_y[["Total domestic final use"]] <-
            use_y[["Final consumptin expenditure"]] +
            use_y[["Gross fixed capital formation"]] +
            use_y[["Changes in inventories"]]
          
          
          # 11.13 Total final uses ----
          
          use_y[["Total final uses"]] <-
            use_y[["Total domestic final use"]] +
            use_y[["Exports"]]
          
          
          # 11.14 IC balancing total ----
          
          basic_supply_total <- sum(
            supply_y[["Total supply at basic prices"]],
            na.rm = TRUE
          )
          
          
          tax_subsidy_total <- sum(
            supply_y[["Taxes on product"]],
            na.rm = TRUE
          )
          
          
          final_use_total <- sum(
            use_y[["Total final uses"]],
            na.rm = TRUE
          )
          
          
          ic_total_target <-
            basic_supply_total +
            tax_subsidy_total -
            final_use_total
          
          
          if (
            !is.finite(ic_total_target) ||
            ic_total_target < 0
          ) {
            
            stop(
              paste0(
                "Invalid intermediate-consumption balancing total in ",
                year,
                ": ",
                ic_total_target
              )
            )
          }
          
          
          # 11.15 IC seed ----
          
          ic_seed <- matrix(
            0,
            nrow = nrow(use_base),
            ncol = nrow(sector_mapping)
          )
          
          
          rownames(ic_seed) <- use_base$Code
          colnames(ic_seed) <- sector_mapping$Code
          
          
          for (j in seq_len(nrow(sector_mapping))) {
            
            use_col <- sector_mapping$use_col[j]
            
            sector_factor <- sector_mapping[[year_col]][j]
            
            x <- as.numeric(
              use_base[[use_col]]
            )
            
            x[!is.finite(x)] <- 0
            x[x < 0] <- 0
            
            
            ic_seed[, j] <-
              x *
              sector_factor
          }
          
          
          # 11.16 IC column targets ----
          
          ic_col_raw <- colSums(
            ic_seed
          )
          
          
          if (sum(ic_col_raw) <= 0) {
            stop(
              paste0(
                "No positive IC structure in ",
                year
              )
            )
          }
          
          
          ic_col_targets <-
            ic_col_raw /
            sum(ic_col_raw) *
            ic_total_target
          
          
          # 11.17 Minimum IC ----
          #
          # Margin =
          # IC + final uses - basic supply - tax/subsidy
          #
          # Only G and H may be negative.
          
          basic_plus_tax <-
            supply_y[["Total supply at basic prices"]] +
            supply_y[["Taxes on product"]]
          
          
          final_use_product <-
            use_y[["Total final uses"]]
          
          
          minimum_ic <- ifelse(
            supply_y$Code %in%
              allowed_negative_margin_codes,
            0,
            pmax(
              basic_plus_tax -
                final_use_product,
              0
            )
          )
          
          
          minimum_ic_total <- sum(
            minimum_ic
          )
          
          
          if (
            minimum_ic_total >
            ic_total_target +
            tol *
            pmax(
              1,
              abs(ic_total_target)
            )
          ) {
            
            problem_rows <- tibble::tibble(
              Code = supply_y$Code,
              Description = supply_y$Description,
              basic_plus_tax = basic_plus_tax,
              final_use = final_use_product,
              minimum_ic = minimum_ic
            ) |>
              dplyr::filter(
                minimum_ic > 0
              ) |>
              dplyr::arrange(
                dplyr::desc(
                  minimum_ic
                )
              )
            
            
            cat(
              "\n===== MARGIN FEASIBILITY PROBLEM =====\n"
            )
            
            
            print(
              problem_rows,
              n = Inf,
              width = Inf
            )
            
            
            stop(
              paste0(
                "Margin sign restrictions are infeasible in ",
                year
              )
            )
          }
          
          
          # 11.18 IC row targets ----
          
          row_weights <- rowSums(
            ic_seed
          )
          
          
          row_weights[
            !is.finite(row_weights) |
              row_weights < 0
          ] <- 0
          
          
          if (sum(row_weights) <= 0) {
            row_weights <- rep(
              1,
              length(row_weights)
            )
          }
          
          
          remaining_ic <-
            ic_total_target -
            minimum_ic_total
          
          
          ic_row_targets <-
            minimum_ic +
            remaining_ic *
            row_weights /
            sum(row_weights)
          
          
          # 11.19 RAS ----
          
          ic_balanced <- ras_balance(
            seed = ic_seed,
            row_targets = ic_row_targets,
            col_targets = ic_col_targets
          )
          
          
          for (j in seq_len(nrow(sector_mapping))) {
            use_y[[sector_mapping$use_col[j]]] <- ic_balanced[, j]
          }
          
          
          # 11.20 Total intermediate consumption ----
          
          use_y[["Total supply at purchasers' prices"]] <-
            rowSums(
              use_y[
                ,
                use_sector_cols,
                drop = FALSE
              ],
              na.rm = TRUE
            )
          
          
          # 11.21 Total use ----
          
          use_y[["TOTAL_USE_PURCHASERS"]] <-
            use_y[["Total supply at purchasers' prices"]] +
            use_y[["Total final uses"]]
          
          
          # 11.22 Trade and transport margin ----
          
          supply_y[["Trade and transport margin"]] <-
            use_y[["TOTAL_USE_PURCHASERS"]] -
            supply_y[["Total supply at basic prices"]] -
            supply_y[["Taxes on product"]]
          
          
          # 11.23 Total supply ----
          
          supply_y[["Total supply at purchasers' prices"]] <-
            supply_y[["Total supply at basic prices"]] +
            supply_y[["Trade and transport margin"]] +
            supply_y[["Taxes on product"]]
          
          
          # 11.24 Expected imports ----
          
          imports_expected_vector <-
            base_imports *
            import_factor
          
          
          # 11.25 Tax detail ----
          
          tax_detail_by_year[[as.character(year)]] <-
            tibble::tibble(
              year = year,
              Code = supply_y$Code,
              Description = supply_y$Description,
              benchmark = base_taxes,
              type = dplyr::case_when(
                base_taxes > 0 ~ "Tax",
                base_taxes < 0 ~ "Subsidy",
                TRUE ~ "Zero"
              ),
              factor_used = dplyr::case_when(
                base_taxes > 0 ~ tax_factor,
                base_taxes < 0 ~ subsidy_factor,
                TRUE ~ 1
              ),
              expected = tax_expected_vector,
              actual = supply_y[["Taxes on product"]]
            )
          
          
          # 11.26 Numeric totals ----
          
          household_expected_total <- sum(
            household_expected_vector,
            na.rm = TRUE
          )
          
          
          household_actual_total <- sum(
            use_y[["Final consumption expenditure by households"]],
            na.rm = TRUE
          )
          
          
          imports_expected_total <- sum(
            imports_expected_vector,
            na.rm = TRUE
          )
          
          
          imports_actual_total <- sum(
            supply_y[["Total Imports"]],
            na.rm = TRUE
          )
          
          
          exports_expected_total <- sum(
            exports_expected_vector,
            na.rm = TRUE
          )
          
          
          exports_actual_total <- sum(
            use_y[["Exports"]],
            na.rm = TRUE
          )
          
          
          inventories_expected_total <- sum(
            inventories_expected_vector,
            na.rm = TRUE
          )
          
          
          inventories_actual_total <- sum(
            use_y[["Changes in inventories"]],
            na.rm = TRUE
          )
          
          
          taxes_expected_total <- sum(
            tax_expected_vector[
              base_taxes > 0
            ],
            na.rm = TRUE
          )
          
          
          taxes_actual_total <- sum(
            supply_y[["Taxes on product"]][
              base_taxes > 0
            ],
            na.rm = TRUE
          )
          
          
          subsidies_expected_total <- sum(
            tax_expected_vector[
              base_taxes < 0
            ],
            na.rm = TRUE
          )
          
          
          subsidies_actual_total <- sum(
            supply_y[["Taxes on product"]][
              base_taxes < 0
            ],
            na.rm = TRUE
          )
          
          
          net_tax_expected_total <- sum(
            tax_expected_vector,
            na.rm = TRUE
          )
          
          
          net_tax_actual_total <- sum(
            supply_y[["Taxes on product"]],
            na.rm = TRUE
          )
          
          
          margin_total <- sum(
            supply_y[["Trade and transport margin"]],
            na.rm = TRUE
          )
          
          
          numeric_check_by_year[[as.character(year)]] <-
            tibble::tibble(
              year = year,
              
              household_factor = household_factor,
              household_expected = household_expected_total,
              household_actual = household_actual_total,
              
              imports_factor = import_factor,
              imports_expected = imports_expected_total,
              imports_actual = imports_actual_total,
              
              exports_factor = export_factor,
              exports_expected = exports_expected_total,
              exports_actual = exports_actual_total,
              
              inventories_factor = inventories_factor,
              inventories_expected = inventories_expected_total,
              inventories_actual = inventories_actual_total,
              
              tax_factor = tax_factor,
              taxes_expected = taxes_expected_total,
              taxes_actual = taxes_actual_total,
              
              subsidy_factor = subsidy_factor,
              subsidies_expected = subsidies_expected_total,
              subsidies_actual = subsidies_actual_total,
              
              net_tax_subsidy_expected = net_tax_expected_total,
              net_tax_subsidy_actual = net_tax_actual_total,
              
              margin_total = margin_total
            )
          
          
          # 11.27 Validation ----
          
          tax_rows <- base_taxes > 0
          subsidy_rows <- base_taxes < 0
          zero_tax_rows <- base_taxes == 0
          
          
          tax_growth_exact <- TRUE
          
          if (any(tax_rows)) {
            tax_growth_exact <- all(
              near_target(
                supply_y[["Taxes on product"]][tax_rows],
                base_taxes[tax_rows] *
                  tax_factor
              )
            )
          }
          
          
          subsidy_growth_exact <- TRUE
          
          if (any(subsidy_rows)) {
            subsidy_growth_exact <- all(
              near_target(
                supply_y[["Taxes on product"]][subsidy_rows],
                base_taxes[subsidy_rows] *
                  subsidy_factor
              )
            )
          }
          
          
          zero_tax_exact <- TRUE
          
          if (any(zero_tax_rows)) {
            zero_tax_exact <- all(
              near_target(
                supply_y[["Taxes on product"]][zero_tax_rows],
                0
              )
            )
          }
          
          
          validation_by_year[[as.character(year)]] <-
            tibble::tibble(
              year = year,
              
              household_product_growth = all(
                near_target(
                  use_y[["Final consumption expenditure by households"]],
                  household_expected_vector
                )
              ),
              
              npish_product_growth = all(
                near_target(
                  use_y[["Final consumption expenditure by NPISH"]],
                  npish_expected_vector
                )
              ),
              
              government_product_growth = all(
                near_target(
                  use_y[["Final consumption expenditure by government"]],
                  government_expected_vector
                )
              ),
              
              gfcf_product_growth = all(
                near_target(
                  use_y[["Gross fixed capital formation"]],
                  gfcf_expected_vector
                )
              ),
              
              inventories_product_growth = all(
                near_target(
                  use_y[["Changes in inventories"]],
                  inventories_expected_vector
                )
              ),
              
              exports_product_growth = all(
                near_target(
                  use_y[["Exports"]],
                  exports_expected_vector
                )
              ),
              
              imports_product_growth = all(
                near_target(
                  supply_y[["Total Imports"]],
                  imports_expected_vector
                )
              ),
              
              taxes_product_growth = tax_growth_exact,
              
              subsidies_product_growth = subsidy_growth_exact,
              
              zero_tax_rows_preserved = zero_tax_exact,
              
              tax_subsidy_sign_preserved = all(
                sign(
                  supply_y[["Taxes on product"]]
                ) ==
                  sign(
                    base_taxes
                  )
              ),
              
              net_tax_subsidy_total_exact = near_target(
                net_tax_actual_total,
                net_tax_expected_total
              ),
              
              margin_sum_zero = near_target(
                margin_total,
                0
              ),
              
              margin_sign_rule = !any(
                supply_y[["Trade and transport margin"]] < -tol &
                  !supply_y$Code %in%
                  allowed_negative_margin_codes
              ),
              
              product_supply_equals_use = all(
                near_target(
                  supply_y[["Total supply at purchasers' prices"]],
                  use_y[["TOTAL_USE_PURCHASERS"]]
                )
              ),
              
              aggregate_supply_equals_use = near_target(
                sum(
                  supply_y[["Total supply at purchasers' prices"]],
                  na.rm = TRUE
                ),
                sum(
                  use_y[["TOTAL_USE_PURCHASERS"]],
                  na.rm = TRUE
                )
              ),
              
              final_consumption_identity = all(
                near_target(
                  use_y[["Final consumptin expenditure"]],
                  use_y[["Final consumption expenditure by households"]] +
                    use_y[["Final consumption expenditure by NPISH"]] +
                    use_y[["Final consumption expenditure by government"]]
                )
              ),
              
              domestic_final_use_identity = all(
                near_target(
                  use_y[["Total domestic final use"]],
                  use_y[["Final consumptin expenditure"]] +
                    use_y[["Gross fixed capital formation"]] +
                    use_y[["Changes in inventories"]]
                )
              ),
              
              total_final_use_identity = all(
                near_target(
                  use_y[["Total final uses"]],
                  use_y[["Total domestic final use"]] +
                    use_y[["Exports"]]
                )
              )
            )
          
          
          # 11.28 Save tables ----
          
          sut_supply_by_year[[as.character(year)]] <- supply_y
          sut_use_by_year[[as.character(year)]] <- use_y
        }
        
        
        # 12. Final tables ----
        
        sut_supply_2024 <- sut_supply_by_year[["2024"]]
        sut_use_2024 <- sut_use_by_year[["2024"]]
        
        sut_supply_2025 <- sut_supply_by_year[["2025"]]
        sut_use_2025 <- sut_use_by_year[["2025"]]
        
        
        # 13. Diagnostics ----
        
        numeric_check <- dplyr::bind_rows(
          numeric_check_by_year
        )
        
        final_validation <- dplyr::bind_rows(
          validation_by_year
        )
        
        tax_detail <- dplyr::bind_rows(
          tax_detail_by_year
        )
        
        
        # 14. Factors used ----
        
        cat(
          "\n===== FACTORS USED =====\n"
        )
        
        print(
          factors_used,
          n = Inf,
          width = Inf
        )
        
        
        # 15. Expected vs actual ----
        
        cat(
          "\n===== EXPECTED VS ACTUAL =====\n"
        )
        
        print(
          numeric_check,
          n = Inf,
          width = Inf
        )
        
        
        # 16. Tax/subsidy detail ----
        
        cat(
          "\n===== TAX / SUBSIDY DETAIL =====\n"
        )
        
        print(
          tax_detail,
          n = Inf,
          width = Inf
        )
        
        
        # 17. Validation ----
        
        cat(
          "\n===== FINAL VALIDATION =====\n"
        )
        
        print(
          final_validation,
          n = Inf,
          width = Inf
        )
        
        
        # 18. Final result ----
        
        all_checks <- all(
          unlist(
            final_validation[
              ,
              setdiff(
                names(final_validation),
                "year"
              ),
              drop = FALSE
            ]
          )
        )
        
        
        cat(
          "\n===== FINAL RESULT =====\n"
        )
        
        
        cat(
          "Household growth exact       :",
          all(final_validation$household_product_growth),
          "\n"
        )
        
        
        cat(
          "NPISH growth exact           :",
          all(final_validation$npish_product_growth),
          "\n"
        )
        
        
        cat(
          "Government growth exact      :",
          all(final_validation$government_product_growth),
          "\n"
        )
        
        
        cat(
          "GFCF growth exact            :",
          all(final_validation$gfcf_product_growth),
          "\n"
        )
        
        
        cat(
          "Inventories growth exact     :",
          all(final_validation$inventories_product_growth),
          "\n"
        )
        
        
        cat(
          "Exports growth exact         :",
          all(final_validation$exports_product_growth),
          "\n"
        )
        
        
        cat(
          "Imports growth exact         :",
          all(final_validation$imports_product_growth),
          "\n"
        )
        
        
        cat(
          "Positive taxes growth exact  :",
          all(final_validation$taxes_product_growth),
          "\n"
        )
        
        
        cat(
          "Subsidies growth exact       :",
          all(final_validation$subsidies_product_growth),
          "\n"
        )
        
        
        cat(
          "Tax/subsidy signs preserved  :",
          all(final_validation$tax_subsidy_sign_preserved),
          "\n"
        )
        
        
        cat(
          "Net tax/subsidy total exact  :",
          all(final_validation$net_tax_subsidy_total_exact),
          "\n"
        )
        
        
        cat(
          "Margins sum to zero          :",
          all(final_validation$margin_sum_zero),
          "\n"
        )
        
        
        cat(
          "Margin sign rule             :",
          all(final_validation$margin_sign_rule),
          "\n"
        )
        
        
        cat(
          "Product Supply = Use         :",
          all(final_validation$product_supply_equals_use),
          "\n"
        )
        
        
        cat(
          "Aggregate Supply = Use       :",
          all(final_validation$aggregate_supply_equals_use),
          "\n"
        )
        
        
        cat(
          "ALL CHECKS                   :",
          all_checks,
          "\n"
        )
        
        
        

# III. Extrapolation of VAT matrix --------------------------------------------------------------------

        # 1. Settings ----
        
        vat_product_codes <- LETTERS[1:21]
        
        vat_exempt_codes <- c(
          "K",
          "O",
          "P",
          "Q"
        )
        
        vat_tol <- 1e-8
        
        
        # 2. Required objects ----
        
        vat_required_objects <- c(
          "sut_supply_2024",
          "sut_use_2024",
          "sut_supply_2025",
          "sut_use_2025",
          "ETR",
          "vat_targets"
        )
        
        vat_missing_objects <- vat_required_objects[
          !vapply(
            vat_required_objects,
            exists,
            logical(1),
            envir = .GlobalEnv
          )
        ]
        
        if (length(vat_missing_objects) > 0) {
          stop(
            paste0(
              "Missing object(s): ",
              paste(vat_missing_objects, collapse = ", ")
            )
          )
        }
        
        
        # 3. Helpers ----
        
        vat_clean_code <- function(x) {
          
          x <- stringr::str_to_upper(
            stringr::str_trim(
              as.character(x)
            )
          )
          
          x[x == ""] <- NA_character_
          
          x
        }
        
        
        vat_clean_sut <- function(df) {
          
          out <- df |>
            dplyr::mutate(
              Code = vat_clean_code(Code),
              Description = stringr::str_squish(as.character(Description))
            ) |>
            dplyr::filter(
              Code %in% vat_product_codes
            )
          
          out <- out[
            match(
              vat_product_codes,
              out$Code
            ),
            ,
            drop = FALSE
          ]
          
          out
        }
        
        
        vat_safe_numeric <- function(x) {
          
          x <- suppressWarnings(
            as.numeric(x)
          )
          
          x[!is.finite(x)] <- 0
          
          x
        }
        
        
        vat_safe_divide <- function(x, y) {
          
          out <- x / y
          
          out[!is.finite(out)] <- 0
          
          out
        }
        
        
        vat_gross_to_vat <- function(gross_value, rate) {
          
          gross_value <- vat_safe_numeric(gross_value)
          rate <- vat_safe_numeric(rate)
          
          if (any(rate < 0)) {
            stop(
              "Negative VAT rate found."
            )
          }
          
          gross_value * rate / (1 + rate)
        }
        
        
        vat_find_sector_col <- function(df, code) {
          
          hit <- names(df)[
            stringr::str_detect(
              names(df),
              paste0("^", code, "-")
            )
          ]
          
          if (length(hit) != 1) {
            stop(
              paste0(
                "Sector column not uniquely found for ",
                code
              )
            )
          }
          
          hit
        }
        
        
        vat_get_sector_cols <- function(df) {
          
          unname(
            vapply(
              vat_product_codes,
              function(code) {
                vat_find_sector_col(
                  df,
                  code
                )
              },
              character(1)
            )
          )
        }
        
        
        vat_near <- function(x, y) {
          
          abs(x - y) <=
            vat_tol *
            pmax(
              1,
              abs(y)
            )
        }
        
        
        # 4. Prepare ETR ----
        
        if (!all(c("ETR", "Description") %in% names(ETR))) {
          stop(
            "ETR must contain columns `ETR` and `Description`."
          )
        }
        
        
        vat_etr <- ETR |>
          dplyr::mutate(
            Description = stringr::str_squish(as.character(Description)),
            Code = stringr::str_extract(Description, "^[A-U](?=-)"),
            ETR = suppressWarnings(as.numeric(ETR))
          ) |>
          dplyr::select(
            Code,
            Description,
            ETR
          )
        
        
        if (any(is.na(vat_etr$Code))) {
          stop(
            "Some ETR descriptions do not start with A- through U-."
          )
        }
        
        
        if (anyDuplicated(vat_etr$Code)) {
          stop(
            "Duplicate A-U codes found in ETR."
          )
        }
        
        
        if (!setequal(vat_etr$Code, vat_product_codes)) {
          stop(
            "ETR must contain exactly one rate for each product A-U."
          )
        }
        
        
        if (any(!is.finite(vat_etr$ETR))) {
          stop(
            "ETR contains missing or invalid values."
          )
        }
        
        
        if (any(vat_etr$ETR < 0)) {
          stop(
            "ETR contains negative values."
          )
        }
        
        
        vat_etr <- vat_etr[
          match(
            vat_product_codes,
            vat_etr$Code
          ),
          ,
          drop = FALSE
        ]
        
        
        vat_etr_vector <- as.numeric(vat_etr$ETR)
        
        names(vat_etr_vector) <- vat_product_codes
        
        
        # 5. Prepare VAT targets ----
        
        if (!all(c("Description", "2024", "2025") %in% names(vat_targets))) {
          stop(
            "vat_targets must contain Description, 2024 and 2025."
          )
        }
        
        
        vat_target_row <- which(
          stringr::str_to_lower(
            stringr::str_squish(
              as.character(vat_targets$Description)
            )
          ) == "vat"
        )
        
        
        if (length(vat_target_row) != 1) {
          stop(
            "vat_targets must contain exactly one row with Description = VAT."
          )
        }
        
        
        vat_target_2024 <- as.numeric(vat_targets[["2024"]][vat_target_row])
        vat_target_2025 <- as.numeric(vat_targets[["2025"]][vat_target_row])
        
        
        if (!is.finite(vat_target_2024) || vat_target_2024 < 0) {
          stop(
            "Invalid 2024 VAT target."
          )
        }
        
        
        if (!is.finite(vat_target_2025) || vat_target_2025 < 0) {
          stop(
            "Invalid 2025 VAT target."
          )
        }
        
        
        # 6. Build VAT matrix ----
        
        vat_build_matrix <- function(
    supply_table,
    use_table,
    year
        ) {
          
          supply_y <- vat_clean_sut(
            supply_table
          )
          
          use_y <- vat_clean_sut(
            use_table
          )
          
          
          if (!identical(as.character(supply_y$Code), vat_product_codes)) {
            stop(
              paste0(
                "Supply ",
                year,
                " does not contain A-U correctly."
              )
            )
          }
          
          
          if (!identical(as.character(use_y$Code), vat_product_codes)) {
            stop(
              paste0(
                "Use ",
                year,
                " does not contain A-U correctly."
              )
            )
          }
          
          
          # 6.1 Industry columns ----
          
          supply_sector_cols <- vat_get_sector_cols(
            supply_y
          )
          
          use_sector_cols <- vat_get_sector_cols(
            use_y
          )
          
          
          if (length(supply_sector_cols) != 21) {
            stop(
              paste0(
                "Supply ",
                year,
                " does not have exactly 21 industry columns."
              )
            )
          }
          
          
          if (length(use_sector_cols) != 21) {
            stop(
              paste0(
                "Use ",
                year,
                " does not have exactly 21 industry columns."
              )
            )
          }
          
          
          # 6.2 Supply production matrix ----
          
          supply_output <- as.matrix(
            supply_y[
              ,
              supply_sector_cols,
              drop = FALSE
            ]
          )
          
          storage.mode(supply_output) <- "double"
          
          supply_output[!is.finite(supply_output)] <- 0
          
          
          if (any(supply_output < -vat_tol)) {
            stop(
              paste0(
                "Negative production values found in Supply ",
                year,
                "."
              )
            )
          }
          
          
          supply_output[supply_output < 0] <- 0
          
          
          # 6.3 PROPEX ----
          #
          # PROPEX_j =
          # exempt production of industry j
          # /
          # total production of industry j
          #
          # Exempt products:
          # K, O, P, Q
          
          exempt_rows <- supply_y$Code %in%
            vat_exempt_codes
          
          
          total_output_by_industry <- colSums(
            supply_output,
            na.rm = TRUE
          )
          
          
          exempt_output_by_industry <- colSums(
            supply_output[
              exempt_rows,
              ,
              drop = FALSE
            ],
            na.rm = TRUE
          )
          
          
          propex_vector <- vat_safe_divide(
            exempt_output_by_industry,
            total_output_by_industry
          )
          
          
          propex_vector <- pmin(
            pmax(
              propex_vector,
              0
            ),
            1
          )
          
          
          propex_table <- tibble::tibble(
            Year = year,
            Code = vat_product_codes,
            Industry = supply_sector_cols,
            Total_output = as.numeric(total_output_by_industry),
            Exempt_output = as.numeric(exempt_output_by_industry),
            PROPEX = as.numeric(propex_vector)
          )
          
          
          # 6.4 Intermediate consumption ----
          
          ic_gross <- as.matrix(
            use_y[
              ,
              use_sector_cols,
              drop = FALSE
            ]
          )
          
          storage.mode(ic_gross) <- "double"
          
          ic_gross[!is.finite(ic_gross)] <- 0
          
          
          if (any(ic_gross < -vat_tol)) {
            stop(
              paste0(
                "Negative intermediate consumption found in Use ",
                year,
                "."
              )
            )
          }
          
          
          ic_gross[ic_gross < 0] <- 0
          
          
          # 6.5 IC effective VAT rates ----
          #
          # rate_i,j = ETR_i * PROPEX_j
          
          ic_effective_rate <- outer(
            vat_etr_vector,
            as.numeric(propex_vector),
            "*"
          )
          
          
          rownames(ic_effective_rate) <- vat_product_codes
          colnames(ic_effective_rate) <- use_sector_cols
          
          
          # 6.6 IC VAT ----
          
          ic_vat <-
            ic_gross *
            ic_effective_rate /
            (
              1 +
                ic_effective_rate
            )
          
          
          rownames(ic_vat) <- vat_product_codes
          colnames(ic_vat) <- use_sector_cols
          
          
          # 6.7 Household VAT ----
          
          household_gross <- vat_safe_numeric(use_y[["Final consumption expenditure by households"]])
          
          household_vat <- vat_gross_to_vat(
            household_gross,
            vat_etr_vector
          )
          
          
          # 6.8 NPISH VAT ----
          
          npish_gross <- vat_safe_numeric(use_y[["Final consumption expenditure by NPISH"]])
          
          npish_vat <- vat_gross_to_vat(
            npish_gross,
            vat_etr_vector
          )
          
          
          # 6.9 Government VAT ----
          
          government_gross <- vat_safe_numeric(use_y[["Final consumption expenditure by government"]])
          
          government_vat <- vat_gross_to_vat(
            government_gross,
            vat_etr_vector
          )
          
          
          # 6.10 Final consumption VAT ----
          
          final_consumption_vat <-
            household_vat +
            npish_vat +
            government_vat
          
          
          # 6.11 GFCF ----
          
          gfcf_gross <- vat_safe_numeric(use_y[["Gross fixed capital formation"]])
          
          
          ic_row_total <- rowSums(
            ic_gross,
            na.rm = TRUE
          )
          
          
          gfcf_share <- matrix(
            1 / length(use_sector_cols),
            nrow = length(vat_product_codes),
            ncol = length(use_sector_cols)
          )
          
          
          rows_with_ic <- which(
            ic_row_total > vat_tol
          )
          
          
          if (length(rows_with_ic) > 0) {
            
            gfcf_share[
              rows_with_ic,
            ] <-
              ic_gross[
                rows_with_ic,
                ,
                drop = FALSE
              ] /
              ic_row_total[
                rows_with_ic
              ]
          }
          
          
          rownames(gfcf_share) <- vat_product_codes
          colnames(gfcf_share) <- use_sector_cols
          
          
          gfcf_gross_matrix <- sweep(
            gfcf_share,
            1,
            gfcf_gross,
            "*"
          )
          
          
          gfcf_vat_matrix <-
            gfcf_gross_matrix *
            ic_effective_rate /
            (
              1 +
                ic_effective_rate
            )
          
          
          gfcf_vat <- rowSums(
            gfcf_vat_matrix,
            na.rm = TRUE
          )
          
          
          # 6.12 Inventories and exports ----
          
          inventories_vat <- rep(
            0,
            length(vat_product_codes)
          )
          
          exports_vat <- rep(
            0,
            length(vat_product_codes)
          )
          
          
          # 6.13 Build VAT matrix ----
          #
          # Start with ONLY Code, Description and ETR.
          
          vat_matrix <- data.frame(
            Code = as.character(use_y$Code),
            Description = as.character(use_y$Description),
            ETR = as.numeric(vat_etr_vector),
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
          
          
          # Add the 21 FULL industry names one by one.
          # This prevents creation of standalone A-U columns.
          
          for (j in seq_along(use_sector_cols)) {
            vat_matrix[[use_sector_cols[j]]] <- ic_vat[, j]
          }
          
          
          # 6.14 Add total/final-use columns ----
          
          vat_matrix[["Total supply at purchasers' prices"]] <- rowSums(ic_vat, na.rm = TRUE)
          
          vat_matrix[["Final consumption expenditure by households"]] <- household_vat
          
          vat_matrix[["Final consumption expenditure by NPISH"]] <- npish_vat
          
          vat_matrix[["Final consumption expenditure by government"]] <- government_vat
          
          vat_matrix[["Final consumptin expenditure"]] <- final_consumption_vat
          
          vat_matrix[["Gross fixed capital formation"]] <- gfcf_vat
          
          vat_matrix[["Changes in inventories"]] <- inventories_vat
          
          vat_matrix[["Exports"]] <- exports_vat
          
          vat_matrix[["Total domestic final use"]] <- vat_matrix[["Final consumptin expenditure"]] + vat_matrix[["Gross fixed capital formation"]] + vat_matrix[["Changes in inventories"]]
          
          vat_matrix[["Total final uses"]] <- vat_matrix[["Total domestic final use"]] + vat_matrix[["Exports"]]
          
          vat_matrix[["TOTAL_USE_PURCHASERS"]] <- vat_matrix[["Total supply at purchasers' prices"]] + vat_matrix[["Total final uses"]]
          
          
          # 6.15 Force exact final columns ----
          
          final_cols <- c(
            "Code",
            "Description",
            "ETR",
            unname(use_sector_cols),
            "Total supply at purchasers' prices",
            "Final consumption expenditure by households",
            "Final consumption expenditure by NPISH",
            "Final consumption expenditure by government",
            "Final consumptin expenditure",
            "Gross fixed capital formation",
            "Changes in inventories",
            "Exports",
            "Total domestic final use",
            "Total final uses",
            "TOTAL_USE_PURCHASERS"
          )
          
          
          missing_final_cols <- setdiff(
            final_cols,
            names(vat_matrix)
          )
          
          
          if (length(missing_final_cols) > 0) {
            stop(
              paste0(
                "Missing VAT columns in ",
                year,
                ": ",
                paste(missing_final_cols, collapse = ", ")
              )
            )
          }
          
          
          vat_matrix <- vat_matrix[
            ,
            final_cols,
            drop = FALSE
          ]
          
          
          vat_matrix <- tibble::as_tibble(
            vat_matrix
          )
          
          
          # 6.16 Confirm no standalone letters ----
          
          standalone_letter_cols <- intersect(
            vat_product_codes,
            names(vat_matrix)
          )
          
          
          if (length(standalone_letter_cols) > 0) {
            stop(
              paste0(
                "Standalone A-U columns unexpectedly found in ",
                year,
                ": ",
                paste(standalone_letter_cols, collapse = ", ")
              )
            )
          }
          
          
          # 6.17 Effective-rate detail ----
          
          ic_rate_table <- data.frame(
            Code = as.character(use_y$Code),
            Description = as.character(use_y$Description),
            ETR = as.numeric(vat_etr_vector),
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
          
          
          for (j in seq_along(use_sector_cols)) {
            ic_rate_table[[use_sector_cols[j]]] <- ic_effective_rate[, j]
          }
          
          
          ic_rate_table <- tibble::as_tibble(
            ic_rate_table
          )
          
          
          # 6.18 GFCF detail ----
          
          gfcf_detail <- data.frame(
            Code = as.character(use_y$Code),
            Description = as.character(use_y$Description),
            ETR = as.numeric(vat_etr_vector),
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
          
          
          for (j in seq_along(use_sector_cols)) {
            gfcf_detail[[use_sector_cols[j]]] <- gfcf_vat_matrix[, j]
          }
          
          
          gfcf_detail[["Gross fixed capital formation gross"]] <- gfcf_gross
          
          gfcf_detail[["Gross fixed capital formation VAT"]] <- gfcf_vat
          
          
          gfcf_detail <- tibble::as_tibble(
            gfcf_detail
          )
          
          
          list(
            year = year,
            industry_cols = unname(use_sector_cols),
            propex = propex_table,
            vat_matrix = vat_matrix,
            ic_rates = ic_rate_table,
            gfcf_detail = gfcf_detail,
            use_table = use_y
          )
        }
        
        
        # 7. Calculate uncapped 2024 ----
        
        vat_result_2024 <- vat_build_matrix(
          supply_table = sut_supply_2024,
          use_table = sut_use_2024,
          year = 2024L
        )
        
        
        VAT_uncapped_2024 <- vat_result_2024$vat_matrix
        
        PROPEX_2024 <- vat_result_2024$propex
        
        IC_rates_2024 <- vat_result_2024$ic_rates
        
        GFCF_VAT_2024 <- vat_result_2024$gfcf_detail
        
        
        # 8. Calculate uncapped 2025 ----
        
        vat_result_2025 <- vat_build_matrix(
          supply_table = sut_supply_2025,
          use_table = sut_use_2025,
          year = 2025L
        )
        
        
        VAT_uncapped_2025 <- vat_result_2025$vat_matrix
        
        PROPEX_2025 <- vat_result_2025$propex
        
        IC_rates_2025 <- vat_result_2025$ic_rates
        
        GFCF_VAT_2025 <- vat_result_2025$gfcf_detail
        
        
        # 9. VAT target cap function ----
        
        vat_apply_target_cap <- function(
    vat_matrix,
    industry_cols,
    vat_target,
    year
        ) {
          
          uncapped_total <- sum(
            vat_matrix[["TOTAL_USE_PURCHASERS"]],
            na.rm = TRUE
          )
          
          
          if (!is.finite(uncapped_total) || uncapped_total < 0) {
            stop(
              paste0(
                "Invalid uncapped VAT total in ",
                year
              )
            )
          }
          
          
          if (uncapped_total > 0) {
            
            vat_scale_factor <- vat_target / uncapped_total
            
          } else if (vat_target == 0) {
            
            vat_scale_factor <- 0
            
          } else {
            
            stop(
              paste0(
                "Cannot scale zero VAT matrix to positive VAT target in ",
                year
              )
            )
          }
          
          
          vat_value_cols <- c(
            unname(industry_cols),
            "Total supply at purchasers' prices",
            "Final consumption expenditure by households",
            "Final consumption expenditure by NPISH",
            "Final consumption expenditure by government",
            "Final consumptin expenditure",
            "Gross fixed capital formation",
            "Changes in inventories",
            "Exports",
            "Total domestic final use",
            "Total final uses",
            "TOTAL_USE_PURCHASERS"
          )
          
          
          final_matrix <- vat_matrix
          
          
          for (col in vat_value_cols) {
            final_matrix[[col]] <- final_matrix[[col]] * vat_scale_factor
          }
          
          
          final_total <- sum(
            final_matrix[["TOTAL_USE_PURCHASERS"]],
            na.rm = TRUE
          )
          
          
          if (!vat_near(final_total, vat_target)) {
            stop(
              paste0(
                "Final VAT does not equal declared VAT target in ",
                year,
                ". Target = ",
                vat_target,
                ", final VAT = ",
                final_total
              )
            )
          }
          
          
          summary <- tibble::tibble(
            Year = year,
            VAT_target = vat_target,
            VAT_uncapped = uncapped_total,
            VAT_scale_factor = vat_scale_factor,
            VAT_final = final_total,
            Cap_applied = !vat_near(uncapped_total, vat_target),
            Check_VAT_le_target = final_total <= vat_target + vat_tol,
            Check_VAT_equal_target = vat_near(final_total, vat_target)
          )
          
          
          list(
            matrix = final_matrix,
            summary = summary
          )
        }
        
        
        # 10. Apply 2024 target ----
        
        vat_capped_2024 <- vat_apply_target_cap(
          vat_matrix = VAT_uncapped_2024,
          industry_cols = vat_result_2024$industry_cols,
          vat_target = vat_target_2024,
          year = 2024L
        )
        
        
        VAT_2024 <- vat_capped_2024$matrix
        
        vat_target_check_2024 <- vat_capped_2024$summary
        
        
        # 11. Apply 2025 target ----
        
        vat_capped_2025 <- vat_apply_target_cap(
          vat_matrix = VAT_uncapped_2025,
          industry_cols = vat_result_2025$industry_cols,
          vat_target = vat_target_2025,
          year = 2025L
        )
        
        
        VAT_2025 <- vat_capped_2025$matrix
        
        vat_target_check_2025 <- vat_capped_2025$summary
        
        
        # 12. Final cleanup ----
        #
        # Do NOT use identical() here.
        # We explicitly select the required columns.
        
        final_vat_cols_2024 <- c(
          "Code",
          "Description",
          "ETR",
          unname(vat_result_2024$industry_cols),
          "Total supply at purchasers' prices",
          "Final consumption expenditure by households",
          "Final consumption expenditure by NPISH",
          "Final consumption expenditure by government",
          "Final consumptin expenditure",
          "Gross fixed capital formation",
          "Changes in inventories",
          "Exports",
          "Total domestic final use",
          "Total final uses",
          "TOTAL_USE_PURCHASERS"
        )
        
        
        final_vat_cols_2025 <- c(
          "Code",
          "Description",
          "ETR",
          unname(vat_result_2025$industry_cols),
          "Total supply at purchasers' prices",
          "Final consumption expenditure by households",
          "Final consumption expenditure by NPISH",
          "Final consumption expenditure by government",
          "Final consumptin expenditure",
          "Gross fixed capital formation",
          "Changes in inventories",
          "Exports",
          "Total domestic final use",
          "Total final uses",
          "TOTAL_USE_PURCHASERS"
        )
        
        
        VAT_2024 <- VAT_2024 |>
          dplyr::select(
            dplyr::all_of(final_vat_cols_2024)
          )
        
        
        VAT_2025 <- VAT_2025 |>
          dplyr::select(
            dplyr::all_of(final_vat_cols_2025)
          )
        
        
        VAT_uncapped_2024 <- VAT_uncapped_2024 |>
          dplyr::select(
            dplyr::all_of(final_vat_cols_2024)
          )
        
        
        VAT_uncapped_2025 <- VAT_uncapped_2025 |>
          dplyr::select(
            dplyr::all_of(final_vat_cols_2025)
          )
        
        
        # 13. Duplicate-column check ----
        
        bad_cols_2024 <- intersect(
          vat_product_codes,
          names(VAT_2024)
        )
        
        bad_cols_2025 <- intersect(
          vat_product_codes,
          names(VAT_2025)
        )
        
        
        if (length(bad_cols_2024) > 0) {
          stop(
            paste0(
              "Unwanted standalone columns in VAT_2024: ",
              paste(bad_cols_2024, collapse = ", ")
            )
          )
        }
        
        
        if (length(bad_cols_2025) > 0) {
          stop(
            paste0(
              "Unwanted standalone columns in VAT_2025: ",
              paste(bad_cols_2025, collapse = ", ")
            )
          )
        }
        
        
        # 14. Column-count check ----
        
        if (ncol(VAT_2024) != 35) {
          stop(
            paste0(
              "VAT_2024 has ",
              ncol(VAT_2024),
              " columns instead of 35."
            )
          )
        }
        
        
        if (ncol(VAT_2025) != 35) {
          stop(
            paste0(
              "VAT_2025 has ",
              ncol(VAT_2025),
              " columns instead of 35."
            )
          )
        }
        
        
        # 15. VAT target summary ----
        
        vat_target_check <- dplyr::bind_rows(
          vat_target_check_2024,
          vat_target_check_2025
        )
        
        
        # 16. Capacity validation ----
        
        vat_capacity_check <- tibble::tibble(
          Year = c(
            2024L,
            2025L
          ),
          
          VAT_target = c(
            vat_target_2024,
            vat_target_2025
          ),
          
          VAT_final = c(
            sum(VAT_2024[["TOTAL_USE_PURCHASERS"]], na.rm = TRUE),
            sum(VAT_2025[["TOTAL_USE_PURCHASERS"]], na.rm = TRUE)
          ),
          
          VAT_le_target = c(
            sum(VAT_2024[["TOTAL_USE_PURCHASERS"]], na.rm = TRUE) <= vat_target_2024 + vat_tol,
            sum(VAT_2025[["TOTAL_USE_PURCHASERS"]], na.rm = TRUE) <= vat_target_2025 + vat_tol
          ),
          
          No_A_U_duplicate_columns = c(
            length(intersect(vat_product_codes, names(VAT_2024))) == 0,
            length(intersect(vat_product_codes, names(VAT_2025))) == 0
          ),
          
          Number_of_columns = c(
            ncol(VAT_2024),
            ncol(VAT_2025)
          )
        )
        
        
        # 17. VAT totals ----
        
        vat_totals <- tibble::tibble(
          Year = c(
            2024L,
            2025L
          ),
          
          VAT_IC = c(
            sum(VAT_2024[["Total supply at purchasers' prices"]], na.rm = TRUE),
            sum(VAT_2025[["Total supply at purchasers' prices"]], na.rm = TRUE)
          ),
          
          VAT_households = c(
            sum(VAT_2024[["Final consumption expenditure by households"]], na.rm = TRUE),
            sum(VAT_2025[["Final consumption expenditure by households"]], na.rm = TRUE)
          ),
          
          VAT_NPISH = c(
            sum(VAT_2024[["Final consumption expenditure by NPISH"]], na.rm = TRUE),
            sum(VAT_2025[["Final consumption expenditure by NPISH"]], na.rm = TRUE)
          ),
          
          VAT_government = c(
            sum(VAT_2024[["Final consumption expenditure by government"]], na.rm = TRUE),
            sum(VAT_2025[["Final consumption expenditure by government"]], na.rm = TRUE)
          ),
          
          VAT_GFCF = c(
            sum(VAT_2024[["Gross fixed capital formation"]], na.rm = TRUE),
            sum(VAT_2025[["Gross fixed capital formation"]], na.rm = TRUE)
          ),
          
          VAT_total = c(
            sum(VAT_2024[["TOTAL_USE_PURCHASERS"]], na.rm = TRUE),
            sum(VAT_2025[["TOTAL_USE_PURCHASERS"]], na.rm = TRUE)
          ),
          
          VAT_target = c(
            vat_target_2024,
            vat_target_2025
          )
        )
        
        
        # 18. Print results ----
        
        cat(
          "\n===== PROPEX 2024 =====\n"
        )
        
        print(
          PROPEX_2024,
          n = Inf,
          width = Inf
        )
        
        
        cat(
          "\n===== PROPEX 2025 =====\n"
        )
        
        print(
          PROPEX_2025,
          n = Inf,
          width = Inf
        )
        
        
        cat(
          "\n===== VAT TARGET CHECK =====\n"
        )
        
        print(
          vat_target_check,
          n = Inf,
          width = Inf
        )
        
        
        cat(
          "\n===== VAT TOTALS =====\n"
        )
        
        print(
          vat_totals,
          n = Inf,
          width = Inf
        )
        
        
        cat(
          "\n===== FINAL CHECK =====\n"
        )
        
        print(
          vat_capacity_check,
          n = Inf,
          width = Inf
        )
        
        
        # 19. Show final VAT columns ----
        
        cat(
          "\n===== VAT_2024 COLUMNS =====\n"
        )
        
        print(
          tibble::tibble(
            Number = seq_along(names(VAT_2024)),
            Column = names(VAT_2024)
          ),
          n = Inf,
          width = Inf
        )
        
        
        VAT_2024$ETR<-NULL
        
        cat(
          "\n===== VAT_2025 COLUMNS =====\n"
        )
        
        print(
          tibble::tibble(
            Number = seq_along(names(VAT_2025)),
            Column = names(VAT_2025)
          ),
          n = Inf,
          width = Inf
        )
        
        VAT_2025$ETR<-NULL
       

# IV.. Export tables ---- --------------------------------------------------

        # 20. Export yearly SUT and VAT files ----
        
       
        
        vat_path_export <- paste0(path1, "/Data/VAT/Data_raw/SUTs/")
        setwd(vat_path_export)
        getwd()
        
        
        writexl::write_xlsx(
          x = list(
            Supply = sut_supply_2024,
            Use = sut_use_2024,
            VAT = VAT_2024
          ),
          path = "SUTs_extrapolated_2024.xlsx"
        )

        writexl::write_xlsx(
          x = list(
            Supply = sut_supply_2025,
            Use = sut_use_2025,
            VAT = VAT_2025
          ),
          path = "SUTs_extrapolated_2025.xlsx"
        )

        cat(
          "\n===== EXCEL EXPORT =====\n"
        )

        cat(
          "File created:",
          normalizePath("SUTs_extrapolated_2024.xlsx"),
          "\n"
        )

        cat(
          "File created:",
          normalizePath("SUTs_extrapolated_2025.xlsx"),
          "\n"
        )
        
        
        # Seeting path for Initial Load script
        
        tmp_vat <- paste0(path1)
        setwd(tmp_vat)
        getwd()
        
        