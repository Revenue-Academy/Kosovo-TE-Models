# VAT-DATA TRANSFORMATION

start.time <- proc.time()

suppressMessages({
  
  
  # 1. Settings ----
  
  benchmark_tax_rate_bu <- 0.18
  
  TE_EXEMPT <- 0.00
  TE_REDUCED_RATE <- 0.00
  
  
  # 2. Validate active inputs ----
  
  required_active_objects <- c(
    "SUPPLY_ACTIVE",
    "USE_ACTIVE",
    "USE_VAT_raw",
    "CPA_TAXABLE_PROPORTIONS_BU"
  )
  
  missing_active_objects <- setdiff(
    required_active_objects,
    ls(envir = .GlobalEnv)
  )
  
  if (length(missing_active_objects) > 0) {
    stop(
      paste0(
        "Missing required active object(s): ",
        paste(missing_active_objects, collapse = ", ")
      )
    )
  }
  
  if (!all(c("Code", "Description") %in% names(SUPPLY_ACTIVE))) {
    stop("SUPPLY_ACTIVE must contain Code and Description.")
  }
  
  if (!all(c("Code", "Description") %in% names(USE_ACTIVE))) {
    stop("USE_ACTIVE must contain Code and Description.")
  }
  
  if (!all(c("Code", "Description") %in% names(USE_VAT_raw))) {
    stop("USE_VAT_raw must contain Code and Description.")
  }
  
  if (nrow(SUPPLY_ACTIVE) != 21) {
    stop("SUPPLY_ACTIVE must contain exactly 21 product rows A-U.")
  }
  
  if (nrow(USE_ACTIVE) != 21) {
    stop("USE_ACTIVE must contain exactly 21 product rows A-U.")
  }
  
  if (nrow(USE_VAT_raw) != 21) {
    stop("USE_VAT_raw must contain exactly 21 product rows A-U.")
  }
  
  
  # 3. Build original raw objects ----
  
  local({
    
    PRODUCT_CODES <- LETTERS[1:21]
    
    PRODUCT_NAMES <- CPA_TAXABLE_PROPORTIONS_BU$PRODUCT_INDUSTRY_NAME[
      match(
        PRODUCT_CODES,
        CPA_TAXABLE_PROPORTIONS_BU$PRODUCT_INDUSTRY_CODE
      )
    ]
    
    
    # 3.1 Identify Supply A-U columns ----
    
    SUPPLY_INDUSTRY_COLUMNS <- unname(
      vapply(
        PRODUCT_CODES,
        function(x) {
          
          hit <- names(SUPPLY_ACTIVE)[
            stringr::str_detect(
              names(SUPPLY_ACTIVE),
              paste0("^", x, "-")
            )
          ]
          
          if (length(hit) != 1) {
            stop(
              paste0(
                "Cannot identify Supply industry column ",
                x
              )
            )
          }
          
          hit
          
        },
        character(1)
      )
    )
    
    
    # 3.2 Identify Use A-U columns ----
    
    USE_INDUSTRY_COLUMNS <- unname(
      vapply(
        PRODUCT_CODES,
        function(x) {
          
          hit <- names(USE_ACTIVE)[
            stringr::str_detect(
              names(USE_ACTIVE),
              paste0("^", x, "-")
            )
          ]
          
          if (length(hit) != 1) {
            stop(
              paste0(
                "Cannot identify Use industry column ",
                x
              )
            )
          }
          
          hit
          
        },
        character(1)
      )
    )
    
    
    # 3.3 Identify VAT A-U columns independently ----
    
    VAT_INDUSTRY_COLUMNS <- unname(
      vapply(
        PRODUCT_CODES,
        function(x) {
          
          hit <- names(USE_VAT_raw)[
            stringr::str_detect(
              names(USE_VAT_raw),
              paste0("^", x, "-")
            )
          ]
          
          if (length(hit) != 1) {
            stop(
              paste0(
                "Cannot identify VAT industry column ",
                x
              )
            )
          }
          
          hit
          
        },
        character(1)
      )
    )
    
    
    # 3.4 Canonical industry names ----
    
    INDUSTRY_NAMES <- USE_INDUSTRY_COLUMNS
    
    names(INDUSTRY_NAMES) <- PRODUCT_CODES
    
    
    # 3.5 SUPPLY_raw ----
    
    SUPPLY_MATRIX_LONG <- SUPPLY_ACTIVE |>
      dplyr::select(
        Code,
        dplyr::all_of(SUPPLY_INDUSTRY_COLUMNS)
      ) |>
      tidyr::pivot_longer(
        cols = -Code,
        names_to = "INDUSTRY_NAME",
        values_to = "value"
      ) |>
      dplyr::mutate(
        PRODUCT_INDUSTRY_CODE = as.character(Code),
        PRODUCT_INDUSTRY_NAME = PRODUCT_NAMES[
          match(
            PRODUCT_INDUSTRY_CODE,
            PRODUCT_CODES
          )
        ],
        INDUSTRY_CODE = stringr::str_extract(
          INDUSTRY_NAME,
          "^[A-U]"
        ),
        INDUSTRY_NAME = INDUSTRY_NAMES[
          INDUSTRY_CODE
        ],
        value = suppressWarnings(
          as.numeric(value)
        )
      ) |>
      dplyr::select(
        PRODUCT_INDUSTRY_CODE,
        PRODUCT_INDUSTRY_NAME,
        INDUSTRY_CODE,
        INDUSTRY_NAME,
        value
      )
    
    SUPPLY_IMPORTS <- tibble::tibble(
      PRODUCT_INDUSTRY_CODE = PRODUCT_CODES,
      PRODUCT_INDUSTRY_NAME = PRODUCT_NAMES,
      INDUSTRY_CODE = "NA",
      INDUSTRY_NAME = "Imports CIF",
      value = suppressWarnings(as.numeric(SUPPLY_ACTIVE[["Total Imports"]]))
    )
    
    SUPPLY_MARGINS <- tibble::tibble(
      PRODUCT_INDUSTRY_CODE = PRODUCT_CODES,
      PRODUCT_INDUSTRY_NAME = PRODUCT_NAMES,
      INDUSTRY_CODE = "NA",
      INDUSTRY_NAME = "Trade and transport margins",
      value = suppressWarnings(as.numeric(SUPPLY_ACTIVE[["Trade and transport margin"]]))
    )
    
    SUPPLY_TAXES <- tibble::tibble(
      PRODUCT_INDUSTRY_CODE = PRODUCT_CODES,
      PRODUCT_INDUSTRY_NAME = PRODUCT_NAMES,
      INDUSTRY_CODE = "NA",
      INDUSTRY_NAME = "Taxes less subsidies on products",
      value = suppressWarnings(as.numeric(SUPPLY_ACTIVE[["Taxes on product"]]))
    )
    
    SUPPLY_raw <<- dplyr::bind_rows(
      SUPPLY_MATRIX_LONG,
      SUPPLY_IMPORTS,
      SUPPLY_MARGINS,
      SUPPLY_TAXES
    )
    
    
    # 3.6 USE_PURCHASER_raw ----
    
    USE_PURCHASER_MATRIX_LONG <- USE_ACTIVE |>
      dplyr::select(
        Code,
        dplyr::all_of(USE_INDUSTRY_COLUMNS)
      ) |>
      tidyr::pivot_longer(
        cols = -Code,
        names_to = "INDUSTRY_NAME",
        values_to = "value"
      ) |>
      dplyr::mutate(
        PRODUCT_INDUSTRY_CODE = as.character(Code),
        PRODUCT_INDUSTRY_NAME = PRODUCT_NAMES[
          match(
            PRODUCT_INDUSTRY_CODE,
            PRODUCT_CODES
          )
        ],
        INDUSTRY_CODE = stringr::str_extract(
          INDUSTRY_NAME,
          "^[A-U]"
        ),
        INDUSTRY_NAME = INDUSTRY_NAMES[
          INDUSTRY_CODE
        ],
        value = suppressWarnings(
          as.numeric(value)
        )
      ) |>
      dplyr::select(
        PRODUCT_INDUSTRY_CODE,
        PRODUCT_INDUSTRY_NAME,
        INDUSTRY_CODE,
        INDUSTRY_NAME,
        value
      )
    
    USE_PURCHASER_HH <- tibble::tibble(
      PRODUCT_INDUSTRY_CODE = PRODUCT_CODES,
      PRODUCT_INDUSTRY_NAME = PRODUCT_NAMES,
      INDUSTRY_CODE = "NA",
      INDUSTRY_NAME = "Final consumption expenditure by households",
      value = suppressWarnings(as.numeric(USE_ACTIVE[["Final consumption expenditure by households"]]))
    )
    
    USE_PURCHASER_NPISH <- tibble::tibble(
      PRODUCT_INDUSTRY_CODE = PRODUCT_CODES,
      PRODUCT_INDUSTRY_NAME = PRODUCT_NAMES,
      INDUSTRY_CODE = "NA",
      INDUSTRY_NAME = "Final consumption expenditure by non-profit organisations serving households (NPISH)",
      value = suppressWarnings(as.numeric(USE_ACTIVE[["Final consumption expenditure by NPISH"]]))
    )
    
    USE_PURCHASER_GOV <- tibble::tibble(
      PRODUCT_INDUSTRY_CODE = PRODUCT_CODES,
      PRODUCT_INDUSTRY_NAME = PRODUCT_NAMES,
      INDUSTRY_CODE = "NA",
      INDUSTRY_NAME = "Final consumption expenditure by government",
      value = suppressWarnings(as.numeric(USE_ACTIVE[["Final consumption expenditure by government"]]))
    )
    
    USE_PURCHASER_GFCF <- tibble::tibble(
      PRODUCT_INDUSTRY_CODE = PRODUCT_CODES,
      PRODUCT_INDUSTRY_NAME = PRODUCT_NAMES,
      INDUSTRY_CODE = "NA",
      INDUSTRY_NAME = "Gross fixed capital formation",
      value = suppressWarnings(as.numeric(USE_ACTIVE[["Gross fixed capital formation"]]))
    )
    
    USE_PURCHASER_INVENTORIES <- tibble::tibble(
      PRODUCT_INDUSTRY_CODE = PRODUCT_CODES,
      PRODUCT_INDUSTRY_NAME = PRODUCT_NAMES,
      INDUSTRY_CODE = "NA",
      INDUSTRY_NAME = "Changes in inventories and acquisition less disposals of valuables",
      value = suppressWarnings(as.numeric(USE_ACTIVE[["Changes in inventories"]]))
    )
    
    USE_PURCHASER_EXPORTS <- tibble::tibble(
      PRODUCT_INDUSTRY_CODE = PRODUCT_CODES,
      PRODUCT_INDUSTRY_NAME = PRODUCT_NAMES,
      INDUSTRY_CODE = "NA",
      INDUSTRY_NAME = "Exports FOB",
      value = suppressWarnings(as.numeric(USE_ACTIVE[["Exports"]]))
    )
    
    USE_PURCHASER_raw <<- dplyr::bind_rows(
      USE_PURCHASER_MATRIX_LONG,
      USE_PURCHASER_HH,
      USE_PURCHASER_NPISH,
      USE_PURCHASER_GOV,
      USE_PURCHASER_GFCF,
      USE_PURCHASER_INVENTORIES,
      USE_PURCHASER_EXPORTS
    )
    
    
    # 3.7 USE_VAT ----
    
    USE_VAT_MATRIX_LONG <- USE_VAT_raw |>
      dplyr::select(
        Code,
        dplyr::all_of(VAT_INDUSTRY_COLUMNS)
      ) |>
      tidyr::pivot_longer(
        cols = -Code,
        names_to = "INDUSTRY_NAME",
        values_to = "value"
      ) |>
      dplyr::mutate(
        PRODUCT_INDUSTRY_CODE = as.character(Code),
        PRODUCT_INDUSTRY_NAME = PRODUCT_NAMES[
          match(
            PRODUCT_INDUSTRY_CODE,
            PRODUCT_CODES
          )
        ],
        INDUSTRY_CODE = stringr::str_extract(
          INDUSTRY_NAME,
          "^[A-U]"
        ),
        INDUSTRY_NAME = INDUSTRY_NAMES[
          INDUSTRY_CODE
        ],
        value = suppressWarnings(
          as.numeric(value)
        )
      ) |>
      dplyr::select(
        PRODUCT_INDUSTRY_CODE,
        PRODUCT_INDUSTRY_NAME,
        INDUSTRY_CODE,
        INDUSTRY_NAME,
        value
      )
    
    USE_VAT_HH <- tibble::tibble(
      PRODUCT_INDUSTRY_CODE = PRODUCT_CODES,
      PRODUCT_INDUSTRY_NAME = PRODUCT_NAMES,
      INDUSTRY_CODE = "NA",
      INDUSTRY_NAME = "Final consumption expenditure by households",
      value = suppressWarnings(as.numeric(USE_VAT_raw[["Final consumption expenditure by households"]]))
    )
    
    USE_VAT_NPISH <- tibble::tibble(
      PRODUCT_INDUSTRY_CODE = PRODUCT_CODES,
      PRODUCT_INDUSTRY_NAME = PRODUCT_NAMES,
      INDUSTRY_CODE = "NA",
      INDUSTRY_NAME = "Final consumption expenditure by non-profit organisations serving households (NPISH)",
      value = suppressWarnings(as.numeric(USE_VAT_raw[["Final consumption expenditure by NPISH"]]))
    )
    
    USE_VAT_GOV <- tibble::tibble(
      PRODUCT_INDUSTRY_CODE = PRODUCT_CODES,
      PRODUCT_INDUSTRY_NAME = PRODUCT_NAMES,
      INDUSTRY_CODE = "NA",
      INDUSTRY_NAME = "Final consumption expenditure by government",
      value = suppressWarnings(as.numeric(USE_VAT_raw[["Final consumption expenditure by government"]]))
    )
    
    USE_VAT_GFCF <- tibble::tibble(
      PRODUCT_INDUSTRY_CODE = PRODUCT_CODES,
      PRODUCT_INDUSTRY_NAME = PRODUCT_NAMES,
      INDUSTRY_CODE = "NA",
      INDUSTRY_NAME = "Gross fixed capital formation",
      value = suppressWarnings(as.numeric(USE_VAT_raw[["Gross fixed capital formation"]]))
    )
    
    USE_VAT_INVENTORIES <- tibble::tibble(
      PRODUCT_INDUSTRY_CODE = PRODUCT_CODES,
      PRODUCT_INDUSTRY_NAME = PRODUCT_NAMES,
      INDUSTRY_CODE = "NA",
      INDUSTRY_NAME = "Changes in inventories and acquisition less disposals of valuables",
      value = suppressWarnings(as.numeric(USE_VAT_raw[["Changes in inventories"]]))
    )
    
    USE_VAT_EXPORTS <- tibble::tibble(
      PRODUCT_INDUSTRY_CODE = PRODUCT_CODES,
      PRODUCT_INDUSTRY_NAME = PRODUCT_NAMES,
      INDUSTRY_CODE = "NA",
      INDUSTRY_NAME = "Exports FOB",
      value = suppressWarnings(as.numeric(USE_VAT_raw[["Exports"]]))
    )
    
    USE_VAT <<- dplyr::bind_rows(
      USE_VAT_MATRIX_LONG,
      USE_VAT_HH,
      USE_VAT_NPISH,
      USE_VAT_GOV,
      USE_VAT_GFCF,
      USE_VAT_INVENTORIES,
      USE_VAT_EXPORTS
    )
    
    
    # 3.8 USE_BASIC_raw ----
    
    USE_BASIC_raw <<- USE_PURCHASER_raw |>
      dplyr::left_join(
        USE_VAT |>
          dplyr::rename(
            VAT_VALUE = value
          ),
        by = c(
          "PRODUCT_INDUSTRY_CODE",
          "PRODUCT_INDUSTRY_NAME",
          "INDUSTRY_CODE",
          "INDUSTRY_NAME"
        )
      ) |>
      dplyr::mutate(
        VAT_VALUE = dplyr::coalesce(
          VAT_VALUE,
          0
        ),
        value = value - VAT_VALUE
      ) |>
      dplyr::select(
        PRODUCT_INDUSTRY_CODE,
        PRODUCT_INDUSTRY_NAME,
        INDUSTRY_CODE,
        INDUSTRY_NAME,
        value
      )
    
  })
  
  
  # 4. Original aggregate lists ----
  
  CPA_PRODUCTS_raw <- as.list(c(1:4))
  
  names(CPA_PRODUCTS_raw) <- c(
    "Supply",
    "Use_Purchaser",
    "Use_VAT",
    "Use_Basic"
  )
  
  NACE_INDUSTRIES_raw <- as.list(c(1:4))
  
  names(NACE_INDUSTRIES_raw) <- c(
    "Supply",
    "Use_Purchaser",
    "Use_VAT",
    "Use_Basic"
  )
  
  
  # 5. Supply matrix ----
  
  SUPPLY <- SUPPLY_raw
  
  CPA_PRODUCTS_raw$Supply <- SUPPLY |>
    dplyr::filter(
      PRODUCT_INDUSTRY_CODE != "NA",
      INDUSTRY_CODE != "NA"
    ) |>
    dplyr::group_by(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Total_output = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  CPA_PRODUCTS_raw$Supply <- CPA_PRODUCTS_raw$Supply |>
    dplyr::left_join(
      SUPPLY |>
        dplyr::filter(
          INDUSTRY_NAME == "Imports CIF"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Imports_CIF = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Supply <- CPA_PRODUCTS_raw$Supply |>
    dplyr::left_join(
      SUPPLY |>
        dplyr::filter(
          INDUSTRY_NAME == "Trade and transport margins"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Trade_and_transport_margins = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Supply <- CPA_PRODUCTS_raw$Supply |>
    dplyr::left_join(
      SUPPLY |>
        dplyr::filter(
          INDUSTRY_NAME == "Taxes less subsidies on products"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Taxes_less_subsidies_on_products = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Supply <- CPA_PRODUCTS_raw$Supply |>
    dplyr::mutate(
      Total_supply_at_basic_prices = rowSums(
        cbind(
          Total_output,
          Imports_CIF
        ),
        na.rm = TRUE
      ),
      Total_supply_at_purchasers_prices = rowSums(
        cbind(
          Total_supply_at_basic_prices,
          Trade_and_transport_margins,
          Taxes_less_subsidies_on_products
        ),
        na.rm = TRUE
      )
    )
  
  NACE_INDUSTRIES_raw$Supply <- SUPPLY |>
    dplyr::filter(
      PRODUCT_INDUSTRY_CODE != "NA",
      INDUSTRY_CODE != "NA"
    ) |>
    dplyr::group_by(
      INDUSTRY_CODE,
      INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Total_output_by_industries_at_basic_prices = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  
  # 6. Use purchaser matrix ----
  
  USE_PURCHASER <- USE_PURCHASER_raw
  
  CPA_PRODUCTS_raw$Use_Purchaser <- USE_PURCHASER |>
    dplyr::filter(
      PRODUCT_INDUSTRY_CODE != "NA",
      INDUSTRY_CODE != "NA"
    ) |>
    dplyr::group_by(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Total_intermediate_consumption_at_purchasers_prices = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  CPA_PRODUCTS_raw$Use_Purchaser <- CPA_PRODUCTS_raw$Use_Purchaser |>
    dplyr::left_join(
      USE_PURCHASER |>
        dplyr::filter(
          INDUSTRY_NAME == "Final consumption expenditure by households"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Final_consumption_expenditure_by_households = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Use_Purchaser <- CPA_PRODUCTS_raw$Use_Purchaser |>
    dplyr::left_join(
      USE_PURCHASER |>
        dplyr::filter(
          INDUSTRY_NAME == "Final consumption expenditure by non-profit organisations serving households (NPISH)"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Final_consumption_expenditure_NPISH = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Use_Purchaser <- CPA_PRODUCTS_raw$Use_Purchaser |>
    dplyr::left_join(
      USE_PURCHASER |>
        dplyr::filter(
          INDUSTRY_NAME == "Final consumption expenditure by government"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Final_consumption_expenditure_by_government = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Use_Purchaser <- CPA_PRODUCTS_raw$Use_Purchaser |>
    dplyr::left_join(
      USE_PURCHASER |>
        dplyr::filter(
          INDUSTRY_NAME == "Gross fixed capital formation"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Gross_fixed_capital_formation = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Use_Purchaser <- CPA_PRODUCTS_raw$Use_Purchaser |>
    dplyr::left_join(
      USE_PURCHASER |>
        dplyr::filter(
          INDUSTRY_NAME == "Changes in inventories and acquisition less disposals of valuables"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Changes_in_inventories_and_acquisition_less_disposals_of_valuables = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Use_Purchaser <- CPA_PRODUCTS_raw$Use_Purchaser |>
    dplyr::left_join(
      USE_PURCHASER |>
        dplyr::filter(
          INDUSTRY_NAME == "Exports FOB"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Exports_FOB = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Use_Purchaser <- CPA_PRODUCTS_raw$Use_Purchaser |>
    dplyr::mutate(
      Total_final_consumption_expenditure_at_purchasers_prices = rowSums(
        cbind(
          Final_consumption_expenditure_by_households,
          Final_consumption_expenditure_NPISH,
          Final_consumption_expenditure_by_government
        ),
        na.rm = TRUE
      ),
      Gross_capital_formation = rowSums(
        cbind(
          Gross_fixed_capital_formation,
          Changes_in_inventories_and_acquisition_less_disposals_of_valuables
        ),
        na.rm = TRUE
      ),
      Total_final_uses_at_purchasers_prices = rowSums(
        cbind(
          Total_final_consumption_expenditure_at_purchasers_prices,
          Gross_capital_formation,
          Exports_FOB
        ),
        na.rm = TRUE
      ),
      Total_use_at_purchasers_prices = rowSums(
        cbind(
          Total_intermediate_consumption_at_purchasers_prices,
          Total_final_uses_at_purchasers_prices
        ),
        na.rm = TRUE
      )
    )
  
  NACE_INDUSTRIES_raw$Use_Purchaser <- USE_PURCHASER |>
    dplyr::filter(
      PRODUCT_INDUSTRY_CODE != "NA",
      INDUSTRY_CODE != "NA"
    ) |>
    dplyr::group_by(
      INDUSTRY_CODE,
      INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Total_intermediate_consumption_by_industries_at_purchasers_prices = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  
  # 7. Use basic matrix ----
  
  USE_BASIC <- USE_BASIC_raw
  
  CPA_PRODUCTS_raw$Use_Basic <- USE_BASIC |>
    dplyr::filter(
      PRODUCT_INDUSTRY_CODE != "NA",
      INDUSTRY_CODE != "NA"
    ) |>
    dplyr::group_by(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Total_intermediate_consumption_at_basic_prices = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  CPA_PRODUCTS_raw$Use_Basic <- CPA_PRODUCTS_raw$Use_Basic |>
    dplyr::left_join(
      USE_BASIC |>
        dplyr::filter(
          INDUSTRY_NAME == "Final consumption expenditure by households"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Final_consumption_expenditure_by_households = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Use_Basic <- CPA_PRODUCTS_raw$Use_Basic |>
    dplyr::left_join(
      USE_BASIC |>
        dplyr::filter(
          INDUSTRY_NAME == "Final consumption expenditure by non-profit organisations serving households (NPISH)"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Final_consumption_expenditure_NPISH = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Use_Basic <- CPA_PRODUCTS_raw$Use_Basic |>
    dplyr::left_join(
      USE_BASIC |>
        dplyr::filter(
          INDUSTRY_NAME == "Final consumption expenditure by government"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Final_consumption_expenditure_by_government = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Use_Basic <- CPA_PRODUCTS_raw$Use_Basic |>
    dplyr::left_join(
      USE_BASIC |>
        dplyr::filter(
          INDUSTRY_NAME == "Gross fixed capital formation"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Gross_fixed_capital_formation = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Use_Basic <- CPA_PRODUCTS_raw$Use_Basic |>
    dplyr::left_join(
      USE_BASIC |>
        dplyr::filter(
          INDUSTRY_NAME == "Changes in inventories and acquisition less disposals of valuables"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Changes_in_inventories_and_acquisition_less_disposals_of_valuables = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Use_Basic <- CPA_PRODUCTS_raw$Use_Basic |>
    dplyr::left_join(
      USE_BASIC |>
        dplyr::filter(
          INDUSTRY_NAME == "Exports FOB"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Exports_FOB = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Use_Basic$Exports_FOB[
    is.na(
      CPA_PRODUCTS_raw$Use_Basic$Exports_FOB
    )
  ] <- 0
  
  CPA_PRODUCTS_raw$Use_Basic <- CPA_PRODUCTS_raw$Use_Basic |>
    dplyr::mutate(
      Total_final_consumption_expenditure_at_basic_prices = rowSums(
        cbind(
          Final_consumption_expenditure_by_households,
          Final_consumption_expenditure_NPISH,
          Final_consumption_expenditure_by_government
        ),
        na.rm = TRUE
      ),
      Gross_capital_formation = rowSums(
        cbind(
          Gross_fixed_capital_formation,
          Changes_in_inventories_and_acquisition_less_disposals_of_valuables
        ),
        na.rm = TRUE
      ),
      Total_final_uses_at_basic_prices = rowSums(
        cbind(
          Total_final_consumption_expenditure_at_basic_prices,
          Gross_capital_formation,
          Exports_FOB
        ),
        na.rm = TRUE
      ),
      Total_use_at_basic_prices = rowSums(
        cbind(
          Total_intermediate_consumption_at_basic_prices,
          Total_final_uses_at_basic_prices
        ),
        na.rm = TRUE
      )
    )
  
  NACE_INDUSTRIES_raw$Use_Basic <- USE_BASIC |>
    dplyr::filter(
      PRODUCT_INDUSTRY_CODE != "NA",
      INDUSTRY_CODE != "NA"
    ) |>
    dplyr::group_by(
      INDUSTRY_CODE,
      INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Total_intermediate_consumption_by_industries_at_basic_prices = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  
  # 8. Use VAT matrix ----
  
  # USE_VAT is built directly from USE_VAT_raw in Section 3.7.
  
  CPA_PRODUCTS_raw$Use_VAT <- USE_VAT |>
    dplyr::filter(
      PRODUCT_INDUSTRY_CODE != "NA",
      INDUSTRY_CODE != "NA"
    ) |>
    dplyr::group_by(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Total_intermediate_consumption_at_basic_prices = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  CPA_PRODUCTS_raw$Use_VAT <- CPA_PRODUCTS_raw$Use_VAT |>
    dplyr::left_join(
      USE_VAT |>
        dplyr::filter(
          INDUSTRY_NAME == "Final consumption expenditure by households"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Final_consumption_expenditure_by_households = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Use_VAT <- CPA_PRODUCTS_raw$Use_VAT |>
    dplyr::left_join(
      USE_VAT |>
        dplyr::filter(
          INDUSTRY_NAME == "Final consumption expenditure by non-profit organisations serving households (NPISH)"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Final_consumption_expenditure_NPISH = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Use_VAT <- CPA_PRODUCTS_raw$Use_VAT |>
    dplyr::left_join(
      USE_VAT |>
        dplyr::filter(
          INDUSTRY_NAME == "Final consumption expenditure by government"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Final_consumption_expenditure_by_government = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Use_VAT <- CPA_PRODUCTS_raw$Use_VAT |>
    dplyr::left_join(
      USE_VAT |>
        dplyr::filter(
          INDUSTRY_NAME == "Gross fixed capital formation"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Gross_fixed_capital_formation = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Use_VAT <- CPA_PRODUCTS_raw$Use_VAT |>
    dplyr::left_join(
      USE_VAT |>
        dplyr::filter(
          INDUSTRY_NAME == "Changes in inventories and acquisition less disposals of valuables"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Changes_in_inventories_and_acquisition_less_disposals_of_valuables = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Use_VAT <- CPA_PRODUCTS_raw$Use_VAT |>
    dplyr::left_join(
      USE_VAT |>
        dplyr::filter(
          INDUSTRY_NAME == "Exports FOB"
        ) |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          value
        ) |>
        dplyr::rename(
          Exports_FOB = value
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    )
  
  CPA_PRODUCTS_raw$Use_VAT <- CPA_PRODUCTS_raw$Use_VAT |>
    dplyr::mutate(
      Total_final_consumption_expenditure_at_basic_prices = rowSums(
        cbind(
          Final_consumption_expenditure_by_households,
          Final_consumption_expenditure_NPISH,
          Final_consumption_expenditure_by_government
        ),
        na.rm = TRUE
      ),
      Gross_capital_formation = rowSums(
        cbind(
          Gross_fixed_capital_formation,
          Changes_in_inventories_and_acquisition_less_disposals_of_valuables
        ),
        na.rm = TRUE
      ),
      Total_final_uses_at_basic_prices = rowSums(
        cbind(
          Total_final_consumption_expenditure_at_basic_prices,
          Gross_capital_formation,
          Exports_FOB
        ),
        na.rm = TRUE
      ),
      Total_use_at_basic_prices = rowSums(
        cbind(
          Total_intermediate_consumption_at_basic_prices,
          Total_final_uses_at_basic_prices
        ),
        na.rm = TRUE
      )
    )
  
  NACE_INDUSTRIES_raw$Use_VAT <- USE_VAT |>
    dplyr::filter(
      PRODUCT_INDUSTRY_CODE != "NA",
      INDUSTRY_CODE != "NA"
    ) |>
    dplyr::group_by(
      INDUSTRY_CODE,
      INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Total_VAT = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  
  # 9. Original aggregate objects ----
  
  CPA_PRODUCTS <- CPA_PRODUCTS_raw
  NACE_INDUSTRIES <- NACE_INDUSTRIES_raw
  
  
  # 10. Arrange objects ----
  
  NACE_INDUSTRIES$Supply <- NACE_INDUSTRIES$Supply |>
    dplyr::arrange(INDUSTRY_CODE)
  
  NACE_INDUSTRIES$Use_Purchaser <- NACE_INDUSTRIES$Use_Purchaser |>
    dplyr::arrange(INDUSTRY_CODE)
  
  NACE_INDUSTRIES$Use_VAT <- NACE_INDUSTRIES$Use_VAT |>
    dplyr::arrange(INDUSTRY_CODE)
  
  NACE_INDUSTRIES$Use_Basic <- NACE_INDUSTRIES$Use_Basic |>
    dplyr::arrange(INDUSTRY_CODE)
  
  CPA_PRODUCTS$Supply <- CPA_PRODUCTS$Supply |>
    dplyr::arrange(PRODUCT_INDUSTRY_CODE)
  
  CPA_PRODUCTS$Use_Purchaser <- CPA_PRODUCTS$Use_Purchaser |>
    dplyr::arrange(PRODUCT_INDUSTRY_CODE)
  
  CPA_PRODUCTS$Use_VAT <- CPA_PRODUCTS$Use_VAT |>
    dplyr::arrange(PRODUCT_INDUSTRY_CODE)
  
  CPA_PRODUCTS$Use_Basic <- CPA_PRODUCTS$Use_Basic |>
    dplyr::arrange(PRODUCT_INDUSTRY_CODE)
  
  
  # 11. Domestic Share matrix ----
  
  CPA_PRODUCTS$Dom_Share <- CPA_PRODUCTS$Supply |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE
    )
  
  CPA_PRODUCTS$Dom_Share$Domestic_share_of_output_commodity <-
    pmax(
      1 -
        CPA_PRODUCTS$Use_Basic$Exports_FOB /
        CPA_PRODUCTS$Supply$Total_output,
      0
    )
  
  CPA_PRODUCTS$Dom_Share$Domestic_share_of_output_commodity[
    !is.finite(
      CPA_PRODUCTS$Dom_Share$Domestic_share_of_output_commodity
    )
  ] <- 0
  
  DOM_SHARE <- SUPPLY |>
    dplyr::filter(
      PRODUCT_INDUSTRY_CODE != "NA",
      INDUSTRY_CODE != "NA"
    ) |>
    dplyr::left_join(
      NACE_INDUSTRIES$Supply,
      by = c(
        "INDUSTRY_CODE",
        "INDUSTRY_NAME"
      )
    ) |>
    dplyr::mutate(
      value =
        value /
        Total_output_by_industries_at_basic_prices
    ) |>
    dplyr::select(
      -Total_output_by_industries_at_basic_prices
    ) |>
    dplyr::left_join(
      CPA_PRODUCTS$Dom_Share,
      by = c(
        "PRODUCT_INDUSTRY_CODE",
        "PRODUCT_INDUSTRY_NAME"
      )
    ) |>
    dplyr::mutate(
      value =
        value *
        Domestic_share_of_output_commodity
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      INDUSTRY_CODE
    )
  
  DOM_SHARE$value[
    !is.finite(
      DOM_SHARE$value
    )
  ] <- 0
  
  NACE_INDUSTRIES$Dom_Share <- DOM_SHARE |>
    dplyr::group_by(
      INDUSTRY_CODE,
      INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Domestic_Share_of_Output_Industry = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  
  # 12. Net purchaser prices ----
  
  USE_NETPURCH <- USE_PURCHASER |>
    dplyr::filter(
      PRODUCT_INDUSTRY_CODE != "NA",
      INDUSTRY_CODE != "NA"
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      INDUSTRY_CODE
    )
  
  USE_VAT_temp <- USE_VAT |>
    dplyr::filter(
      PRODUCT_INDUSTRY_CODE != "NA",
      INDUSTRY_CODE != "NA"
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      INDUSTRY_CODE
    )
  
  USE_NETPURCH$value[
    is.na(
      USE_NETPURCH$value
    )
  ] <- 0
  
  USE_VAT_temp$value[
    is.na(
      USE_VAT_temp$value
    )
  ] <- 0
  
  USE_NETPURCH$value <-
    USE_NETPURCH$value -
    USE_VAT_temp$value
  
  CPA_PRODUCTS$Use_NetPurch <- USE_NETPURCH |>
    dplyr::group_by(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Total_intermediate_consumption_net_purchasers_prices = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE
    )
  
  CPA_PRODUCTS$Use_Purchaser$Gross_fixed_capital_formation[
    is.na(
      CPA_PRODUCTS$Use_Purchaser$Gross_fixed_capital_formation
    )
  ] <- 0
  
  CPA_PRODUCTS$Use_VAT$Gross_fixed_capital_formation[
    is.na(
      CPA_PRODUCTS$Use_VAT$Gross_fixed_capital_formation
    )
  ] <- 0
  
  CPA_PRODUCTS$Use_NetPurch$Gross_fixed_capital_formation <-
    CPA_PRODUCTS$Use_Purchaser$Gross_fixed_capital_formation -
    CPA_PRODUCTS$Use_VAT$Gross_fixed_capital_formation
  
  CPA_PRODUCTS$Use_NetPurch$Gross_fixed_capital_formation[
    is.na(
      CPA_PRODUCTS$Use_NetPurch$Gross_fixed_capital_formation
    )
  ] <- 0
  
  CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_households[
    is.na(
      CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_households
    )
  ] <- 0
  
  CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_by_households[
    is.na(
      CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_by_households
    )
  ] <- 0
  
  CPA_PRODUCTS$Use_NetPurch$Final_consumption_expenditure_by_households <-
    CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_households -
    CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_by_households
  
  CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_NPISH[
    is.na(
      CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_NPISH
    )
  ] <- 0
  
  CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_NPISH[
    is.na(
      CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_NPISH
    )
  ] <- 0
  
  CPA_PRODUCTS$Use_NetPurch$Final_consumption_expenditure_NPISH <-
    CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_NPISH -
    CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_NPISH
  
  CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_government[
    is.na(
      CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_government
    )
  ] <- 0
  
  CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_by_government[
    is.na(
      CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_by_government
    )
  ] <- 0
  
  CPA_PRODUCTS$Use_NetPurch$Final_consumption_expenditure_by_government <-
    CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_government -
    CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_by_government
  
  
  # 13. Capital Shift Map ----
  
  CPA_PRODUCTS$K_Shift_Map <- USE_NETPURCH |>
    dplyr::distinct(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME
    ) |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME
    ) |>
    dplyr::mutate(
      Share_of_K_in_HH_FD = 0
    )
  
  # CPA_PRODUCTS$K_Shift_Map$Share_of_K_in_HH_FD[
  #   grepl(
  #     "Constructions and construction works",
  #     CPA_PRODUCTS$K_Shift_Map$PRODUCT_INDUSTRY_NAME
  #   )
  # ] <- RC_prc_of_Constructions_and_construction_works
  
  CPA_PRODUCTS$K_Shift_Map$Share_of_K_in_HH_FD[
    CPA_PRODUCTS$K_Shift_Map$PRODUCT_INDUSTRY_CODE == "F"
  ] <- RC_prc_of_Constructions_and_construction_works
  
  
  K_SHIFT_MAP <- USE_NETPURCH |>
    dplyr::left_join(
      CPA_PRODUCTS$Use_NetPurch,
      by = c(
        "PRODUCT_INDUSTRY_CODE",
        "PRODUCT_INDUSTRY_NAME"
      )
    ) |>
    dplyr::left_join(
      CPA_PRODUCTS$K_Shift_Map,
      by = c(
        "PRODUCT_INDUSTRY_CODE",
        "PRODUCT_INDUSTRY_NAME"
      )
    ) |>
    dplyr::mutate(
      value =
        value /
        Total_intermediate_consumption_net_purchasers_prices *
        (
          1 -
            Share_of_K_in_HH_FD
        )
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      INDUSTRY_CODE
    ) |>
    dplyr::select(
      -Share_of_K_in_HH_FD
    )
  
  K_SHIFT_MAP$value[
    !is.finite(
      K_SHIFT_MAP$value
    )
  ] <- 0
  
  CPA_PRODUCTS$K_Shift_Map$Final_consumption_expenditure_by_households <-
    CPA_PRODUCTS$K_Shift_Map$Share_of_K_in_HH_FD
  
  CPA_PRODUCTS$K_Shift_Map$Final_consumption_expenditure_NPISH <- 0
  
  CPA_PRODUCTS$K_Shift_Map$Final_consumption_expenditure_by_government <- 0
  
  
  # 14. Use Capital net purchaser prices ----
  
  USE_K_NETPURCH <- K_SHIFT_MAP |>
    dplyr::mutate(
      value =
        value *
        Gross_fixed_capital_formation
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      INDUSTRY_CODE
    )
  
  USE_NETPURCH <- USE_NETPURCH |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      INDUSTRY_CODE
    )
  
  USE_K_NETPURCH$value <-
    USE_K_NETPURCH$value +
    USE_NETPURCH$value
  
  CPA_PRODUCTS$Use_NetPurch <- CPA_PRODUCTS$Use_NetPurch |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME
    )
  
  CPA_PRODUCTS$K_Shift_Map <- CPA_PRODUCTS$K_Shift_Map |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME
    )
  
  CPA_PRODUCTS$Use_K_NetPurch <- CPA_PRODUCTS$Use_NetPurch |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME
    )
  
  CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_by_households <-
    CPA_PRODUCTS$Use_NetPurch$Final_consumption_expenditure_by_households +
    CPA_PRODUCTS$K_Shift_Map$Final_consumption_expenditure_by_households *
    CPA_PRODUCTS$Use_NetPurch$Gross_fixed_capital_formation
  
  CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_NPISH <-
    CPA_PRODUCTS$Use_NetPurch$Final_consumption_expenditure_NPISH +
    CPA_PRODUCTS$K_Shift_Map$Final_consumption_expenditure_NPISH *
    CPA_PRODUCTS$Use_NetPurch$Gross_fixed_capital_formation
  
  CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_NPISH[
    is.na(
      CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_NPISH
    )
  ] <- 0
  
  CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_by_government <-
    CPA_PRODUCTS$Use_NetPurch$Final_consumption_expenditure_by_government +
    CPA_PRODUCTS$K_Shift_Map$Final_consumption_expenditure_by_government *
    CPA_PRODUCTS$Use_NetPurch$Gross_fixed_capital_formation
  
  CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_by_government[
    is.na(
      CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_by_government
    )
  ] <- 0
  
  
  # 15. Use_K_Dom_NetPurch ----
  
  USE_K_DOM_NETPURCH <- USE_K_NETPURCH |>
    dplyr::select(
      PRODUCT_INDUSTRY_NAME,
      PRODUCT_INDUSTRY_CODE,
      INDUSTRY_NAME,
      INDUSTRY_CODE,
      value
    ) |>
    dplyr::left_join(
      NACE_INDUSTRIES$Dom_Share,
      by = c(
        "INDUSTRY_CODE",
        "INDUSTRY_NAME"
      )
    ) |>
    dplyr::mutate(
      value =
        value *
        Domestic_Share_of_Output_Industry
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      INDUSTRY_CODE
    )
  
  
  # 16. Supply_Dom ----
  
  SUPPLY_DOM <- SUPPLY |>
    dplyr::filter(
      PRODUCT_INDUSTRY_CODE != "NA",
      INDUSTRY_CODE != "NA"
    ) |>
    dplyr::left_join(
      CPA_PRODUCTS$Dom_Share,
      by = c(
        "PRODUCT_INDUSTRY_CODE",
        "PRODUCT_INDUSTRY_NAME"
      )
    ) |>
    dplyr::mutate(
      value =
        value *
        Domestic_share_of_output_commodity
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      INDUSTRY_CODE
    )
  
  NACE_INDUSTRIES$Supply_Dom <- SUPPLY_DOM |>
    dplyr::group_by(
      INDUSTRY_CODE,
      INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Total_output_by_industries_at_basic_prices = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  
  # 17. Completion ----
  
  print(
    "Script Data Transformation Done !"
  )
  
})