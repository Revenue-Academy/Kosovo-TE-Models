# I. Packages ---------------------------------------------------------------

req_pkgs <- c(
  "pdftools",
  "dplyr",
  "stringr",
  "purrr",
  "tibble",
  "openxlsx"
)

to_install <- req_pkgs[!req_pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

library(pdftools)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(openxlsx)

# II. User inputs -----------------------------------------------------------

pdf_file  <- "TARIC_2024.pdf"
xlsx_file <- "Kosovo_TARIK_2024_extracted_pages_34_923.xlsx"

page_start <- 34
page_end   <- 923
pages_use  <- page_start:page_end

# III. Helpers --------------------------------------------------------------

clean_txt <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x |>
    str_replace_all("[\r\t]+", " ") |>
    str_replace_all("￾", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

is_header_line <- function(x) {
  z <- tolower(clean_txt(x))
  str_detect(z, "goods code") ||
    str_detect(z, "^description$") ||
    str_detect(z, "supple") ||
    str_detect(z, "trade policy") ||
    str_detect(z, "rates of duty") ||
    str_detect(z, "preferential rates") ||
    str_detect(z, "^vat") ||
    str_detect(z, "rates of excises") ||
    str_detect(z, "cefta / msa") ||
    str_detect(z, "^import$") ||
    str_detect(z, "^export$")
}

is_page_number_line <- function(x) {
  str_detect(clean_txt(x), "^\\d+$")
}

starts_with_any_code <- function(x) {
  str_detect(clean_txt(x), "^\\d{4}(?:\\s?\\d{2}){0,3}\\b")
}

starts_with_full_code <- function(x) {
  str_detect(clean_txt(x), "^\\d{4}(?:\\s?\\d{2}){3}\\b")
}

format_code <- function(x) {
  z <- clean_txt(x) |> str_replace_all("\\s+", "")
  dplyr::case_when(
    str_detect(z, "^\\d{10}$") ~ str_replace(z, "^(\\d{4})(\\d{2})(\\d{2})(\\d{2})$", "\\1 \\2 \\3 \\4"),
    str_detect(z, "^\\d{8}$")  ~ str_replace(z, "^(\\d{4})(\\d{2})(\\d{2})$", "\\1 \\2 \\3"),
    str_detect(z, "^\\d{6}$")  ~ str_replace(z, "^(\\d{4})(\\d{2})$", "\\1 \\2"),
    str_detect(z, "^\\d{4}$")  ~ z,
    TRUE ~ clean_txt(x)
  )
}

is_true_dash_heading <- function(x) {
  z <- clean_txt(x)
  
  is_dash <- str_detect(z, "^[-–]{1,4}\\s*[^-– ].+")
  looks_measure <- str_detect(z, "^[-–]{1,4}\\s*(PRO\\(|L\\d|FIT\\b|VET\\b|CEN\\b|PCE\\b)")
  looks_tail <- str_detect(z, "^[-–]{1,4}\\s*\\d+(?:[.,]\\d+)?(?:\\(\\d+\\))?\\s+CEFTA\\s*:")
  looks_only_dash <- str_detect(z, "^[-–]{1,4}$")
  
  is_dash && !looks_measure && !looks_tail && !looks_only_dash
}

extract_last_token <- function(x) {
  x <- clean_txt(x)
  if (x == "" || !str_detect(x, "\\S")) {
    return(list(left = "", token = ""))
  }
  
  m <- str_match(x, "^(.*?)([^ ]+)$")
  list(
    left = clean_txt(m[, 2]),
    token = clean_txt(m[, 3])
  )
}

is_duty_or_vat_token <- function(x) {
  x <- clean_txt(x)
  
  str_detect(x, "^-$") ||
    str_detect(x, "^\\d+(?:[.,]\\d+)?$") ||
    str_detect(x, "^\\d+(?:[.,]\\d+)?\\(\\d+\\)$")
}

is_excise_token <- function(x) {
  x <- clean_txt(x)
  
  str_detect(x, "^-$") ||
    str_detect(x, "^\\d+(?:[.,]\\d+)?$") ||
    str_detect(x, "^\\d+(?:[.,]\\d+)?\\(\\d+\\)$") ||
    str_detect(x, "^\\d+(?:[.,]\\d+)?€/[A-Za-z]+$") ||
    str_detect(x, "^\\d+(?:[.,]\\d+)?€/[A-Za-z]+\\(\\d+\\)$")
}

looks_like_measure_token <- function(x) {
  x <- clean_txt(x)
  
  str_detect(x, "^-$") ||
    str_detect(x, "^[A-Z]{2,}(?:\\(\\d+\\))?$") ||
    str_detect(x, "^[A-Z]\\d{2}(?:\\(\\d+\\))?$") ||
    str_detect(x, "^[A-Z]{2,}\\(\\d+\\)$")
}

get_content_level <- function(description) {
  d <- clean_txt(description)
  
  out <- ifelse(
    d == "", 0L,
    ifelse(
      str_detect(d, "^----"), 4L,
      ifelse(
        str_detect(d, "^---"), 3L,
        ifelse(
          str_detect(d, "^--"), 2L,
          ifelse(str_detect(d, "^-"), 1L, 0L)
        )
      )
    )
  )
  
  as.integer(out)
}

strip_leading_hyphens <- function(description) {
  clean_txt(description) |>
    str_replace("^[-–]+\\s*", "")
}

# IV. Rebuild logical rows --------------------------------------------------

rebuild_logical_rows <- function(page_lines) {
  page_lines <- clean_txt(page_lines)
  page_lines <- page_lines[page_lines != ""]
  page_lines <- page_lines[!vapply(page_lines, is_header_line, logical(1))]
  page_lines <- page_lines[!vapply(page_lines, is_page_number_line, logical(1))]
  
  out <- list()
  k <- 0L
  current <- ""
  
  flush_current <- function(val, out, k) {
    if (clean_txt(val) != "") {
      k <- k + 1L
      out[[k]] <- clean_txt(val)
    }
    list(out = out, k = k)
  }
  
  for (ln in page_lines) {
    ln <- clean_txt(ln)
    if (ln == "") next
    
    if (starts_with_any_code(ln)) {
      tmp <- flush_current(current, out, k)
      out <- tmp$out
      k <- tmp$k
      current <- ln
      next
    }
    
    if (is_true_dash_heading(ln)) {
      tmp <- flush_current(current, out, k)
      out <- tmp$out
      k <- tmp$k
      current <- ln
      next
    }
    
    if (current == "") {
      current <- ln
    } else {
      current <- paste(current, ln)
    }
  }
  
  tmp <- flush_current(current, out, k)
  out <- tmp$out
  
  clean_txt(unlist(out))
}

# V. Parsing ---------------------------------------------------------------

extract_pref_block <- function(x) {
  m <- str_match(
    x,
    "(CEFTA\\s*:\\s*.*?/\\s*MSA(?:/GB)?\\s*:\\s*.*?/\\s*TR\\s*:\\s*[^ ]+)"
  )
  clean_txt(m[, 2])
}

split_around_pref <- function(x) {
  x <- clean_txt(x)
  pref <- extract_pref_block(x)
  
  if (is.na(pref) || pref == "") return(NULL)
  
  loc <- str_locate(x, fixed(pref))
  if (is.na(loc[1, 1])) return(NULL)
  
  list(
    left  = clean_txt(substr(x, 1, loc[1, 1] - 1)),
    pref  = clean_txt(pref),
    right = clean_txt(substr(x, loc[1, 2] + 1, nchar(x)))
  )
}

parse_measure_block <- function(left_text) {
  txt <- clean_txt(left_text)
  
  a1 <- extract_last_token(txt)
  duty_rate <- a1$token
  left1 <- a1$left
  
  if (!is_duty_or_vat_token(duty_rate)) {
    return(NULL)
  }
  
  a2 <- extract_last_token(left1)
  export_measure <- a2$token
  left2 <- a2$left
  
  a3 <- extract_last_token(left2)
  import_measure <- a3$token
  left3 <- a3$left
  
  a4 <- extract_last_token(left3)
  maybe_supp <- a4$token
  maybe_desc <- a4$left
  
  if (looks_like_measure_token(maybe_supp)) {
    supplementary_unit <- maybe_supp
    description <- maybe_desc
  } else {
    supplementary_unit <- ""
    description <- left3
  }
  
  tibble(
    Description = clean_txt(description),
    Supplementary_unit = clean_txt(supplementary_unit),
    Import_measure = clean_txt(import_measure),
    Export_measure = clean_txt(export_measure),
    Duty_rate = clean_txt(duty_rate)
  )
}

parse_vat_excise_block <- function(right_text) {
  txt <- clean_txt(right_text)
  
  if (txt == "") {
    return(tibble(VAT_rate = "", Excise_rate = ""))
  }
  
  parts <- unlist(str_split(txt, "\\s+"))
  parts <- parts[parts != ""]
  
  if (length(parts) == 0) {
    return(tibble(VAT_rate = "", Excise_rate = ""))
  }
  
  vat_rate <- ""
  excise_rate <- ""
  
  # Case 1: first token is VAT, second token is excise
  if (length(parts) >= 1 && is_duty_or_vat_token(parts[1])) {
    vat_rate <- parts[1]
    
    if (length(parts) >= 2 && is_excise_token(parts[2])) {
      excise_rate <- parts[2]
    } else if (length(parts) >= 2 && parts[2] == "-") {
      excise_rate <- "-"
    } else {
      excise_rate <- ""
    }
  }
  
  tibble(
    VAT_rate = clean_txt(vat_rate),
    Excise_rate = clean_txt(excise_rate)
  )
}

parse_tariff_row <- function(x) {
  x <- clean_txt(x)
  
  m_code <- str_match(x, "^(\\d{4}(?:\\s?\\d{2}){3})\\s+(.*)$")
  if (all(is.na(m_code))) return(NULL)
  
  goods_code <- format_code(m_code[, 2])
  rest <- clean_txt(m_code[, 3])
  
  pref_split <- split_around_pref(rest)
  if (is.null(pref_split)) return(NULL)
  
  left_part  <- pref_split$left
  pref_part  <- pref_split$pref
  right_part <- pref_split$right
  
  left_parsed <- parse_measure_block(left_part)
  if (is.null(left_parsed)) return(NULL)
  
  right_parsed <- parse_vat_excise_block(right_part)
  
  tibble(
    Row_Type = "Tariff_line",
    Goods_code = goods_code,
    Description = clean_txt(left_parsed$Description),
    Supplementary_unit = clean_txt(left_parsed$Supplementary_unit),
    Import_measure = clean_txt(left_parsed$Import_measure),
    Export_measure = clean_txt(left_parsed$Export_measure),
    Duty_rate = clean_txt(left_parsed$Duty_rate),
    Preferential_rate = clean_txt(pref_part),
    VAT_rate = clean_txt(right_parsed$VAT_rate),
    Excise_rate = clean_txt(right_parsed$Excise_rate)
  )
}

parse_tariff_row_fallback <- function(x) {
  x <- clean_txt(x)
  
  m_code <- str_match(x, "^(\\d{4}(?:\\s?\\d{2}){3})\\s+(.*)$")
  if (all(is.na(m_code))) return(NULL)
  
  goods_code <- format_code(m_code[, 2])
  rest <- clean_txt(m_code[, 3])
  
  pref_split <- split_around_pref(rest)
  
  duty_rate <- ""
  vat_rate <- ""
  excise_rate <- ""
  pref_rate <- ""
  
  if (!is.null(pref_split)) {
    pref_rate <- pref_split$pref
    
    left_part <- pref_split$left
    right_part <- pref_split$right
    
    a1 <- extract_last_token(left_part)
    if (is_duty_or_vat_token(a1$token)) {
      duty_rate <- a1$token
    }
    
    right_parsed <- parse_vat_excise_block(right_part)
    vat_rate <- right_parsed$VAT_rate
    excise_rate <- right_parsed$Excise_rate
  }
  
  tibble(
    Row_Type = "Tariff_line",
    Goods_code = goods_code,
    Description = rest,
    Supplementary_unit = "",
    Import_measure = "",
    Export_measure = "",
    Duty_rate = clean_txt(duty_rate),
    Preferential_rate = clean_txt(pref_rate),
    VAT_rate = clean_txt(vat_rate),
    Excise_rate = clean_txt(excise_rate)
  )
}

parse_structure_row <- function(x) {
  x <- clean_txt(x)
  
  m1 <- str_match(x, "^(\\d{4}(?:\\s?\\d{2}){0,2})\\s+(.*)$")
  if (!all(is.na(m1))) {
    code_raw <- clean_txt(m1[, 2]) |> str_replace_all("\\s+", "")
    if (nchar(code_raw) < 10) {
      return(tibble(
        Row_Type = "Structure_row",
        Goods_code = format_code(m1[, 2]),
        Description = clean_txt(m1[, 3]),
        Supplementary_unit = "",
        Import_measure = "",
        Export_measure = "",
        Duty_rate = "",
        Preferential_rate = "",
        VAT_rate = "",
        Excise_rate = ""
      ))
    }
  }
  
  if (is_true_dash_heading(x)) {
    return(tibble(
      Row_Type = "Structure_row",
      Goods_code = "",
      Description = x,
      Supplementary_unit = "",
      Import_measure = "",
      Export_measure = "",
      Duty_rate = "",
      Preferential_rate = "",
      VAT_rate = "",
      Excise_rate = ""
    ))
  }
  
  NULL
}

parse_logical_row <- function(x) {
  x <- clean_txt(x)
  
  if (starts_with_full_code(x)) {
    res <- parse_tariff_row(x)
    if (!is.null(res)) return(res)
    
    res_fb <- parse_tariff_row_fallback(x)
    if (!is.null(res_fb)) return(res_fb)
  }
  
  res2 <- parse_structure_row(x)
  if (!is.null(res2)) return(res2)
  
  tibble(
    Row_Type = "Other_text",
    Goods_code = "",
    Description = x,
    Supplementary_unit = "",
    Import_measure = "",
    Export_measure = "",
    Duty_rate = "",
    Preferential_rate = "",
    VAT_rate = "",
    Excise_rate = ""
  )
}

# VI. Hierarchy -------------------------------------------------------------

build_content_path <- function(df) {
  lvl0 <- character(nrow(df))
  lvl1 <- character(nrow(df))
  lvl2 <- character(nrow(df))
  lvl3 <- character(nrow(df))
  
  cur0 <- ""
  cur1 <- ""
  cur2 <- ""
  cur3 <- ""
  
  for (i in seq_len(nrow(df))) {
    lev <- df$content_level[i]
    txt <- df$Description_clean[i]
    
    if (df$Row_Type[i] == "Structure_row") {
      if (lev <= 0) {
        cur0 <- txt
        cur1 <- ""
        cur2 <- ""
        cur3 <- ""
      } else if (lev == 1) {
        cur1 <- txt
        cur2 <- ""
        cur3 <- ""
      } else if (lev == 2) {
        cur2 <- txt
        cur3 <- ""
      } else {
        cur3 <- txt
      }
    }
    
    lvl0[i] <- cur0
    lvl1[i] <- cur1
    lvl2[i] <- cur2
    lvl3[i] <- cur3
  }
  
  df |>
    mutate(
      content_level_0 = lvl0,
      content_level_1 = lvl1,
      content_level_2 = lvl2,
      content_level_3 = lvl3,
      content_path = pmap_chr(
        list(content_level_0, content_level_1, content_level_2, content_level_3),
        function(a, b, c, d) {
          vals <- c(a, b, c, d)
          vals <- vals[vals != ""]
          paste(vals, collapse = " > ")
        }
      )
    )
}

# VII. Read and parse -------------------------------------------------------

pdf_txt <- pdftools::pdf_text(pdf_file)

parsed_pages <- list()

for (pg in pages_use) {
  message("Processing page: ", pg)
  
  raw_lines <- str_split(pdf_txt[pg], "\n", simplify = FALSE)[[1]]
  logical_rows <- rebuild_logical_rows(raw_lines)
  
  page_df <- map_dfr(seq_along(logical_rows), function(i) {
    parse_logical_row(logical_rows[i]) |>
      mutate(
        PDF_Page = pg,
        Row_in_Page = i,
        .before = 1
      )
  })
  
  parsed_pages[[length(parsed_pages) + 1L]] <- page_df
}

all_rows <- bind_rows(parsed_pages)

# VIII. Clean ---------------------------------------------------------------

all_rows <- all_rows |>
  mutate(
    Goods_code = clean_txt(Goods_code),
    Description = clean_txt(Description),
    Description_clean = strip_leading_hyphens(Description),
    Supplementary_unit = clean_txt(Supplementary_unit),
    Import_measure = clean_txt(Import_measure),
    Export_measure = clean_txt(Export_measure),
    Duty_rate = clean_txt(Duty_rate),
    Preferential_rate = clean_txt(Preferential_rate),
    VAT_rate = clean_txt(VAT_rate),
    Excise_rate = clean_txt(Excise_rate),
    content_level = get_content_level(Description)
  ) |>
  arrange(PDF_Page, Row_in_Page)

all_rows <- build_content_path(all_rows)

tariff_lines <- all_rows |>
  filter(Row_Type == "Tariff_line") |>
  mutate(
    code_nospace = str_replace_all(Goods_code, "\\s+", ""),
    chapter = substr(code_nospace, 1, 2),
    heading = substr(code_nospace, 1, 4),
    subheading = substr(code_nospace, 1, 6),
    item = code_nospace
  ) |>
  select(
    PDF_Page,
    Row_in_Page,
    Goods_code,
    chapter,
    heading,
    subheading,
    Description = Description_clean,
    Supplementary_unit,
    Import_measure,
    Export_measure,
    Duty_rate,
    Preferential_rate,
    VAT_rate,
    Excise_rate,
    item,
    content_level,
    content_level_0,
    content_level_1,
    content_level_2,
    content_level_3,
    content_path
  )

structure_rows <- all_rows |>
  filter(Row_Type == "Structure_row") |>
  select(
    PDF_Page,
    Row_in_Page,
    Goods_code,
    Description,
    Description_clean,
    content_level,
    content_level_0,
    content_level_1,
    content_level_2,
    content_level_3,
    content_path
  )

other_text_rows <- all_rows |>
  filter(Row_Type == "Other_text") |>
  transmute(
    PDF_Page,
    Row_in_Page,
    Combined_text = Description
  )

# IX. Export ----------------------------------------------------------------

wb <- createWorkbook()

addWorksheet(wb, "Tariff_lines")
writeData(wb, "Tariff_lines", tariff_lines)

addWorksheet(wb, "Structure_rows")
writeData(wb, "Structure_rows", structure_rows)

addWorksheet(wb, "Other_text_rows")
writeData(wb, "Other_text_rows", other_text_rows)

freezePane(wb, "Tariff_lines", firstRow = TRUE)
freezePane(wb, "Structure_rows", firstRow = TRUE)
freezePane(wb, "Other_text_rows", firstRow = TRUE)

setColWidths(wb, "Tariff_lines", cols = 1:ncol(tariff_lines), widths = "auto")
setColWidths(wb, "Structure_rows", cols = 1:ncol(structure_rows), widths = "auto")
setColWidths(wb, "Other_text_rows", cols = 1:ncol(other_text_rows), widths = "auto")

saveWorkbook(wb, xlsx_file, overwrite = TRUE)

# X. Summary ----------------------------------------------------------------

cat("\nDone.\n")
cat("Pages processed: ", page_start, " to ", page_end, "\n", sep = "")
cat("Tariff lines: ", nrow(tariff_lines), "\n", sep = "")
cat("Structure rows: ", nrow(structure_rows), "\n", sep = "")
cat("Other text rows: ", nrow(other_text_rows), "\n", sep = "")