'PIT FUNCTIONS'


get_param_fun <- function(params_dt, param_name) {
  params_dt[Parameters == param_name, Value]
}


# Function to sum the specified columns in the list and store the results in a data frame
summarize_PIT_fun <- function(PIT_list, suffix) {
  result <- data.frame(
    scenarios = character(),
    stringsAsFactors = FALSE
  )
  
  for (scenario in names(PIT_list)) {
    df <- PIT_list[[scenario]]
    calc_columns <- names(df)[grepl("^(calc|pit)", names(df))]
    calc_sums <- sapply(df[, ..calc_columns], function(col) sum(col, na.rm = TRUE))
    scenario_result <- data.frame(
      scenarios = scenario,
      as.list(calc_sums),
      stringsAsFactors = FALSE
    )
    result <- rbind(result, scenario_result)
  }
  names(result)[-1] <- paste0(names(result)[-1], suffix)
  
  return(result)
}



# 
# make_breaks_unique_fun <- function(breaks) {
#   if (length(unique(breaks)) != length(breaks)) {
#     breaks <- breaks + cumsum(c(0, diff(breaks) == 0)) * .Machine$double.eps * 1000
#   }
#   return(breaks)
# }


# # # Define the function for weighted deciles
# cal_weighted_deciles_fun <- function(total_inc, weight) {
#   deciles <- wtd.quantile(total_inc, weights = weight, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
#   deciles <- make_breaks_unique_fun(deciles)  # Ensure breaks are unique
#   decile_group <- cut(total_inc, breaks = deciles, include.lowest = TRUE, labels = FALSE)
#   return(decile_group)
# }
# 
# 
# # # Define the function for weighted centiles
# cal_weighted_centiles_fun <- function(total_inc, weight, centiles = seq(0, 1, by = 0.01)) {
#   boundaries <- wtd.quantile(total_inc, weights = weight, probs = centiles, na.rm = TRUE)
#   boundaries <- make_breaks_unique_fun(boundaries)  # Ensure breaks are unique
#   centile_group <- cut(total_inc, breaks = boundaries, include.lowest = TRUE, labels = FALSE)
#   return(centile_group)
# }
