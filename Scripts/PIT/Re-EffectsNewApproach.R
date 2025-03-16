
# Function to calculate Gini coefficients for pre-tax and after-tax income
weighted_gini <- function(total_net, g_total_gross, w) {
  order_index <- order(g_total_gross)
  total_net <- total_net[order_index]
  g_total_gross <- g_total_gross[order_index]
  w <- w[order_index]
  
  cum_w <- cumsum(w) / sum(w)
  cum_gross <- cumsum(g_total_gross * w) / sum(g_total_gross * w)
  cum_net <- cumsum(total_net * w) / sum(total_net * w)
  
  area_lx <- sum(cum_gross[-1] * diff(cum_w))
  area_lx_t <- sum(cum_net[-1] * diff(cum_w))
  
  gini_gx <- 1 - 2 * area_lx
  gini_gx_t <- 1 - 2 * area_lx_t
  
  return(list(gini_gx = gini_gx, gini_gx_t = gini_gx_t))
}

# Function to calculate Concentration Coefficients
weighted_concentration <- function(total_net, g_total_gross, w) {
  order_index <- order(g_total_gross)
  total_net <- total_net[order_index]
  g_total_gross <- g_total_gross[order_index]
  w <- w[order_index]
  
  tax <- g_total_gross - total_net
  cum_w <- cumsum(w) / sum(w)
  cum_gross <- cumsum(g_total_gross * w) / sum(g_total_gross * w)
  cum_net <- cumsum(total_net * w) / sum(total_net * w)
  cum_tax <- cumsum(tax * w) / sum(tax * w)
  
  area_lx_t <- sum(cum_net[-1] * diff(cum_w))
  area_lt <- sum(cum_tax[-1] * diff(cum_w))
  
  concentration_cx_t <- 1 - 2 * area_lx_t
  concentration_ct <- 1 - 2 * area_lt
  
  return(list(concentration_cx_t = concentration_cx_t, concentration_ct = concentration_ct))
}

# Function to calculate the Atkinson Index
weighted_atkinson <- function(x, w, epsilon = 0.5) {
  weighted_mean <- sum(x * w) / sum(w)
  
  if (epsilon == 0) {
    x_equiv <- weighted_mean
  } else if (epsilon == 1) {
    x_equiv <- exp(sum(w * log(x)) / sum(w))
  } else if (epsilon == 2) {
    x_equiv <- sum(w) / sum(w / x)
  } else {
    x_equiv <- (sum(w * x^(1 - epsilon)) / sum(w))^(1 / (1 - epsilon))
  }
  
  atkinson_index <- 1 - (x_equiv / weighted_mean)
  return(atkinson_index)
}

# Generalized Entropy Functions
entropy_measure <- function(income, theta, weights = NULL) {
  if (is.null(weights)) {
    mu <- mean(income)
    n <- length(income)
    weighted_sum <- sum((income / mu)^theta)
  } else {
    mu <- sum(income * weights) / sum(weights)
    n <- sum(weights)
    weighted_sum <- sum(weights * (income / mu)^theta)
  }
  
  if (theta == 0) {
    E_theta <- sum(weights * log(mu / income)) / sum(weights)
  } else if (theta == 1) {
    E_theta <- sum(weights * (income / mu) * log(income / mu)) / sum(weights)
  } else {
    E_theta <- (1 / (theta * (theta - 1))) * (weighted_sum / n - 1)
  }
  
  return(E_theta)
}

coefficient_of_squared_variation <- function(income, weights = NULL) {
  I2 <- 2 * entropy_measure(income, theta = 2, weights)
  return(I2)
}

mean_logarithmic_deviation <- function(income, weights = NULL) {
  I0 <- entropy_measure(income, theta = 0, weights)
  return(I0)
}

# Function to calculate the Kakwani Index
calculate_kakwani_index <- function(calc_pitax, g_total_gross, w) {
  order_index <- order(g_total_gross)
  calc_pitax <- calc_pitax[order_index]
  g_total_gross <- g_total_gross[order_index]
  w <- w[order_index]
  
  tax <- g_total_gross - calc_pitax
  cum_w <- cumsum(w) / sum(w)
  cum_gross <- cumsum(g_total_gross * w) / sum(g_total_gross * w)
  cum_tax <- cumsum(tax * w) / sum(tax * w)
  
  area_lx <- sum(cum_gross[-1] * diff(cum_w))
  area_lt <- sum(cum_tax[-1] * diff(cum_w))
  
  gini_gx <- 1 - 2 * area_lx
  concentration_ct <- 1 - 2 * area_lt
  
  kakwani_index <- concentration_ct - gini_gx
  return(kakwani_index)
}

# Calculate all indicators and store in a table
indicator_table <- data.frame(
  Indicator = c(
    "Gini coefficient for pre-tax income",
    "Gini coefficient for after-tax income",
    "Concentration coefficient for after-tax income w.r.t. pre-tax income",
    "Concentration coefficient for tax w.r.t. pre-tax income",
    "Atkinson Index",
    "Coefficient of squared variation (I2)",
    "Mean logarithmic deviation (I0)",
    "Kakwani Index"
  ),
  Name = c(
    "gini_gx",
    "gini_gx_t",
    "concentration_cx_t",
    "concentration_ct",
    "atkinson_index",
    "I2",
    "I0",
    "kakwani_index"
  ),
  Value = c(
    weighted_gini(PIT_BU_simulation_year_df$total_net, PIT_BU_simulation_year_df$g_total_gross, PIT_BU_simulation_year_df$weight)$gini_gx,
    weighted_gini(PIT_BU_simulation_year_df$total_net, PIT_BU_simulation_year_df$g_total_gross, PIT_BU_simulation_year_df$weight)$gini_gx_t,
    weighted_concentration(PIT_BU_simulation_year_df$total_net, PIT_BU_simulation_year_df$g_total_gross, PIT_BU_simulation_year_df$weight)$concentration_cx_t,
    weighted_concentration(PIT_BU_simulation_year_df$total_net, PIT_BU_simulation_year_df$g_total_gross, PIT_BU_simulation_year_df$weight)$concentration_ct,
    weighted_atkinson(PIT_BU_simulation_year_df$total_net, PIT_BU_simulation_year_df$weight, epsilon = 0.5),
    coefficient_of_squared_variation(PIT_BU_simulation_year_df$g_total_gross, PIT_BU_simulation_year_df$weight),
    mean_logarithmic_deviation(PIT_BU_simulation_year_df$g_total_gross, PIT_BU_simulation_year_df$weight),
    calculate_kakwani_index(PIT_BU_simulation_year_df$calc_pitax, PIT_BU_simulation_year_df$g_total_gross, PIT_BU_simulation_year_df$weight)
  )
)


# new

# Function to calculate the average tax rate t
average_tax_rate <- function(pre_tax_income, after_tax_income) {
  Q <- sum(pre_tax_income - after_tax_income)  # Total tax revenue
  m <- sum(pre_tax_income)  # Total pre-tax income
  t <- Q / m  # Average tax rate
  return(t)
}

# Function to calculate the Redistributive Effect (R), H, and V
calculate_redistributive_effect <- function(concentration_cx_t, gini_gx_t, gini_gx, kakwani_index, t) {
  H <- (concentration_cx_t - gini_gx_t) / gini_gx
  V <- (t * kakwani_index) / ((1 - t) * gini_gx)
  R <- H + V
  return(list(H = H, V = V, R = R))
}

# Calculate the required indicators
concentration_cx_t <- weighted_concentration(PIT_BU_simulation_year_df$total_net, PIT_BU_simulation_year_df$g_total_gross, PIT_BU_simulation_year_df$weight)$concentration_cx_t
gini_gx_t <- weighted_gini(PIT_BU_simulation_year_df$total_net, PIT_BU_simulation_year_df$g_total_gross, PIT_BU_simulation_year_df$weight)$gini_gx_t
gini_gx <- weighted_gini(PIT_BU_simulation_year_df$total_net, PIT_BU_simulation_year_df$g_total_gross, PIT_BU_simulation_year_df$weight)$gini_gx
kakwani_index <- calculate_kakwani_index(PIT_BU_simulation_year_df$calc_pitax, PIT_BU_simulation_year_df$g_total_gross, PIT_BU_simulation_year_df$weight)
t <- average_tax_rate(PIT_BU_simulation_year_df$g_total_gross, PIT_BU_simulation_year_df$calc_pitax)

# Calculate H, V, and R
redistributive_effects <- calculate_redistributive_effect(concentration_cx_t, gini_gx_t, gini_gx, kakwani_index, t)

# Add the results to the indicator table
indicator_table <- rbind(indicator_table, data.frame(
  Indicator = c("Horizontal Inequality (H)", "Vertical Inequality (V)", "Redistributive Effect (R)"),
  Name = c("H", "V", "R"),
  Value = c(redistributive_effects$H, redistributive_effects$V, redistributive_effects$R)
))

# Print the updated table
print(indicator_table)



### new TEST

# Function to calculate Concentration Coefficients
weighted_concentration <- function(calc_pitax, total_net, g_total_gross, w) {
            # Sort data by g_total_gross (pre-tax income)
            order_index_gross <- order(g_total_gross)
            
            # Sorting by pre-tax income for concentration index calculation
            calc_pitax <- calc_pitax[order_index_gross]
            total_net_ranked_by_gross <- total_net[order_index_gross]
            w_ranked_by_gross <- w[order_index_gross]
            
            # Calculate cumulative sums for the Lorenz curve based on the ranking by pre-tax income
            cum_w_by_gross <- cumsum(w_ranked_by_gross) / sum(w_ranked_by_gross)
            cum_net_by_gross <- cumsum(total_net_ranked_by_gross * w_ranked_by_gross) / sum(total_net_ranked_by_gross * w_ranked_by_gross)
            cum_tax_by_gross <- cumsum(calc_pitax * w_ranked_by_gross) / sum(calc_pitax * w_ranked_by_gross)
            
            # Calculate areas under the concentration curves
            area_lx_t <- sum(cum_net_by_gross * diff(c(0, cum_w_by_gross)))  # Concentration for after-tax income w.r.t. pre-tax income
            area_lt <- sum(cum_tax_by_gross * diff(c(0, cum_w_by_gross)))    # Concentration for tax w.r.t. pre-tax income
            
            # Calculate concentration coefficients
            concentration_cx_t <- 1 - 2 * area_lx_t
            concentration_ct <- 1 - 2 * area_lt
            
            return(list(concentration_cx_t = concentration_cx_t, concentration_ct = concentration_ct))
          }

            # Example usage with your dataset
            result <- weighted_concentration(
              PIT_BU_simulation_year_df$calc_pitax,
              PIT_BU_simulation_year_df$total_net,
              PIT_BU_simulation_year_df$g_total_gross,
              PIT_BU_simulation_year_df$weight
            )
            
            print(result$concentration_cx_t)  # Concentration for after-tax income w.r.t. pre-tax income
            print(result$concentration_ct)    # Concentration for tax w.r.t. pre-tax income


