

# Table 1 for proportions of people who have used HIVST

# calculate prop function

calculate_proportions <- function(data) {
survey_designs <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = data, nest = TRUE)
  prop <- svyciprop(~I(hivst_use == 1), design = survey_designs, method = "logit", level = 0.95)
  return(prop)
}

# ----------DHS--------------
#calculate_proportions(list_dhs[[1]]) # checking function
#calculate_proportions(list_dhs[[2]])
#calculate_proportions(list_dhs[[3]])
lapply(list_dhs, calculate_proportions)


# MICS (need to check NAs)

# Modify the design to handle single-PSU strata
options(survey.lonely.psu = "adjust") # This adjusts the variance for single-PSU strata
lapply(cleaned_list_mics, calculate_proportions)



# ------------PHIA--------------
lapply(list_phia, calculate_proportions)

# Function to extract proportion and confidence intervals
extract_proportion_ci <- function(result) {
  prop <- as.numeric(result[1])       # Proportion (point estimate)
  lower <- attr(result, "ci")[1]      # Lower bound of 95% CI
  upper <- attr(result, "ci")[2]      # Upper bound of 95% CI
  return(c(prop, lower, upper))
}

# Apply the function to the list of results
proportions_and_ci <- t(sapply(lapply(list_phia, calculate_proportions), extract_proportion_ci))

# Define the country, survey, and year details
country_info <- data.frame(
  Country = c("Namibia", "Kenya", "Lesotho", "Zimbabwe", "Malawi", "Mozambique", "Eswatini"),
  Survey = c("PHIA", "PHIA", "PHIA", "PHIA", "PHIA", "PHIA", "PHIA"),
  Year = c(2017, 2018, 2020, 2020, 2020, 2021, 2021)
)

# Combine the country info with the proportions and CIs
proportions_df <- cbind(
  country_info, 
  "Proportion of People who used HIV self-test" = proportions_and_ci[, 1],
  "Lower 95% CI" = proportions_and_ci[, 2],
  "Upper 95% CI" = proportions_and_ci[, 3]
)

# Display the dataframe
print(proportions_df)
