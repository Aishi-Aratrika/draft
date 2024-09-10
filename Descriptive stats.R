
# --------Table 1: proportions of people who knows about and used HIVST------------

# Function to calculate proportions for hivst knowledge and hivst use

calculate_proportions_knwldge <- function(data) {
  survey_designs <- svydesign( 
    ids = ~psu, strata = ~strata, weights = ~ind_wt, data = data, nest = TRUE)
  prop <- svyciprop(~I(hivst_knwldge == 1), design = survey_designs, method = "logit", level = 0.95)
  return(prop)
}

calculate_proportions_use <- function(data) {
survey_designs <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = data, nest = TRUE)
  prop <- svyciprop(~I(hivst_use == 1), design = survey_designs, method = "logit", level = 0.95)
  return(prop)
}


# ----------DHS--------------
#calculate_proportions_use(list_dhs[[1]]) # checking function
#calculate_proportions_use(list_dhs[[2]])
#calculate_proportions_use (list_dhs[[3]])

# Apply the function to the list of DHS datasets
lapply(list_dhs, calculate_proportions_knwldge)
lapply(list_dhs, calculate_proportions_use)


# ------------PHIA--------------
lapply(list_phia, calculate_proportions_use)

# only malawi phia has hivst knowledge information
# 0.317(0.304, 0.329)

#------------KAIS(no HIVST knowledge)---------------

#prop_hivst_use_kais #0.03 (0.02, 0.04) 

#----------BAIS (no HIVST knowledge)----------------
#prop_hivst_use_bais #0.0213 (0.0179, 0.0252)


#----------MICS(some have NAs in strata and individual weight)-------

#HIVST knowledge

calculate_proportions_knwldge <- function(data) {
  # Filter out rows with NA or zero weights
  data_clean <- data %>%
    filter(!is.na(ind_wt), ind_wt > 0)
  
  # Define survey design with the cleaned data
  survey_designs <- svydesign( 
    ids = ~psu, strata = ~strata, weights = ~ind_wt, data = data_clean, nest = TRUE)
  
  # Calculate proportion
  prop <- svyciprop(~I(hivst_knwldge == 1), design = survey_designs, method = "logit", level = 0.95)
  
  return(prop)
}

lapply(list_mics, calculate_proportions_knwldge)

#HIVST use

calculate_proportions_use_mics <- function(data) {
  # Filter out rows with NA or zero weights
  data_clean <- data %>%
    filter(!is.na(ind_wt), ind_wt > 0)
  
  # Define survey design with the cleaned data
  survey_designs <- svydesign(
    ids = ~psu, strata = ~strata, weights = ~ind_wt, data = data_clean, nest = TRUE)
  
  # Calculate proportion
  prop <- svyciprop(~I(hivst_use == 1), design = survey_designs, method = "logit", level = 0.95)
  
  return(prop)
}

lapply(list_mics, calculate_proportions_use_mics)


# Sample size for each dataset

sample_sizes_dhs <- lapply(list_dhs, nrow) #DHS
sample_sizes_phia <- lapply(list_phia, nrow) #PHIA
sample_sizes_mics <- lapply(list_mics, function(df) {
  nrow(df %>% filter(!is.na(strata), !is.na(ind_wt)))
}) #MICS
sample_sizes_kais <- 13720 #KAIS
sample_sizes_bais <- 17205 #BAIS



# Extracting the survey id 

survey_ids_dhs <- sapply(list_dhs, function(df) unique(df$survey_id))
survey_ids_mics <- sapply(list_mics, function(df) unique(df$survey_id))
survey_ids_phia <- sapply(list_phia, function(df) unique(df$survey_id))
survey_id_kais <- "KEN2012KAIS"
survey_ids_bais <- "BWA2021BAIS"


# Creating Table 1













