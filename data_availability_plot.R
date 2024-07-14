setwd("D:/Downloads")
library(readxl)
library(dplyr)
library(ggplot2)

# Load file
data_avail <- read_excel("MSc Thesis/hiv-selftesting/Survey_Data_Availability_Plot.xlsx")
print(names(data_avail))

# Adjust the 'Year' column (in ranges) by taking the first year
data_avail$Year <- as.integer(sub("-.*", "", data_avail$Year))

# Create a single column for HIVST category
data_avail <- data_avail %>%
  mutate(HIVST_Category = case_when(
    `Only HIVST Awareness Question (heard of hivst)` == 1 ~ "HIVST Awareness",
    `Only HIVST uptake Question (ever used hivst)` == 1 ~ "HIVST Uptake",
    `Both questions` == 1 ~ "Both"
  ))

# maximum year in the dataset
max_year <- max(data_avail$Year)

# Define the region for each country
country_to_region <- c(
  "Comoros" = "Eastern", "Kenya" = "Eastern", "Madagascar" = "Eastern", 
  "Malawi" = "Eastern", "Mozambique" = "Eastern", "Rwanda" = "Eastern", 
  "Tanzania" = "Eastern", "Uganda" = "Eastern", "Zambia" = "Eastern", 
  "Zimbabwe" = "Eastern", "Burundi" = "Central", "Cameroon" = "Central", 
  "Central African Republic" = "Central", "Chad" = "Central", 
  "Democratic Republic of Congo (DRC)" = "Central", "Gabon" = "Central", 
  "São Tomé and Príncipe" = "Central", "Botswana" = "Southern", 
  "Eswatini" = "Southern", "Lesotho" = "Southern", "Namibia" = "Southern", 
  "South Africa" = "Southern", "Benin" = "Western", "Burkina Faso" = "Western", 
  "Cote d'Ivoire" = "Western", "Gambia" = "Western", "Ghana" = "Western", 
  "Guinea" = "Western", "Guinea-Bissau" = "Western", "Liberia" = "Western", 
  "Mali" = "Western", "Mauritania" = "Western", "Nigeria" = "Western", 
  "Senegal" = "Western", "Sierra Leone" = "Western", "Togo" = "Western"
)

# Create a factor with levels ordered by the region for plotting
data_avail$Region <- country_to_region[data_avail$Country]
data_avail$Country <- factor(data_avail$Country, levels = names(country_to_region)[order(country_to_region)])

# Calculate the midpoint of each region for labeling
region_midpoints <- data_avail %>%
  group_by(Region) %>%
  summarise(Midpoint = mean(as.numeric(Country))) %>%
  ungroup()

# Join this back to the main data 
data_avail <- left_join(data_avail, region_midpoints, by = "Region")

# Create the plot with ordered countries and region labels
plot <- ggplot(data_avail, aes(x = Year, y = Country, color = Survey, shape = HIVST_Category)) +
  geom_point(size = 3, alpha = 0.6) +  # Display the points
  scale_shape_manual(values = c("HIVST Awareness" = 16, "HIVST Uptake" = 17, "Both" = 18)) +
  scale_color_manual(values = c("DHS" = "#8E24AA", "MICS" = "#43A047", "PHIA" = "#D32F2F", "KAIS" = "#FFB300", "BAIS" = "#1E88E5")) +
  scale_x_continuous(breaks = 2012:2022, labels = 2012:2022) +
  labs(title = "Survey Data Availability on HIVST Uptake and Awareness", x = "Year", y = "Country", color = "Survey Type", shape = "HIVST Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right") +
  geom_text(data = region_midpoints, aes(x = max_year + 0.5, y = Midpoint, label = Region), angle = 90, hjust = 1, vjust = 0.5, inherit.aes = FALSE)  # Add vertical region labels on the right

# Print the plot
print(plot)




###--------Plot without regions---------------
# Create the plot
#plot <- ggplot(data_avail, aes(x = Year, y = Country, color = Survey, shape = HIVST_Category)) +
 # geom_point(size = 3, alpha = 0.6) +  # Adjust size and transparency of points
 # scale_shape_manual(values = c("HIVST Awareness" = 16, "HIVST Uptake" = 17, "Both" = 18)) +  # Specify shapes for each HIVST category
 # scale_color_manual(values = c("DHS" = "#8E24AA",   # Deep blue
                               # "MICS" = "#43A047",  # Emerald green
                              #  "PHIA" = "#D32F2F",  # Crimson red
                               # "KAIS" = "#FFB300",  # Amber gold
                               # "BAIS" = "#1E88E5")) +  # Rich purple
 # scale_x_continuous(breaks = 2012:2022, labels = 2012:2022) +  # Set x-axis to show every year from 2012 to 2022 as integers
  #labs(title = "Survey Data Availability on HIVST Uptake and Awareness",
      # x = "Year",
       # y = "Country",
       # color = "Survey Type",
       # shape = "HIVST Category") +
 # theme_minimal() +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
       # legend.position = "right")  # Move legends to the right side of the plot

# Print the plot
#print(plot)



