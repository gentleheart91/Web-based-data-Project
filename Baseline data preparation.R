#load Packages
library(psych)
library(dplyr)
library(skimr)
library(Hmisc)
library(ggplot2)

USG <- read.csv("C:/Users/MCS/Downloads/_Baseline_USGo_2023_12_06.csv")
# Specify the file path
file_path <- "C:/Users/MCS/Desktop/Dr Ugochi project/Project with Dr Vivian/stressful life events and CVD/Final/Final/harmonized_dataset_USGo_BL_2023_12_01.csv"
# Read the CSV file into a data frame with potential adjustments
USG_data <- read.csv(file_path, sep = ",", header = TRUE, fileEncoding = "UTF-8")

# Display the first few rows of the data
head(USG_data)
names(USG_data)
 unique(USG_data_no_na$sdc_age_0)
# Baseline Descriptive Statistics -----------------------------------------

age_na_count <- sum(is.na(USG_data$sdc_age_0))
 print(paste("Total number of NA values:", age_na_count))
 
 #Exclude all NAs
 USG_data_no_na <- USG_data[!is.na(USG_data$sdc_age_0), ]
 # Create the age groups
 USG_data_no_na$age_group <- cut(USG_data_no_na$sdc_age_0, 
                                 breaks = c(20, 30, 40, 50, 60, 70, 80, 90),
                                 labels = c("21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90"),
                                 include.lowest = TRUE,
                                 right = FALSE)
 unique(USG_data_no_na$age_group)
 # Count the number of individuals in each age group
 age_group_counts <- table(USG_data_no_na$age_group)
 
 # Print the results
 print(age_group_counts)
#Sex
 unique(USG_data_no_na$sdc_eb_other_0)
 SE_count <- table(USG_data_no_na$sdc_sex_0)
 
 summary(USG_data$sdc_eb_white_0)
 
table(USG_data$sdc_eb_white_0)
 
 # Create a new column 'race' based on the conditions
 USG_data_no_na$Race <- ifelse(USG_data_no_na$sdc_eb_white_0 == 1, "White",
                               ifelse(USG_data_no_na$sdc_eb_black_0 == 1, "Black",
                                      ifelse(USG_data_no_na$sdc_eb_other_0 == 1, "Other",
                                             ifelse(USG_data_no_na$sdc_eb_white_0 == 0 & 
                                                      USG_data_no_na$sdc_eb_black_0 == 0 & 
                                                      USG_data_no_na$sdc_eb_other_0 == 0, "NA",
                                                    NA))))
unique(USG_data_no_na$Race)
# Check accuracy

# Check the distribution of the new Race column
table(USG_data_no_na$Race, useNA = "ifany")

# Verify White classification
sum(USG_data_no_na$Race == "White" & USG_data_no_na$sdc_eb_white_0 == 1, na.rm = TRUE)
sum(USG_data_no_na$Race == "White" & USG_data_no_na$sdc_eb_white_0 != 1, na.rm = TRUE)

# Verify Black classification
sum(USG_data_no_na$Race == "Black" & USG_data_no_na$sdc_eb_black_0 == 1, na.rm = TRUE)
sum(USG_data_no_na$Race == "Black" & USG_data_no_na$sdc_eb_black_0 != 1, na.rm = TRUE)

# Verify Other classification
sum(USG_data_no_na$Race == "Other" & USG_data_no_na$sdc_eb_other_0 == 1, na.rm = TRUE)
sum(USG_data_no_na$Race == "Other" & USG_data_no_na$sdc_eb_other_0 != 1, na.rm = TRUE)

# Verify NA classification
sum(USG_data_no_na$Race == "NA" & 
      USG_data_no_na$sdc_eb_white_0 == 0 & 
      USG_data_no_na$sdc_eb_black_0 == 0 & 
      USG_data_no_na$sdc_eb_other_0 == 0, na.rm = TRUE)

# Check for any misclassifications
sum(USG_data_no_na$Race == "NA" & 
      (USG_data_no_na$sdc_eb_white_0 == 1 | 
         USG_data_no_na$sdc_eb_black_0 == 1 | 
         USG_data_no_na$sdc_eb_other_0 == 1), na.rm = TRUE)

# Check for any NA values in Race that don't meet the NA condition
sum(is.na(USG_data_no_na$Race) & 
      (USG_data_no_na$sdc_eb_white_0 == 1 | 
         USG_data_no_na$sdc_eb_black_0 == 1 | 
         USG_data_no_na$sdc_eb_other_0 == 1 |
         (USG_data_no_na$sdc_eb_white_0 == 0 & 
            USG_data_no_na$sdc_eb_black_0 == 0 & 
            USG_data_no_na$sdc_eb_other_0 == 0)), na.rm = TRUE)

#Plot the race distribution
# Calculate counts and percentages
race_summary <- USG_data_no_na %>%
  count(Race) %>%
  mutate(percentage = n / sum(n) * 100)

# Create the plot
race_plot <- ggplot(race_summary, aes(x = Race, y = n)) +
    geom_bar(stat = "identity", fill = "cornflowerblue", color = "black") +
    geom_text(aes(label = paste0("\n(", round(percentage, 1), "%)")),
              position = position_stack(vjust = 1.05),
              color = "black", size = 3.5) +
    labs(title = "Distribution of Races",
         x = "Race",
         y = "Count") +
  theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
# Save the plot
ggsave(filename = "C:/Users/MCS/Desktop/Dr Ugochi project/Project with Dr Vivian/stressful life events and CVD/Final/race_distribution.png",
       plot = race_plot,
       width = 10, height = 6, units = "in", dpi = 300)
#USA Born
unique(USG_data_no_na$sdc_birth_usa_0)
table(USG_data_no_na$sdc_birth_usa_0, useNA = "ifany")




print(SE_count)

# count the unique race distribution
Race_distribution <- table(USG_data_no_na$Race)
print(Race_distribution)
# Function to calculate the total number of participants under each unique level for each column

calculate_unique_levels <- function(df) {
  result <- list()
  for (col in names(df)) {
    unique_counts <- df %>%
      group_by(!!sym(col)) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count))
    unique_counts$Column <- col
    result[[col]] <- unique_counts
  }
  return(result)
}

# Apply the function to the dataset
unique_levels_counts <- calculate_unique_levels(USG_data)

# Combine all data frames into one
combined_df <- bind_rows(unique_levels_counts)

# Specify the file path where you want to save the CSV file
output_file_path <- "C:/Users/MCS/Desktop/Dr Ugochi project/Project with Dr Vivian/stressful life events and CVD/Final/Final/unique_levels_statistics.csv"

# Save the combined data frame as a CSV file
write.csv(combined_df, file = output_file_path, row.names = FALSE)

table(USG_data_no_na$lev_life_events_0, USG_data_no_na$lev_life_events_cat_0)
