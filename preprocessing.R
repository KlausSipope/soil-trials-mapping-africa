# Name: Mr. Klaus Sipope
# Position: Junior Data Scientist
# Description: Create map for all countries
# Date first written: Sun, 27-Oct-2024
# Date last updated: Sun, 27-Oct-2024


# Load packages
library(readxl)
library(writexl)
library(dplyr)

# Read xlsx file
path_mozambique = "general_dataset_mozambique.xlsx"
path_tanzania = "general_dataset_tanzania.xlsx"
path_uganda = "general_dataset_uganda.xlsx"
df_mozambique = read_excel(path = path_mozambique)
df_tanzania = read_excel(path = path_tanzania)
df_uganda = read_excel(path = path_uganda)

# List of dataframes
dfs <- list(
  df1 = df_mozambique, 
  df2 = df_tanzania,
  df3 = df_uganda
)

# Function to find common columns across all dataframes
find_common_columns <- function(dataframes) {
  Reduce(intersect, lapply(dataframes, colnames))  # Find common columns
}

# Function to drop different columns and combine datasets
combine_dataframes <- function(dataframes) {
  common_columns <- find_common_columns(dataframes)  # Find common columns
  
  # Subset each dataframe to retain only the common columns
  aligned_dataframes <- lapply(dataframes, function(df) df[, common_columns, drop = FALSE])
  
  # Combine all dataframes into one using rbind
  combined_df <- do.call(rbind, aligned_dataframes)
  
  return(combined_df)
}

# Example usage
combined_df <- combine_dataframes(dfs)
View(combined_df)

# Modify trial names
updated_df <- combined_df %>% 
  mutate(Trial= case_when(
    Trial %in% c("IFOT", "I-FOT (Trial on Station)", "Intensive") ~ "I-FOT",
    Trial %in% c("E-FOT (Trial on farm)", "Extensive") ~ "E-FOT",
    TRUE ~ Trial  # Keep other names unchanged
  ))

View(updated_df)

# Download it
path = "general_dataset.xlsx"
write_xlsx(updated_df, path)
