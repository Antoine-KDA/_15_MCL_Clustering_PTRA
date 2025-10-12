# Convert XML to CSV
# Install required packages if needed
if (!require('xml2')) install.packages('xml2')
if (!require('dplyr')) install.packages('dplyr')
if (!require('readr')) install.packages('readr')

library(xml2)
library(dplyr)
library(readr)

# Read the XML file
xml_file <- "c:/Users/kwhr625/Box/Personal/Old_Studies/MCL/ptra/ptra_test/icd10cm_tabular_2022.xml"
xml_data <- read_xml(xml_file)

# Method 1: Extract all nodes with their attributes
# This approach extracts all XML elements and their attributes into a data frame

# Get all nodes
all_nodes <- xml_find_all(xml_data, ".//*")

# Create a data frame with node information
df <- data.frame(
  node_name = xml_name(all_nodes),
  node_text = xml_text(all_nodes),
  stringsAsFactors = FALSE
)

# Add attributes if they exist
attrs <- xml_attrs(all_nodes)
max_attrs <- max(lengths(attrs))

# Create attribute columns
for (i in 1:max_attrs) {
  attr_names <- sapply(attrs, function(x) if(length(x) >= i) names(x)[i] else NA)
  attr_values <- sapply(attrs, function(x) if(length(x) >= i) x[i] else NA)
  
  df[[paste0("attr_name_", i)]] <- attr_names
  df[[paste0("attr_value_", i)]] <- attr_values
}

# Save to CSV
write_csv(df, "icd10cm_tabular_2022_basic.csv")

# Method 2: Specific extraction for ICD-10 structure
# This is more tailored for ICD-10 XML structure

# Try to find specific ICD elements (adjust based on your XML structure)
icd_codes <- xml_find_all(xml_data, ".//diag | .//section | .//code")

if (length(icd_codes) > 0) {
  icd_df <- data.frame(
    element_type = xml_name(icd_codes),
    code = xml_attr(icd_codes, "code"),
    description = xml_text(icd_codes),
    stringsAsFactors = FALSE
  )
  
  # Clean up the data
  icd_df <- icd_df %>%
    filter(!is.na(code) | !is.na(description)) %>%
    mutate(description = trimws(description))
  
  write_csv(icd_df, "icd10cm_tabular_2022_structured.csv")
  print("Structured CSV created successfully!")
} else {
  print("No specific ICD elements found. Using basic extraction.")
}

print("Basic CSV created successfully!")
print(paste("Number of rows in basic extraction:", nrow(df)))