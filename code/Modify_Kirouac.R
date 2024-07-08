#modify Kirouac
#Code to modify the original Human-2010-Kirouac-LR-pairs.xlsx to show the gene symbols only.

# Load the readxl package to read xlsx files
library(readxl)
# Load the dplyr package for data manipulation
library(dplyr)
library(writexl)

# Read the xlsx file
df <- read_excel("/Users/diandra/rlp_meta/data/LewisLabUCSD/Human-2010-Kirouac-LR-pairs.xlsx")

# Define a function to refine the data
refine_data <- function(x) {
  if (grepl("\\(", x)) {
    if (grepl("^(CCL|CX)", x)) {
      return(gsub("\\s*\\(.*\\)", "", x))
    } else {
      outside <- gsub("\\(.*\\)", "", x)
      inside <- gsub(".*\\((.*)\\).*", "\\1", x)
      outside <- gsub("^\\s+|\\s+$", "", outside)  # remove leading and trailing spaces
      inside <- gsub("^\\s+|\\s+$", "", inside)  # remove leading and trailing spaces
      if (grepl("/", inside)) {
        return(inside)
      } else {
        if (nchar(inside) > 0) {
          outside_capitals <- sum(gregexpr("[A-Z]", outside)[[1]]>0)
          inside_capitals <- sum(gregexpr("[A-Z]", inside)[[1]]>0)
          if (abs(outside_capitals - inside_capitals) <= 2) {
            if (nchar(outside) > nchar(inside)) {
              return(inside)
            } else {
              return(outside)
            }
          } else {
            if (inside_capitals >= outside_capitals) {
              return(inside)
            } else {
              return(outside)
            }
          }
        } else {
          return(outside)
        }
      }
    }
  } else {
    return(x)
  }
}
# Vectorize the function
refine_data <- Vectorize(refine_data)

# Apply the function to each cell in the dataframe
df <- df %>% mutate(across(everything(), refine_data))
# Save the refined dataframe to a new xlsx file
write_xlsx(df, "/Users/diandra/rlp_meta/data/new_files/Human-2010-Kirouac-LR-pairs_new.xlsx")

#modify the just created file
df <- read_excel("/Users/diandra/rlp_meta/data/new_files/Human-2010-Kirouac-LR-pairs_new.xlsx")
# Define the extract_ligand function
extract_ligand <- function(x) {
  if (grepl("/", x)) {
    ligands <- strsplit(x, "/")[[1]]
    counts <- sapply(ligands, function(y) sum(grepl("[A-Z0-9]", strsplit(y, "")[[1]])))
    
    # If there's a tie, keep the first one
    max_count <- max(counts)
    if (sum(counts == max_count) > 1) {
      return(ligands[1])
    } else {
      return(ligands[which.max(counts)])
    }
  } else {
    return(x)  # Return the original string if no / is present
  }
}


# Apply the extract_ligand function to the Ligand column
df$LIGAND <- sapply(as.character(df$LIGAND), extract_ligand)

# Write the updated data back to the xlsx file
write_xlsx(df, "/Users/diandra/rlp_meta/data/new_files/Human-2010-Kirouac-LR-pairs_new.xlsx")

