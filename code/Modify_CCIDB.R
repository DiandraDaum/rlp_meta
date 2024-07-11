#Modify CCIDB: keep only ligand-receptor interaction

library(readxl)
library(writexl)

df <- read_xlsx("~/rlp_meta/data/CCIDB/CCIDB_Human.xlsx")

#remove rows that are not ligand receptor pairs under interaction type
# Filter the rows where interaction_type is "Ligand-receptor"
# Filter the rows where interaction_type is "Ligand-receptor"
df_removed <- df %>% 
  filter(interaction_type!= "Ligand-receptor")

df_filtered <- df %>% 
  filter(interaction_type == "Ligand-receptor")

# Print the removed lines
print("Removed lines:")
print(df_removed)

write_xlsx(df_filtered, "~/rlp_meta/data/new_files/CCIDB_Human_new.xlsx")

