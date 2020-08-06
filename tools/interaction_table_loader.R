library("readxl")

load_interaction_tables <- function() {
  # Create an empty dataframe to hold interaction data
  drug_interactions <- data.frame(drug.A = character(),
                                  drug.B =character(), 
                                  Effect.interaction.drugA.drugB=character(), 
                                  Tanimoto.coefficient..TC. = double(),
                                  stringsAsFactors=FALSE)
  
  # load the 3 files and combine them all into one dataframe
  for (index in 1:3) {
    xl_file <- read_excel(base::paste("drug interaction tables/interaction_table_",
                                      index, ".xls", sep = ""), skip = 1)
    drug_interactions <- rbind(drug_interactions, data.frame(xl_file) %>% 
                                 subset(select = c("drug.A", "drug.B", 
                                                   "Effect.interaction.drugA.drugB", 
                                                   "Tanimoto.coefficient..TC.")))
  }
  
  # Remove duplicates (prioritize table 1 > table 2 > table 3)
  drug_interactions <<- drug_interactions %>%
    distinct(drug.A, drug.B, .keep_all = TRUE)

}