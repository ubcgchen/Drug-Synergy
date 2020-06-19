library(tidyverse)
library(dplyr)

directory_path = "../Sepsis Data/E-GEOD-66099.processed.1"

reference_matrix <- read.delim("../Sepsis Data/E-GEOD-66099.sdrf-ref-matrix.txt") %>%
  rename_at(vars(c('Source.Name','Characteristics..disease.')), ~ c('Sample','Condition')) %>% 
  select(1, 5) %>% 
  filter(Condition == "SepticShock" | 
           Condition == "control") 
reference_matrix$Sample <- word(reference_matrix$Sample, 1)
reference_matrix$Condition <- factor(reference_matrix$Condition)

N <- nrow(reference_matrix)
data_path <- data.frame(Sample=rep("", N), Path=rep("", 1),
                        stringsAsFactors=FALSE)
index <- 1

files <- list.files(path=directory_path, pattern="*.txt", full.names=TRUE, recursive=FALSE)

for (file in files) {
  sample_name <- str_extract(basename(file), "[^_]+")
  if (sample_name %in% reference_matrix$Sample) {
    data_path[index, 1] <- sample_name
    data_path[index, 2] <- file
    index = index + 1
  }
}

consolidated_df <- merge(x = reference_matrix, y = data_path, by = "Sample", all = TRUE)
grouped_df <- split(consolidated_df, consolidated_df$Condition) 



