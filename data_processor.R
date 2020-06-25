library(tidyverse)
library(dplyr)
library(limma)

directory_path = "../Sepsis Data/E-GEOD-66099.processed.1"

reference_matrix <- read.delim("../Sepsis Data/E-GEOD-66099.sdrf-ref-matrix.txt") %>%
  rename_at(vars(c('Source.Name','Characteristics..disease.')), ~ c('Sample','Condition')) %>% 
  select(1, 5) %>% 
  filter(Condition == "sepsis" | 
           Condition == "control") 
reference_matrix$Sample <- word(reference_matrix$Sample, 1)
reference_matrix$Condition <- factor(reference_matrix$Condition)

N <- nrow(reference_matrix)
data_path <- data.frame(Sample=rep("", N), Path=rep("", 1),
                        stringsAsFactors=FALSE)

files <- list.files(path=directory_path, pattern="*.txt", full.names=TRUE, recursive=FALSE)
index <- 1

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

expression_matrix <- data.frame()[1:54675, ]

for (condition in grouped_df) {
  index <- 1
  for (path in condition$Path) {
    expression_matrix$placeholder <- as.numeric(unlist(read.delim(path, header = TRUE)[2]))
    names(expression_matrix)[names(expression_matrix) == "placeholder"] <- grouped_df[[condition$Condition[1]]]$Sample[index]
    index <- index+1
  }
}

rownames(expression_matrix) <- as.character(c(read.delim(grouped_df$control$Path[1], header = TRUE))$ID_REF)
design_matrix <- cbind(1 , c(rep(0 , 47) , rep(1 , 18)))

fit <- lmFit(expression_matrix , design_matrix)
fit <- eBayes(fit)
tt <- topTable(fit, coef = 2, adjust.method = "BH", sort.by = "p", number = 200)

rm(condition)
rm(consolidated_df)
rm(data_path)
rm(fit)
rm(grouped_df)
rm(reference_matrix)
rm(directory_path)
rm(file)
rm(files)
rm(index)
rm(path)
rm(sample_name)
rm(N)
rm(design_matrix)
rm(expression_matrix)


