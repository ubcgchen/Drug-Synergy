library(tidyverse)
library(dplyr)
library(limma)
library(conflicted)
source("http://bioconductor.org/biocLite.R")
library("AnnotationDbi")
library("hgu133plus2.db")
library(reticulate)
library(xlsx)

directory_path = "../Sepsis Data/E-GEOD-66099.processed.1"

reference_matrix <- read.delim("../Sepsis Data/E-GEOD-66099.sdrf-ref-matrix.txt") %>%
  rename_at(vars(c('Source.Name','Characteristics..disease.')), ~ c('Sample','Condition')) %>% 
  dplyr::select(1, 5) %>% 
  dplyr::filter(Condition == "sepsis" | 
           Condition == "control")
reference_matrix$Sample <- word(reference_matrix$Sample, 1)
reference_matrix$Condition <- factor(reference_matrix$Condition)

N <- nrow(reference_matrix)
data_path <- data.frame(Sample=rep("", N), Path=rep("", 1),
                        stringsAsFactors=FALSE)

files <- list.files(path=directory_path, pattern="*.txt", full.names=TRUE, recursive=FALSE)
index <- 1

use_python("/Applications/Python 3.7", required = TRUE)


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

affy_entrez_map <- AnnotationDbi::select(hgu133plus2.db, row.names(tt), c("ENTREZID")) %>%
  na.omit()

tt <- rownames_to_column(tt, 'PROBEID')
tt <- as.data.frame(inner_join(affy_entrez_map, tt))
tt <- aggregate(list(logFC = tt$logFC, AveExpr = tt$AveExpr, t = tt$t, 
                 P.Value = tt$ P.Value, adj.P.Val = tt$adj.P.Val, B = tt$B), 
            by=list(ENTREZID = tt$ENTREZID), FUN=mean) %>%
  arrange(desc(abs(t))) %>%
  column_to_rownames('ENTREZID')
tt <- tt[ , !(names(tt) %in% c("PROBEID"))]

positive_DEG <- tt %>% 
  rownames_to_column('entrez_id') %>%
  dplyr::filter(logFC > 0) %>%
  column_to_rownames('entrez_id')

negative_DEG <- tt %>% 
  rownames_to_column('entrez_id') %>%
  dplyr::filter(logFC < 0) %>%
  column_to_rownames('entrez_id')

write.table(positive_DEG, "upreg.txt", sep="\t")
write.table(negative_DEG, "downreg.txt", sep="\t")

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
rm(tt)
rm(affy_entrez_map)
