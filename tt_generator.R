# Output is 2 files: uptag.gmt and dntag.gmt. These files are the top 150 most
# up- and down-regulated genes in the disease signature and can directly be
# used to query CMap

library(tidyverse)
library(dplyr)
library(limma)
library(conflicted)
library(AnnotationDbi)
library(hgu133plus2.db)
library(cmapR)

get_file_paths <- function(AE_data) {
  files <- list.files(path=AE_data, pattern="*.txt", full.names=TRUE, recursive=FALSE)
  
  data_path <- data.frame(Sample=rep("", nrow(SDRF)), Path=rep("", 1),
                          stringsAsFactors=FALSE)
  
  index<-1
  for (file in files) {
    sample_name <- str_extract(basename(file), "[^_]+")
    if (sample_name %in% SDRF$Sample) {
      data_path[index, 1] <- sample_name
      data_path[index, 2] <- file
      index = index + 1
    }
  }
  return(data_path)
}

build_expression_matrix <- function(file_paths, SDRF, control) {
  consolidated_df <- S4Vectors::merge(x = SDRF, y = file_paths, 
                                      by = "Sample", all = TRUE)
  grouped_df <- split(consolidated_df, consolidated_df$Condition)
  
  file_len <- nrow(read.delim(grouped_df[[control]]$Path[1], header = TRUE))
  expression_matrix <- data.frame()[1:file_len, ]
  
  for (condition in grouped_df) {
    index <- 1
    for (path in condition$Path) {
      expression_matrix$placeholder <- 
        as.numeric(unlist(read.delim(path, header = TRUE)[2]))
      names(expression_matrix)[names(expression_matrix) == "placeholder"] <- 
        grouped_df[[condition$Condition[1]]]$Sample[index]
      index <- index+1
    }
  }
  
  rownames(expression_matrix) <- as.character(
    c(read.delim(grouped_df[[control]]$Path[1], header = TRUE))$ID_REF)
  
  return(list(expression_matrix, grouped_df))
}

build_design_matrix <- function(control, condition) {
  return(cbind(1 , c(rep(0 , control) , rep(1 , condition))))
}

fit_data <- function(expression_matrix, design_matrix) {
  fit <- lmFit(expression_matrix , design_matrix)
  fit <- eBayes(fit)
  tt <- topTable(fit, coef = 2, adjust.method = "BH", sort.by = "p", number = 700)
}

affy_to_entrez <- function(tt) {
  affy_entrez_map <- AnnotationDbi::select(hgu133plus2.db, 
                                           row.names(tt), 
                                           c("ENTREZID")) %>%
    na.omit()
  
  tt <- rownames_to_column(tt, 'PROBEID')
  tt <- as.data.frame(inner_join(affy_entrez_map, tt))
  tt <- aggregate(list(logFC = tt$logFC, AveExpr = tt$AveExpr, t = tt$t, 
                       P.Value = tt$ P.Value, 
                       adj.P.Val = tt$adj.P.Val, 
                       B = tt$B), 
                  by=list(ENTREZID = tt$ENTREZID), FUN=mean) %>%
    arrange(dplyr::desc(abs(t))) %>%
    column_to_rownames('ENTREZID')
  tt <- tt[ , !(names(tt) %in% c("PROBEID"))]
  return(tt)
}

greater_than_zero <- function(num) {
  return(num > 0)
}

less_than_zero <- function(num) {
  return(num < 0)
}

generate_DEG_table <- function(func, tt) {
  return(tt %>% 
           rownames_to_column('entrez_id') %>%
           dplyr::filter(func(logFC)) %>%
           column_to_rownames('entrez_id') %>%
           head(150))
}

write_gmt_files <- function(positive_DEG, negative_DEG) {
  TAG_DN <- list()
  TAG_DN$head = "TAG_DN"
  TAG_DN$desc = ""
  TAG_DN$entry = row.names(negative_DEG)
  TAG_DN$len = length(row.names((negative_DEG)))
  to_write <- list()
  to_write$TAG_DN <- TAG_DN
  write_gmt(to_write, "dntag.gmt")
  
  TAG_UP <- list()
  TAG_UP$head = "TAG_UP"
  TAG_UP$desc = ""
  TAG_UP$entry = row.names(positive_DEG)
  TAG_UP$len = length(row.names((positive_DEG)))
  to_write <- list()
  to_write$TAG_UP <- TAG_UP
  write_gmt(to_write, "uptag.gmt")
}

# files <- list.files(AE_data, full.names = TRUE)
# file.remove(files)
# unlink(AE_data, recursive = TRUE)
# 
# rm(design_matrix)
# rm(expression_matrix)
# rm(file_paths)
# rm(negative_DEG)
# rm(positive_DEG)
# rm(SDRF)
# rm(accession_code)
# rm(col_names)
# rm(AE_data)
