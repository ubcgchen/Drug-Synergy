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

fit_data <- function(expression_matrix, design_matrix) {
  fit <- lmFit(expression_matrix , design_matrix)
  fit <- eBayes(fit)
  tt <- topTable(fit, coef = 2, adjust.method = "BH", 
                 sort.by = "p", number = 700)
  
  return(tt)
}

generate_DEG_table <- function(func, tt, filters, num_genes) {
  num_genes <- as.numeric(num_genes)
  if (is.na(num_genes)) stop("number of genes must be a number")
  if (num_genes > 150) stop("number of genes must be <= 150")
  
  return(tt %>% 
           rownames_to_column('entrez_id') %>%
           dplyr::filter(func(logFC)) %>%
           column_to_rownames('entrez_id') %>%
           apply_user_filters(filters) %>%
           head(num_genes))
}

apply_user_filters <- function(tt, filters) {
  for (filter in filters) {
    source("filter_functions.R")
    column <- filter[1]
    func <- get_func(filter[2])
    value <- as.numeric(filter[3])
    
    #error check
    if (is.na(value)) stop("filter value must be a number")
    
    # apply filter
    tt <- dplyr::filter(tt, func(tt[[column]], value))
  }
  return(tt)
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

# Coordinate analysis
do_analysis <- function(expression_matrix, design_matrix, user_filters,
                        num_upgenes, num_downgenes) {
  top_table <- fit_data(expression_matrix, design_matrix)
  
  source("affy_entrez_converter.R")
  top_table <- affy_to_entrez(top_table, TRUE)
  
  source("filter_functions.R")
  positive_DEG <- generate_DEG_table(greater_than_zero, top_table, 
                                     user_filters, num_upgenes)
  negative_DEG <- generate_DEG_table(less_than_zero, top_table,
                                     user_filters, num_downgenes)
  
  write_gmt_files(positive_DEG, negative_DEG)
  
  return (list(positive_DEG = positive_DEG, negative_DEG = negative_DEG,
               positive_DEG_len = nrow(positive_DEG), 
               negative_DEG_len = nrow(negative_DEG)))
}
