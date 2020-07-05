library(tidyverse)
library(dplyr)
library(limma)
library(conflicted)
library(AnnotationDbi)
library(hgu133plus2.db)
library(cmapR)
library(ArrayExpress)

download_AE_data <- function() {
  AE_data <- tempdir()
  if (!dir.exists(AE_data)) {
    dir.create(AE_data)
  }
  getAE(accession_code, path = AE_data, type = "processed")
  return(AE_data)
}

load_SDRF <- function(AE_data) {
  sdrf_location <- file.path(AE_data, base::paste(accession_code, ".sdrf.txt",
                                                  sep = ""))
  # Parse and process the SDRF matrix
  SDRF <- read.delim(sdrf_location) %>%
    rename_at(vars(col_names), ~ c('Sample','Condition')) %>% 
    dplyr::select(1, 5) %>% 
    dplyr::filter(Condition == "sepsis" | 
                    Condition == "control")
  SDRF$Sample <- word(SDRF$Sample, 1)
  SDRF$Condition <- factor(SDRF$Condition)
  return(SDRF)
}

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

build_expression_matrix <- function(file_paths, SDRF) {
  consolidated_df <- S4Vectors::merge(x = SDRF, y = file_paths, 
                                      by = "Sample", all = TRUE)
  grouped_df <- split(consolidated_df, consolidated_df$Condition)
  
  expression_matrix <- data.frame()[1:54675, ]
  
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
    c(read.delim(grouped_df$control$Path[1], header = TRUE))$ID_REF)
  
  return(expression_matrix)
}

build_design_matrix <- function() {
  return(cbind(1 , c(rep(0 , 47) , rep(1 , 18))))
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

accession_code <- "E-GEOD-66099"
col_names <- c('Source.Name', 'Characteristics..disease.')

AE_data <- download_AE_data()
SDRF <- load_SDRF(AE_data)
file_paths <- get_file_paths(AE_data)

expression_matrix <- build_expression_matrix(file_paths, SDRF)
design_matrix <- build_design_matrix()

top_table <- fit_data(expression_matrix, design_matrix)
top_table <- affy_to_entrez(top_table)

positive_DEG <- generate_DEG_table(greater_than_zero, top_table)
negative_DEG <- generate_DEG_table(less_than_zero, top_table)

write_gmt_files(positive_DEG, negative_DEG)

rm(design_matrix)
rm(expression_matrix)
rm(file_paths)
rm(negative_DEG)
rm(positive_DEG)
rm(SDRF)
rm(accession_code)
rm(col_names)
rm(AE_data)
