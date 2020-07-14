library(ArrayExpress)
library(data.table)
library(tidyverse)

download_AE_data <- function(accession_code) {
  AE_data <- tempdir()
  if (!dir.exists(AE_data)) {
    dir.create(AE_data)
  }
  getAE(accession_code, path = AE_data, type = "processed")
  return(AE_data)
}

load_SDRF <- function(AE_data, accession_code) {
  sdrf_location <- file.path(AE_data, base::paste(accession_code, ".sdrf.txt",
                                                  sep = ""))
  SDRF <- read.delim(sdrf_location)
  return(SDRF)
}

update_sdrf <- function(SDRF, col_names, prefix, suffix) {
  updated_SDRF <- data.frame(SDRF)
  setnames(updated_SDRF, old=col_names, new=c("Sample", "Condition"))
  updated_SDRF <- updated_SDRF[ , c("Sample", "Condition")]
  updated_SDRF$Sample <- word(updated_SDRF$Sample, 1)
  updated_SDRF$Sample <- gsub(prefix, "", updated_SDRF$Sample)
  updated_SDRF$Sample <- gsub(suffix, "", updated_SDRF$Sample)
  updated_SDRF$Condition <- factor(updated_SDRF$Condition)
  return(updated_SDRF)
}

filter_sdrf <- function(SDRF, condition, control) {
  # only keep the rows that were specified as condition(s) or control(s)
  SDRF <- dplyr::filter(SDRF, select_sdrf_rows(condition, control, Condition))
  
  # Refactor the Condition column to contain both "condition" and "control" levels
  levels(SDRF$Condition) <- c(levels(SDRF$Condition), "condition", "control")
  
  # Change all rows specified as a condition to "condition", same for control
  for (i in 1:length(SDRF$Condition)) {
    if (SDRF$Condition[i] %in% condition) {SDRF$Condition[i] = "condition"}
    else if (SDRF$Condition[i] %in% control) {SDRF$Condition[i] = "control"}
  }
  
  # Refactor to remove the unused levels
  SDRF$Condition <- factor(SDRF$Condition)
  
  return(SDRF)
}

select_sdrf_rows <- function(cond1, cond2, Condition) {
  return(Condition %in% cond1 |
           Condition %in% cond2)
}
 