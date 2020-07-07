library(ArrayExpress)
library(data.table)

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
  SDRF <- setnames(SDRF, old=col_names, new=c("Sample", "Condition"))
  SDRF <- SDRF[ , c("Sample", "Condition")]
  SDRF$Sample <- word(SDRF$Sample, 1)
  SDRF$Sample <- gsub(prefix, "", SDRF$Sample)
  SDRF$Sample <- gsub(suffix, "", SDRF$Sample)
  SDRF$Condition <- factor(SDRF$Condition)
  return(SDRF)
}

filter_sdrf <- function(SDRF, cond1, cond2) {
  SDRF <- dplyr::filter(SDRF, select_sdrf_rows(cond1, cond2, Condition))
  return(SDRF)
}

select_sdrf_rows <- function(cond1, cond2, Condition) {
  return(Condition == cond1 |
           Condition == cond2)
}
 