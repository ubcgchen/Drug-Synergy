library(ArrayExpress)
library(data.table)
library(tidyverse)

load_SDRF <- function(AE_data, accession_code) {
  
  # retrieve the SDRF file path and read it in
  sdrf_location <- file.path(AE_data, base::paste(accession_code, 
                                                  ".sdrf.txt",
                                                  sep = ""))
  SDRF <- read.delim(sdrf_location)
  
  return(SDRF)
}

update_sdrf <- function(SDRF, col_names, prefix, suffix) {
  
  if (col_names[1] == col_names[2]) {
    stop("The sample column must be different from the condition column")
  }
  
  tryCatch({
    # Make a copy of the original SDRF
    updated_SDRF <- data.frame(SDRF)
    
    # Rename the user-specified Sample and Condition column to "Sample" and
    # "Condition," respectively.
    setnames(updated_SDRF, old=col_names, new=c("Sample", "Condition"))
    
    # Remove all other columns
    updated_SDRF <- updated_SDRF[ , c("Sample", "Condition")]
    
    # Rename sample cells according to user-specified prefixes/suffixes
    updated_SDRF$Sample <- word(updated_SDRF$Sample, 1)
    updated_SDRF$Sample <- gsub(prefix, "", updated_SDRF$Sample)
    updated_SDRF$Sample <- gsub(suffix, "", updated_SDRF$Sample)
    
    # Factor the condition column
    updated_SDRF$Condition <- factor(updated_SDRF$Condition)

    return(updated_SDRF)
  },
  error = function(cond) {
    stop("Check to make sure your prefix or suffix are correctly spelled")
  })
}

filter_sdrf <- function(SDRF, condition, control) {
  
  # Make sure condition/control are not empty and non-intersecting
  if (length(condition) == 0 || length(control) == 0) {
    stop("Please select at least one level for both condition and control")
  } else if (length(intersect(condition, control)) != 0) {
    stop("Condition and control levels cannot intersect")
  }
  
  # Only keep the rows that were specified as condition(s) or control(s)
  SDRF <- dplyr::filter(SDRF, select_sdrf_rows(condition, control, Condition))
  
  # Refactor the Condition column to contain both "condition" and "control" levels
  levels(SDRF$Condition) <- c(levels(SDRF$Condition), "condition", "control")
  
  # Change all rows specified as a condition to "condition". Do the same for 
  # all rows specified as a control.
  for (i in 1:length(SDRF$Condition)) {
    if (SDRF$Condition[i] %in% condition) {SDRF$Condition[i] = "condition"}
    else if (SDRF$Condition[i] %in% control) {SDRF$Condition[i] = "control"}
  }
  
  # Refactor to remove the unused levels (i.e. not "condition" or "control")
  SDRF$Condition <- factor(SDRF$Condition)
  
  return(SDRF)
}

select_sdrf_rows <- function(condition, control, Condition) {
  # Filter function. Return true if the condition was specified as a condition
  # or control.
  return(Condition %in% condition |
           Condition %in% control)
}
 