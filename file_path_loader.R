get_file_paths <- function(AE_data, SDRF) {
  # Get all the txt files from the processed data
  files <- list.files(path=AE_data, pattern="*.txt", 
                      full.names=TRUE, recursive=FALSE)
  
  # Set up empty an dataframe to hold all the file paths
  data_path <- data.frame(Sample=rep(NA, nrow(SDRF)), Path=rep(NA, 1),
                          stringsAsFactors=FALSE)
  
  # fill the dataframe by matching the files are listed in the filtered SDRF
  index<-1
  for (file in files) {
    sample_name <- str_extract(basename(file), "[^_]+")
    if (sample_name %in% SDRF$Sample) {
      data_path[index, 1] <- sample_name
      data_path[index, 2] <- file
      index = index + 1
    }
  }
  
  # If there are any empty spots, some error occured
  if (any(is.na(data_path))) {
    stop(base::paste("Please go back and check to make sure you", 
                     "selected the correct sample column and/or have", 
                     "the correct prefix and suffix"))
  }
  
  data_path <- S4Vectors::merge(x = SDRF, y = data_path, 
                                      by = "Sample", all = TRUE)
  
  return(data_path)
}

