download_AE_data <- function(accession_code) {
  
  # make sure an accession code is provided
  if(accession_code == "") {
    stop("You must provide an Accession Code!")
  }
  
  # create temporary directory to store data
  AE_data <- tempdir()
  if (!dir.exists(AE_data)) {
    dir.create(AE_data)
  }
  
  # clear temp directory
  files <- list.files(AE_data, full.names = T)
  file.remove(files)
  
  tryCatch(
    # download data from ArrayExpress
    getAE(accession_code, path = AE_data, type = "processed"),
    warning = function(cond) {
      stop("Please check your spelling and follow the suggested format")
    }
  )
  
  return(AE_data)
}