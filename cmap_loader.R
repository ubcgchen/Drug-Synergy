# Input is the final poll object from CMap containing all the request and 
# download information

library(cmapR)
library(tidyverse)


download_cmap_data <- function(poll) {
  # download and expand the data
  download.file(substring(poll$download_url, 3),
                destfile = "cmap_compressed.tar.gz")
  untar("cmap_compressed.tar.gz")
  return(poll$job_id)
}

unpack_cmap_data <- function(job_id) {
  # load the data file of interest (ps_pert_summary.gctx) into a data frame
  fname <- base::paste("my_analysis.sig_gutc_tool.", job_id, sep="")
  pert_gctx <- base::paste(fname, 
                           "/matrices/gutc/ps_pert_summary.gctx", sep = "")
  ds <- parse_gctx(pert_gctx)
  return(ds)
}

load_cmap_data <- function(poll) {
  job_id <- download_cmap_data(poll)
  ds <- unpack_cmap_data(job_id)
  return(ds)
}
