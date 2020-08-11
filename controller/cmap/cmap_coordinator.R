library(compare)

coordinate_cmap <- function(accession_code, positive_DEG, negative_DEG) {

  upreg_length <- nrow(positive_DEG)
  downreg_length <- nrow(negative_DEG)
  
  source("caches/cmap_cache.R")
  job_info <- check_cmap_cache(positive_DEG, negative_DEG)

  if (is.null(job_info)) {
    source("controller/cmap/cmap_querier.R")
    poll <- query_cmap(base::paste(accession_code, "_up", upreg_length,
                                   "_dn", downreg_length, sep = ""))
    job_info <- list(id = poll$job_id, link = poll$download_url)
    add_to_cmap_cache(positive_DEG, negative_DEG, job_info)
  }

  source("controller/cmap/cmap_loader.R")
  ds <- load_cmap_data(job_info$id)
  source("controller/cmap/cmap_processor.R")
  top_drugs <<- process_cmap_data(ds)
}
