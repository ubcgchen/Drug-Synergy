if (!exists("cmap_cache")) {
  cmap_cache = list()
}

check_cmap_cache <- function(positive_DEG, negative_DEG) {
  
  for (index in 1:length(cmap_cache)) {
    item <- cmap_cache[[index]]
    if (compare(item$positive_DEG, positive_DEG)$result && 
        compare(item$negative_DEG, negative_DEG)$result) {
      if (file.exists(base::paste("my_analysis.sig_gutc_tool.", item$job_info$id, sep=""))) {
        return(item$job_info)
      } else {
        cmap_cache[[index]] <- NULL
      }
    }
  }
  
  return(NULL)
}

add_to_cmap_cache <- function(positive_DEG, negative_DEG, job_info) {
  cmap_cache <<- list.append(cmap_cache, list(positive_DEG = positive_DEG,
                                              negative_DEG = negative_DEG,
                                              job_info = job_info))
}

clear_cmap_cache <- function() {
  # clear cache and collect garbage
  cmap_cache() <<- list()
  gc()
}