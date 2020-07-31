if (!exists("synergize_cache")) {
  synergize_cache = list()
}

check_synergize_cache <- function(drug_name) {
  
  for (item in synergize_cache) {
    if (item$name == drug_name) {
      return(item$sig)
    }
  }
  
  return(NULL)
}

add_to_synergize_cache <- function(drug_name, sig) {
  synergize_cache <<- list.append(synergize_cache, list(name = drug_name, 
                                                      sig = sig))
}

clear_synergize_cache <- function() {
  # clear cache and collect garbage
  synergize_cache() <<- list()
  gc()
}