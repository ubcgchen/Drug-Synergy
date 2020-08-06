library(rlist)

if (!exists("analysis_cache")) {
  analysis_cache <- list()
}

check_analysis_cache <- function(user_filters, num_upgenes, 
                        num_downgenes, conditions, controls) {
  
  for (item in analysis_cache) {
    if (lists_are_equal(user_filters, item$filters) && 
        item$up == num_upgenes && item$down == num_downgenes && 
        all(item$conditions == conditions, TRUE) && 
        all(item$controls == controls, TRUE)) {
      return(item$res)
    }
  }
  
  return(NULL)
}

add_to_analysis_cache <- function(user_filters, num_upgenes, 
                      num_downgenes, conditions, controls, res) {
  analysis_cache <<- list.append(analysis_cache, list(filters = user_filters, 
                                                      up = num_upgenes, 
                                                      down = num_downgenes, 
                                                      conditions = conditions, 
                                                      controls = controls, 
                                                      res = res))
}

clear_analysis_cache <- function() {
  # clear cache and collect garbage
  analysis_cache <<- list()
  gc()
}

# Checks if lists are the same
lists_are_equal <- function(list1, list2) {
  if (is_empty(list1) && is_empty(list2)) return(TRUE)
  if (length(list1) != length(list2)) return(FALSE)
  
  for (index in 1:length(list1)) {
    if(!all(list1[[index]] == list2[[index]], TRUE)) return(FALSE)
  }
  
  return(TRUE)
}

