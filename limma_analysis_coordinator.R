coordinate_analysis <- function(user_filters, num_upgenes, num_downgenes,
                                conditions, controls) {
  
  source("caches/analysis_cache.R")
  res <- check_cache(user_filters, num_upgenes, num_downgenes,
                    conditions, controls)
  
  if (is.null(res)) {
    # filter SDRF based on user-specified condition/control levels
    source("SDRF_loader.R")
    filtered_SDRF <- filter_sdrf(updated_SDRF, conditions, controls)
    
    # generate file paths for all data files
    source("file_path_loader.R")
    file_paths <- get_file_paths(AE_data, filtered_SDRF)
    
    # generate matrices
    source("matrices_generator.R")
    matrices <- build_matrices(file_paths)
    expression_matrix <- matrices$expression_matrix
    design_matrix <- matrices$design_matrix
    
    # Do analysis
    source("tt_generator.R")
    res <- do_analysis(expression_matrix, design_matrix, user_filters,
                       num_upgenes, num_downgenes)
    
    source("caches/analysis_cache.R")
    add_to_cache(user_filters, num_upgenes, 
              num_downgenes, conditions, controls, res)
  }
  
  source("write_up_dn_files.R")
  write_gmt_files(res$positive_DEG, res$negative_DEG)
  
  return(res)
}
