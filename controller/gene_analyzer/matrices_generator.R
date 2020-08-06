build_expression_matrix <- function(file_paths) {
  # split dataframe into a list of condition and a list of control
  grouped_df <- split(file_paths, file_paths$Condition)

  # ASSUMPTION: all files will have the same number of rows
  first_file <- read.delim(grouped_df$control$Path[1], header = TRUE)
  file_len <- nrow(first_file)
  
  # Make an empty dataframe to hold the expression data
  expression_matrix <- data.frame()[1:file_len, ]
  
  # For each path of each condition (condition + control), load the data in.
  for (condition in c("control", "condition")) {
    cur_df <- grouped_df[[condition]]
    index <- 1
    for (path in cur_df$Path) {
      # load expression data into new placeholder column
      expression_matrix$placeholder <-
        as.numeric(unlist(read.delim(path, header = TRUE)[2]))
      
      # rename placeholder column to sample name
      names(expression_matrix)[names(expression_matrix) == "placeholder"] <-
        cur_df$Sample[index]
      
      index <- index+1
    }
  }
  
  # ASSUMPTION: all files will have probe IDs in the same order
  rownames(expression_matrix) <- as.character(c(first_file$ID_REF))
  
  return_val <- list(expression_matrix = expression_matrix, 
                     control_len = nrow(grouped_df$control),
                     condition_len = nrow(grouped_df$condition))
  
  return(return_val)
}

memoised_expression_mtrx <- memoise(build_expression_matrix)

build_design_matrix <- function(control, condition) {
  return(cbind(1 , c(rep(0 , control) , rep(1 , condition))))
}

build_matrices <- function(file_paths) {
  res <- memoised_expression_mtrx(file_paths)
  design_matrix <- build_design_matrix(res$control_len, res$condition_len)
  
  return(list(expression_matrix = res$expression_matrix,
              design_matrix = design_matrix))
}

