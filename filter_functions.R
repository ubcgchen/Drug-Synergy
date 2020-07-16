get_func <- function(comparator) {
  if (comparator == "<") return(less_than)
  else if (comparator == "<=") return(leq_than)
  else if (comparator == ">") return(greater_than)
  else if (comparator == ">=") return(geq_than)
  else if (comparator == "=") return(eq_to)
  
}

less_than <- function(val, reference) {
  return (abs(val) < reference)
}

leq_than <- function(val, reference) {
  return (abs(val) <= reference)
}

greater_than <- function(val, reference) {
  return (abs(val) > reference)
}

geq_than <- function(val, reference) {
  return (abs(val) >= reference)
}

eq_to <- function(val, reference) {
  return (abs(val) == reference)
}


