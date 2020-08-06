# Output is dataframe (top_drugs) with columns: id, pert_iname, pert_type, TAG
# - id is the CMap perturbagen ID
# - pert_iname is the perturbagen name
# - pert_type is the perturbagen type. trt_cp = compound, trt_lig = peptides
#   or other biologically active agents
# - TAG: CMap connectivity score (tau). <0 = reverse query signature. <-90 
#   is significant enough for futher study

library(tidyverse)
library(dplyr)
library(compare)

process_cmap_data <- function(ds) {
  mtrx <- as.data.frame(ds@mat) %>%
    rownames_to_column('id')
  rdesc <- ds@rdesc
  rdesc <- rdesc[ , !(names(rdesc) %in% c("pert_id"))]
  
  # Order by tau score, only keep compounds and ligands, and only keep drugs
  # that reverse the signature (tau < 0)
  top_drugs <- S4Vectors::merge(x = rdesc, y = mtrx, by = "id") %>%
    arrange(TAG) %>%
    dplyr::filter(pert_type == "trt_cp" | pert_type == "trt_lig") %>%
    dplyr::filter(TAG < 0)
  return(top_drugs)
}