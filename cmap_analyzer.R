library(cmapR)
library(tidyverse)
library(dplyr)

pert_gctx <- "cmap_results/matrices/gutc/ps_pert_summary.gctx"

ds <- parse_gctx(pert_gctx)

mtrx <- as.data.frame(ds@mat) %>%
  rownames_to_column('id')
rdesc <- ds@rdesc
rdesc <- rdesc[ , !(names(rdesc) %in% c("pert_id"))]
merged <- merge(x = rdesc, y = mtrx, by = "id") %>%
  arrange(TAG) %>%
  dplyr::filter(pert_type == "trt_cp" | pert_type == "trt_lig") %>%
  dplyr::filter(TAG < 0)
merged <- merged[ , !(names(merged) %in% c("pert_type"))]

rm(ds)
rm(mtrx)
rm(rdesc)
rm(pert_gctx)

# Output: dataframe with columns id, pert_iname, pert_type, TAG
# - id is the pert_id field
# - pert_iname is the perturbagen name
# - pert_type is the perturbagen type. trt_cp = compound, trt_lig = peptides
# or other biologically active agents
# - TAG: CMap connectivity score (tau). <-90 = good for further study