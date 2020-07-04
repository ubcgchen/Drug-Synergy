# Input is GMT files for up- and down-regulated genes (gene entrez IDs)
# - uptag.gmt are the up-regulated genes
# - dntag.gmt are the down-regulated genes

# Output is dataframe (top_drugs) with columns: id, pert_iname, pert_type, TAG
# - id is the CMap perturbagen ID
# - pert_iname is the perturbagen name
# - pert_type is the perturbagen type. trt_cp = compound, trt_lig = peptides
#   or other biologically active agents
# - TAG: CMap connectivity score (tau). <0 = reverse query signature. <-90 
#   is significant enough for futher study

library(cmapR)
library(tidyverse)
library(dplyr)
library(httr)
library(jsonlite)
library(config)

build_request_body <- function(query_name) {
  req_body <- list(
    tool_id="sig_gutc_tool",
    "uptag-cmapfile"=upload_file("uptag.gmt"),
    name = query_name,
    "dntag-cmapfile"=upload_file("dntag.gmt"),
    ignoreWarnings = "true",
    data_type = "L1000",
    dataset = "Touchstone"
  )
}

send_request <- function() {
  req <- httr::POST(url="https://api.clue.io/api/jobs",
                    httr::add_headers(user_key = config::get("api_key")),
                    httr::add_headers("Content-Type" = "multipart/form-data"),
                    body = req_body,
                    encode = "multipart")
}

poll_cmap <- function() {
  repeat {
    poll <- httr::GET(url=base::paste("http://api.clue.io/api/jobs/findByJobId/",
                                      req$result$job_id, sep = ""),
                      httr::add_headers(Accept = "application/json"),
                      httr::add_headers(user_key = config::get("api_key"))) %>%
      httr::content(as = 'text') %>% 
      jsonlite::fromJSON()
    if (!is.null(poll$download_status) && poll$download_status == "completed") break
    Sys.sleep(60)
  }
}

get_results <- function() {
  if (req$status_code == 200) {
    # Parse request content
    req <- httr::content(req, as = 'text') %>%
      jsonlite::fromJSON(req)
    poll_cmap()
  }
}

query_cmap <- function(query_name) {
  build_request_body(query_name)
  send_request()
  get_results()
}

download_cmap_data <- function() {
  download.file(substring(poll$download_url, 3),
                destfile = "cmap_compressed.tar.gz")
  untar("cmap_compressed.tar.gz")
}

load_cmap_data <- function() {
  fname <- base::paste("my_analysis.sig_gutc_tool.", req$result$job_id, sep="")
  pert_gctx <- base::paste(fname, "/matrices/gutc/ps_pert_summary.gctx", sep = "")
  ds <- parse_gctx(pert_gctx)
}

process_cmap_data <- function() {
  mtrx <- as.data.frame(ds@mat) %>%
    rownames_to_column('id')
  rdesc <- ds@rdesc
  rdesc <- rdesc[ , !(names(rdesc) %in% c("pert_id"))]
  top_drugs <- S4Vectors::merge(x = rdesc, y = mtrx, by = "id") %>%
    arrange(TAG) %>%
    dplyr::filter(pert_type == "trt_cp" | pert_type == "trt_lig") %>%
    dplyr::filter(TAG < 0)
}

clean_environment <- function() {
  rm(ds)
  rm(mtrx)
  rm(rdesc)
  rm(pert_gctx)
  rm(req)
  rm(req_body)
  rm(poll)
  rm(fname)
}

query_cmap("GSE66099_up150_dn150")
download_cmap_data()
load_cmap_data()
process_cmap_data()
clean_environment()
