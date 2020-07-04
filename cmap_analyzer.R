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
  return(req_body)
}

send_request <- function(req_body) {
  req <- httr::POST(url="https://api.clue.io/api/jobs",
                    httr::add_headers(user_key = config::get("api_key")),
                    httr::add_headers("Content-Type" = "multipart/form-data"),
                    body = req_body,
                    encode = "multipart")
  return(req)
}

poll_cmap <- function(req) {
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
  return(poll)
}

get_results <- function(req) {
  if (req$status_code == 200) {
    # Parse request content
    req <- httr::content(req, as = 'text') %>%
      jsonlite::fromJSON(req)
    poll_cmap(req)
  }
}

query_cmap <- function(query_name) {
  req_body <- build_request_body(query_name)
  req <- send_request(req_body)
  get_results(req)
}

download_cmap_data <- function(poll) {
  download.file(substring(poll$download_url, 3),
                destfile = "cmap_compressed.tar.gz")
  untar("cmap_compressed.tar.gz")
  return(poll$job_id)
}

load_cmap_data <- function(job_id) {
  fname <- base::paste("my_analysis.sig_gutc_tool.", job_id, sep="")
  pert_gctx <- base::paste(fname, "/matrices/gutc/ps_pert_summary.gctx", sep = "")
  ds <- parse_gctx(pert_gctx)
  return(ds)
}

process_cmap_data <- function(ds) {
  mtrx <- as.data.frame(ds@mat) %>%
    rownames_to_column('id')
  rdesc <- ds@rdesc
  rdesc <- rdesc[ , !(names(rdesc) %in% c("pert_id"))]
  top_drugs <- S4Vectors::merge(x = rdesc, y = mtrx, by = "id") %>%
    arrange(TAG) %>%
    dplyr::filter(pert_type == "trt_cp" | pert_type == "trt_lig") %>%
    dplyr::filter(TAG < 0)
  return(top_drugs)
}

poll <- query_cmap("GSE66099_up150_dn150")
download_cmap_data(poll)
ds <- load_cmap_data(poll$job_id)
top_drugs <- process_cmap_data(ds)

rm(ds)
rm(poll)
