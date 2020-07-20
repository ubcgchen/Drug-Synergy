# Input is GMT files for up- and down-regulated genes (gene entrez IDs)
# - uptag.gmt are the up-regulated genes
# - dntag.gmt are the down-regulated genes

library(cmapR)
library(tidyverse)
library(dplyr)
library(httr)
library(jsonlite)
library(config)

build_request_body <- function(query_name) {
  # Set POST request body settings
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
  # Build CMap POST request and send it
  req <- httr::POST(url="https://api.clue.io/api/jobs",
                    httr::add_headers(user_key = config::get("api_key")),
                    httr::add_headers("Content-Type" = "multipart/form-data"),
                    body = req_body,
                    encode = "multipart")
  return(req)
}

poll_cmap <- function(req) {
  flag <- FALSE
  repeat {
    tryCatch({
      # Poll CMap for completion every 2 minutes
      poll <- httr::GET(url=base::paste("http://api.clue.io/api/jobs/findByJobId/",
                                        req$result$job_id, sep = ""),
                        httr::add_headers(Accept = "application/json"),
                        httr::add_headers(user_key = config::get("api_key"))) %>%
        httr::content(as = 'text') %>% 
        jsonlite::fromJSON()
      
      # Exit loop if job finishes
      if (job_is_completed(poll)) break
      
      # Wait 20 mins before first poll, then 120 seconds between polls
      if (flag) {
        Sys.sleep(120)
      } else {
        Sys.sleep(1200)
        flag <- TRUE
      }
    },
    error = function(cond) {
     # silently ignore error and try again (errors usually due to network)
    },
    warning = function(cond) {
      #silently ignore warning and try again (warnings usually due to network)
    })
  }
  return(poll)
}

job_is_completed <- function(poll) {
  return(!is.null(poll$download_status) && poll$download_status == "completed")
}

get_results <- function(req) {
  if (req$status_code == 200) {
    # Parse request content
    req <- httr::content(req, as = 'text') %>%
      jsonlite::fromJSON(req)
    
    # Poll CMap until the job completes
    poll_cmap(req)
  }
}

query_cmap <- function(query_name) {
  req_body <- build_request_body(query_name)
  req <- send_request(req_body)
  get_results(req)
}

