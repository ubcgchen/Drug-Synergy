build_bs_collapse <- function(length, drugs) {
  
  # Get information on each drug
  descriptions <<- get_descriptions(drugs)
  
  # Download the structure for each drug
  for(description in descriptions) {
      download.file(description$structure_link,
                    destfile = base::paste("www/", description$name,
                                           ".png", sep = ""))
    }
  
  # Build the expression to generate drop-down panel
  code <- "bsCollapse(id = \"drug_panel\", open = NULL, multiple = FALSE,"
  for (i in 1:length) {
    code <- c(code, base::paste(
      "bsCollapsePanel(drugs[",i,"],",
                      "HTML(\"<b>drug description:</b>\"),",
                      "HTML(base::paste(descriptions[[",i,"]]$description, \"<br>\")),",
                      "HTML(\"<b>pubchem id:</b>\"),",
                      "HTML(descriptions[[1]]$pubchem_id, \"<br>\"),",
                      "HTML(\"<b>structure:</b><br><br>\"),",
                      "img(src = base::paste(drugs[",i,"], \".png\",sep = \"\")),",
                      "style = \"default\"),", sep = ""))
  }

  # Formate and evaluate the expression
  code <- base::paste(code, collapse = " ")
  code <- sub(",([^,]*)$", ")\\1", code)
  ui_element <- eval(parse(text=code))

  return(ui_element)
}

get_descriptions <- function(drugs) {
  
  descriptions = list()
  
  for (drug in drugs) {
    pert_url <- base::paste("https://api.clue.io/api/perts?filter={\"where\":",
                            "{\"pert_iname\":\"",
                            drug,
                            "\"}}&user_key=",
                            config::get("api_key"),
                            sep = "")
    
    res <- httr::GET(pert_url) %>%
      httr::content(as = 'text') %>%
      jsonlite::fromJSON()
    
    # Get the drug description, pubchem id, structure, and name
    description = list(description = res$description, 
                       pubchem_id = res$pubchem_cid, 
                       structure_link = res$structure_url,
                       name = res$pert_iname)

    descriptions <- append(descriptions, list(description))
  }
  
  return(descriptions)
}
