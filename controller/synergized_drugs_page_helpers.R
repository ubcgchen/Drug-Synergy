build_bs_collapse <- function(length, drugs) {

  # Get information on each drug
  descriptions <- get_descriptions(drugs)
  
  # Download the structure for each drug
  for(description in descriptions) {
    destfile_name <- base::paste("www/", description$name[1],
                                 ".png", sep = "")
    print(file.exists(destfile_name))
    if (!file.exists(destfile_name)) {
      download.file(description$structure_link[1],
                    destfile = destfile_name)
    }
    }
  
  # Build the expression to generate drop-down panel
  code <- "bsCollapse(id = \"drug_panel\", open = NULL, multiple = FALSE,"
  for (i in 1:length) {
    code <- base::paste(code,
      "bsCollapsePanel(drugs[[",i,"]]$drug,",
      "HTML(\"<b>drug description:</b>\"),",
      "HTML(base::paste(descriptions[[",i,"]]$description, \"<br>\")),",
      "HTML(\"<b>pubchem id:</b>\"),",
      "HTML(base::paste(descriptions[[",i,"]]$pubchem_id, \"<br>\")),",
      "HTML(\"<b>structure:</b><br><br>\"),",
      "img(src = base::paste(drugs[[",i,"]]$drug, \".png\",sep = \"\")),",
      sep = "")
    
    if (!is.null(drugs[[i]]$ortho.score)) {
      code <- base::paste(code,
        "HTML(\"<br><b>synergy score:</b>\"),",
        "HTML(base::paste(drugs[[",i,"]]$ortho.score, \"<br>\")),",
        sep = "")
    }
    
    if (!is.null(drugs[[i]]$interaction.score)) {
      code <- base::paste(code, "HTML(\"<b>drug interaction scores and details:</b><br>\"),")
      
      for (index in 1:length(drugs[[i]]$interaction.score)) {
        drug_name <- names(drugs[[i]]$interaction.score[index])
        value <- drugs[[i]]$interaction.score[[index]]
        message <- drugs[[i]]$interaction.message[[index]]
        code <- base::paste(code,
                            "HTML(base::paste(\"&emsp;\",\"", as.character(drug_name), "\",\":\")),", sep = "")
        code <- base::paste(code,
                            "HTML(base::paste(\"<br>&emsp;&emsp;Tanimoto Coefficient:\",\"", value,"\")),", sep = "")
        code <- base::paste(code,
                            "HTML(base::paste(\"<br>&emsp;&emsp;Interaction Comments:\",\"", message,"\",\"<br>\")),", sep = "")
      }
      
    }

    code <- base::paste(code,
                        "style = \"default\"),",sep = "")
    
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
                            drug$drug,
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
