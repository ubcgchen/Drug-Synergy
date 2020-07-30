build_bs_collapse <- function(length, drugs) {
  
  descriptions <<- get_descriptions(drugs)

  if (length == 2) {
    
    for(description in descriptions) {
      download.file(description$structure_link,
                    destfile = base::paste("www/", description$name, 
                                           ".png", sep = ""))
    }
    
    ui_element <- 
      bsCollapse(id = "collapseExample", open = NULL, multiple = FALSE,
                 bsCollapsePanel(drugs[1],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[1]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[1]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[1], ".png", 
                                                       sep = "")),
                                 style = "default"),
                 bsCollapsePanel(drugs[2],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[2]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[2]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[2], ".png", 
                                                       sep = "")),
                                 style = "default"))
    
  } 
  else if(length == 3) {
    
    for(description in descriptions) {
      download.file(description$structure_link,
                    destfile = base::paste("www/", description$name, 
                                           ".png", sep = ""))
    }
    
    ui_element <- 
      bsCollapse(id = "collapseExample", open = NULL, multiple = FALSE,
                 bsCollapsePanel(drugs[1],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[1]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[1]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[1], ".png", 
                                                       sep = "")),
                                 style = "default"),
                 bsCollapsePanel(drugs[2],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[2]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[2]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[2], ".png", 
                                                       sep = "")),
                                 style = "default"),
                 bsCollapsePanel(drugs[3],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[3]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[3]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[3], ".png", 
                                                       sep = "")),
                                 style = "default"))
  } 
  else if(length == 4) {
    for(description in descriptions) {
      download.file(description$structure_link,
                    destfile = base::paste("www/", description$name, 
                                           ".png", sep = ""))
    }
    
    ui_element <- 
      bsCollapse(id = "collapseExample", open = NULL, multiple = FALSE,
                 bsCollapsePanel(drugs[1],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[1]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[1]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[1], ".png", 
                                                       sep = "")),
                                 style = "default"),
                 bsCollapsePanel(drugs[2],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[2]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[2]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[2], ".png", 
                                                       sep = "")),
                                 style = "default"),
                 bsCollapsePanel(drugs[3],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[3]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[3]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[3], ".png", 
                                                       sep = "")),
                                 style = "default"),
                 bsCollapsePanel(drugs[4],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[4]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[4]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[4], ".png", 
                                                       sep = "")),
                                 style = "default"))
  } 
  else if(length == 5) {
    for(description in descriptions) {
      download.file(description$structure_link,
                    destfile = base::paste("www/", description$name, 
                                           ".png", sep = ""))
    }
    
    ui_element <- 
      bsCollapse(id = "collapseExample", open = NULL, multiple = FALSE,
                 bsCollapsePanel(drugs[1],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[1]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[1]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[1], ".png", 
                                                       sep = "")),
                                 style = "default"),
                 bsCollapsePanel(drugs[2],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[2]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[2]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[2], ".png", 
                                                       sep = "")),
                                 style = "default"),
                 bsCollapsePanel(drugs[3],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[3]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[3]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[3], ".png", 
                                                       sep = "")),
                                 style = "default"),
                 bsCollapsePanel(drugs[4],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[4]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[4]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[4], ".png", 
                                                       sep = "")),
                                 style = "default"),
                 bsCollapsePanel(drugs[5],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[5]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[5]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[5], ".png", 
                                                       sep = "")),
                                 style = "default"))
  } 
  else if(length == 6) {
    for(description in descriptions) {
      download.file(description$structure_link,
                    destfile = base::paste("www/", description$name, 
                                           ".png", sep = ""))
    }
    
    ui_element <- 
      bsCollapse(id = "collapseExample", open = NULL, multiple = FALSE,
                 bsCollapsePanel(drugs[1],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[1]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[1]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[1], ".png", 
                                                       sep = "")),
                                 style = "default"),
                 bsCollapsePanel(drugs[2],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[2]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[2]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[2], ".png", 
                                                       sep = "")),
                                 style = "default"),
                 bsCollapsePanel(drugs[3],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[3]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[3]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[3], ".png", 
                                                       sep = "")),
                                 style = "default"),
                 bsCollapsePanel(drugs[4],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[4]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[4]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[4], ".png", 
                                                       sep = "")),
                                 style = "default"),
                 bsCollapsePanel(drugs[5],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[5]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[5]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[5], ".png", 
                                                       sep = "")),
                                 style = "default"),
                 bsCollapsePanel(drugs[6],
                                 HTML("<b>drug description:</b>"),
                                 HTML(base::paste(descriptions[[6]]$description, "<br>")),
                                 HTML("<b>pubchem id:</b>"),
                                 HTML(descriptions[[6]]$pubchem_id, "<br>"),
                                 HTML("<b>structure:</b><br><br>"),
                                 img(src = base::paste(drugs[6], ".png", 
                                                       sep = "")),
                                 style = "default"))
  }
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
    
    description = list(description = res$description, 
                       pubchem_id = res$pubchem_cid, 
                       structure_link = res$structure_url,
                       name = res$pert_iname)

    descriptions <- append(descriptions, list(description))
  }
  
  return(descriptions)
}
