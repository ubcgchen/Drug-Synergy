render_synergized_drugs_page <- function(output, drugs) {
  
  source("pages/cmap_results_page.R")
  manage_cmap_results_page(shinyjs::hide)
  
  output$synergized_drug_tag <- renderUI({
    length <- length(drugs)-1
    if (length == 1) {
      h4(base::paste("Top", length, "synergized drug with reference to", 
                     drugs[[1]]$drug))
    } else {
      h4(base::paste("Top", length, "synergized drugs with reference to", 
                     drugs[[1]]$drug))
    }
  })
  
  output$synergized_drug_panel <- renderUI({
    source("synergized_drugs_page_helpers.R")
    build_bs_collapse(length(drugs), drugs)
  })
  
  output$cmap_results_back_button <- renderUI({
    actionButton("cmap_results_back_button","Back", 
                 icon("arrow-left", lib = "glyphicon"))
  })
  
  manage_synthesized_drugs_page(shinyjs::show)
  
  return(output)
}

manage_synthesized_drugs_page <- function(func) {
  func(id = "synergized_drug_panel")
  func(id = "synergized_drug_tag")
  func(id = "cmap_results_back_button")
}