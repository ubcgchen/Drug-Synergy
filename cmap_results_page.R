render_cmap_results_page <- function(output) {
  
  source("top_table_page.R")
  manage_top_table_page(shinyjs::hide)
  
  output$top_drugs <- DT::renderDataTable({
    top_drugs
  })
  
  output$top_table_back_button <- renderUI({
    actionButton("top_table_back_button", "Back", 
                 icon("arrow-left", lib = "glyphicon"))
  })
  
  manage_cmap_results_page(shinyjs::show)
  
  return(output)
}

manage_cmap_results_page <- function(func) {
  func(id = "top_drugs")
  func(id = "top_table_back_button")
}