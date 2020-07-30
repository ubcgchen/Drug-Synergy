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
  
  output$synergy_number <- renderUI({
    numericInput("synergy_number", 
                 "How many synergistic drugs would you like to find?", 
                 1, min = 1, max = 5)
  })
  
  output$reference_drug <- renderUI({
    textInput("reference_drug",
              "Reference drug (leave blank to default to top CMap drug)")
  })
  
  output$find_synergy <- renderUI({
    actionButton("find_synergy", "Find Synergistic Drug Combinations", 
                 icon("ok", lib = "glyphicon"))
  })
  
  manage_cmap_results_page(shinyjs::show)
  
  return(output)
}

manage_cmap_results_page <- function(func) {
  func(id = "top_drugs")
  func(id = "find_synergy")
  func(id = "top_table_back_button")
  func(id = "synergy_number")
  func(id = "reference_drug")
}