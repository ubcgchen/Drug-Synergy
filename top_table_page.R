render_top_table_page <- function(output, positive_DEG, negative_DEG, 
                                  num_upgenes, num_downgenes) {
  
  source("level_selection_page.R")
  manage_level_selection_page(shinyjs::hide)

  output$positive_DEG_Tag <- renderUI({
    h4(base::paste("Top", num_upgenes, "up-regulated genes"))
  })
  
  output$positive_DEG <- DT::renderDataTable({
    positive_DEG
  })
  
  output$negative_DEG_Tag <- renderUI({
    h4(base::paste("Top", num_downgenes, "down-regulated genes"))
  })
  
  output$negative_DEG <- DT::renderDataTable({
    negative_DEG
  })
  
  output$confirm_DEG <- renderUI({
    actionButton("confirm_DEG","Submit Query to CMap",
                 icon("ok", lib = "glyphicon"))
  })
  
  output$level_selection_back_button <- renderUI({
    actionButton("level_selection_back_button", "Back", 
                 icon("arrow-left", lib = "glyphicon"))
  })
  
  manage_top_table_page(shinyjs::show)
}

manage_top_table_page <- function (func) {
  func(id = "positive_DEG_Tag")
  func(id = "positive_DEG")
  func(id = "negative_DEG_Tag")
  func(id = "negative_DEG")
  func(id = "confirm_DEG")
  func(id = "level_selection_back_button")
}
