render_top_table_page <- function(output, positive_DEG, negative_DEG) {
  shinyjs::hide(id = "select_control_level")
  shinyjs::hide(id = "select_condition_level")
  shinyjs::hide(id = "submit_levels")
  shinyjs::hide(id = "raw_sdrf_back_button")
  
  output$positive_DEG_Tag <- renderUI({
    HTML(base::paste('<br/>', '<br/>', '<br/>','<br/>', '<br/>', '<br/>', '<br/>','<br/>',
                     h4("Top 150 up-regulated genes")))
  })
  
  output$positive_DEG <- DT::renderDataTable({
    positive_DEG
  })
  
  output$negative_DEG_Tag <- renderUI({
    HTML(base::paste('<br/>', '<br/>',
                     h4("Top 150 down-regulated genes")))
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
  
  shinyjs::show(id = "positive_DEG_Tag")
  shinyjs::show(id = "positive_DEG")
  shinyjs::show(id = "negative_DEG_Tag")
  shinyjs::show(id = "negative_DEG")
  shinyjs::show(id = "confirm_DEG")
  shinyjs::show(id = "level_selection_back_button")
}