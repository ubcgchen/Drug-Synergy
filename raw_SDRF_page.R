render_raw_SDRF_page <- function(input, output) {
  shinyjs::disable("geo")

  output$dropdown_sample <- renderUI({
    selectInput(inputId = "select_sample", 
                label = "Select the column containing sample names", 
                choices = names(SDRF),
                multiple = F)
  })
  
  output$dropdown_condition <- renderUI({
    selectInput(inputId = "select_condition", 
                label = "Select the column containing the condition", 
                choices = names(SDRF),
                multiple = F)
  })
  
  output$text_prefix <- renderUI({
    textInput(inputId = "text_prefix",
              label = "The SDRF file names may include unnecessary prefixes or suffixes. Below, enter the prefixes and suffixes you would like ignored during analysis",
              placeholder = "prefix")
  })
  
  output$text_suffix <- renderUI({
    textInput(inputId = "text_suffix",
              label = "",
              placeholder = "suffix")
  })
  
  output$update_sdrf_table <- renderUI({
    actionButton("sdrf","Update SDRF", icon("refresh", lib = "glyphicon"))
  })
  
  output$raw_SDRF <- DT::renderDataTable({
    SDRF
  })
  
  return(output)
}