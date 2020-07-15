render_cmap_results_page <- function(output) {
  shinyjs::hide(id = "positive_DEG_Tag")
  shinyjs::hide(id = "negative_DEG_Tag")
  shinyjs::hide(id = "positive_DEG")
  shinyjs::hide(id = "negative_DEG")
  shinyjs::hide(id = "confirm_DEG")
  
  output$top_drugs <- DT::renderDataTable({
    top_drugs
  })
  
  shinyjs::show(id = "top_drugs")
  
  return(output)
}