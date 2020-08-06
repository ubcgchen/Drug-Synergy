render_filters <- function(input, output, filters) {
  
  output$newInputs <- renderUI({
    # for every filter
    lapply(filters$input_filters, function(curr) {
      tagList(
        selectInput(inputId = base::paste(curr[1], "column", sep = ""),
                    label = "",
                    selected = input[[base::paste(curr, "column", sep = "")]],
                    choices = c("logFC", "AveExpr", "t", "P.Value",
                                "adj.P.Val", "B"),
                    multiple = F),
        selectInput(inputId = base::paste(curr[1], "comparator", sep = ""), 
                    label = "",
                    selected = input[[base::paste(curr, "comparator", sep = "")]],
                    choices = c(">", ">=", "<", "<=", "="),
                    multiple = F),
        textInput(inputId = base::paste(curr[1], "value", sep = ""),
                  value = input[[base::paste(curr, "value", sep = "")]],
                  label = ""),
        HTML("<br>")
      )
    })
  })
  
  return(output)
}