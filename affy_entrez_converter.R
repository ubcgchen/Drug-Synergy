affy_to_entrez <- function(tt, is_entrez) {
  # CMap takes Entrez IDs as input, LINCS takes gene symbol as input
  if (is_entrez) convert <- "ENTREZID"
  else convert <- "SYMBOL"
  
  affy_entrez_map <- AnnotationDbi::select(hgu133plus2.db, 
                                           row.names(tt), 
                                           c(convert)) %>%
    na.omit()
  
  tt <- rownames_to_column(tt, 'PROBEID')
  tt <- as.data.frame(inner_join(affy_entrez_map, tt))
  
  aggregate_list <- list()
  aggregate_list[[convert]] = tt[[convert]]

  tt <- aggregate(list(logFC = tt$logFC, AveExpr = tt$AveExpr, t = tt$t, 
                       P.Value = tt$ P.Value, 
                       adj.P.Val = tt$adj.P.Val, 
                       B = tt$B), 
                  by=aggregate_list, FUN=mean) %>%
    arrange(dplyr::desc(abs(t))) %>%
    column_to_rownames(convert)
  
  tt <- tt[ , !(names(tt) %in% c("PROBEID"))]
  
  return(tt)
}