affy_to_entrez <- function(tt) {
  affy_entrez_map <- AnnotationDbi::select(hgu133plus2.db, 
                                           row.names(tt), 
                                           c("ENTREZID")) %>%
    na.omit()
  
  tt <- rownames_to_column(tt, 'PROBEID')
  tt <- as.data.frame(inner_join(affy_entrez_map, tt))
  tt <- aggregate(list(logFC = tt$logFC, AveExpr = tt$AveExpr, t = tt$t, 
                       P.Value = tt$ P.Value, 
                       adj.P.Val = tt$adj.P.Val, 
                       B = tt$B), 
                  by=list(ENTREZID = tt$ENTREZID), FUN=mean) %>%
    arrange(dplyr::desc(abs(t))) %>%
    column_to_rownames('ENTREZID')
  tt <- tt[ , !(names(tt) %in% c("PROBEID"))]
  return(tt)
}