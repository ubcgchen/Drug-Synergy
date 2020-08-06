library(cmapR)

write_gmt_files <- function(positive_DEG, negative_DEG) {
  TAG_DN <- list()
  TAG_DN$head = "TAG_DN"
  TAG_DN$desc = ""
  TAG_DN$entry = row.names(negative_DEG)
  TAG_DN$len = length(row.names((negative_DEG)))
  to_write <- list()
  to_write$TAG_DN <- TAG_DN
  write_gmt(to_write, "dntag.gmt")
  
  TAG_UP <- list()
  TAG_UP$head = "TAG_UP"
  TAG_UP$desc = ""
  TAG_UP$entry = row.names(positive_DEG)
  TAG_UP$len = length(row.names((positive_DEG)))
  to_write <- list()
  to_write$TAG_UP <- TAG_UP
  write_gmt(to_write, "uptag.gmt")
}
