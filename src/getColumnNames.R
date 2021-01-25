getColumnNames <- function(url){
  url_ <- sprintf(
    "%s?$limit=10&$offset=0"
    , url)
  coln_ <- colnames(data.table(RSocrata::read.socrata(url = url_)))
  coln_ <- coln_[order(coln_)]
  coln_
}
