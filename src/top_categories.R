top_categories_multiple <- function(d, cat1 = NULL, cats, var, top = 3){
  if(is.null(cat1)){
    d$Total <- "*TOTAL*"
    cat1 = "Total"
  } 
  x = top_categories(d = d, cat1 = cat1, cat2 = cats[1], var = var, top = top)
  if(length(cats) > 1){
    for (i in 2:length(cats)) {
      x <- merge.data.table(
        x = x
        , y = top_categories(d = d, cat1 = cat1, cat2 = cats[i], var = var, top = top)
        , by = cat1
        , all = T)
    }
  }
  x
}

top_categories <- function(d, cat1, cat2, var, top = 3){
  d <- data.table(d)
  d$cat1 <- d[[cat1]]
  d$cat2 <- d[[cat2]]
  d$var <- d[[var]]
  tmp <- d[!is.na(cat1) & !is.na(cat2), .(
    Variable = sum(var, na.rm = T)
  ), keyby = .(cat1, cat2)][order(Variable, decreasing = T)]
  tmp <- tmp[, .(`Top` = top_(cat2, Variable, top))
             , keyby = cat1]
  setkey(tmp, "cat1")
  setnames(x = tmp, old = "cat1", new = cat1)
  setnames(x = tmp, old = "Top", new = paste0("Top ", var, " by ", cat2))
  tmp
}
top_ <- function(x, y, top = 3){
  paste0(head(x, top), " (", round(100*head(y, top)/sum(y), 1), "%)", collapse = ", ")
}
