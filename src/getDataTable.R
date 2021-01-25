getDataTable <- function(url, select = NULL, where = "", offset = 0, limit = 1000){
  # Función que extrae los datos de SOCRATA
  # El where debe incluir el "&" al inicio
  # Ejemplo: where = "departamento_entidad=Huila"
  if(is.null(select)){
    select_ <- sprintf("$select=%s",paste(getColumnNames(url), collapse = ","))
  } else {
    select_ <- sprintf("$select=%s",paste(select, collapse = ","))
  }
  limit_ <- sprintf("$limit=%s", limit)
  offset_ <- sprintf("$offset=%s", offset)
  url_ <- sprintf(
    "%s?%s&%s&%s%s"
    , url
    , select_
    , limit_
    , offset_
    , where
  )
  d <- data.table(RSocrata::read.socrata(url = url_))
  d
}
