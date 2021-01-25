require(RSocrata)
require(data.table)

source(file = "src/top_categories.R")

# Socrata documentation for queries
# https://dev.socrata.com/docs/queries/

url <- 
# https://socratadiscovery.docs.apiary.io/#reference
result_objects <- c(
  "resource"
  , "classification"
  , "metadata"
  , "permalink"
  , "link"
  , "preview_image_url"
  , "owner"
  , "published_copy"
  )
  
RSocrata::validateUrl(url = "https://www.datos.gov.co/Gastos-Gubernamentales/SECOP-I/xvdy-vvsk", app_token = NULL)
RSocrata::ls.socrata("https://www.datos.gov.co/Gastos-Gubernamentales/SECOP-I/xvdy-vvsk")

# Extraer los nombres de las columnas del link
getColumnNames <- function(url){
  url_ <- sprintf(
    "%s?$limit=10&$offset=0"
    , url)
  coln_ <- colnames(data.table(RSocrata::read.socrata(url = url_)))
  coln_ <- coln_[order(coln_)]
  coln_
}

secop1 <- "https://www.datos.gov.co/Gastos-Gubernamentales/SECOP-I/xvdy-vvsk"
secop2_pc <- "https://www.datos.gov.co/Gastos-Gubernamentales/SECOP-II-Procesos-de-Contrataci-n/p6dx-8zbt"
secop2_ce <- "https://www.datos.gov.co/Gastos-Gubernamentales/SECOP-II-Contratos-Electr-nicos/jbjy-vk9h"
secop <- "https://www.datos.gov.co/Gastos-Gubernamentales/SECOP-Integrado/rpmr-utcd"
tienda <- "https://www.datos.gov.co/Gastos-Gubernamentales/Tienda-Virtual-del-Estado-Colombiano-Consolidado/rgxm-mmea"

url <- secop2_pc

getColumnNames(secop1)
getColumnNames(secop2_pc)
getColumnNames(secop2_ce)
getColumnNames(secop)
getColumnNames(tienda)

getData <- function(url, select = NULL, where = "", offset = 0, limit = 1000){
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

where <- list(
  "departamento_entidad" = "Huila"
  , "adjudicado" = "No"
  , "estado_de_apertura_del_proceso" = "Abierto"
)
col_ <- getColumnNames(url)
d <- getData(
  url = url
  , where = "&departamento_entidad=Huila&adjudicado=No&estado_de_apertura_del_proceso=Abierto"
  , select = col_)
for(col_ in colnames(d)){
  print(d[,.N, col_][order(N, decreasing = T)])
}

d[
  grepl(x = descripci_n_del_procedimiento, pattern = "datos", ignore.case = T)
  , .(descripci_n_del_procedimiento, precio_base, duracion, urlproceso)
  ]


# TODO: Ordenarlos por entidad con mejor índice de transparencia


colnames(client)
logic <- client$nombre_de_la_entidad %in% "HUILA - GOBERNACIÓN" & client$tipo_de_contrato %in% "Prestación de Servicios" & client$nombre_familia %in% "Servicios de recursos humanos"
x <- client[logic, .(
  .N
  , Valor = sum(valor_contrato_con_adiciones)/1000000
  ), .(nom_raz_social_contratista)][order(Valor, decreasing = T)]
y <- top_categories_multiple(
  d = client[logic,]
  , cat1 = "nom_raz_social_contratista"
  , cats = c("objeto_a_contratar", "tipo_de_contrato", "municipio_entidad", "nombre_familia", "nivel_entidad")
  , var = 'valor_contrato_con_adiciones', top = 1)
proveedores <- merge(x = x, y = y, by = "nom_raz_social_contratista", all = T)
proveedores <- proveedores[order(Valor, decreasing = T)]
head(proveedores, 100)
