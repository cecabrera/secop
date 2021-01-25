require(RSocrata)
require(data.table)

for(file_ in list.files(path = "src", full.names = T, recursive = T)){
  source(file = file_, local = T, encoding = "utf-8")
}

# Socrata documentation for queries
# https://dev.socrata.com/docs/queries/

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

secop1 <- "https://www.datos.gov.co/Gastos-Gubernamentales/SECOP-I/xvdy-vvsk"
secop2_pc <- "https://www.datos.gov.co/Gastos-Gubernamentales/SECOP-II-Procesos-de-Contrataci-n/p6dx-8zbt"
secop2_ce <- "https://www.datos.gov.co/Gastos-Gubernamentales/SECOP-II-Contratos-Electr-nicos/jbjy-vk9h"
secop <- "https://www.datos.gov.co/Gastos-Gubernamentales/SECOP-Integrado/rpmr-utcd"
tienda <- "https://www.datos.gov.co/Gastos-Gubernamentales/Tienda-Virtual-del-Estado-Colombiano-Consolidado/rgxm-mmea"

getColumnNames(secop1)
getColumnNames(secop2_pc)
getColumnNames(secop2_ce)
getColumnNames(secop)
getColumnNames(tienda)

url <- secop2_pc
col_ <- getColumnNames(url)


where <- list(
  "departamento_entidad" = "Huila"
  , "adjudicado" = "No"
  , "estado_de_apertura_del_proceso" = "Abierto"
  , "descripci_n_del_procedimiento" = "%25datos%25"
  , "nombre_del_procedimiento" = "%25datos%25"
  , "fecha_de_publicacion_del" = "> '2021-01-10T00:00:00'"
  , 'estado_del_procedimiento'
)
d <- getDataTable(
  url = url
  , where = "&adjudicado=No&estado_de_apertura_del_proceso='Abierto'&$q='datos'&$order=fecha_de_publicacion_del DESC"
  , select = col_
  # , limit = 1000
  )
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
