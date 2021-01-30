library('RSelenium')
library(rebus)

scraping <- function(url, remDr, ind){
  
  if (ind == 1){
    remDr$navigate(url)
    print("----ReCaptcha----")
    # Sys.sleep(120)
  } else{
    remDr$navigate(url)
  }
  
  print("----Get title value ----")
  webElem <- remDr$findElement(using = "xpath", value = '//*[@id="fdsRequestSummaryInfo_tblDetail_trRowName_tdCell2_spnRequestName"]')
  
  titleVal <- webElem$getElementText()[[1]]
  
  count=0
  print("----Procedure type---")
  # typePro <- remDr$findElement(using = "xpath", value = '//*[@id="fdsRequestSummaryInfo_tblDetail_trRowProcedureType_tdCell2_spnProcedureType"]')
  # typePro <- typePro$getElementText()[[1]]
  # if(typePro == "Contratación directa."){
  #   count=count+1
  # }
  
  print("--- Get contract date ---")
  
  show_condition <- function(code) {
    tryCatch(code,
             error = function(c) "error",
             warning = function(c) "warning",
             message = function(c) "message"
    )
  }
  
  getDates <- function(xpath){
    show_condition(remDr$findElement(using = "xpath", value = xpath))
    
  }
  
  dates_ <- getDates('//*[@id="dtmbScheduleDateTime_48_txt"]')
  
  if(typeof(dates_) !="character"){
    dates_ <- strsplit(dates_$getElementText()[[1]], " ")
    
    if (!is.na(as.Date(dates_[[1]][1], format = "%d/%m/%Y"))){
      count = count+1
    }
  }
  
  print("----getTables---")
  webElem2 <- remDr$findElement(using = "xpath", value = '//*[@id="grdGridDocumentList_tbl"]')
  
  rowsList <- webElem2$findChildElements(using = "tag name", value = "td")
  # rowsList[[1]]$getElementText()
  
  lista <- c()
  
  print("---Get rows data ----")
  for (i in 1:length(rowsList)){
    
    t <- rowsList[[i]]$getElementText()[[1]]
    lista <- c(lista,t)
  }
  
  print("--- check contract availab. ----")
  for (i in lista){
    if (grepl(pattern = zero_or_more(any_char()) %R% or("contrata", "firma", titleVal) %R% zero_or_more(any_char()), x = i, ignore.case = T)){
      count=count+1
    }
  }
  
  count
  
  # remDr$close()
  
}



