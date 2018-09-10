#' Convert ICD Codes From Short To Decimal Forms
#'
#' Convert codes between short and decimal forms
#'
#' @import dplyr
#'
#' @param icdList ICD codes
#' @param icdVer ICD Version: ICD-9 or ICD-10
#'
convertIcdShortToDecimal<-function(icdList, icdVer){
  icdVer <- tolower(deparse(substitute(icdVer)))
  if(icdVer =="icd9"){
    icdVerMap <-`icd9withTwoFormat`
  }else{
    icdVerMap <- `icd10withTwoFormat`
  }
  convertIcd <- left_join(data.frame(Short = icdList, stringsAsFactors = FALSE), icdVerMap, by = "Short")
  newicdList <- ifelse(!grepl("[.]",icdList), convertIcd$Decimal[!is.na(convertIcd)], icdList)
  newicdList
}
