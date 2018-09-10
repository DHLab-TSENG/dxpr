if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "icd9withTwoFormat",
  "icd10withTwoFormat"))
#' Convert ICD Codes From Decimal To Short Forms
#'
#' Convert codes between short and decimal forms
#'
#' @import dplyr
#'
#' @param icdList ICD codes
#' @param icdVer ICD Version: ICD-9 or ICD-10
#'
convertIcdDecimaltoShort<-function(icdList, icdVer){
  icdVer <- tolower(deparse(substitute(icdVer)))
  if(icdVer =="icd9"){
    icdVerMap <-`icd9withTwoFormat`
  }else{
    icdVerMap <- `icd10withTwoFormat`
  }
  convertIcd <- left_join(data.frame(Decimal = icdList, stringsAsFactors = FALSE), icdVerMap, by = "Decimal")
  newicdList <- ifelse(grepl("[.]",icdList), convertIcd$Short[!is.na(convertIcd)], icdList)
  newicdList
}
