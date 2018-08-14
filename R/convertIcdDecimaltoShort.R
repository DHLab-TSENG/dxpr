#' Convert ICD Codes From Decimal To Short Forms
#'
#' Convert codes between short and decimal forms
#'
#' @import icd
#'
#' @param icdList ICD codes
#'
convertIcdDecimaltoShort<-function(icdList){
  newicdList <- ifelse(grepl("[.]",icdList), icd_decimal_to_short(icdList), icdList)
  return(newicdList)
}
