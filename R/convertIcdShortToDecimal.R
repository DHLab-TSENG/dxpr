#' Convert ICD Codes From Short To Decimal Forms
#'
#' Convert codes between short and decimal forms
#'
#' @import icd
#'
#' @param icdList ICD codes
#'
convertIcdShortToDecimal<-function(icdList){
  newicdList <- ifelse(!grepl("[.]",icdList), icd_short_to_decimal(icdList), icdList)
  return(newicdList)
}
