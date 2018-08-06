#' Get the PheWAS Phecode/ description
#'
#' This can be used to select the PheWAS Phecode/ description
#' based on ICD code in clinical diagnostic data,
#' return CCS single and multiple category/ description based on ICD
#'
#' @import stringr
#' @import icd
#' @import plyr
#' @import dplyr
#' @import PheWAS
#' @param icdList An icd code list
#'
convertIcdShortToDecimal<-function(icdList){
  newicdList <- ifelse(!grepl("[.]",icdList), icd_short_to_decimal(icdList), icdList)
  return(newicdList)
}
