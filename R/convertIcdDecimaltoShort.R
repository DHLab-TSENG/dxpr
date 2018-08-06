#' Get the Clinical Classifications Software (CCS) single and multiple category/ description
#'
#' This can be used to select the Clinical Classifications Software (CCS) single and multiple category/ description
#' based on ICD code in clinical diagnostic data,
#' return Clinical Classifications Software (CCS) single and multiple category/ description based on ICD-9 and ICD-10
#' @import stringr
#' @import icd
#' @import plyr
#' @import dplyr
#' @param icdList An icd code list
convertIcdDecimaltoShort<-function(icdList){
  newicdList <- ifelse(grepl("[.]",icdList), icd_decimal_to_short(icdList), icdList)
  return(newicdList)
}
