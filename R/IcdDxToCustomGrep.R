if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "grepTable"))
#' Get the categories of ICD-9 and ICD-10 codes on diagnoses, the grouping rules are based on your standards.
#'
#' This can be used to select the first diagnosis record
#' based on ICD code (grepl language) in DxDataFile,
#' return first diagnosis record based on factIcd Data
#'
#' @param icdFile ICD-9 and ICD-10 codes
#' @param icdColName A column for ICD of DxDataFile
#' @param grepTable Grouping rules of clustering the ICD is based on yourself! There are two column in the dataframe: Group, GrepICD#'
#' @export
#' @examples
#' icdFile <- data.frame(ICD = c("I95.0", "I952", "I110", "01091"), stringsAsFactors = FALSE)
#' grepTable <- data.frame(Group = c("Hypotension", "Hypertension"),
#'                             grep_pattern = c("^I95|^I952", "^I11"),
#'                             stringsAsFactors = FALSE)
#' IcdDxToCustomGrep(icdFile, ICD, grepTable)
#'
IcdDxToCustomGrep <- function(icdFile, icdColName, grepTable){
  icdFile <- icdFile[, c(deparse(substitute(icdColName)))]
  names(icdFile) <- "ICD"
  icdFile$Group<-""

  for (rule in 1:nrow(grepTable)){
    icdFile$Group<-ifelse(grepl(grepTable$grep_pattern[rule],icdFile$ICD), grepTable$Group[rule], icdFile$Group)
  }
  return(icdFile$Group)
}
