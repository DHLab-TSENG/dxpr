if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "icdFile",
  "groupingTable"))
#' Get the categories of ICD-9 and ICD-10 codes on diagnoses, the grouping rules are based on your standards.
#'
#' This can be used to select the first diagnosis record
#' based on ICD code (grepl language) in DxDataFile,
#' return first diagnosis record based on factIcd Data
#'
#' @param icdFile A file of clinical diagnostic data with at least one column: ICD
#' @param groupingTable Grouping rules of clustering the ICD is based on yourself! There are two column in the dataframe: Group, GrepICD
#' @export
#' @examples
#' icdFile <- data.frame(ICD = c("I95.0", "I952", "I110", "01091"), stringsAsFactors = FALSE)
#' groupingTable <- data.frame(Group = c("Hypotension","Hypotension", "Hypertension", "Hypertension"),
#'                             ICD = c("I95.0","I951","I110","I11.9"),
#'                             stringsAsFactors = FALSE)
#' groupIcdToCustomGrep(icdFile, groupingTable)
#'
groupIcdToCustomGrep <- function(icdFile, groupingTable){
  icdFile$ICD <- convertIcdDecimaltoShort(icdFile$ICD)$Short
  groupingTable$ICD <- convertIcdDecimaltoShort(groupingTable$ICD)$Short

  GroupDf <- left_join(icdFile, groupingTable, by = "ICD")$Group

  GroupDf
}
