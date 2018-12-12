if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "icdFile",
  "groupingTable"))
#' Get the categories of ICD-9 and ICD-10 codes on diagnoses, the grouping rules are based on your standards.
#'
#' This can be used to select the first diagnosis record
#' based on ICD code (grepl language) in DxDataFile,
#' return first diagnosis record based on factIcd Data
#'
#' @import dplyr
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", and "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param groupingTable Grouping rules of clustering the ICD is based on yourself! There are two column in the dataframe: "group" and "ICD"
#' @export
#' @examples
#' groupingTable <- data.frame(group = rep("Cardiac dysrhythmias",6),
#'                             ICD = c("427.1","427.2","427.31","427.61","427.81","427.89"),
#'                             stringsAsFactors = FALSE)
#' IcdDxToCustom(sampleDxFile, ID, ICD, Date, groupingTable)
#'
IcdDxToCustom <- function(DxDataFile, idColName, icdColName, dateColName, groupingTable){
  customICD <- DxDataFile[, c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(customICD) <- c("ID", "ICD", "Date")
  customICD$Date <- as.Date(customICD$Date)
  conversionCustomICD <- IcdDxDecimalToShort(customICD$ICD)
  customICD$Short <- conversionCustomICD$Short
  groupingTable$ICD <- IcdDxDecimalToShort(groupingTable$ICD)$Short

  groupedICD <- left_join(customICD, groupingTable, by = c("Short" = "ICD"))

  groupedICDLong <- groupedICD[!is.na(groupedICD$group),] %>%
    group_by(ID,group) %>%
    summarise(firstCaseDate = min(Date),
              endCaseDate = max(Date),
              period = endCaseDate - firstCaseDate,
              count = n())

  wrongFormat <- conversionCustomICD$Error

  if(nrow(wrongFormat) > 0){
    message(paste0("wrong Format: ", unique(wrongFormat$ICD), sep = "\t\n"))
    message("\n")
    warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = F)
    warning('"wrong Format" means the ICD has wrong format', call. = F)
  }
  return(list(groupedDf = groupedICD,
              groupedData_Long = groupedICDLong))
}
