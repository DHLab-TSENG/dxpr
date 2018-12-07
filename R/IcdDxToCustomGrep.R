if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "grepTable",
  "grepIcd",
  "group"))
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
#' @param grepTable grouping rules of clustering the ICD is based on yourself! There are two column in the dataframe: "group", "grepIcd"
#' @export
#' @examples
#'
#' grepTable <- data.frame(group = c("Cardiac dysrhythmias"),
#'                         grepIcd = c("^427|^I48"),
#'                         stringsAsFactors = FALSE)
#' IcdToCustomGrep(testDxFile, ID, ICD, Date, grepTable)
#'
IcdToCustomGrep <- function(DxDataFile, idColName, icdColName, dateColName, grepTable){
  grepIcd <-   DxDataFile <- DxDataFile[, c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(grepIcd) <- c("ID", "ICD", "Date")
  grepIcd$Date <- as.Date(grepIcd$Date)
  grepIcd$group<-""

  for (rule in 1:nrow(grepTable)){
    grepIcd$group<-ifelse(grepl(grepTable$grepIcd[rule],grepIcd$ICD), grepTable$group[rule], grepIcd$group)
  }
  grepIcdLong <- grepIcd[nchar(grepIcd$group)>0,] %>%
    group_by(ID, group) %>%
    summarise(firstCaseDate = min(Date),
              endCaseDate = max(Date),
              period = endCaseDate - firstCaseDate,
              count = n())
  wrongFormat <- IcdDxDecimalToShort(grepIcd$ICD)$Error

  if(nrow(wrongFormat) > 0){
    message(paste0("wrong Format: ", unique(wrongFormat$ICD), sep = "\t\n"))
    message("\n")
    warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = F)
    warning('"wrong Format" means the ICD has wrong format', call. = F)
  }
  return(list(groupedDf = grepIcd,
              groupedData_Long = grepIcdLong))
}
