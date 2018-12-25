if(getRversion() >= "2.15.1") utils::globalVariables(c("CustomGroupingTable", "group"))
#' Get the categories of ICD-9 and ICD-10 codes on diagnoses, the grouping rules are based on your standards.
#'
#' This can be used to select the first diagnosis record
#' based on ICD code (grepl language) in DxDataFile,
#' return first diagnosis record based on factIcd Data
#'
#' @import data.table
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", and "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param CustomGroupingTable grouping rules of clustering the ICD is based on yourself! There are two column in the dataframe: "group", "grepIcd"
#' @export
# @examples
#'
# grepTable <- data.table(group = c("Cardiac dysrhythmias"),
#                         grepIcd = c("^427|^I48"))
# IcdDxToCustomGrep(sampleDxFile, ID, ICD, Date,
#                 CustomGroupingTable = grepTable)
#'
IcdDxToCustomGrep <- function(DxDataFile, idColName, icdColName, dateColName, CustomGroupingTable){
  GrepedIcd <- as.data.table(DxDataFile)
  DataCol  <-c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  GrepedIcd <- GrepedIcd[,DataCol,with = FALSE]
  names(GrepedIcd) <- c("ID", "ICD", "Date")
  GrepedIcd[,"Date"] <- as.Date(GrepedIcd[, Date])
  GrepedIcd[, group:=""]

  for (rule in 1:nrow(CustomGroupingTable)){
    GrepedIcd$group<-ifelse(grepl(CustomGroupingTable[rule,"grepIcd"],GrepedIcd[,ICD]), CustomGroupingTable[rule,group], GrepedIcd[,group])
  }

  GrepedIcdLong <- GrepedIcd[nchar(group)>0,
                             list(firstCaseDate = min(Date),
                                  endCaseDate = max(Date),
                                  count = .N),by = list(ID,group)][,period := (endCaseDate - firstCaseDate),]

  return(list(groupedDf = GrepedIcd,
              groupedData_Long = GrepedIcdLong))
}
