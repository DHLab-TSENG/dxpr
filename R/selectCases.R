if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "grepICD",
  "ICDNumber",
  "minimumINRofDays",
  "maximumINRofDays",
  "InTimeINR",
  "firstCaseDate",
  "endCaseDate",
  "period",
  "MostCommonICD",
  "MostCommonICDCount"))
#' Select cases based on ICD code and the number of ICD codes
#'
#' This can be used to select qualified cases from factIcd data
#' based on the ICD code searching criteria and number of ICD code
#' per patients in the inout factIcd dataset.
#' Return qualified Members' data
#'
#' @import data.table
#' @param grepICD ICD selection rules with grepl expression
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param ICDNumber a threshold of number of ICD for case selection
#' @param minimumINRofDays Minimum interval of Days, defaults is 30 days (one month)
#' @param maximumINRofDays Maximum interval of Days, defaults is 365 days (one year)
#' @export
#' @examples
#'
#' selectCases("^785", sampleDxFile, ID, ICD, Date, 2)
#'
selectCases <- function(grepICD, DxDataFile, idColName, icdColName, dateColName, ICDNumber, minimumINRofDays = 30, maximumINRofDays = 365){
  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "MostCommonICD", "Date")
  DxDataFile[,"Date"] <- as.Date(DxDataFile[,Date])
  DxDataFile[,Number:=1:nrow(DxDataFile)]


  Count <- DxDataFile[grepl(grepICD, DxDataFile$MostCommonICD),
                      list(firstCaseDate = min(Date), endCaseDate = max(Date),count = .N),
                      by = ID][,period := (endCaseDate - firstCaseDate),][,InTimeINR := period >= minimumINRofDays & period < maximumINRofDays,][count >= ICDNumber & InTimeINR ==TRUE,][,-"InTimeINR"]

  CaseICD <- DxDataFile[grepl(grepICD, DxDataFile$MostCommonICD),
                        list(MostCommonICDCount = .N), by = list(ID,MostCommonICD)][order(MostCommonICDCount,decreasing = T),][!duplicated(ID),]

  CaseCount <- merge(Count,CaseICD[,list(ID,MostCommonICD,MostCommonICDCount)],"ID")

  CaseCount
}
