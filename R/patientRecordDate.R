#'
#' patients' first and last record date.
#'
#' @import data.table
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @export
#' @examples
#' head(sampleDxFile)
#' patientRecordDate(sampleDxFile, ID, ICD, Date)
#'                 
patientRecordDate <- function(DxDataFile, idColName, icdColName, dateColName){
  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile[,"Date"] <- as.Date(DxDataFile[,Date])
  
  recordDate <- DxDataFile[,list(firstRecordDate = min(Date), endRecordDate = max(Date)),by = "ID"][order(firstRecordDate,endRecordDate),]
  
  recordDate
}

