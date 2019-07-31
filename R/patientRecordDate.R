#' @rdname recordPeriod
#' @export
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

