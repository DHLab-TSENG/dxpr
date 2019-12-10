#' @rdname recordPeriod
#' @export
#'
patientRecordDate <- function(dxDataFile, idColName, icdColName, dateColName){
  dxDataFile <- as.data.table(dxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  dxDataFile <- dxDataFile[,DataCol,with = FALSE]
  names(dxDataFile) <- c("ID", "ICD", "Date")
  dxDataFile[,"Date"] <- as.Date(dxDataFile[,Date])

  recordDate <- dxDataFile[,list(firstRecordDate = min(Date), endRecordDate = max(Date)),by = "ID"][order(firstRecordDate,endRecordDate),]

  recordDate
}

