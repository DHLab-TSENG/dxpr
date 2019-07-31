#' @rdname DataSplit
#' @export
#'
splitDataByDate <- function(DxDataFile, idColName, icdColName, dateColName, indexDateFile, Gap = 30){
  DxDataFile <- as.data.table(DxDataFile)
  indexDateFile <- as.data.table(indexDateFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile[,"Date"] <- as.Date(DxDataFile$Date)

  splitedData <- merge(DxDataFile, indexDateFile,
                       all.x = TRUE)[,diff := Date - as.Date(indexDate)][diff >= 0, timeTag := "A"][diff < 0, timeTag := "B"][,window := abs((as.integer(diff) %/% Gap)),][timeTag == "A", window := window +1,][order(ID,Date), -"diff"]

  splitedData
}
