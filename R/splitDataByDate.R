#' @rdname dataSplit
#' @export
#'
splitDataByDate <- function(dxDataFile, idColName, icdColName, dateColName, indexDateFile, gap = 30){
  dxDataFile <- as.data.table(dxDataFile)
  indexDateFile <- as.data.table(indexDateFile)
  dataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  dxDataFile <- dxDataFile[,dataCol,with = FALSE]
  names(dxDataFile) <- c("ID", "ICD", "Date")
  dxDataFile[,"Date"] <- as.Date(dxDataFile$Date)

  splitedData <- merge(dxDataFile, indexDateFile,
                       all.x = TRUE)[,diff := Date - as.Date(indexDate)][diff >= 0, timeTag := "A"][diff < 0, timeTag := "B"][,window := abs((as.integer(diff) %/% gap)),][timeTag == "A", window := window +1,][order(ID,Date), -"diff"]

  splitedData
}
