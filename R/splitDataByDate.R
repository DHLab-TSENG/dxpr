
#' split data based on index date
#'
#' @import data.table
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param indexDateFile An exact date of diagnosis for a period of observation.
#' @param Gap gap length of window. Default is set to \code{30}.
#' @export
#' @examples
#' sampleDxFile <- sampleDxFile[grepl("A0|B0|C0|D0",ID),]
#' head(sampleDxFile)
#' indexDateTable <- data.frame(ID = c("A0","B0","C0","D0"),
#'                              indexDate = c("2023-08-12", "2015-12-26",
#'                                            "2015-12-05", "2017-01-29"),
#'                              stringsAsFactors = FALSE)
#' splitedData <- splitDataByDate(sampleDxFile, ID, ICD, Date,
#'                                indexDateFile = indexDateTable,
#'                                Gap = 30)
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
