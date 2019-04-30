
#' Select cases based on ICD code and the number of ICD codes
#'
#' This can be used to select qualified cases from factIcd data
#' based on the ICD code searching criteria and number of ICD code
#' per patients in the inout factIcd dataset.
#' Return qualified Members' data
#'
#' @import data.table
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param indexDateFile An exact date of diagnosis for a period of observation.
#' @param windowGap gap length of window. By default it is set to \code{30}.
#' @export
#' @examples
#' head(sampleDxFile)
#' indexDateTable <- data.frame(ID = c("A","B","C","D"),
#'                              indexDate = c("2006-05-03", "2006-05-03",
#'                                            "2008-03-30", "2006-05-03"),
#'                              stringsAsFactors = FALSE)
#' splitDataByDate(sampleDxFile, ID, ICD, Date,
#'                 indexDateFile = indexDateTable,
#'                 windowGap = 30)
splitDataByDate <- function(DxDataFile, idColName, icdColName, dateColName, indexDateFile, windowGap = 30){
  DxDataFile <- as.data.table(DxDataFile)
  indexDateFile <- as.data.table(indexDateFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile[,"Date"] <- as.Date(DxDataFile$Date)

  splitedData <- merge(DxDataFile,indexDateTable,
                       all.x = T)[,Gap := Date - as.Date(indexDate)][Gap >= 0, timeTag := "A"][Gap < 0, timeTag := "B"][,window := abs((as.integer(Gap) %/% windowGap)),][timeTag == "A", window := window +1,][order(ID,Date), -"Gap"]

  splitedData
}
