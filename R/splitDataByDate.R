if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "greplICD","timeTag"))
#' Select cases based on ICD code and the number of ICD codes
#'
#' This can be used to select qualified cases from factIcd data
#' based on the ICD code searching criteria and number of ICD code
#' per patients in the inout factIcd dataset.
#' Return qualified Members' data
#'
#' @import data.table
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param IndexDate An exact date of diagnosis for a period of observation.
#' @param window Length of condition era, default is 30 days
#' @export
# @examples
#'
# splitDataByDate(sampleDxFile, ID, ICD, Date,"2015-10-01",30)
#'
splitDataByDate <- function(DxDataFile,idColName,icdColName,dateColName,IndexDate, window = 30){
  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile[,"Date"] <- as.Date(DxDataFile[,Date])
  After <- DxDataFile[Date >= IndexDate,][,timeTag := "A"][,Gap := Date - as.Date(IndexDate)]
  After$Window <- (as.integer(After$Gap) %/% window) + 1

  Before <- DxDataFile[Date < IndexDate,][order(Date,decreasing = T),][,timeTag := "B"][,Gap := as.Date(IndexDate) - Date]
  Before$Window <- (as.integer(Before$Gap) %/% window) + 1

  splitedData <- rbind(After,Before)
  splitedData$Gap <- NULL
  splitedData
}
