if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "greplICD"))
#' Select cases based on ICD code and the number of ICD codes
#'
#' This can be used to select qualified cases from factIcd data
#' based on the ICD code searching criteria and number of ICD code
#' per patients in the inout factIcd dataset.
#' Return qualified Members' data
#'
#' @import dplyr
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param IndexDate An exact date of diagnosis for a period of observation.
#' @param window Length of condition era, default is 30 days
#' @export
#'
# @examples
#'
# splitDataByDate(testFile4005, ID, ICD, Date,"2015-10-01",30)
# start_time <- Sys.time()
# testFile_split <- splitDataByDate(testFile4005, ID, ICD, Date,"2015-10-01",30) #0.100255 secs (458 obs)
# end_time <- Sys.time()
# end_time - start_time

splitDataByDate <- function(DxDataFile,idColName,icdColName,dateColName,IndexDate, window = 30){
  DxDataFile <- DxDataFile[, c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile$Date <- as.Date(DxDataFile$Date)

  After <- DxDataFile[DxDataFile$Date >= IndexDate,] %>%
    arrange(Date) %>%
    group_by(ID) %>%
    mutate(timeTag = "A",
           Gap = Date - IndexDate)
  After$Window <- (as.integer(After$Gap) %/% window) + 1

  Before <- DxDataFile[DxDataFile$Date < IndexDate,] %>%
    group_by(ID) %>%
    arrange(desc(Date)) %>%
    mutate(timeTag = "B",
           Gap = IndexDate - Date)
  Before$Window <- (as.integer(Before$Gap) %/% window) + 1

  splitedData <- rbind(After,Before) %>% select(-Gap)
  splitedData
}
