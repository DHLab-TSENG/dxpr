if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "CCS",
  "ID",
  "Date",
  "episode"))
#' Get the condition era
#'
#' A Condition Era is defined as a span of time when the member is assumed to have a given condition.
#' Condition Eras are periods of Condition Occurrence.
#' Combining individual Condition Occurrences into a single Condition Era based on ICD code in clinical diagnostic data.
#' Condition Eras are built with a Persistence Window,deafault is 30 days, meaning, if no occurence of the same member id happens within 30 days of any one occurrence, it will be considered the end date of the last condition occurrence.
#'
#' return DxDataFile with new column, condition era.
#'
#' @import icd
#' @import dplyr
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate Icd 10 using date
#' @param gapDate Length ofcondition era, default is 30 days
#' @param icdorCCS Stratified by icd or ccs, default is CCS
#' @param isCCSDescription  Clinical Classifications Software (CCS) single level categories (False) and description (True) for ICD-9 or ICD-10, default is False.
#' @export
#' @examples
#' DxDataFile <- data.frame(ID=c("A","A","A"),
#'                          ICD=c("6929","V433","I350"),
#'                          Date=as.Date(c("2013-03-31","2013-01-29","2016-03-10")),
#'                          stringsAsFactors = FALSE)
#' getConditionEra(DxDataFile, ID, ICD, Date, "2016-01-01", 30, ccs, FALSE)
#' getConditionEra(DxDataFile, ID, ICD, Date, "2016-01-01", 30, ICD, FALSE)
#'
getConditionEra <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, gapDate = 30, icdorCCS = CCS, isCCSDescription = FALSE){
  DxDataFile <- DxDataFile[ ,c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile$ICD <- convertIcdDecimaltoShort(DxDataFile$ICD)
  icdorCCS <- toupper(deparse(substitute(icdorCCS)))
  errorID <- 0

  if(icdorCCS == "CCS"){
    DxDataFile <- DxDataFile %>%
      mutate(CCS = groupIcdToCCS(DxDataFile, ID, ICD, Date, icd10usingDate, isCCSDescription)) %>%
      arrange(ID, CCS, Date) %>%
      group_by(ID, CCS) %>%
      mutate(Gap = Date - lag(Date))
  }else if(icdorCCS == "ICD"){
    DxDataFile <- DxDataFile %>%
      arrange(ID, ICD, Date) %>%
      group_by(ID, ICD) %>%
      mutate(Gap = Date - lag(Date))
  }else{
    stop("'please enter icd or ccs for 'icdorCCS'", call. = FALSE)
  }
  DxDataFile$episode <- DxDataFile$Gap > gapDate
  DxDataFile$episode[is.na(DxDataFile$episode)] <- TRUE
  if(icdorCCS == "CCS"){
    DxDataFile <- DxDataFile %>%
      group_by(ID, CCS) %>%
      mutate(Era = cumsum(episode))
    errorID <- is.na(DxDataFile$CCS)

  }else if(icdorCCS == "ICD"){
    DxDataFile <- DxDataFile %>%
      group_by(ID, ICD) %>%
      mutate(Era = cumsum(episode))
  }
  DxDataFile <- select(DxDataFile, c(-"Gap", -"episode"))
  if(sum(errorID) >= 1){
    message(paste0("wrong format: ", DxDataFile$ICD[is.na(DxDataFile$CCS)], sep = "\t\n"))
  }
  DxDataFile
}
