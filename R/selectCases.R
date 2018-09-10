if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "greplICD",
  "ICDNumber",
  "minimumINRofDays",
  "maximumINRofDays",
  "InTimeINR",
  "CaseNum"))
#' Select cases based on ICD code and the number of ICD codes
#'
#' This can be used to select qualified cases from factIcd data
#' based on the ICD code searching criteria and number of ICD code
#' per patients in the inout factIcd dataset. Return MemberID
#'
#' @import dplyr
#' @param greplICD ICD selection rules with grepl expression
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate ICD-10 using date
#' @param ICDNumber a threshold of number of ICD for case selection
#' @param minimumINRofDays Minimum interval of Days, defaults is 30 days (one month)
#' @param maximumINRofDays Maximum interval of Days, defaults is 365 days (one year)
#' @export
#' @examples
#' DxDataFile <- data.frame(ID = c("A", "A", "A", "A"),
#'                          ICD = c("I072","I071", "I072", "I071"),
#'                          Date = as.Date(c("2016-03-31", "2016-01-29", "2016-02-10", "2018-03-10")),
#'                          stringsAsFactors = FALSE)
#' greplICD <- "^I0"
#' ICDNumber <- 2
#' icd10usingDate <- "2016-01-01"
#' selectCases(greplICD, DxDataFile, ID, ICD, Date, icd10usingDate, ICDNumber)
#'
selectCases <- function(greplICD, DxDataFile, idColName, icdColName, dateColName, ICDNumber,icd10usingDate, minimumINRofDays = 30, maximumINRofDays = 365){
  DxDataFile <- DxDataFile[, c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile) <- c("ID", "ICD", "Date")

  icd10 <- DxDataFile[DxDataFile$Date >= icd10usingDate,]
  icd10$ICD <- convertIcdDecimaltoShort(icd10$ICD, icd10)
  icd9 <- DxDataFile[DxDataFile$Date < icd10usingDate,]
  icd9$ICD <- convertIcdDecimaltoShort(icd9$ICD, icd9)
  if(nrow(icd9) <= 0){
    DxDataFile <- icd10
  }else if(nrow(icd9) <= 0){
    DxDataFile <- icd9
  }else{
    DxDataFile <- full_join(icd9, icd10, by = c("ID", "ICD", "Date"))
  }

  CaseCount <- DxDataFile %>% filter(grepl(greplICD, ICD)) %>%
    arrange(ID, ICD, Date) %>%
    group_by(ID,ICD) %>%
    mutate(Gap = Date - lag(Date)) %>%
    mutate(InTimeINR = Gap >= minimumINRofDays & Gap < maximumINRofDays)

  CaseCount$InTimeINR[is.na(CaseCount$InTimeINR)] <- TRUE

  CaseCountInTimeINR <- CaseCount %>% filter(InTimeINR == T) %>%
    group_by(ID, ICD) %>%
    mutate(CaseNum = cumsum(InTimeINR)) %>%
    filter(CaseNum >= ICDNumber) %>%
    select(ID,ICD,Date)

  CaseCountInTimeINR
}
