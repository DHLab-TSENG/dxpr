if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "grepICD",
  "ICDNumber",
  "minimumINRofDays",
  "maximumINRofDays",
  "InTimeINR",
  "firstCaseDate",
  "endCaseDate",
  "period",
  "MostCommonICD",
  "MostCommonICDCount"))
#' Select cases based on ICD code and the number of ICD codes
#'
#' This can be used to select qualified cases from factIcd data
#' based on the ICD code searching criteria and number of ICD code
#' per patients in the inout factIcd dataset.
#' Return qualified Members' data
#'
#' @import dplyr
#' @param grepICD ICD selection rules with grepl expression
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param ICDNumber a threshold of number of ICD for case selection
#' @param minimumINRofDays Minimum interval of Days, defaults is 30 days (one month)
#' @param maximumINRofDays Maximum interval of Days, defaults is 365 days (one year)
#' @export
#' @examples
#' DxDataFile <- data.frame(ID = c("A", "A", "A", "A"),
#'                          ICD = c("I072","I071", "I072", "I071"),
#'                          Date = as.Date(c("2016-03-31", "2016-01-29", "2016-02-10", "2018-03-10")),
#'                          stringsAsFactors = FALSE)
#' selectCases("^I0", DxDataFile, ID, ICD, Date, 2)
#'
selectCases <- function(grepICD, DxDataFile, idColName, icdColName, dateColName, ICDNumber, minimumINRofDays = 30, maximumINRofDays = 365){
  DxDataFile <- DxDataFile[, c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile) <- c("ID", "MostCommonICD", "Date")
  DxDataFile$Date <- as.Date(DxDataFile$Date)
  DxDataFile <- DxDataFile %>% mutate(Number =  1:nrow(DxDataFile))

  Count <- DxDataFile[grepl(grepICD, DxDataFile$MostCommonICD),] %>%
    group_by(ID) %>%
    summarise(firstCaseDate = min(Date),
              endCaseDate = max(Date),
              period = endCaseDate - firstCaseDate,
              Count = n(),
              InTimeINR = period >= minimumINRofDays & period < maximumINRofDays) %>%
    filter(Count >= ICDNumber & InTimeINR ==TRUE) %>%
    select (-InTimeINR)

  CaseICD <- DxDataFile[grepl(grepICD, DxDataFile$MostCommonICD),] %>%
    group_by(ID,MostCommonICD) %>%
    summarise(MostCommonICDCount = n()) %>%
    arrange(ID,desc(MostCommonICDCount)) %>% slice(1)

  CaseCount <- left_join(Count,CaseICD,"ID")
  CaseCount
}
