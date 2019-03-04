if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "grepICD",
  "ICDNumber",
  "INRofDayRange",
  "InTimeINR",
  "firstCaseDate",
  "endCaseDate",
  "period",
  "MostCommonICDCount","selectedCase"))
#' Select cases based on ICD code and the number of ICD codes
#'
#' This can be used to select qualified cases from factIcd data
#' based on the ICD code searching criteria and number of ICD code
#' per patients in the inout factIcd dataset.
#' Return qualified Members' data
#'
#' @import data.table
#' @param grepICD ICD selection rules with grepl expression
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param ICDNumber a threshold of number of ICD for case selection
#' @param INRofDayRange Determines what is the interval of days of interest for performing the case selection. By default it is set to from 30 to 365 days.
#' @param selectedCaseType Aggregation  of selected cases name. By default it is set to \code{"selected"}.
#' @export
#' @examples
#' head(sampleDxFile)
#' selectCases("^785", sampleDxFile, ID, ICD, Date, 2)
#'
selectCases <- function(grepICD, DxDataFile, idColName, icdColName, dateColName, ICDNumber, INRofDayRange = c(30, 365), selectedCaseType = "selected"){
  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile[,"Date"] <- as.Date(DxDataFile[,Date])
  nonSelectedCaseType <- paste0("non",selectedCaseType,sep = "")

  Case <- DxDataFile[grepl(grepICD, DxDataFile$ICD),]
  Control <- DxDataFile[!Case, on = "ID"][,selectedCase := nonSelectedCaseType][,-c("ICD","Date")]

  Count <- Case[,list(firstCaseDate = min(Date),endCaseDate = max(Date),Count = .N),
                by = ID][, period := (endCaseDate - firstCaseDate),]

  CaseCount <- Count[,InTimeINR := period >= INRofDayRange[1] & period < INRofDayRange[2],][Count >= ICDNumber & InTimeINR ==TRUE,][,-"InTimeINR"]
  ControlCount <- Count[!CaseCount, on = "ID"][,-"InTimeINR"]

  CaseMostICDCount <- Case[,list(MostCommonICDCount = .N),by = list(ID,ICD)][order(MostCommonICDCount,decreasing = T),]
  selectedCase <- merge(CaseCount,CaseMostICDCount,"ID")[,selectedCase := selectedCaseType]
  setnames(selectedCase,"ICD","MostCommonICD")
  nonSelectedCase <- merge(Control,ControlCount,"ID",all = T)[,selectedCase := nonSelectedCaseType][,list(ID,selectedCase)]

  allData <- merge(selectedCase,nonSelectedCase,by = names(nonSelectedCase),all=T)[!duplicated(ID),][order(MostCommonICDCount,decreasing = T),]

  allData
}
