#' Get the condition era
#'
#' A Condition Era is defined as a span of time when the member is assumed to have a given condition.
#' Condition Eras are periods of Condition Occurrence.
#' Combining individual Condition Occurrences into a single Condition Era based on ICD code in clinical diagnostic data.
#' Condition Eras are built with a Persistence Window,deafault is 30 days, meaning, if no occurence of the same member id happens within 30 days of any one occurrence, it will be considered the end date of the last condition occurrence.
#'
#' return DxDataFile with new column, condition era.
#'
#' @import data.table
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate Icd 10 using date
#' @param groupDataType  Four Stratified methods can be chosen: CCS (\code{'ccs'}), CCS levels (\code{'ccslvl1'}, \code{'ccslvl2'}, \code{'ccslvl3'}, \code{'ccslvl4'}), phecode (\code{'phecode'}), comorbidities (\code{'ahrq'},\code{'charlson'}, \code{'elix'}), grepICD or customICD (\code{'customGrepIcdGroup'}, \code{'customIcdGroup'}). Change it to any of the other possible variables, default it is set to \code{"ccs"}.
#' @param CustomGroupingTable Table is for groupDataType
#' @param isDescription  CCS/Phecode categories or description for ICD-CM codes, default is \code{'TRUE'}.
#' @param gapDate Length of condition era,By default it is set to 30 days \code{"30"}.
#' @export
#' @examples
#' head(sampleDxFile)
#' getConditionEra(sampleDxFile, ID, ICD, Date, "2015-10-01", groupDataType = CCSlvl2)
#' grepTable <- data.frame(group = "Cardiac dysrhythmias",
#'                         grepIcd = "^427|^I48",
#'                         stringsAsFactors = FALSE)
#' getConditionEra(sampleDxFile, ID, ICD, Date, "2015-10-01",
#'                 groupDataType = customGrepIcdGroup,
#'                 CustomGroupingTable = grepTable)
#'
getConditionEra <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, groupDataType = ccs, CustomGroupingTable, isDescription = TRUE, gapDate = 30){

  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile[,"Date"] <- as.Date(DxDataFile[,Date])

  groupDataType <- tolower(deparse(substitute(groupDataType)))
  groupedData <- groupMethodSelect(DxDataFile, ID, ICD, Date,
                                   icd10usingDate, groupDataType, CustomGroupingTable, isDescription)
  if(groupDataType != "icd"){
    groupedData <- groupedData$groupedDT
  }
  groupDataType <- names(groupedData)[ncol(groupedData)]
  groupByCol <- c("ID",groupDataType)

  count <- groupedData[nchar(eval(parse(text = paste(groupDataType)))) > 0  & !is.na(eval(parse(text = paste(groupDataType))))][order(eval(parse(text = paste(groupByCol))),Date)][,NextDate := c(Date[-1],NA),by = groupByCol][is.na(NextDate),NextDate := Date][,Gap := NextDate- Date]

  conditionEra <- count[,episode := Gap > gapDate][is.na(episode),episode :=TRUE][,list(episodeCount = cumsum(episode)+1,
                                                                                        firstCaseDate = min(Date),
                                                                                        endCaseDate = max(Date),
                                                                                        count = .N),by = groupByCol][,era := max(episodeCount),by = groupByCol][,period := endCaseDate - firstCaseDate,][,-"episodeCount"]

  conditionEra <- unique(conditionEra)

  conditionEra
}


