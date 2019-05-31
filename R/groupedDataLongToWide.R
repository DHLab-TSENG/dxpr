#'
#' Grouped data format conversion
#'
#' convert long format to wide format
#'
#' return numeric or binary wide format
#'
#' @import data.table
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate Icd 10 using date
#' @param groupDataType  Four Stratified methods can be chosen: CCS (\code{'ccs'}), CCS levels (\code{'ccslvl1'}, \code{'ccslvl2'}, \code{'ccslvl3'}, \code{'ccslvl4'}), PheWAS (\code{'PheWAS'}), comorbidities (\code{'ahrq'},\code{'charlson'}, \code{'elix'}), grepICD or customICD (\code{'customGrepIcdGroup'}, \code{'customIcdGroup'}). Change it to any of the other possible variables, default it is set to \code{"ccs"}.
#' @param CustomGroupingTable Table is for groupDataType
#' @param isDescription  CCS/PheWAS categories or description for ICD-CM codes, default is \code{'TRUE'}.
#' @param numericOrBinary  Members have same diagnostic categories, type `N` or `B`, default is Binary \code{'B'}.
#' @param selectedCaseFile Table for selectedCases. Default is \code{'NULL'}
#' @export
#' @examples
#' groupingTable <- data.frame(group = rep("Cardiac dysrhythmias",6),
#'                             ICD = c("427.1","427.2","427.31","427.61","427.81","427.89"),
#'                             stringsAsFactors = FALSE)
#' groupedDataLongToWide(sampleDxFile, ID, ICD, Date, "2015-10-01",
#'                       groupDataType = customIcdGroup,
#'                       CustomGroupingTable = groupingTable)
#'
#' selectedCaseFile <- selectCases(sampleDxFile, ID, ICD, Date,
#'                                 icd10usingDate = "2015/10/01",
#'                                 groupDataType = ccslvl2,
#'                                 caseCondition = "Diseases of the heart",
#'                                 caseCount = 2)
#' groupedDataLongToWide(sampleDxFile, ID, ICD, Date,
#'                       "2015-10-01", ccslvl2,
#'                       numericOrBinary = N,
#'                       selectedCaseFile = selectedCaseFile)
#'
groupedDataLongToWide <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, groupDataType = ccs, CustomGroupingTable, isDescription = TRUE, numericOrBinary = B,selectedCaseFile = NULL){
  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")

  groupDataType <- toupper(deparse(substitute(groupDataType)))
  groupedData <- groupMethodSelect(DxDataFile, ID, ICD, Date,
                                   icd10usingDate, groupDataType, CustomGroupingTable, isDescription)
  if(groupDataType != "ICD"){
    groupedData <- groupedData$summarised_groupedDT
  }else{
    groupedData <- groupedData[,list(firstCaseDate = min(Date),endCaseDate = max(Date),count = .N),by = c("ID","Short")][,period := (endCaseDate - firstCaseDate),]
  }

  groupedData_wide <- dcast(groupedData, ID~eval(parse(text = paste(names(groupedData)[2]))), value.var = c("count"))

  if(length(groupedData_wide$ID) != length(DxDataFile$ID)){
    OtherPatientID <- DxDataFile[!groupedData_wide, on = "ID"][!duplicated(ID),ID]
    OtherPatientDt <- data.table(matrix(ncol = ncol(groupedData_wide),nrow = length(OtherPatientID)))
    names(OtherPatientDt) <- names(groupedData_wide)
    OtherPatientDt[,"ID"] <- OtherPatientID
    wideData <- rbind(groupedData_wide, OtherPatientDt)
  }
  wideData[is.na(wideData)] <- 0L

  numericOrBinary <- toupper(deparse(substitute(numericOrBinary)))
  if(numericOrBinary == "B"){
    wideData_B <- as.data.frame(wideData >= 1L)
    wideData_B$ID <- wideData$ID
    wideData <- wideData_B
  }else if(numericOrBinary != "B" && numericOrBinary != "N"){
    stop("'please enter N or B for 'numericOrBinary'", call. = FALSE)
  }

  if(!is.null(selectedCaseFile)){
    wideData_selected <- merge(wideData, selectedCaseFile[,list(ID, selectedCase)],by = "ID")
    return(wideData_selected)
  }else{
    return(wideData)
  }
}
