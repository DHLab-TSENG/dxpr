
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
#' @param groupDataType  Four Stratified methods can be chosen: CCS (\code{'ccs'}), CCS levels (\code{'ccslvl1'}, \code{'ccslvl2'}, \code{'ccslvl3'}, \code{'ccslvl4'}), phecode (\code{'phecode'}), comorbidities (\code{'ahrq'},\code{'charlson'}, \code{'elix'}), grepICD or customICD (\code{'customGrepIcdGroup'}, \code{'customIcdGroup'}). Change it to any of the other possible variables, default it is set to \code{"ccs"}.
#' @param CustomGroupingTable Table is for groupDataType
#' @param isDescription  CCS/Phecode categories or description for ICD-CM codes, default is \code{'TRUE'}.
#' @param numericOrBinary  Members have same diagnostic categories, type `N` or `B`, default is Binary \code{'B'}.
#' @param selectedCaseFile Table for selectedCases. Default is \code{'NULL'}
#' @export
#' @examples
#' groupingTable <- data.frame(group = rep("Cardiac dysrhythmias",6),
#'                             ICD = c("427.1","427.2","427.31","427.61","427.81","427.89"),
#'                             stringsAsFactors = FALSE)
#' groupedDataLongToWide(sampleDxFile, ID, ICD, Date, "2015-10-01",
#'                       customIcdGroup,
#'                       CustomGroupingTable = groupingTable)
#' selectedCaseFile <- selectCases(sampleDxFile, ID, ICD, Date,
#'                                 icd10usingDate = "2015/10/01",
#'                                 groupDataType = ccslvl2,
#'                                 caseCondition = "Diseases of the heart",
#'                                 ICDNumber = 2)
#' groupedDataLongToWide(sampleDxFile, ID, ICD, Date,
#'                       "2015-10-01", ccslvl2, N)#,selectedCaseFile =  selectedCaseFile)
#'
groupedDataLongToWide <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, groupDataType = ccs, CustomGroupingTable, isDescription = TRUE, numericOrBinary = B,selectedCaseFile = NULL){
  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")

  groupDataType <- tolower(deparse(substitute(groupDataType)))
  groupedData <- groupMethodSelect(DxDataFile, ID, ICD, Date,
                                   icd10usingDate, groupDataType, CustomGroupingTable, isDescription)
  if(groupDataType != "icd"){
    groupedData <- groupedData$groupedData_Long
    groupDataType <- names(groupedData)[2]
  }
  wideDt <- dcast(groupedData, ID~eval(parse(text = paste(names(groupedData)[2]))), value.var = c("count"))
  if(length(wideDt$ID) != length(DxDataFile$ID)){
    wideDt <- merge(wideDt, DxDataFile[!duplicated(ID),"ID"], all = T)
  }
  wideDt[is.na(wideDt)] <- 0L
  numericOrBinary <- toupper(deparse(substitute(numericOrBinary)))
  if(numericOrBinary == "B"){
    wideDt_N <-as.data.frame(wideDt >= 1L)
    wideDt_N$ID <- wideDt$ID
  }else if(numericOrBinary != "B" && numericOrBinary != "N"){
    stop("'please enter N or B for 'numericOrBinary'", call. = FALSE)
  }

  if(!is.null(selectedCaseFile)){
    if(numericOrBinary == "B"){
      wideDt_selected <- merge(wideDt_N, selectedCaseFile[,list(ID, selectedCase)], all.x = T)
    }else{
      wideDt_selected <- merge(wideDt, selectedCaseFile[,list(ID, selectedCase)], all.x = T)
    }
    return(wideDt_selected)
  }else{
    if(numericOrBinary == "B"){
      return(wideDt_N)
    }else{
      return(wideDt)
    }
  }
}
