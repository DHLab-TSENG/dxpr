if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "groupedICDMethod",
  "CustomGroupingTable",
  "isDescription",
  "N",
  "ahrq",
  "charlson",
  "elix"))
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
#' @param groupedICDMethod  Four grouping method can be chosen: CCS (\code{'ccs'}), CCS levels (\code{'ccslvl1'}, \code{'ccslvl2'}, \code{'ccslvl3'}, \code{'ccslvl4'}), phecode (\code{'phecode'}), comorbidities (\code{'ahrq'},\code{'charlson'}, \code{'elix'}),
#'  grepICD or customICD (\code{'customGrepIcdGroup'}, \code{'customIcdGroup'}). Change it to any of the other possible variables.
#' @param isDescription  CCS/Phecode categories or description for ICD-CM codes, default is \code{'TRUE'}.
#' @param CustomGroupingTable Table is for groupedICDMethod
#' @param numericOrBinary  Members have same diagnostic categories, type `N` or `B`, default is Binary \code{'B'}.
#' @param selectedCases  Select cases based on ICD code and the number of ICD codes, default is \code{'FALSE'}.
#' @param selectedCaseFile Table is for selectedCases.
#' @export
#' @examples
#' groupingTable <- data.frame(group = rep("Cardiac dysrhythmias",6),
#'                             ICD = c("427.1","427.2","427.31","427.61","427.81","427.89"),
#'                             stringsAsFactors = FALSE)
#' groupedDataLongToWide(sampleDxFile, ID, ICD, Date, "2015-10-01",
#'                       customIcdGroup,
#'                       CustomGroupingTable = groupingTable)
#' selectedCaseFile <- selectCases("^785", sampleDxFile, ID, ICD, Date, 2)
#' groupedDataLongToWide(sampleDxFile, ID, ICD, Date, "2015-10-01",
#'                       ahrq, selectedCases = TRUE,
#'                       selectedCaseFile =  selectedCaseFile)
#'
groupedDataLongToWide <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, groupedICDMethod, isDescription = T, CustomGroupingTable, numericOrBinary=N, selectedCases = F, selectedCaseFile){
  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")

  groupedICDMethod <- tolower(deparse(substitute(groupedICDMethod)))
  if(grepl("ccs", groupedICDMethod)){
    groupedData <- IcdDxToCCS(DxDataFile, ID, ICD, Date, icd10usingDate, isDescription)
  }else if(grepl("ccslvl", groupedICDMethod)){
    CCSLevel <- as.numeric(sub("[A-Za-z]+","",groupedICDMethod))
    groupedData <- IcdDxToCCSLvl(DxDataFile, ID, ICD, Date, icd10usingDate, CCSLevel, isDescription)
  }else if(grepl("phecode", groupedICDMethod)){
    groupedData <- IcdDxToPhecode(DxDataFile, ID, ICD, Date, icd10usingDate, isDescription)
  }else if(grepl("ahrq", groupedICDMethod)){
    groupedData <- IcdDxToComorbid(DxDataFile, ID, ICD, Date, icd10usingDate, ahrq)
  }else if(grepl("charlson", groupedICDMethod)){
    groupedData <- IcdDxToComorbid(DxDataFile, ID, ICD, Date, icd10usingDate, charlson)
  }else if(grepl("elix", groupedICDMethod)){
    groupedData <- IcdDxToComorbid(DxDataFile, ID, ICD, Date, icd10usingDate, elix)
  }else if(grepl("customgrepicdgroup", groupedICDMethod)){
    groupedData <- IcdDxToCustomGrep(DxDataFile, ID, ICD, Date, CustomGroupingTable)
  }else if(grepl("customicdgroup", groupedICDMethod)){
    groupedData <- IcdDxToCustom(DxDataFile, ID, ICD, Date, CustomGroupingTable)
  }else{
    stop("'please enter `ccs`,`ccslvl`, `phecode`, `ahrq`, `charlson`, `elix` `customgrepicdgroup`, `customicdgroup` for 'groupedICDMethod'", call. = FALSE)
  }
  longFormat <- groupedData$groupedData_Long
  wideDt <- dcast(longFormat, ID~eval(parse(text = paste(names(longFormat)[2]))), value.var = c("count"))
  wideDt[is.na(wideDt)] <- 0L

  if(toupper(deparse(substitute(numericOrBinary))) == "B"){
    wideDt <-as.data.frame(wideDt >= 1L)
    wideDt$ID <- wideDt$ID
  }else if(toupper(deparse(substitute(numericOrBinary))) != "B" && toupper(deparse(substitute(numericOrBinary))) != "N"){
    stop("'please enter N or B for 'numericOrBinary'", call. = FALSE)
  }

  if(selectedCases == T){
    wideDt_selected <- merge(wideDt, selectedCaseFile[,list(ID, selectedCase)], all.x = T)
    return(wideDt_selected)
  }else{
    return(wideDt)
  }
}
