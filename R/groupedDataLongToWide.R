if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "groupedICDMethod",
  "CustomGroupingTable",
  "isDescription",
  "N",
  "ahrq",
  "charlson",
  "elix"))
#' grouping comorbid method comorbidities (AHRQ, Charlson and Elixhauser Comorbidity) infers whether to use ICD-9 or ICD-10 codes
#'
#' Get comorbidities using the comorbidity methods based on ICD code in clinical diagnostic data.
#'
#' return comorbidity meseaures based on ICD diagnosis codes
#'
#' @import reshape2
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate Icd 10 using date
#' @param groupedICDMethod  Four grouping method can be chosen: CCS (or CCS levels), phecode (only for icd-9), comorbidities, grepICD or customICD , type `ccs`,`ccslvl`, `phecode`, `ahrq`, `charlson`, `elix` `grepICD`, `customGroup`
#' @param isDescription  Clinical Classifications Software (CCS) categories/description for ICD-9 or ICD-10, default is True
#' @param CCSLevel Clinical Classifications Software (CCS) multiple level
#' @param CustomGroupingTable Grouping rules of clustering the ICD is based on yourself! There are two column in the dataframe: Group, ICD
#' @param numericOrBinary  Member have one (or more) diagnostic comorbidities, type `N` or `B`, default is `B` (Binary)

#' @export
#' @examples
#' groupingTable <- data.frame(group = rep("Cardiac dysrhythmias",6),
#'                             ICD = c("427.1","427.2","427.31","427.61","427.81","427.89"),
#'                             stringsAsFactors = FALSE)
#' grepTable <- data.frame(group = c("Cardiac dysrhythmias"),
#'                         grepIcd = c("^427|^I48"),
#'                         stringsAsFactors = FALSE)
#' groupedDataLongToWide(testDxFile, ID, ICD, Date, "2015-10-01", ccs)
#' groupedDataLongToWide(testDxFile, ID, ICD, Date, "2015-10-01", ccslvl)
#' groupedDataLongToWide(testDxFile, ID, ICD, Date, "2015-10-01", phecode)
# groupedDataLongToWide(testDxFile, ID, ICD, Date, "2015-10-01", ahrq)
# groupedDataLongToWide(testDxFile, ID, ICD, Date, "2015-10-01", charlson)
#' groupedDataLongToWide(testDxFile, ID, ICD, Date, "2015-10-01", groupedICDMethod = elix)
#' groupedDataLongToWide(testDxFile, ID, ICD, Date, "2015-10-01", grepicd,
#'                       CustomGroupingTable = grepTable)
#' groupedDataLongToWide(testDxFile, ID, ICD, Date, "2015-10-01", customgroup,
#'                       CustomGroupingTable = groupingTable)
#'
groupedDataLongToWide <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, groupedICDMethod, isDescription = T, CCSLevel = 2, CustomGroupingTable, numericOrBinary=N){
  DxDataFile <- DxDataFile[ , c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  groupedICDMethod <- tolower(deparse(substitute(groupedICDMethod)))
  if(grepl("ccs", groupedICDMethod)){
    groupedData <- IcdDxToCCS(DxDataFile, ID, ICD, Date, icd10usingDate, isDescription)
  }else if(grepl("ccslvl", groupedICDMethod)){
    groupedData <- IcdDxToCCSLvl(DxDataFile, ID, ICD, Date, icd10usingDate, CCSLevel, isDescription)
  }else if(grepl("phecode", groupedICDMethod)){
    groupedData <- IcdDxToPhecode(DxDataFile, ID, ICD, Date, icd10usingDate, isDescription)
  }else if(grepl("ahrq", groupedICDMethod)){
    groupedData <- IcdDxToComorbid(DxDataFile, ID, ICD, Date, icd10usingDate, ahrq)
  }else if(grepl("charlson", groupedICDMethod)){
    groupedData <- IcdDxToComorbid(DxDataFile, ID, ICD, Date, icd10usingDate, charlson)
  }else if(grepl("elix", groupedICDMethod)){
    groupedData <- IcdDxToComorbid(DxDataFile, ID, ICD, Date, icd10usingDate, elix)
  }else if(grepl("grepicd", groupedICDMethod)){
    groupedData <- IcdToCustomGrep(DxDataFile, ID, ICD, Date, CustomGroupingTable)
  }else if(grepl("customgroup", groupedICDMethod)){
    groupedData <- IcdDxToCustom(DxDataFile, ID, ICD, Date, CustomGroupingTable)
  }else{
    stop("'please enter `ccs`,`ccslvl`, `phecode`, `ahrq`, `charlson`, `elix` `grepICD`, `customGroup` for 'groupedICDMethod'", call. = FALSE)
  }
  longFormat <- groupedData$groupedData_Long
  wideNumericDt <- dcast(longFormat, ID~eval(parse(text = paste(names(longFormat)[2]))), value.var = c("count"))
  wideNumericDt[is.na(wideNumericDt)] <- 0L
  if(toupper(deparse(substitute(numericOrBinary))) == "B"){
    wideBinaryDt <-as.data.frame(wideNumericDt >= 1L)
    wideBinaryDt$ID <- wideNumericDt$ID
    return(wideBinaryDt)
  }else if(toupper(deparse(substitute(numericOrBinary))) == "N"){
    return(wideNumericDt)
  }else{
    stop("'please enter N or B for 'numericOrBinary'", call. = FALSE)
  }
}
