#'
#' Select code classification method
#'
#' return grouped data
#' @import data.table
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate Icd 10 using date
#' @param groupMethod  Four Stratified methods can be chosen: CCS (\code{'ccs'}), CCS levels (\code{'ccslvl1'}, \code{'ccslvl2'}, \code{'ccslvl3'}, \code{'ccslvl4'}), PheWAS (\code{'PheWAS'}), comorbidities (\code{'ahrq'},\code{'charlson'}, \code{'elix'}), grepICD or customICD (\code{'customGrepIcdGroup'}, \code{'customIcdGroup'}). Change it to any of the other possible variables.
#' @param CustomGroupingTable Table is for groupDataType
#' @param isDescription  CCS/PheWAS categories or description for ICD-CM codes, default is \code{'TRUE'}.
#'
groupMethodSelect <- function(DxDataFile,idColName, icdColName, dateColName, icd10usingDate, groupMethod, CustomGroupingTable, isDescription){
  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile[,"Date"] <- as.Date(DxDataFile[,Date])

  if(groupMethod == "CCS"){
    groupedData <- IcdDxToCCS(DxDataFile, ID, ICD, Date, icd10usingDate, isDescription)
  }else if(grepl("CCSLVL" ,groupMethod)){
    CCSLevel <- as.numeric(sub("[A-Za-z]+","",groupMethod))
    groupedData <- IcdDxToCCSLvl(DxDataFile, ID, ICD, Date, icd10usingDate, CCSLevel, isDescription)
  }else if(groupMethod == "PHEWAS"){
    groupedData <- IcdDxToPheWAS(DxDataFile, ID, ICD, Date, icd10usingDate, isDescription)
  }else if(groupMethod == "AHRQ"){
    groupedData <- IcdDxToComorbid(DxDataFile, ID, ICD, Date, icd10usingDate, ahrq, isDescription)
  }else if(groupMethod == "CHARLSON"){
    groupedData <- IcdDxToComorbid(DxDataFile, ID, ICD, Date, icd10usingDate, charlson, isDescription)
  }else if(groupMethod == "ELIX"){
    groupedData <- IcdDxToComorbid(DxDataFile, ID, ICD, Date, icd10usingDate, elix, isDescription)
  }else if(groupMethod == "CUSTOMGREPICDGROUP"){
    groupedData <- IcdDxToCustomGrep(DxDataFile, ID, ICD, Date, CustomGroupingTable)
  }else if(groupMethod == "CUSTOMICDGROUP"){
    groupedData <- IcdDxToCustom(DxDataFile, ID, ICD, Date, CustomGroupingTable)
  }else if(groupMethod == "ICD"){
    groupedData <- DxDataFile[, Short :=IcdDxDecimalToShort(DxDataFile, ICD, Date, icd10usingDate)$ICD]
  }else{
    stop("'please enter `ccs`,`ccslvl`, `PheWAS`, `ahrq`, `charlson`, `elix` `customgrepicdgroup`, `customicdgroup` for 'groupMethod'", call. = FALSE)
  }
  groupedData
}
