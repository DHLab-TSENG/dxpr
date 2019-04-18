#' Select cases based on ICD code and the number of ICD codes
#'
#' This can be used to select qualified cases from factIcd data
#' based on the ICD code searching criteria and number of ICD code
#' per patients in the inout factIcd dataset.
#' Return qualified Members' data
#'
#' @import data.table
#' @param caseCondition ICD selection rules with grepl expression
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate Icd 10 using date
#' @param groupDataType  Four Stratified methods can be chosen: CCS (\code{'ccs'}), CCS levels (\code{'ccslvl1'}, \code{'ccslvl2'}, \code{'ccslvl3'}, \code{'ccslvl4'}), phecode (\code{'phecode'}), comorbidities (\code{'ahrq'},\code{'charlson'}, \code{'elix'}), grepICD or customICD (\code{'customGrepIcdGroup'}, \code{'customIcdGroup'}). Change it to any of the other possible variables, default it is set to \code{"ccs"}.
#' @param CustomGroupingTable Table is for groupDataType
#' @param isDescription  CCS/Phecode categories or description for ICD-CM codes, default is \code{'TRUE'}.
#' @param ICDNumber a threshold of number of ICD for case selection
#' @param INRofDayRange Determines the interval of days of interest for performing the case selection. By default it is set from 30 to 365 days.
#' @param selectCaseType Aggregation  of selected cases name. By default it is set to \code{"selected"}.
#' @export
#' @examples
#' head(sampleDxFile)
#' selectCases(DxDataFile = sampleDxFile,
#'             ID, ICD, Date,
#'             groupDataType = ccslvl2,
#'             icd10usingDate = "2015/10/01",
#'             caseCondition = "Diseases of the heart",
#'             ICDNumber = 2)
#'
selectCases <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, groupDataType = ICD, CustomGroupingTable, isDescription = TRUE, caseCondition, ICDNumber, INRofDayRange = c(30, 365), selectCaseType = "Selected"){
  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile[,"Date"] <- as.Date(DxDataFile[,Date])
  nonSelectCaseType <- paste0("non",selectCaseType)
  semiSelectCaseType <- paste0(selectCaseType,"*")

  groupDataType <- tolower(deparse(substitute(groupDataType)))
  groupedData <- groupMethodSelect(DxDataFile, ID, ICD, Date,
                                   icd10usingDate, groupDataType, CustomGroupingTable, isDescription)

  if(groupDataType != "icd"){
    groupedData <- groupedData$groupedDT
  }
  groupDataType <- names(groupedData)[ncol(groupedData)]
  groupByCol <- c("ID",groupDataType)

  Case <- groupedData[grepl(caseCondition, groupedData[,eval(parse(text = paste(groupDataType)))]),][order(ID,Date)]
  Case <- Case[!duplicated(Case)][,NextDate := c(Date[-1],NA),by = "ID"][is.na(NextDate),NextDate := Date][,diffDay := NextDate-Date][,Out := FALSE][diffDay > INRofDayRange[2],Out := TRUE][,OutCount:=cumsum(Out),by = "ID"][!Out == TRUE,]

  if(nrow(Case) > 0){
    CaseCount <- Case[,Gap := cumsum(as.integer(diffDay)),by = c("ID","OutCount")][,InTimeINR := Gap >= INRofDayRange[1] & Gap < INRofDayRange[2],][is.na(InTimeINR),InTimeINR := FALSE][,list(count = cumsum(InTimeINR), firstCaseDate = min(Date), endCaseDate = max(NextDate),period = Gap),by = c("ID","OutCount")][order(ID, count, decreasing = T)][!duplicated(ID),][count >= ICDNumber,][,selectedCase := selectCaseType][,-"OutCount"]

    CaseMostICDCount <- Case[InTimeINR ==TRUE,list(MostCommonICDCount = .N),by = list(ID,ICD)][order(MostCommonICDCount,decreasing = T),]
    selectedCase <- merge(CaseCount,CaseMostICDCount,"ID")[!duplicated(ID),]
    setnames(selectedCase,"ICD","MostCommonICD")
  }else{
    nonSelectedCase <- DxDataFile[,list(ID)][,selectedCase := nonSelectCaseType][!duplicated(ID),][order(ID),]
    return(nonSelectedCase)
  }
  nonSelectedCase <- DxDataFile[!Case, on = "ID", list(ID)][,selectedCase := nonSelectCaseType][!duplicated(ID),]

  if(length(unique(Case$ID)) > length(unique(selectedCase$ID))){
    semiCase <- Case[!selectedCase, on = "ID", list(ID)][,selectedCase := semiSelectCaseType][!duplicated(ID),]
    nonSelectedCase <- rbind(nonSelectedCase,semiCase)
  }

  allData <- merge(selectedCase,nonSelectedCase,by = names(nonSelectedCase),all=T)[order(MostCommonICDCount,decreasing = T),]

  allData
}
