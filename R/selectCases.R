#' @rdname selectCase
#' @export
#'
selectCases <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, groupDataType = ccs, CustomGroupingTable, isDescription = TRUE, caseCondition, caseCount, PeriodRange = c(30, 365), CaseName = "Selected"){

  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol, with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile[,"Date"] <- as.Date(DxDataFile[,Date])
  nonCaseName <- paste0("non-",CaseName)
  semiCaseName <- paste0(CaseName,"*")

  groupDataType <- toupper(deparse(substitute(groupDataType)))
  groupedData <- groupMethodSelect(DxDataFile, ID, ICD, Date,
                                   icd10usingDate, groupDataType, CustomGroupingTable, isDescription)

  if(groupDataType != "ICD"){
    groupedData <- groupedData$groupedDT[,-"ICD"]
  }else{
    groupedData <- groupedData[,-"ICD"]
  }

  names(groupedData) <- gsub("Short|Decimal", "ICD", names(groupedData))
  groupDataType <- names(groupedData)[ncol(groupedData)]
  groupByCol <- c("ID",groupDataType)
  Case <- unique(groupedData[grepl(caseCondition, groupedData[,eval(parse(text = paste(groupDataType)))]),][order(ID, Date)])

  if(nrow(Case) > 0){
    if(caseCount > 1){
      CaseCount <- Case[, endCaseDate := shift(Date, caseCount -1 , type = "lead"), by = "ID"][is.na(endCaseDate), endCaseDate := Date][, period := endCaseDate - Date, by = "ID"][between(period, PeriodRange[1], PeriodRange[2], incbounds = TRUE),][,count := caseCount,]

      setnames(CaseCount,"Date", "firstCaseDate")

    }else{
      CaseCount <- Case[, c("firstCaseDate","endCaseDate","count") := list(min(Date), max(Date),.N), by = "ID"][,period := endCaseDate - firstCaseDate,][,-"Date"]
      CaseCount <- unique(CaseCount)
    }
  }else{
    nonSelectedCase <- DxDataFile[,list(ID)][,selectedCase := nonCaseName][!duplicated(ID),][order(ID),]
    message("No matching Case")
    return(nonSelectedCase)
  }

  CaseMostICDCount <- CaseCount[,list(MostCommonICDCount = .N),by = list(ID,ICD)][order(MostCommonICDCount, decreasing = TRUE),][!duplicated(ID),]
  selectedCase <- merge(CaseCount[,-"ICD"], CaseMostICDCount,"ID")[,selectedCase := CaseName]
  setnames(selectedCase,"ICD","MostCommonICD")
  nonSelectedCase <- DxDataFile[!Case, on = "ID", list(ID)][,selectedCase := nonCaseName][!duplicated(ID),]

  if(length(unique(Case$ID)) > length(unique(selectedCase$ID))){
    semiCase <- Case[!selectedCase, on = "ID", list(ID)][,selectedCase := semiCaseName][!duplicated(ID),]
    nonSelectedCase <- rbind(nonSelectedCase,semiCase)
  }

  allData <- rbindlist(list(nonSelectedCase, selectedCase),fill = TRUE, use.names = TRUE)[order(MostCommonICDCount,decreasing = TRUE),]
  allData <- allData[,c("ID","selectedCase","count","firstCaseDate","endCaseDate","period","MostCommonICD","MostCommonICDCount")]
  allData <- unique(allData)
  allData
}
