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
      Case <- Case[,NextDate := c(Date[-1], NA),by = "ID"][is.na(NextDate), NextDate := Date][, diffDay := NextDate-Date][, OutRange := FALSE][diffDay > PeriodRange[2], OutRange := TRUE][,OutCount:=cumsum(OutRange), by = "ID"][!OutRange == TRUE,]

      CaseCount <- Case[,Gap := cumsum(as.integer(diffDay)), by = c("ID","OutCount")][, InRange := Gap >= PeriodRange[1] & Gap < PeriodRange[2]][is.na(InRange),InRange := FALSE][,list(count = cumsum(InRange),firstCaseDate = min(Date),endCaseDate = max(NextDate),period = Gap),by = c("ID","OutCount")][!duplicated(ID),][count >= caseCount,][,-"OutCount"]
      CaseMostICDCount <- Case[InRange == TRUE,list(MostCommonICDCount = .N),by = list(ID,ICD)]

    }else{
      CaseCount <- Case[,list(count = .N,firstCaseDate = min(Date),endCaseDate = max(Date)),by = c("ID")][,period := endCaseDate-firstCaseDate,]
      CaseMostICDCount <- Case[,list(MostCommonICDCount = .N),by = list(ID,ICD)]
    }
  }else{
    nonSelectedCase <- DxDataFile[,list(ID)][,selectedCase := nonCaseName][!duplicated(ID),][order(ID),]
    return(nonSelectedCase)
  }

  selectedCase <- merge(CaseCount,CaseMostICDCount,"ID")[!duplicated(ID),][,selectedCase := CaseName]
  setnames(selectedCase,"ICD","MostCommonICD")
  nonSelectedCase <- DxDataFile[!Case, on = "ID", list(ID)][,selectedCase := nonCaseName][!duplicated(ID),]
  if(length(unique(Case$ID)) > length(unique(selectedCase$ID))){
    semiCase <- Case[!selectedCase, on = "ID", list(ID)][,selectedCase := semiCaseName][!duplicated(ID),]
    nonSelectedCase <- rbind(nonSelectedCase,semiCase)
  }

  allData <- merge(selectedCase,nonSelectedCase,by = names(nonSelectedCase),all = TRUE)[order(MostCommonICDCount,decreasing = TRUE),]

  allData
}
