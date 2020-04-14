#' @rdname selectCases
#' @export
#'

selectCases <- function(dxDataFile, idColName, icdColName, dateColName, icdVerColName = NULL, icd10usingDate = NULL, groupDataType = CCS, customGroupingTable, isDescription = TRUE, caseCondition, caseCount, periodRange = c(30, 365), caseName = "Selected"){

  dxDataFile <- as.data.table(dxDataFile)
  if(deparse(substitute(icdVerColName)) != "NULL"){
    dataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)), deparse(substitute(icdVerColName)))
    dxDataFile <- dxDataFile[,dataCol, with = FALSE]
    names(dxDataFile) <- c("ID", "ICD", "Date", "Version")
  }else{
    dataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
    dxDataFile <- dxDataFile[,dataCol, with = FALSE]
    names(dxDataFile) <- c("ID", "ICD", "Date")
  }

  dxDataFile[,"Date"] <- as.Date(format(dxDataFile[,Date]))
  nonCaseName <- paste0("non-",caseName)
  semiCaseName <- paste0(caseName,"*")

  groupDataType <- toupper(deparse(substitute(groupDataType)))

  if(deparse(substitute(icdVerColName)) != "NULL"){
    groupedData <- groupMethodSelect(dxDataFile, idColName = ID, icdColName = ICD, dateColName = Date,
                                     icdVerColName = Version, groupMethod = groupDataType, customGroupingTable = customGroupingTable, isDescription = isDescription)
  }else{
    groupedData <- groupMethodSelect(dxDataFile, idColName = ID, icdColName = ICD, dateColName = Date,
                                     icd10usingDate = icd10usingDate, groupMethod = groupDataType, customGroupingTable = customGroupingTable, isDescription = isDescription)
  }
  if(groupDataType != "ICD"){
    groupedData <- groupedData$groupedDT
    names(groupedData) <- gsub("Short|Decimal", "UNIICD", names(groupedData))
  }else{
    names(groupedData) <- gsub("Short|Decimal", "UNIICD", names(groupedData))
  }
  groupDataType <- names(groupedData)[ncol(groupedData)]
  groupByCol <- c("ID",groupDataType)
  if (groupDataType == "UNIICD"){
    Case <- unique(groupedData[grepl(caseCondition, groupedData[,eval(parse(text = paste(groupDataType)))])|grepl(caseCondition, groupedData[,ICD]),][order(ID, Date)]) # EVERY ICD IS UNIQUEs
  }else{
    Case <- unique(groupedData[grepl(toupper(caseCondition), toupper(groupedData[,eval(parse(text = paste(groupDataType)))])),][order(ID, Date)]) # EVERY ICD only ONCE at ONE DAY
  }

  if(nrow(Case) > 0){
    CaseMostICD <- Case[,list(MostCommonICDCount = .N),by = list(ID,ICD)][order(MostCommonICDCount, decreasing = TRUE),][!duplicated(ID),]
    setnames(CaseMostICD, "ICD", "MostCommonICD")
    if(caseCount > 1){
      chosenCase <- Case[, endCaseDate := shift(Date, caseCount -1 , type = "lead"), by = "ID"][is.na(endCaseDate), endCaseDate := Date][, period := endCaseDate - Date, by = "ID"][,mark := ifelse(between(period, periodRange[1], periodRange[2], incbounds = TRUE), 1,0)][order(mark, decreasing = TRUE),][!duplicated(ID),]
      chosenCase <- chosenCase[,selectedCase := ifelse(mark == 1, caseName, semiCaseName)][,c("ID", "selectedCase"),with=FALSE]
    }else{
      chosenCase <- Case[,selectedCase := caseName][,c("ID", "selectedCase"),with=FALSE][!duplicated(ID),]
    }
    CaseCount <- Case[, c("firstCaseDate","endCaseDate") := list(min(Date), max(Date)), by = "ID"][,period := endCaseDate - firstCaseDate,]
    CaseCount <- unique(CaseCount, by = c('ID', 'Date'))
    CaseCount <- CaseCount[,count := .N, by = ID][,-"Date"][!duplicated(ID), c("ID", "firstCaseDate", "endCaseDate", "count", "period")]
  }else{
    nonSelectedCase <- dxDataFile[,list(ID)][,selectedCase := nonCaseName][!duplicated(ID),][order(ID),]
    message("No matching Case")
    return(nonSelectedCase)
  }

  selectedCase <- merge(CaseCount, CaseMostICD, "ID") # allow.cartesian = TRUE
  selectedCase <- merge(selectedCase, chosenCase, "ID")  # allow.cartesian = TRUE
  nonSelectedCase <- dxDataFile[!Case, on = "ID", list(ID)][,selectedCase := nonCaseName][!duplicated(ID),]

  allData <- rbindlist(list(nonSelectedCase, selectedCase),fill = TRUE, use.names = TRUE)[order(MostCommonICDCount,decreasing = TRUE),]
  allData <- allData[,c("ID","selectedCase","count","firstCaseDate","endCaseDate","period","MostCommonICD","MostCommonICDCount")]
  allData <- unique(allData)
  allData
}
