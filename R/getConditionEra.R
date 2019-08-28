#' @rdname era
#' @export
#'
getConditionEra <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, groupDataType = ccs, CustomGroupingTable, isDescription = TRUE, gapDate = 30, selectedCaseFile = NULL){

  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile[,"Date"] <- as.Date(DxDataFile[,Date])
  groupDataType <- toupper(deparse(substitute(groupDataType)))
  groupedData <- groupMethodSelect(DxDataFile, ID, ICD, Date,
                                   icd10usingDate, groupDataType, CustomGroupingTable, isDescription)

  if(groupDataType != "ICD"){
    groupedData <- groupedData$groupedDT[,-"ICD"]
  }else{
    groupedData <- groupedData[,-"ICD"]
    names(groupedData) <- gsub("Short|Decimal", "ICD", names(groupedData))
  }

  groupDataType <- names(groupedData)[ncol(groupedData)]
  groupByCol <- c("ID",groupDataType)

  if(is.null(groupedData) | nrow(groupedData[is.na(eval(parse(text = paste(groupDataType))))]) == nrow(groupedData)){
    return(groupedData)
  }else{
    groupedData <- na.omit(groupedData, groupDataType)
  }

  eraCount <- groupedData[order(eval(parse(text = paste(groupByCol))),Date)][,LastDate := shift(Date, type = "lag"),by = groupByCol][is.na(LastDate), LastDate := Date][,Gap := Date- LastDate][,episode := Gap > gapDate][,era := cumsum(episode), by = groupByCol][,-"episode"]

  if(sum(eraCount$era == 0) > 0){
    eraPlusOne_ID <- eraCount[grepl(0, era), groupByCol, with = FALSE]
    eraPlusOne_ID <- unique(eraPlusOne_ID)
    eraPlusOne <- merge(eraCount, eraPlusOne_ID, by = names(eraPlusOne_ID))[,"era" := era+1,]
    eraCount <- rbind(eraPlusOne, eraCount[!eraPlusOne, on = groupByCol])
  }

  conditionEra <- eraCount[,list(firstCaseDate = min(Date),
                                 endCaseDate = max(Date),
                                 count = .N,
                                 period = Gap),by = c(groupByCol,"era")][!(period == 0 & count > 1),]

  if(!is.null(selectedCaseFile)){
    conditionEra <- merge(conditionEra, selectedCaseFile[,list(ID, selectedCase)], all.x = TRUE)
  }
  conditionEra <- conditionEra[order(ID),]
  conditionEra
}
