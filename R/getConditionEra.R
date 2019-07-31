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
  if(is.null(groupedData)){return(groupedData)}
  groupDataType <- names(groupedData)[ncol(groupedData)]
  groupByCol <- c("ID",groupDataType)

  if(nrow(groupedData[is.na(eval(parse(text = paste(groupDataType))))]) == nrow(groupedData)){return(groupedData)}

  count <- groupedData[nchar(eval(parse(text = paste(groupDataType)))) > 0  & !is.na(eval(parse(text = paste(groupDataType))))][order(eval(parse(text = paste(groupByCol))),Date)][,NextDate := c(Date[-1], NA),by = groupByCol][is.na(NextDate),NextDate := Date][,Gap := NextDate- Date]

  conditionEra <- count[,episode := Gap > gapDate][is.na(episode),episode :=TRUE][,list(episodeCount = cumsum(episode)+1,
                                                                                        firstCaseDate = min(Date),
                                                                                        endCaseDate = max(Date),
                                                                                        count = .N),by = groupByCol][,era := max(episodeCount),by = groupByCol][,period := endCaseDate - firstCaseDate,][,-"episodeCount"]

  conditionEra <- unique(conditionEra)

  if(!is.null(selectedCaseFile)){
    conditionEra <- merge(conditionEra, selectedCaseFile[,list(ID, selectedCase)], all.x = TRUE)
  }
  conditionEra <- conditionEra[order(ID),]
  conditionEra
}
