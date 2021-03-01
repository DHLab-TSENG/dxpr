#' @rdname getConditionEra
#' @export
#'

getConditionEra <- function(dxDataFile, idColName, icdColName, dateColName, icdVerColName = NULL, icd10usingDate = NULL, groupDataType = ccs, customGroupingTable, isDescription = TRUE, gapDate = 30, selectedCaseFile = NULL){
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

  dxDataFile[,"Date"] <- as.Date(dxDataFile[,Date])
  groupDataType <- toupper(deparse(substitute(groupDataType)))

  if(deparse(substitute(icdVerColName)) != "NULL"){
    groupedData <- groupMethodSelect(dxDataFile, idColName = ID, icdColName = ICD, dateColName = Date,
                                     icdVerColName = Version, groupMethod = groupDataType, customGroupingTable = customGroupingTable, isDescription = isDescription)
  }else{
    groupedData <- groupMethodSelect(dxDataFile, idColName = ID, icdColName = ICD, dateColName = Date,
                                     icd10usingDate = icd10usingDate, groupMethod = groupDataType, customGroupingTable = customGroupingTable, isDescription = isDescription)
  }

  if(groupDataType != "ICD"){
    groupedData <- unique(groupedData$groupedDT[,-"ICD"])
  }else{
    groupedData <- unique(groupedData[,-"ICD"])
    names(groupedData) <- gsub("Short|Decimal", "ICD", names(groupedData))
  }

  groupDataType <- names(groupedData)[ncol(groupedData)]
  groupByCol <- c("ID",groupDataType)


  if(is.null(groupedData) | nrow(groupedData[is.na(eval(parse(text = paste(groupDataType))))]) == nrow(groupedData)){
    return(groupedData)
  }else{
    groupedData <- na.omit(groupedData, groupDataType)
  }

  #  groupedData <- groupedData[order(eval(parse(text = paste(groupByCol))),Date)]
  setorderv(groupedData, c(groupByCol, "Date"))

  eraCount <- groupedData[,Gap :=  Date - shift(Date, fill = Date[1L], type = "lag")][rowid(ID, eval(as.name(groupDataType)))==1, Gap := 0][,episode := Gap > gapDate][,era := cumsum(episode) +1 , by = groupByCol]#[,-"episode"]
  eraCount <- eraCount[,list(firstCaseDate = min(Date),
                             endCaseDate = max(Date),
                             count = .N,
                             period = max(Date) - min(Date)), by = c(groupByCol,"era")]#[order(ID),]
  if(!is.null(selectedCaseFile)){
    eraCount <- merge(eraCount, selectedCaseFile[,list(ID, selectedCase)], all.x = TRUE)
  }
  eraCount
}
