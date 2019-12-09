#' @rdname dataWide
#' @export
#'
groupedDataLongToWide <- function(dxDataFile, idColName, icdColName, dateColName, icdVerColName = NULL, icd10usingDate = NULL, groupDataType = ccs, customGroupingTable = NULL, isDescription = TRUE, numericOrBinary = B, selectedCaseFile = NULL){
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

  groupDataType <- toupper(deparse(substitute(groupDataType)))
  if(deparse(substitute(icdVerColName)) != "NULL"){
    groupedData <- groupMethodSelect(dxDataFile, idColName = ID, icdColName = ICD, dateColName = Date,
                                     icdVerColName = Version, groupMethod = groupDataType, customGroupingTable = customGroupingTable, isDescription = isDescription)
  }else{
    groupedData <- groupMethodSelect(dxDataFile, idColName = ID, icdColName = ICD, dateColName = Date,
                                     icd10usingDate = icd10usingDate, groupMethod = groupDataType, customGroupingTable = customGroupingTable, isDescription = isDescription)
  }

  if(groupDataType != "ICD"){
    groupedData <- groupedData$summarised_groupedDT
    if(is.null(groupedData)){
      return(groupedData)
    }
  }else{
    groupedData <- groupedData[, list(firstCaseDate = min(Date), endCaseDate = max(Date), count = .N), by = c("ID","Short")][, period := (endCaseDate - firstCaseDate),]
  }
  groupedData_wide <- dcast(groupedData, ID~eval(parse(text = paste(names(groupedData)[2]))), value.var = c("count"))

  if(length(groupedData_wide$ID) != length(dxDataFile$ID)){
    OtherPatientID <- dxDataFile[!groupedData_wide, on = "ID"][!duplicated(ID),ID]
    OtherPatientDt <- data.table(matrix(ncol = ncol(groupedData_wide),nrow = length(OtherPatientID)))
    names(OtherPatientDt) <- names(groupedData_wide)
    OtherPatientDt[,"ID"] <- OtherPatientID
    wideData <- rbind(groupedData_wide, OtherPatientDt)
  }

  wideData[is.na(wideData)] <- 0L
  numericOrBinary <- toupper(deparse(substitute(numericOrBinary)))

  if(numericOrBinary == "B"){
    wideData_B <- as.data.frame(wideData >= 1L)
    wideData_B$ID <- wideData$ID
    wideData <- wideData_B
  }else if(numericOrBinary != "B" && numericOrBinary != "N"){
    stop("'please enter N or B for 'numericOrBinary'", call. = FALSE)
  }

  if(!is.null(selectedCaseFile)){
    wideData_selected <- merge(wideData, selectedCaseFile[,list(ID, selectedCase)],by = "ID")
    if(isDescription == FALSE  && groupDataType == "ICD|CCS|CCSLVL|PHEWAS"){
      names(wideData_selected)[2:(ncol(wideData_selected)-1)] <- paste0(groupDataType,"_",names(wideData_selected)[2:(ncol(wideData_selected)-1)])
    }
    return(wideData_selected)
  }else{
    if(isDescription == FALSE && groupDataType == "ICD|CCS|CCSLVL|PHEWAS"){
      names(wideData)[2:ncol(wideData)] <- paste0(groupDataType,"_",names(wideData)[2:ncol(wideData)])
    }
    return(wideData)
  }
}
