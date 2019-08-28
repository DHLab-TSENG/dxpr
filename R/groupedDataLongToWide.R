#' @rdname dataWide
#' @export
#'
groupedDataLongToWide <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, groupDataType = ccs, CustomGroupingTable, isDescription = TRUE, numericOrBinary = B, selectedCaseFile = NULL){
  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")

  groupDataType <- toupper(deparse(substitute(groupDataType)))
  groupedData <- groupMethodSelect(DxDataFile, ID, ICD, Date, icd10usingDate, groupDataType, CustomGroupingTable, isDescription)

  if(groupDataType != "ICD"){
    groupedData <- groupedData$summarised_groupedDT

    if(is.null(groupedData)){return(groupedData)}

  }else{

    groupedData <- groupedData[,list(firstCaseDate = min(Date),endCaseDate = max(Date),count = .N),by = c("ID","Short")][,period := (endCaseDate - firstCaseDate),]

  }

  groupedData_wide <- dcast(groupedData, ID~eval(parse(text = paste(names(groupedData)[2]))), value.var = c("count"))

  if(length(groupedData_wide$ID) != length(DxDataFile$ID)){
    OtherPatientID <- DxDataFile[!groupedData_wide, on = "ID"][!duplicated(ID),ID]
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
