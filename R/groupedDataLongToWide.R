#' @rdname groupedDataLongToWide
#' @export
#'

groupedDataLongToWide <- function(dxDataFile, idColName, categoryColName, dateColName, reDup = TRUE, numericOrBinary = B, count = 1, selectedCaseFile = NULL){
  dxDataFile <- as.data.table(dxDataFile)

  dataCol <- c(deparse(substitute(idColName)), deparse(substitute(categoryColName)), deparse(substitute(dateColName)))
  dxDataFile <- dxDataFile[,dataCol, with = FALSE]
  names(dxDataFile) <- c("ID", "Category", "Date")

  if (reDup == TRUE){
    dxDataFile <- unique(dxDataFile)
  }
  groupedData <- dxDataFile[!is.na(Category), .(count = .N), by = c("ID","Category")]

  groupedData_wide <- dcast(groupedData, ID ~ Category , value.var = c("count"))

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
    wideData_B <- as.data.table(wideData >= count)
    wideData_B$ID <- wideData$ID
    wideData <- wideData_B
  }else if(numericOrBinary != "B" && numericOrBinary != "N"){
    stop("'please enter N or B for 'numericOrBinary'", call. = FALSE)
  }

  if(!is.null(selectedCaseFile)){
    wideData_selected <- merge(wideData, selectedCaseFile[,list(ID, selectedCase)],by = "ID")
    return(wideData_selected)
  }else{
    return(wideData)
  }
}
