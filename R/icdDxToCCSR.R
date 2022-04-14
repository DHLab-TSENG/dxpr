#' @importFrom stats complete.cases
#' @export
#' @rdname dxCCSR
#'
icdDxToCCSR <- function(dxDataFile, idColName, icdColName, dateColName, icdVerColName = NULL, icd10usingDate = NULL, isDescription = TRUE){
  dxDataFile <- as.data.table(dxDataFile)

  if(deparse(substitute(icdVerColName)) != "NULL"){
    dataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)), deparse(substitute(icdVerColName)))
    dxDataFile <- dxDataFile[,dataCol,with = FALSE]
    names(dxDataFile) <- c("ID", "ICD", "Date", "Version")
  }else{
    dataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
    dxDataFile <- dxDataFile[,dataCol,with = FALSE]
    names(dxDataFile) <- c("ID", "ICD", "Date")
  }

  dxDataFile[,c("Date", "Number") := list(as.Date(format(Date)), 1:nrow(dxDataFile))]

  if(deparse(substitute(icdVerColName)) != "NULL"){
    Conversion <- icdDxDecimalToShort(dxDataFile, ICD, Date, icdVerColName = Version, icd10usingDate = NULL)
  }else{
    Conversion <- icdDxDecimalToShort(dxDataFile, ICD, Date, icdVerColName = NULL, icd10usingDate = icd10usingDate)
  }

  dxDataFile[,Short := Conversion$ICD]

  if (isDescription){
    ccsr_col <- "CCSR_CATEGORY_DESCRIPTION"
  }else {
    ccsr_col <- "CCSR_CATEGORY"
  }
  if (deparse(substitute(icdVerColName)) != "NULL"){
    allCCSR <- rbind(dxDataFile[Version == 9,],
                    merge(dxDataFile[Version == 10,],ccsrDxSingle[,c("ICD",ccsr_col)],by.x ="Short",by.y = "ICD",all.x = TRUE),  fill=TRUE)
  }else{
    allCCSR <- rbind(dxDataFile[Date <icd10usingDate],
                    merge(dxDataFile[Date >=icd10usingDate],ccsrDxSingle[,c("ICD",ccsr_col)],by.x ="Short",by.y = "ICD",all.x = TRUE),  fill=TRUE)
  }

  allCCSR <- allCCSR[order(Number),-"Number"]

  if(nrow(allCCSR[is.na(eval(parse(text = paste(ccsr_col))))]) < nrow(allCCSR)){
    summarisedIcdToCCSR <- allCCSR[!is.na(eval(parse(text = paste(ccsr_col)))),
                                 list(firstCaseDate = min(Date),
                                      endCaseDate = max(Date),
                                      count = .N),
                                 by = c("ID",ccsr_col)][,period := (endCaseDate - firstCaseDate),][order(ID)]

    return(list(groupedDT = allCCSR,
                summarised_groupedDT = summarisedIcdToCCSR,
                Error = Conversion$Error))
  }else{
    return(list(groupedDT = allCCSR,
                Error = Conversion$Error))
  }
}
