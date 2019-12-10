#' @importFrom stats complete.cases
#' @export
#' @rdname dxCCS
#'
icdDxToCCS <- function(dxDataFile, idColName, icdColName, dateColName, icdVerColName = NULL, icd10usingDate = NULL, isDescription = TRUE){
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

  dxDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(dxDataFile))]

  if(deparse(substitute(icdVerColName)) != "NULL"){
    Conversion <- icdDxDecimalToShort(dxDataFile, ICD, Date, icdVerColName = Version, icd10usingDate = NULL)
  }else{
    Conversion <- icdDxDecimalToShort(dxDataFile, ICD, Date, icdVerColName = NULL, icd10usingDate = icd10usingDate)
  }

  dxDataFile[,Short := Conversion$ICD]

  if (isDescription){
    ccs_col <- "CCS_CATEGORY_DESCRIPTION"
  }else {
    ccs_col <- "CCS_CATEGORY"
  }
  if (deparse(substitute(icdVerColName)) != "NULL"){
    allCCS <- rbind(merge(dxDataFile[Version == 9,],ccsDxICD9[,c("ICD",ccs_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                    merge(dxDataFile[Version == 10,],ccsDxICD10[,c("ICD",ccs_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))
  }else{
    allCCS <- rbind(merge(dxDataFile[Date <icd10usingDate],ccsDxICD9[,c("ICD",ccs_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                    merge(dxDataFile[Date >=icd10usingDate],ccsDxICD10[,c("ICD",ccs_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))
  }

  allCCS <- allCCS[order(Number),-"Number"]

  if(nrow(allCCS[is.na(eval(parse(text = paste(ccs_col))))]) < nrow(allCCS)){
    summarisedIcdToCCS <- allCCS[!is.na(eval(parse(text = paste(ccs_col)))),
                                 list(firstCaseDate = min(Date),
                                      endCaseDate = max(Date),
                                      count = .N),
                                 by = c("ID",ccs_col)][,period := (endCaseDate - firstCaseDate),][order(ID)]

    return(list(groupedDT = allCCS,
                summarised_groupedDT = summarisedIcdToCCS,
                Error = Conversion$Error))
  }else{
    return(list(groupedDT = allCCS,
                Error = Conversion$Error))
  }
}
