#' @importFrom stats complete.cases
#' @export
#' @rdname DxCCS
#'
IcdDxToCCS <- function(DxDataFile, idColName, icdColName, dateColName, icdVerColName, icd10usingDate, isDescription = TRUE){
  DxDataFile <- as.data.table(DxDataFile)

  if(deparse(substitute(icdVerColName)) != "NULL"){
    DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)), deparse(substitute(icdVerColName)))
    DxDataFile <- DxDataFile[,DataCol,with = FALSE]
    names(DxDataFile) <- c("ID", "ICD", "Date", "Version")
  }else{
    DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
    DxDataFile <- DxDataFile[,DataCol,with = FALSE]
    names(DxDataFile) <- c("ID", "ICD", "Date")
  }

  DxDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(DxDataFile))]

  if(deparse(substitute(icdVerColName)) != "NULL"){
    Conversion <- IcdDxDecimalToShort(DxDataFile, ICD, Date, icdVerColName = Version, icd10usingDate = NULL)
  }else{
    Conversion <- IcdDxDecimalToShort(DxDataFile, ICD, Date, icdVerColName = NULL, icd10usingDate = icd10usingDate)
  }

  DxDataFile[,Short := Conversion$ICD]

  if (isDescription){
    ccs_col <- "CCS_CATEGORY_DESCRIPTION"
  }else {
    ccs_col <- "CCS_CATEGORY"
  }
  if (deparse(substitute(icdVerColName)) != "NULL"){
    allCCS <- rbind(merge(DxDataFile[Version == 9,],ccsDxICD9[,c("ICD",ccs_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                    merge(DxDataFile[Version == 10,],ccsDxICD10[,c("ICD",ccs_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))
  }else{
    allCCS <- rbind(merge(DxDataFile[Date <icd10usingDate],ccsDxICD9[,c("ICD",ccs_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                    merge(DxDataFile[Date >=icd10usingDate],ccsDxICD10[,c("ICD",ccs_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))
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
