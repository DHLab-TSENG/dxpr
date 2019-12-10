#' @export
#' @rdname prCCS
#'
icdPrToCCS <- function(prDataFile, idColName, icdColName, dateColName, icdVerColName = NULL, icd10usingDate = NULL, isDescription = TRUE){
  prDataFile <- as.data.table(prDataFile)

  if(deparse(substitute(icdVerColName)) != "NULL"){
    dataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)), deparse(substitute(icdVerColName)))
    prDataFile <- prDataFile[,dataCol,with = FALSE]
    names(prDataFile) <- c("ID", "ICD", "Date", "Version")
  }else{
    dataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
    prDataFile <- prDataFile[,dataCol,with = FALSE]
    names(prDataFile) <- c("ID", "ICD", "Date")
  }
  if(deparse(substitute(icdVerColName)) != "NULL"){
    Conversion <- icdPrDecimalToShort(prDataFile, ICD, Date, icdVerColName = Version, icd10usingDate = NULL)
  }else{
    Conversion <- icdPrDecimalToShort(prDataFile, ICD, Date, icdVerColName = NULL, icd10usingDate = icd10usingDate)
  }

  prDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(prDataFile))]
  prDataFile[,Short := Conversion$ICD]


  if (isDescription) {
    ccs_col <- "CCS_CATEGORY_DESCRIPTION"
  } else {
    ccs_col <- "CCS_CATEGORY"
  }

  if (deparse(substitute(icdVerColName)) != "NULL"){
    icdToCCS <- rbind(merge(prDataFile[Version == 9,],ccsPrICD9[,c("ICD",ccs_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                     merge(prDataFile[Version == 10,],ccsPrICD10[,c("ICD",ccs_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))
  }else{
    icdToCCS <- rbind(merge(prDataFile[Date <icd10usingDate],ccsPrICD9[,c("ICD",ccs_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                      merge(prDataFile[Date >=icd10usingDate],ccsPrICD10[,c("ICD",ccs_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))
  }
  icdToCCS <- icdToCCS[order(Number), -"Number"]


  return(list(groupedDT = icdToCCS,
              Error = Conversion$Error))
}
