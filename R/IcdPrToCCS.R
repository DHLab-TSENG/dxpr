#' @export
#' @rdname PrCCS
#'
IcdPrToCCS <- function(PrDataFile, idColName, icdColName, dateColName, icd10usingDate, isDescription = TRUE){
  PrDataFile <- as.data.table(PrDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  PrDataFile <- PrDataFile[,DataCol,with = FALSE]
  names(PrDataFile) <- c("ID","ICD", "Date")
  Conversion <- IcdPrDecimalToShort(PrDataFile,ICD,Date,icd10usingDate)
  PrDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(PrDataFile))]
  PrDataFile[,Short := Conversion$ICD]


  if (isDescription) {
    ccs_col <- "CCS_CATEGORY_DESCRIPTION"
  } else {
    ccs_col <- "CCS_CATEGORY"
  }
  IcdToCCS <- rbind(merge(PrDataFile[Date <icd10usingDate],ccsPrICD9[,c("ICD",ccs_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                    merge(PrDataFile[Date >=icd10usingDate],ccsPrICD10[,c("ICD",ccs_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))
  IcdToCCS <- IcdToCCS[order(Number), -"Number"]

  return(list(groupedDT = IcdToCCS,
              Error = Conversion$Error))
}


