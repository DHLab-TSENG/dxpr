#' @rdname PC
#' @export
#'
IcdPrToProcedureClass <- function(PrDataFile, idColName, icdColName, dateColName, icd10usingDate, isDescription = TRUE){
  PrDataFile <- as.data.table(PrDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  PrDataFile <- PrDataFile[,DataCol,with = FALSE]
  names(PrDataFile) <- c("ID","ICD", "Date")
  Conversion <- IcdPrDecimalToShort(PrDataFile,ICD,Date,icd10usingDate)
  PrDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(PrDataFile))]
  PrDataFile[,Short := Conversion$ICD]

  if (isDescription) {
    PC_col <- "PROCEDURE_CLASS_NAME"
  } else {
    PC_col <- "PROCEDURE_CLASS"
  }
  IcdToPC <- rbind(merge(PrDataFile[Date < icd10usingDate],pcICD9[,c("ICD", PC_col), with = F],by.x ="Short",by.y = "ICD",all.x = TRUE),
                   merge(PrDataFile[Date >= icd10usingDate],pcICD10[,c("ICD", PC_col), with = F],by.x ="Short",by.y = "ICD",all.x = TRUE))
  IcdToPC <- IcdToPC[order(Number),-"Number"]
  IcdToPC

  return(list(groupedDT = IcdToPC,
              Error = Conversion$Error))
}
