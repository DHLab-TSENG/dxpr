#' @rdname PC
#' @export
#'
icdPrToProcedureClass <- function(prDataFile, idColName, icdColName, dateColName, icdVerColName = NULL, icd10usingDate = NULL, isDescription = TRUE){
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
    PC_col <- "PROCEDURE_CLASS_NAME"
  } else {
    PC_col <- "PROCEDURE_CLASS"
  }
  if (deparse(substitute(icdVerColName)) != "NULL"){
    icdToPC <- rbind(merge(prDataFile[Version == 9,],pcICD9[,c("ICD",PC_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                      merge(prDataFile[Version == 10,],pcICD9[,c("ICD",PC_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))
  }else{
    icdToPC <- rbind(merge(prDataFile[Date <icd10usingDate],pcICD9[,c("ICD",PC_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                      merge(prDataFile[Date >=icd10usingDate],pcICD10[,c("ICD",PC_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))
  }

  icdToPC <- icdToPC[order(Number),-"Number"]
  icdToPC

  return(list(groupedDT = icdToPC,
              Error = Conversion$Error))
}
