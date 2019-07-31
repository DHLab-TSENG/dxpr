#' @export
#' @rdname PrCCS
#'
IcdPrToCCSLvl <- function(PrDataFile, idColName, icdColName, dateColName, icd10usingDate, CCSLevel = 1, isDescription = TRUE){
  PrDataFile <- as.data.table(PrDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  PrDataFile <- PrDataFile[,DataCol,with = FALSE]
  names(PrDataFile) <- c("ID","ICD", "Date")
  Conversion <- IcdPrDecimalToShort(PrDataFile,ICD,Date,icd10usingDate)
  PrDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(PrDataFile))]
  PrDataFile[,Short := Conversion$ICD]

  if(isDescription){
    CCSLvlCol <- paste0("CCS_LVL_", CCSLevel, "_LABEL")
  }else{
    CCSLvlCol <- paste0("CCS_LVL_", CCSLevel)
  }

  if(CCSLevel <= 2){
    IcdToCCSLvl <- rbind(merge(PrDataFile[Date < icd10usingDate],ccsPrICD9[,c("ICD", CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                         merge(PrDataFile[Date >= icd10usingDate],ccsPrICD10[,c("ICD", CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))
  }else{
    IcdToCCSLvl <- merge(merge(PrDataFile[Date < icd10usingDate],ccsPrICD9[,c("ICD", CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                         PrDataFile[Date >= icd10usingDate], by = names(PrDataFile), all = TRUE)
  }
  IcdToCCSLvl <- IcdToCCSLvl[order(Number),-"Number"]

  return(list(groupedDT = IcdToCCSLvl,
              Error = Conversion$Error))
}
