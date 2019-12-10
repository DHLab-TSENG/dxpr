#' @export
#' @rdname prCCS
#'
icdPrToCCSLvl <- function(prDataFile, idColName, icdColName, dateColName, icdVerColName = NULL, icd10usingDate = NULL, CCSLevel = 1, isDescription = TRUE){
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

  if(isDescription){
    CCSLvlCol <- paste0("CCS_LVL_", CCSLevel, "_LABEL")
  }else{
    CCSLvlCol <- paste0("CCS_LVL_", CCSLevel)
  }

  if(CCSLevel <= 2){
    if (deparse(substitute(icdVerColName)) != "NULL"){
      icdToCCSLvl <- rbind(merge(prDataFile[Version == 9,],ccsPrICD9[,c("ICD",CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                           merge(prDataFile[Version == 10,],ccsPrICD10[,c("ICD",CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))
    }else{
      icdToCCSLvl <- rbind(merge(prDataFile[Date < icd10usingDate],ccsPrICD9[,c("ICD",CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                           merge(prDataFile[Date >= icd10usingDate],ccsPrICD10[,c("ICD",CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))
    }
  }else{
    if (deparse(substitute(icdVerColName)) != "NULL"){
      icdToCCSLvl <- merge(merge(prDataFile[Version == 9,],ccsPrICD9[,c("ICD", CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                           prDataFile[Version == 10,], by = names(prDataFile), all = TRUE)
    }else{
      icdToCCSLvl <- merge(merge(prDataFile[Date < icd10usingDate],ccsPrICD9[,c("ICD", CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                           prDataFile[Date >= icd10usingDate], by = names(prDataFile), all = TRUE)
    }

  }
  icdToCCSLvl <- icdToCCSLvl[order(Number),-"Number"]

  return(list(groupedDT = icdToCCSLvl,
              Error = Conversion$Error))
}
