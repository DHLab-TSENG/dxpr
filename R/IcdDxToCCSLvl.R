#' @rdname DxCCS
#' @export
#'
IcdDxToCCSLvl <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, CCSLevel = 1, isDescription = TRUE){
  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(DxDataFile))]
  Conversion <- IcdDxDecimalToShort(DxDataFile,ICD,Date,icd10usingDate)
  DxDataFile[,Short := Conversion$ICD]

  if(isDescription){
    CCSLvlCol <- paste0("CCS_LVL_", CCSLevel, "_LABEL")
  }else{
    CCSLvlCol <- paste0("CCS_LVL_", CCSLevel)
  }

  if(CCSLevel <= 2){
    allCCSLvl <- rbind(merge(DxDataFile[Date < icd10usingDate],ccsDxICD9[,c("ICD", CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                         merge(DxDataFile[Date >= icd10usingDate],ccsDxICD10[,c("ICD", CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))
  }else{
    allCCSLvl <- merge(merge(DxDataFile[Date < icd10usingDate],ccsDxICD9[,c("ICD", CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                         DxDataFile[Date >= icd10usingDate], by = names(DxDataFile), all = TRUE)
  }
  allCCSLvl <- allCCSLvl[order(Number),-"Number"]

  if(nrow(allCCSLvl[is.na(eval(parse(text = paste(CCSLvlCol))))]) < nrow(allCCSLvl)){
    summarisedCCSLvl <- allCCSLvl[!is.na(eval(parse(text = paste(CCSLvlCol)))),
                                  list(firstCaseDate = min(Date),
                                       endCaseDate = max(Date),
                                       count = .N),
                                  by = c("ID",CCSLvlCol)][,period := (endCaseDate - firstCaseDate),][order(ID),]

    return(list(groupedDT = allCCSLvl,
                summarised_groupedDT = summarisedCCSLvl,
                Error = Conversion$Error))
  }else{
    return(list(groupedDT = allCCSLvl,
                Error = Conversion$Error))
  }

}
