#' @rdname dxCCS
#' @export
#'
icdDxToCCSLvl <- function(dxDataFile, idColName, icdColName, dateColName, icdVerColName = NULL, icd10usingDate = NULL, CCSLevel = 1, isDescription = TRUE){

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

  if(isDescription){
    CCSLvlCol <- paste0("CCS_LVL_", CCSLevel, "_LABEL")
  }else{
    CCSLvlCol <- paste0("CCS_LVL_", CCSLevel)
  }

  if(CCSLevel <= 2){
    if (deparse(substitute(icdVerColName)) != "NULL"){
      allCCSLvl <- rbind(merge(dxDataFile[Version == 9], ccsDxICD9[,c("ICD", CCSLvlCol), with = FALSE], by.x ="Short", by.y = "ICD", all.x = TRUE),
                         merge(dxDataFile[Version == 10], ccsDxICD10[,c("ICD", CCSLvlCol), with = FALSE], by.x ="Short", by.y = "ICD", all.x = TRUE))
    }else{
      allCCSLvl <- rbind(merge(dxDataFile[Date < icd10usingDate],ccsDxICD9[,c("ICD", CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                         merge(dxDataFile[Date >= icd10usingDate],ccsDxICD10[,c("ICD", CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))
    }
  }else{
    if (deparse(icdVerColName) != "NULL"){
      allCCSLvl <- merge(merge(dxDataFile[Version == 9],ccsDxICD9[,c("ICD", CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                         dxDataFile[Version == 10], by = names(dxDataFile), all = TRUE)
    }else{
      allCCSLvl <- merge(merge(dxDataFile[Date < icd10usingDate],ccsDxICD9[,c("ICD", CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                         dxDataFile[Date >= icd10usingDate], by = names(dxDataFile), all = TRUE)
    }
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

