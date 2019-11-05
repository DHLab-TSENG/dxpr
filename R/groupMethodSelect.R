#' code classification method
#'
#' @inherit common_DxArg
#' @param groupMethod  Four Stratified methods can be chosen: CCS (\code{'ccs'}), CCS levels (\code{'ccslvl1'}, \code{'ccslvl2'}, \code{'ccslvl3'}, \code{'ccslvl4'}), PheWAS (\code{'PheWAS'}), comorbidities (\code{'ahrq'},\code{'charlson'}, \code{'elix'}), grepICD or customICD (\code{'customGrepIcdGroup'}, \code{'customIcdGroup'}). Change it to any of the other possible variables.
#' @param CustomGroupingTable Table is for groupDataType
#'
groupMethodSelect <- function(DxDataFile,idColName, icdColName, dateColName, icdVerColName = NULL, icd10usingDate = NULL, groupMethod, CustomGroupingTable, isDescription){

  DxDataFile <- as.data.table(DxDataFile)

  if(deparse(substitute(icdVerColName)) != "NULL"){
    DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)), deparse(substitute(icdVerColName)))
    DxDataFile <- DxDataFile[, DataCol, with = FALSE]
    names(DxDataFile) <- c("ID", "ICD", "Date", "Version")
  }else{
    DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
    DxDataFile <- DxDataFile[, DataCol, with = FALSE]
    names(DxDataFile) <- c("ID", "ICD", "Date")
  }

  DxDataFile[,"Date"] <- as.Date(DxDataFile[,Date])

  if(groupMethod == "CCS"){
    if(deparse(substitute(icdVerColName)) != "NULL"){
      groupedData <- IcdDxToCCS(DxDataFile, ID, ICD, Date, Version, NULL, isDescription)
    }else{
      groupedData <- IcdDxToCCS(DxDataFile, ID, ICD, Date, NULL, icd10usingDate, isDescription)
    }
  }else if(grepl("CCSLVL" ,groupMethod)){
    CCSLevel <- as.numeric(sub("[A-Za-z]+","",groupMethod))
    if(deparse(substitute(icdVerColName)) != "NULL"){
      groupedData <- IcdDxToCCSLvl(DxDataFile, ID, ICD, Date, Version, NULL, CCSLevel, isDescription)
    }else{
      groupedData <- IcdDxToCCSLvl(DxDataFile, ID, ICD, Date, NULL, icd10usingDate, CCSLevel, isDescription)
    }
  }else if(groupMethod == "PHEWAS"){
    if(deparse(substitute(icdVerColName)) != "NULL"){
      groupedData <- IcdDxToPheWAS(DxDataFile, ID, ICD, Date, Version, NULL, isDescription)
    }else{
      groupedData <- IcdDxToPheWAS(DxDataFile, ID, ICD, Date, NULL, icd10usingDate, isDescription)
    }
  }else if(groupMethod == "AHRQ"){
    if(deparse(substitute(icdVerColName)) != "NULL"){
      groupedData <- IcdDxToComorbid(DxDataFile, ID, ICD, Date, Version, NULL, ahrq, isDescription)
    }else{
      groupedData <- IcdDxToComorbid(DxDataFile, ID, ICD, Date, NULL, icd10usingDate, ahrq, isDescription)
    }
  }else if(groupMethod == "CHARLSON"){
    if(deparse(substitute(icdVerColName)) != "NULL"){
      groupedData <- IcdDxToComorbid(DxDataFile, ID, ICD, Date, Version, NULL, charlson, isDescription)
    }else{
      groupedData <- IcdDxToComorbid(DxDataFile, ID, ICD, Date, NULL, icd10usingDate, charlson, isDescription)
    }
  }else if(groupMethod == "ELIX"){
    if(deparse(substitute(icdVerColName)) != "NULL"){
      groupedData <- IcdDxToComorbid(DxDataFile, ID, ICD, Date, Version, NULL, elix, isDescription)
    }else{
      groupedData <- IcdDxToComorbid(DxDataFile, ID, ICD, Date, NULL, icd10usingDate, elix, isDescription)
    }
  }else if(groupMethod == "CUSTOMGREPICDGROUP"){
    groupedData <- IcdDxToCustomGrep(DxDataFile, ID, ICD, Date, CustomGroupingTable)
  }else if(groupMethod == "CUSTOMICDGROUP"){
    groupedData <- IcdDxToCustom(DxDataFile, ID, ICD, Date, CustomGroupingTable)
  }else if(groupMethod == "ICD"){
    if(deparse(substitute(icdVerColName)) != "NULL"){
      groupedData <- DxDataFile[, Short :=IcdDxDecimalToShort(DxDataFile, ICD, Date, icdVerColName = Version)$ICD]
    }else{
      groupedData <- DxDataFile[, Short :=IcdDxDecimalToShort(DxDataFile, ICD, Date, icd10usingDate = icd10usingDate)$ICD]
    }
  }else{
    stop("'please enter `ccs`,`ccslvl`, `PheWAS`, `ahrq`, `charlson`, `elix` `customgrepicdgroup`, `customicdgroup` for 'groupMethod'", call. = FALSE)
  }

  groupedData
}
