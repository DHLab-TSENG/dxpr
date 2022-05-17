#' code classification method
#'
#' @inherit common_DxArg
#' @param groupMethod  Four Stratified methods can be chosen: CCS (\code{'ccs'}), CCS levels (\code{'ccslvl1'}, \code{'ccslvl2'}, \code{'ccslvl3'}, \code{'ccslvl4'}), PheWAS (\code{'PheWAS'}), comorbidities (\code{'ahrq'},\code{'charlson'}, \code{'elix'}), grepICD or customICD (\code{'customGrepIcdGroup'}, \code{'customIcdGroup'}). Change it to any of the other possible variables.
#' @param customGroupingTable Table is for groupDataType
#'
groupMethodSelect <- function(dxDataFile, idColName, icdColName, dateColName, icdVerColName = NULL, icd10usingDate = NULL, groupMethod = ccs, customGroupingTable, isDescription){

  dxDataFile <- as.data.table(dxDataFile)

  if(deparse(substitute(icdVerColName)) != "NULL"){
    dataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)), deparse(substitute(icdVerColName)))
    dxDataFile <- dxDataFile[, dataCol, with = FALSE]
    names(dxDataFile) <- c("ID", "ICD", "Date", "Version")
  }else{
    dataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
    dxDataFile <- dxDataFile[, dataCol, with = FALSE]
    names(dxDataFile) <- c("ID", "ICD", "Date")
  }

  dxDataFile[,"Date"] <- as.Date(dxDataFile[,Date])

  if(groupMethod == "CCS"){
    if(deparse(substitute(icdVerColName)) != "NULL"){
      groupedData <- icdDxToCCS(dxDataFile, ID, ICD, Date, Version, NULL, isDescription)
    }else{
      groupedData <- icdDxToCCS(dxDataFile, ID, ICD, Date, NULL, icd10usingDate, isDescription)
    }
  }else if(grepl("CCSLVL" ,groupMethod)){
    CCSLevel <- as.numeric(sub("[A-Za-z]+","",groupMethod))
    if(deparse(substitute(icdVerColName)) != "NULL"){
      groupedData <- icdDxToCCSLvl(dxDataFile, ID, ICD, Date, Version, NULL, CCSLevel, isDescription)
    }else{
      groupedData <- icdDxToCCSLvl(dxDataFile, ID, ICD, Date, NULL, icd10usingDate, CCSLevel, isDescription)
    }
  }else if(groupMethod == "CCSR"){
      if(deparse(substitute(icdVerColName)) != "NULL"){
        groupedData <- icdDxToCCSR(dxDataFile, ID, ICD, Date, Version, NULL, isDescription)
      }else{
        groupedData <- icdDxToCCSR(dxDataFile, ID, ICD, Date, NULL, icd10usingDate, isDescription)
      }
  }else if(groupMethod == "PHEWAS"){
    if(deparse(substitute(icdVerColName)) != "NULL"){
      groupedData <- icdDxToPheWAS(dxDataFile, ID, ICD, Date, Version, NULL, isDescription)
    }else{
      groupedData <- icdDxToPheWAS(dxDataFile, ID, ICD, Date, NULL, icd10usingDate, isDescription)
    }
  }else if(groupMethod == "AHRQ"){
    if(deparse(substitute(icdVerColName)) != "NULL"){
      groupedData <- icdDxToComorbid(dxDataFile, ID, ICD, Date, Version, NULL, ahrq, isDescription)
    }else{
      groupedData <- icdDxToComorbid(dxDataFile, ID, ICD, Date, NULL, icd10usingDate, ahrq, isDescription)
    }
  }else if(groupMethod == "CHARLSON"){
    if(deparse(substitute(icdVerColName)) != "NULL"){
      groupedData <- icdDxToComorbid(dxDataFile, ID, ICD, Date, Version, NULL, charlson, isDescription)
    }else{
      groupedData <- icdDxToComorbid(dxDataFile, ID, ICD, Date, NULL, icd10usingDate, charlson, isDescription)
    }
  }else if(groupMethod == "ELIX"){
    if(deparse(substitute(icdVerColName)) != "NULL"){
      groupedData <- icdDxToComorbid(dxDataFile, ID, ICD, Date, Version, NULL, elix, isDescription)
    }else{
      groupedData <- icdDxToComorbid(dxDataFile, ID, ICD, Date, NULL, icd10usingDate, elix, isDescription)
    }
  }else if(groupMethod == "CUSTOMGREPICDGROUP"){
    groupedData <- icdDxToCustomGrep(dxDataFile, ID, ICD, Date, customGroupingTable)
  }else if(groupMethod == "CUSTOMICDGROUP"){
    groupedData <- icdDxToCustom(dxDataFile, ID, ICD, Date, customGroupingTable)
  }else if(groupMethod == "ICD"){
    if(deparse(substitute(icdVerColName)) != "NULL"){
      groupedData <- dxDataFile[, Short :=icdDxDecimalToShort(dxDataFile, ICD, Date, icdVerColName = Version)$ICD]
    }else{
      groupedData <- dxDataFile[, Short :=icdDxDecimalToShort(dxDataFile, ICD, Date, icd10usingDate = icd10usingDate)$ICD]
    }
  }else{
    stop("'please enter `ccs`,`ccslvl`, `PheWAS`, `ahrq`, `charlson`, `elix` `customgrepicdgroup`, `customicdgroup` for 'groupMethod'", call. = FALSE)
  }

  groupedData
}
