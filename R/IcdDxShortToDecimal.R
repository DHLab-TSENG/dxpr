#' Convert ICD Codes From Short To Decimal Forms
#'
#' Convert codes between short and decimal forms
#'
#' @import data.table
#' @importFrom utils head
#' @param DxDataFile A file of clinical diagnostic data with at least 2 columns: "ICD", "Date"
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate icd 10 using date
#' @source \url{https://www.findacode.com/search/search.php}
#' @source \url{https://www.cms.gov/Medicare/Quality-Initiatives-Patient-Assessment-Instruments/HospitalQualityInits/Downloads/HospitalAppendix_F.pdf}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD10/2019-ICD-10-CM.html}
#' @export
#' @examples
#' IcdDxShortToDecimal(sampleDxFile,ICD,Date,"2015/10/01")
IcdDxShortToDecimal <- function(DxDataFile, icdColName, dateColName, icd10usingDate){
  DataCol <- c(deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ICD", "Date")
  DxDataFile[,"Date"] <- as.Date(DxDataFile[,Date])
  DxDataFile[,Number:=1:nrow(DxDataFile)]

  icd_Decimal <- DxDataFile[grepl("[.]",DxDataFile$ICD),]
  icd_Short <- DxDataFile[!icd_Decimal, on = "Number"]

  icd9S <- merge(icd_Short[Date < icd10usingDate], ICD9DxwithTwoFormat, by.x = "ICD", by.y = "Short", all.x = T)
  icd10S <- merge(icd_Short[Date >= icd10usingDate], ICD10DxwithTwoFormat, by.x = "ICD", by.y = "Short", all.x = T)

  icd9D <- merge(icd_Decimal[Date < icd10usingDate], ICD9DxwithTwoFormat, by.x = "ICD", by.y = "Decimal", all.x = T)
  icd10D <- merge(icd_Decimal[Date >= icd10usingDate], ICD10DxwithTwoFormat, by.x = "ICD", by.y = "Decimal", all.x = T)

  StoD <- rbind(icd9S,icd10S)
  DtoD <- rbind(icd9D,icd10D)

  if(anyNA(StoD[,Decimal])){
    icd9SNA <- merge(icd9S[is.na(Decimal),-"Decimal"], ICD10DxwithTwoFormat,by.x = "ICD",by.y = "Short",all.x = T)
    icd10SNA <- merge(icd10S[is.na(Decimal),-"Decimal"], ICD9DxwithTwoFormat,by.x = "ICD",by.y = "Short",all.x = T)
    icd9SWrongFormat <- icd9SNA[is.na(icd9SNA[,Decimal]), list(count = .N),by = ICD]
    icd10SWrongFormat <- icd10SNA[is.na(icd10SNA[,Decimal]), list(count = .N),by = ICD]
    icd9SWrongVer <- icd9SNA[!is.na(icd9SNA[,Decimal]), list(count = .N),by = ICD]
    icd10SWrongVer <- icd10SNA[!is.na(icd10SNA[,Decimal]), list(count = .N),by = ICD]
  }
  if(anyNA(DtoD[,Short])){
    icd9DNA <- merge(icd9D[is.na(Short),-"Short"],ICD10DxwithTwoFormat,by.x = "ICD",by.y = "Decimal",all.x = T)
    icd10DNA <- merge(icd10D[is.na(Short),-"Short"],ICD9DxwithTwoFormat,by.x = "ICD",by.y = "Decimal",all.x = T)
    icd9DWrongFormat <- icd9DNA[is.na(icd9DNA[,Short]), list(count = .N),by = ICD]
    icd10DWrongFormat <- icd10DNA[is.na(icd10DNA[,Short]), list(count = .N),by = ICD]
    icd9DWrongVer <- icd9DNA[!is.na(icd9DNA[,Short]), list(count = .N),by = ICD]
    icd10DWrongVer <- icd10DNA[!is.na(icd10DNA[,Short]), list(count = .N),by = ICD]
  }
  if(anyNA(StoD[,Decimal]) & anyNA(DtoD[,Short])){
    wrongFormat <- rbind(rbind(icd9DWrongFormat, icd10DWrongFormat),
                         rbind(icd9SWrongFormat, icd10SWrongFormat))[order(count,decreasing = T),
                                                                     list(wrongFormat= paste0(ICD," (",count,")","")),]
    wrongVersion <- rbind(rbind(icd9SWrongVer, icd10SWrongVer),
                          rbind(icd9DWrongVer, icd10DWrongVer))[order(count,decreasing = T),
                                                                list(wrongVersion= paste0(ICD," (",count,")")),]
    allWrongICD <- rbind(rbind(icd9DNA[,-"Short"], icd10DNA[,-"Short"]),rbind(icd9SNA[,-'Decimal'], icd10SNA[,-'Decimal']))
  }else if(anyNA(DtoD[,Short])){
    wrongFormat <- rbind(icd9DWrongFormat, icd10DWrongFormat)[order(count,decreasing = T), list(wrongFormat= paste0(ICD," (",count,")","")),]
    wrongVersion <- rbind(icd9DWrongVer, icd10DWrongVer)[order(count,decreasing = T), list(wrongVersion= paste0(ICD," (",count,")","")),]
    allWrongICD <- rbind(icd9DNA[,-"Short"], icd10DNA[,-"Short"])
  }else if(anyNA(StoD[,Decimal])){
    wrongFormat <- rbind(icd9SWrongFormat, icd10SWrongFormat)[order(count,decreasing = T), list(wrongFormat= paste0(ICD," (",count,")","")),]
    wrongVersion <- rbind(icd9SWrongVer, icd10SWrongVer)[order(count,decreasing = T), list(wrongVersion= paste0(ICD," (",count,")","")),]
    allWrongICD <- rbind(icd9SNA[,-'Decimal'], icd10SNA[,-'Decimal'])
  }
  StoD <- StoD[!is.na(Decimal)]
  StoD <- StoD[,-"ICD"]
  setnames(StoD,"Decimal","ICD")
  DtoD <- DtoD[!is.na(Short)]
  DtoD <- DtoD[,-"Short"]
  allDecimalFormat <- rbind(DtoD,StoD)
  if(nrow(allDecimalFormat) < nrow(DxDataFile)){
    if(nrow(wrongFormat)>0){
      message(paste0("wrong ICD Format: total ",nrow(wrongFormat)," ICD codes (the number of occurrences is in brackets)"))
      message(head(wrongFormat,10))
      message(("\t"))
    }
    if(nrow(wrongVersion)>0){
      message(paste0("wrong ICD version: total ",nrow(wrongVersion)," ICD codes (the number of occurrences is in brackets)"))
      message(head(wrongVersion, 10))
      message(("\t"))
    }
    if(nrow(allWrongICD)>0){
      warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = F)
      warning('"wrong Format" means the ICD has wrong format', call. = F)
      warning('"wrong ICD version" means the ICD classify to wrong ICD version (cause the "icd10usingDate" or other issues)', call. = F)
    }
    combine_with_error <- rbind(allWrongICD, allDecimalFormat)[order(Number)]
    return(combine_with_error)

  }else{
    return(allDecimalFormat)
  }
}

