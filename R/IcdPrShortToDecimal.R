#' Convert ICD-9-PCS Codes From Short To Decimal Forms
#'
#' Return codes with Decimal forms
#'
#' @import data.table
#' @param PrDataFile A file of clinical diagnostic data with at least 2 columns: "ICD", "Date"
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate icd 10 using date
#' @source \url{https://www.findacode.com/search/search.php}
#' @source \url{https://www.cms.gov/Medicare/Quality-Initiatives-Patient-Assessment-Instruments/HospitalQualityInits/Downloads/HospitalAppendix_F.pdf}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD10/2019-ICD-10-PCS.html}
#' @export
#' @examples
#' head(samplePrFile)
#' samplePrFile$Decimal <- IcdPrShortToDecimal(samplePrFile,ICD,Date,"2015/10/01")$ICD
#'
IcdPrShortToDecimal<-function(PrDataFile, icdColName, dateColName, icd10usingDate){
  DataCol <- c(deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  PrDataFile <- PrDataFile[,DataCol,with = FALSE]
  names(PrDataFile) <- c("ICD", "Date")
  PrDataFile[,"Date"] <- as.Date(PrDataFile[,Date])
  PrDataFile[,Number:=1:nrow(PrDataFile)]

  icd_Decimal <- PrDataFile[grepl("[.]",PrDataFile$ICD),]
  icd9D <- merge(icd_Decimal[Date < icd10usingDate], ICD9PrwithTwoFormat, by.x = "ICD", by.y = "Decimal", all.x = T)
  icd10D <- icd_Decimal[Date >= icd10usingDate]
  DtoDNA <- rbind(icd9D[is.na(Short),-"Short"], icd10D)
  if(!is.null(nrow(DtoDNA))){
    icd9DwrongFormat <- icd9D[is.na(Short), list(count = .N),by = ICD]
    icd10DwrongFormat <- icd10D[, list(count = .N),by = ICD]
    if(!is.null(nrow(icd9DwrongFormat))){
      wrongFormatMsg_D <- icd9DwrongFormat
      wrongFormat_D <- icd9D[is.na(Short),-"Short"]
      if(!is.null(nrow(icd10DwrongFormat))){
        wrongFormatMsg_D <- rbind(wrongFormatMsg_D,icd10DwrongFormat)
        wrongFormat_D <- rbind(wrongFormat_D,icd10D)
      }
    }else if(!is.null(nrow(icd10DwrongFormat))){
      wrongFormatMsg_D <- icd10DwrongFormat
      wrongFormat_D <- icd10D
    }
  }else{
    icd10DwrongFormat <- NA
    wrongFormatMsg_D <- NA
    wrongFormat_D <- NA
  }

  icd_Short <- PrDataFile[!icd_Decimal, on = "Number"]
  icd9S <- merge(icd_Short[Date < icd10usingDate], ICD9PrwithTwoFormat, by.x = "ICD", by.y = "Short", all.x = T)
  icd10S <- merge(icd_Short[Date >= icd10usingDate], prICD10, by =  "ICD", all.x = T)
  StoDNA <- rbind(icd9S[is.na(Decimal),-"Decimal"],icd10S[is.na(ICD_DESCRIPTION),-"ICD_DESCRIPTION"])
  if(!is.null(nrow(StoDNA))){
    icd9SNA <- merge(icd9S[is.na(Decimal),-"Decimal"], prICD10, by = "ICD", all.x = T)
    icd10SNA <- merge(icd10S[is.na(ICD_DESCRIPTION),-"ICD_DESCRIPTION"],ICD9PrwithTwoFormat,by.x = "ICD",by.y = "Short",all.x = T)
    icd9SwrongFormat <- icd9SNA[is.na(ICD_DESCRIPTION), list(count = .N),by = ICD]
    icd10SwrongFormat <- icd10SNA[is.na(Decimal), list(count = .N),by = ICD]
    if(!is.null(nrow(icd9SwrongFormat))){
      wrongFormatMsg_S <- icd9SwrongFormat
      wrongFormat_S <- icd9SNA[is.na(ICD_DESCRIPTION),-"ICD_DESCRIPTION"]
      if(!is.null(nrow(icd10SwrongFormat))){
        wrongFormatMsg_S<- rbind(wrongFormatMsg_S,icd10SwrongFormat)
        wrongFormat_S <- rbind(wrongFormat_S,icd10SNA[is.na(Decimal),-"Decimal"])
      }
    }else if(!is.null(nrow(icd10SwrongFormat))){
      wrongFormatMsg_S<- icd10SwrongFormat
      wrongFormat_S <- icd10SNA[is.na(Decimal),-"Decimal"]
    }else{
      wrongFormatMsg_S<-NA
      wrongFormat_S <- NA
    }
    icd9SWrongVer <- icd9SNA[!is.na(ICD_DESCRIPTION), list(count = .N) ,by = ICD]
    icd10SWrongVer <- icd10SNA[!is.na(Decimal), list(count = .N) ,by = ICD]
    if(!is.null(nrow(icd9SWrongVer))){
      wrongVersionMsg <-icd9SWrongVer[order(count,decreasing = T), list(wrongFormat= paste0(ICD," (",count,")","")),]
      wrongVersion <- icd9SNA[!is.na(ICD_DESCRIPTION),-"ICD_DESCRIPTION"]
      if(!is.null(nrow(icd10SWrongVer))){
        icd10SWrongVer <- icd10SWrongVer[order(count,decreasing = T), list(wrongFormat= paste0(ICD," (",count,")","")),]
        wrongVersionMsg <- rbind(wrongVersionMsg,icd10SWrongVer)
        wrongVersion <- rbind(wrongVersion,icd10SNA[is.na(Decimal),-"Decimal"])
      }
    }else if(!is.null(nrow(icd10SWrongVer))){
      wrongVersionMsg <- icd10SWrongVer[order(count,decreasing = T), list(wrongFormat= paste0(ICD," (",count,")","")),]
      wrongVersion <- icd10SNA[!is.na(Decimal),-"Decimal"]
    }else{
      wrongVersionMsg <-NA
      wrongVersion <- NA
    }
  }
  if(!is.null(nrow(wrongFormat_D))){
    allWrongFormat <- wrongFormat_D
    allWrongFormatMsg <- wrongFormatMsg_D[order(count,decreasing = T), list(wrongFormat= paste0(ICD," (",count,")","")),]
    if(!is.null(nrow(wrongFormat_S))){
      wrongFormatMsg_S <- wrongFormatMsg_S[order(count,decreasing = T), list(wrongFormat= paste0(ICD," (",count,")","")),]
      allWrongFormatMsg<- rbind(allWrongFormatMsg,wrongFormatMsg_S)
      allWrongFormat <- rbind(allWrongFormat,wrongFormat_S)
    }
  }else if(!is.null(nrow(wrongFormat_S)) & is.null(nrow(wrongFormat_D))){
    allWrongFormat <- wrongFormat_S
    allWrongFormatMsg<- wrongFormatMsg_S[order(count,decreasing = T), list(wrongFormat= paste0(ICD," (",count,")","")),]
  }else{
    allWrongFormat <- NA
  }
  if(!is.null(nrow(allWrongFormat))){
    allWrongICD <- allWrongFormat
    if(!is.null(nrow(wrongVersion))){
      allWrongICD <- rbind(allWrongICD, wrongVersion)
    }
  }else if(!is.null(nrow(wrongVersion))){
    allWrongICD <- wrongVersion
  }else{
    allWrongICD <- NA
  }
  DtoD <- icd9D[!is.na(Short),-"Short"]
  StoD <- icd9S[!is.na(Decimal),-"ICD"]
  setnames(StoD,"Decimal","ICD")
  StoD <- rbind(StoD,icd10S[!is.na(ICD_DESCRIPTION),-"ICD_DESCRIPTION"])
  allDecimalFormat <- rbind(DtoD,StoD)

  if(nrow(allDecimalFormat) < nrow(PrDataFile)){
    message(paste0("Wrong ICD format: total ",nrow(allWrongFormatMsg)," ICD codes (the number of occurrences is in brackets)"))
    if(!is.null(nrow(allWrongFormatMsg))){
      message(head(allWrongFormatMsg,10))
      if(!is.null(nrow(icd10DwrongFormat))){
        message("ICD-10-PCS codes do not have 'Decimal' format")
      }
      message(("\t"))
    }
    if(!is.null(nrow(wrongVersionMsg))){
      message(paste0("Wrong ICD version: total ",nrow(wrongVersionMsg)," ICD codes (the number of occurrences is in brackets)"))
      message(head(wrongVersionMsg, 10))
      message(("\t"))
    }
    warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = F)
    warning('"Wrong ICD format" means the ICD has wrong format', call. = F)
    warning('"Wrong ICD version" means the ICD classify to wrong ICD version (cause the "icd10usingDate" or other issues)', call. = F)

    combine_with_error <- rbind(allWrongICD, allDecimalFormat)[order(Number),-"Number"]
    return(combine_with_error)
  }else{
    return(allDecimalFormat[order(Number),-"Number"])
  }
}
