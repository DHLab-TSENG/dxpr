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
  if(nrow(DtoDNA)>0){
    icd9DWrongFormat <- icd9D[is.na(Short), list(count = .N),by = ICD]
    icd10DWrongFormat <- icd10D[, list(count = .N),by = ICD]
    if(nrow(icd9DWrongFormat)>0){
      wrongFormatMsg_D <- icd9DWrongFormat
      wrongFormat_D <- icd9D[is.na(Short),-"Short"]
      if(nrow(icd10DWrongFormat)>0){
        wrongFormatMsg_D <- rbind(wrongFormatMsg_D,icd10DWrongFormat)
        wrongFormat_D <- rbind(wrongFormat_D,icd10D)
      }
    }else if(nrow(icd10DWrongFormat)>0){
      wrongFormatMsg_D <- icd10DWrongFormat
      wrongFormat_D <- icd10D
    }
  }else{
    icd10DWrongFormat <- NA
    wrongFormatMsg_D <- NA
    wrongFormat_D <- NA
  }

  icd_Short <- PrDataFile[!icd_Decimal, on = "Number"]
  icd9S <- merge(icd_Short[Date < icd10usingDate], ICD9PrwithTwoFormat, by.x = "ICD", by.y = "Short", all.x = T)
  icd10S <- merge(icd_Short[Date >= icd10usingDate], prICD10, by =  "ICD", all.x = T)
  StoDNA <- rbind(icd9S[is.na(Decimal),-"Decimal"],icd10S[is.na(ICD_DESCRIPTION),-"ICD_DESCRIPTION"])
  if(nrow(StoDNA)>0){
    icd9SNA <- merge(icd9S[is.na(Decimal),-"Decimal"], prICD10, by = "ICD", all.x = T)
    icd10SNA <- merge(icd10S[is.na(ICD_DESCRIPTION),-"ICD_DESCRIPTION"],ICD9PrwithTwoFormat,by.x = "ICD",by.y = "Short",all.x = T)
    icd9SWrongFormat <- icd9SNA[is.na(ICD_DESCRIPTION), list(count = .N),by = ICD]
    icd10SWrongFormat <- icd10SNA[is.na(Decimal), list(count = .N),by = ICD]
    if(nrow(icd9SWrongFormat)>0){
      wrongFormatMsg_S <- icd9SWrongFormat
      wrongFormat_S <- icd9SNA[is.na(ICD_DESCRIPTION),-"ICD_DESCRIPTION"]
      if(nrow(icd10SWrongFormat)>0){
        wrongFormatMsg_S<- rbind(wrongFormatMsg_S,icd10SWrongFormat)
        wrongFormat_S <- rbind(wrongFormat_S,icd10SNA[is.na(Decimal),-"Decimal"])
      }
    }else if(nrow(icd10SWrongFormat)>0){
      wrongFormatMsg_S<- icd10SWrongFormat
      wrongFormat_S <- icd10SNA[is.na(Decimal),-"Decimal"]
    }else{
      wrongFormatMsg_S<-NA
      wrongFormat_S <- NA
    }
    icd9SWrongVer <- icd9SNA[!is.na(ICD_DESCRIPTION), list(count = .N) ,by = ICD]
    icd10SWrongVer <- icd10SNA[!is.na(Decimal), list(count = .N) ,by = ICD]
    if(nrow(icd9SWrongVer)>0){
      wrongVersionMsg <-icd9SWrongVer[order(count,decreasing = T), list(wrongFormat= paste0(ICD," (",count,")","")),]
      wrongVersion <- icd9SNA[!is.na(ICD_DESCRIPTION),-"ICD_DESCRIPTION"]
      if(nrow(icd10SWrongVer)>0){
        icd10SWrongVer <- icd10SWrongVer[order(count,decreasing = T), list(wrongFormat= paste0(ICD," (",count,")","")),]
        wrongVersionMsg <- rbind(wrongVersionMsg,icd10SWrongVer)
        wrongVersion <- rbind(wrongVersion,icd10SNA[is.na(Decimal),-"Decimal"])
      }
    }else if(nrow(icd10SWrongVer)>0){
      wrongVersionMsg <- icd10SWrongVer[order(count,decreasing = T), list(wrongFormat= paste0(ICD," (",count,")","")),]
      wrongVersion <- icd10SNA[!is.na(Decimal),-"Decimal"]
    }else{
      wrongVersionMsg <-NA
      wrongVersion <- NA
    }
  }
  if(!is.null(nrow(wrongFormat_D))){
    allwrongFormatMsg <- wrongFormatMsg_D[order(count,decreasing = T), list(wrongFormat= paste0(ICD," (",count,")","")),]
    allWrongFormat <- wrongFormat_D
    if(!is.null(nrow(wrongFormat_S))){
      wrongFormatMsg_S<- wrongFormatMsg_S[order(count,decreasing = T), list(wrongFormat= paste0(ICD," (",count,")","")),]
      allwrongFormatMsg<- rbind(wrongFormatMsg_D,wrongFormatMsg_S)
      allWrongFormat <- rbind(allWrongFormat,wrongFormat_S)
    }
  }else if(!is.null(nrow(wrongFormat_S)) & is.null(nrow(wrongFormat_D))){
    allWrongFormatMsg<- wrongFormatMsg_S[order(count,decreasing = T), list(wrongFormat= paste0(ICD," (",count,")","")),]
    allWrongFormat <- wrongFormat_S
  }else{
    allWrongFormat <- NA
  }
  if(!is.null(nrow(allWrongFormat))){
    allWrongICD <- allWrongFormat
    if(!is.null(nrow(wrongVersion))){
      allWrongICD <- rbind(allWrongFormat,wrongVersion)
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
      if(!is.null(nrow(icd10DWrongFormat))){
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
