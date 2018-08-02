#' Get the PheWAS Phecode/ description
#'
#' This can be used to select the PheWAS Phecode/ description
#' based on ICD code in clinical diagnostic data,
#' return CCS single and multiple category/ description based on ICD
#'
#' @import stringr
#' @import icd
#' @import plyr
#' @import dplyr
#' @import PheWAS
#' @param icdList An icd code list

convertIcdShortToDecimal<-function(icdList){
  for(icd in 1:length(icdList)){
    if(!grepl("[.]",icdList[icd])==TRUE){
      icdList[icd]<-icd_short_to_decimal(icdList[icd])
      icdList[icd]<-gsub("[..]","\\.",icdList[icd])
    }
  }
  return(icdList)
}
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate icd 10 using date
#' @param isPhecodeDescription PheWAS Phecode/ description for icd9, default is True
#' @examples
#' groupICDBasedOnPhecode(DxDataFile, ID, ICD, Date, "2016-01-01", F)
#' @export
groupICDBasedOnPhecode<-function(DxDataFile,idColName, icdColName, dateColName, icd10usingDate, isPhecodeDescription=TRUE){
  DxDataFile<-DxDataFile[ ,c(deparse(substitute(idColName)),deparse(substitute(icdColName)),deparse(substitute(dateColName)) )]
  names(DxDataFile)<-c("ID","ICD","Date")
  DxDataFile$ICD<-convertIcdShortToDecimal(DxDataFile$ICD)


  icd10 <- DxDataFile[DxDataFile$Date >=icd10usingDate,"ICD"]
  icd9  <- DxDataFile[DxDataFile$Date < icd10usingDate,"ICD"]
  icd9 <-left_join(icd9,select(phecode_icd9_2,ICD,PheCode,PheCodeDescription),by="ICD") %>% unique()
  icd10<-left_join(icd10,select(phecode_icd9_2,ICD,PheCode,PheCodeDescription),by="ICD") %>% unique()

  DxDataFile_original<-DxDataFile
  DxDataFile<-full_join(icd9,icd10,by = c("ICD","PheCode", "PheCodeDescription"))
  DxDataFile<-left_join(DxDataFile_original,DxDataFile,by="ICD")

  if(isPhecodeDescription==T){
    DxDataFile<-DxDataFile$PheCodeDescription
  }else{
    DxDataFile<-DxDataFile$PheCode
  }
  DxDataFile<-unlist(unname(DxDataFile))
  if(is.na(DxDataFile[is.na(DxDataFile)])[1] ){
    warning("'NA means pheWAS does not have icd10'",call. = F)
  }
  DxDataFile
}

