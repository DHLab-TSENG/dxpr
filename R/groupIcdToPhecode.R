if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "ICD",
  "PheCode",
  "PheCodeDescription",
  "phecode_icd9_2"))
#' Get the Phecode or description of ICD-9 diagnosis codes
#'
#' This can be used to group Phecode or description based on ICD-9 codes in clinical diagnostic data.
#'
#' return Phecode or description based on ICD-9-CM codes
#'
#' @import icd
#' @import dplyr
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate icd 10 using date
#' @param isPhecodeDescription Phecode/ description for icd9, default is True
#' @export
#' @examples
#' DxDataFile <- data.frame(ID=c("A","A","A"),
#'                          ICD=c("6929","V433","I350"),
#'                          Date=as.Date(c("2013-03-31","2013-01-29","2016-03-10")),
#'                          stringsAsFactors = FALSE)
#' groupIcdToPhecode(DxDataFile, ID, ICD, Date, "2016-01-01", FALSE)
#'
groupIcdToPhecode<-function(DxDataFile,idColName, icdColName, dateColName, icd10usingDate, isPhecodeDescription=TRUE){
  DxDataFile<-DxDataFile[ ,c(deparse(substitute(idColName)),deparse(substitute(icdColName)),deparse(substitute(dateColName)) )]
  names(DxDataFile)<-c("ID","ICD","Date")
  DxDataFile$ICD<-convertIcdShortToDecimal(DxDataFile$ICD)

  icd10 <- DxDataFile[DxDataFile$Date >=icd10usingDate,"ICD"]
  icd9  <- DxDataFile[DxDataFile$Date < icd10usingDate,"ICD"]
  icd9 <-left_join(data.frame(ICD = icd9, stringsAsFactors = F),select(phecode_icd9_2,ICD,PheCode,PheCodeDescription),by="ICD") %>% unique()
  icd10<-left_join(data.frame(ICD = icd10, stringsAsFactors = F),select(phecode_icd9_2,ICD,PheCode,PheCodeDescription),by="ICD") %>% unique()

  DxDataFile_combine<-full_join(icd9,icd10,by = c("ICD","PheCode", "PheCodeDescription"))
  DxDataFile_combine_with_originalFile<-left_join(DxDataFile, DxDataFile_combine,by="ICD")

  if(isPhecodeDescription==T){
    DxDataFile_combine_with_originalFile<-DxDataFile_combine_with_originalFile$PheCodeDescription
  }else{
    DxDataFile_combine_with_originalFile<-DxDataFile_combine_with_originalFile$PheCode
  }
  DxDataFile_combine_with_originalFile<-unlist(unname(DxDataFile_combine_with_originalFile))

  errorID<-is.na(DxDataFile_combine_with_originalFile[is.na(DxDataFile_combine_with_originalFile)])
  if(sum(errorID)>=1){
    warning("'NA means phecode does not have icd10'",call. = F)
  }
  DxDataFile_combine_with_originalFile
}
