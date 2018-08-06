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
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate icd 10 using date
#' @param isPhecodeDescription PheWAS Phecode/ description for icd9, default is True
#' @export
#' @examples
#' DxDataFile <- data.frame(ID=c("A","A","A"),
#'                          ICD=c("6929","V433","I350"),
#'                          Date=as.Date(c("2013-03-31","2013-01-29","2016-03-10")),
#'                          stringsAsFactors = F)
#' groupICDBasedOnPhecode(DxDataFile, ID, ICD, Date, "2016-01-01", F)
#'
groupICDBasedOnPhecode<-function(DxDataFile,idColName, icdColName, dateColName, icd10usingDate, isPhecodeDescription=TRUE){
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
  if(is.na(DxDataFile_combine_with_originalFile[is.na(DxDataFile_combine_with_originalFile)])[1] ){
    warning("'NA means pheWAS does not have icd10'",call. = F)
  }
  DxDataFile_combine_with_originalFile
}
