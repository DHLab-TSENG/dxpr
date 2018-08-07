#' Get the Clinical Classifications Software (CCS) single and multiple category/ description
#'
#' This can be used to select the Clinical Classifications Software (CCS) single and multiple category/ description
#' based on ICD code in clinical diagnostic data,
#' return Clinical Classifications Software (CCS) single and multiple category/ description based on ICD-9 and ICD-10
#' @import stringr
#' @import icd
#' @import plyr
#' @import dplyr
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate icd 10 using date
#' @param CCSLevel Clinical Classifications Software (CCS) multiple level
#' @param CCSLvlLabel Clinical Classifications Software (CCS) multiple level categories/ description for icd9/ 10, default is True
#' @export
#' @examples
#' DxDataFile <- data.frame(ID=c("A","A","A"),
#'                          ICD=c("6929","V433","I350"),
#'                          Date=as.Date(c("2013-03-31","2013-01-29","2016-03-10")),
#'                          stringsAsFactors = F)
#' groupIcdBasedOnCCSLvl (DxDataFile, ID, ICD, Date, "2016-01-01", 2, T)
#'
groupIcdBasedOnCCSLvl<-function(DxDataFile,idColName,icdColName,dateColName,icd10usingDate,CCSLevel=1,CCSLvlLabel=TRUE){
  DxDataFile<-DxDataFile[ ,c(deparse(substitute(idColName)),deparse(substitute(icdColName)),deparse(substitute(dateColName)))]
  names(DxDataFile)<-c("ID","ICD","Date")
  DxDataFile$ICD <- convertIcdDecimaltoShort(DxDataFile$ICD)
  icd9  <- DxDataFile[DxDataFile$Date < icd10usingDate,"ICD"]
  icd9 <- left_join(data.frame(ICD=icd9,stringsAsFactors = F),
                    select(ccsDxICD9,ICD,CCS_LVL_1,CCS_LVL_1_LABEL,CCS_LVL_2,CCS_LVL_2_LABEL,CCS_LVL_3,CCS_LVL_3_LABEL,CCS_LVL_4,CCS_LVL_4_LABEL),
                    by="ICD") %>% unique()
  if(CCSLevel<3){
    icd10 <- DxDataFile[DxDataFile$Date >=icd10usingDate,"ICD"]
    icd10 <- left_join(data.frame(ICD=icd10,stringsAsFactors = F),
                       select(ccsDxICD10,ICD,CCS_LVL_1,CCS_LVL_1_LABEL,CCS_LVL_2,CCS_LVL_2_LABEL),
                       by="ICD") %>% unique()

    DxDataFile_combine<-full_join(icd9,icd10,by = c("ICD","CCS_LVL_1","CCS_LVL_1_LABEL","CCS_LVL_2","CCS_LVL_2_LABEL"))
  }else{
    DxDataFile_combine<-icd9
  }
  DxDataFile_combine[complete.cases(DxDataFile_combine),]
  DxDataFile_combine_with_originalFile<-left_join(DxDataFile,DxDataFile_combine,by="ICD")

  if(CCSLvlLabel==T){
    CCSLevelcol<-as.character(parse(text=paste("CCS_LVL_",CCSLevel,"_LABEL",sep="")))
  }else{
    CCSLevelcol<-as.character(parse(text=paste("CCS_LVL_",CCSLevel,sep="")))
  }
  DxDataFile_combine_with_originalFile<-unlist(unname(DxDataFile_combine_with_originalFile[,CCSLevelcol]))
  if(is.na(DxDataFile_combine_with_originalFile[is.na(DxDataFile_combine_with_originalFile)])[1] && CCSLevel>=3){
    warning("'NA means icd10 CCS multiple levels are 1~2",call. = F)
  }
  DxDataFile_combine_with_originalFile
}
