#' Get the CCS single and multiple category/ description
#'
#' This can be used to select the CCS single and multiple category/ description
#' based on ICD code in clinical diagnostic data,
#' return CCS single and multiple category/ description based on ICD9/ 10
#' @import stringr
#' @import icd
#' @import plyr
#' @import dplyr
#' @param icdList An icd code list

convertIcdDecimaltoShort<-function(icdList){
  for(icd in 1:length(icdList)){
    if(grepl("[.]",icdList[icd])==TRUE){
      icdList[icd]<-icd_decimal_to_short(icdList[icd])
    }
  }
  return(icdList)
}
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate icd 10 using date
#' @param isCCSCategoryDescription  Clinical Classifications Software (CCS) single level categories/ description for icd9/ 10, default is True
#' @examples
#' groupICDBasedOnCCS (DxDataFile, ID, ICD, Date, "2016-01-01", T)
#' @export
groupICDBasedOnCCS<-function(DxDataFile,idColName,icdColName,dateColName,icd10usingDate,isCCSCategoryDescription=TRUE){
  DxDataFile<-DxDataFile[ ,c(deparse(substitute(idColName)),deparse(substitute(icdColName)),deparse(substitute(dateColName)) )]
  names(DxDataFile)<-c("ID","ICD","Date")
  DxDataFile$ICD<-convertIcdDecimaltoShort(DxDataFile$ICD)

  icd10 <- DxDataFile[DxDataFile$Date >=icd10usingDate,"ICD"]
  icd9  <- DxDataFile[DxDataFile$Date < icd10usingDate,"ICD"]
  icd9 <-left_join(data.frame(ICD = icd9, stringsAsFactors = F), select(ccsDxICD9,ICD,CCS_CATEGORY,CCS_CATEGORY_DESCRIPTION),by="ICD") %>% unique()
  icd10<-left_join(data.frame(ICD = icd10, stringsAsFactors = F), select(ccsDxICD10,ICD,CCS_CATEGORY,CCS_CATEGORY_DESCRIPTION),by="ICD") %>% unique()

  DxDataFile_original<-DxDataFile
  DxDataFile<-full_join(icd9,icd10,by = c("ICD","CCS_CATEGORY", "CCS_CATEGORY_DESCRIPTION"))
  DxDataFile<-left_join(DxDataFile_original,DxDataFile,by="ICD")

  if(isCCSCategoryDescription==T){
    DxDataFile<-DxDataFile$CCS_CATEGORY_DESCRIPTION
  }else{
    DxDataFile<-DxDataFile$CCS_CATEGORY
  }
  DxDataFile
}

#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate icd 10 using date
#' @param CCSLevel Clinical Classifications Software (CCS) multiple level
#' @param CCSLvlLabel Clinical Classifications Software (CCS) multiple level categories/ description for icd9/ 10, default is True
#' @examples
#' groupICDBasedOnCCSLvl (DxDataFile, ID, ICD, Date, "2016-01-01", 2, T)
#' @export
groupICDBasedOnCCSLvl<-function(DxDataFile,idColName,icdColName,dateColName,icd10usingDate,CCSLevel=1,CCSLvlLabel=TRUE){
  DxDataFile[,deparse(substitute(icdColName))]<-convertIcdDecimaltoShort(DxDataFile[,deparse(substitute(icdColName))])
  DxDataFile<-DxDataFile[ ,c(deparse(substitute(idColName)),deparse(substitute(icdColName)),deparse(substitute(dateColName)) )]
  names(DxDataFile)<-c("ID","ICD","Date")
  DxDataFile_original<-DxDataFile
  icd9  <- DxDataFile[DxDataFile$Date < icd10usingDate,"ICD"]
  icd9 <-left_join(icd9,select(ccsDxICD9,ICD,CCS_LVL_1,CCS_LVL_1_LABEL,CCS_LVL_2,CCS_LVL_2_LABEL,CCS_LVL_3,CCS_LVL_3_LABEL,CCS_LVL_4,CCS_LVL_4_LABEL),
                   by="ICD") %>% unique()
  if(CCSLevel<3){
    icd10 <- DxDataFile[DxDataFile$Date >=icd10usingDate,"ICD"]
    icd10<-left_join(icd10,select(ccsDxICD10,ICD,CCS_LVL_1,CCS_LVL_1_LABEL,CCS_LVL_2,CCS_LVL_2_LABEL),
                     by="ICD") %>% unique()

    DxDataFile<-full_join(icd9,icd10,by = c("ICD","CCS_LVL_1","CCS_LVL_1_LABEL","CCS_LVL_2","CCS_LVL_2_LABEL"))
  }else{
    DxDataFile<-icd9
  }
  DxDataFile<-left_join(DxDataFile_original,DxDataFile,by="ICD")

  if(CCSLvlLabel==T){
    CCSLevelcol<-as.character(parse(text=paste("CCS_LVL_",CCSLevel,"_LABEL",sep="")))
  }else{
    CCSLevelcol<-as.character(parse(text=paste("CCS_LVL_",CCSLevel,sep="")))
  }
  DxDataFile<-unlist(unname(DxDataFile[,CCSLevelcol]))
  if(is.na(DxDataFile[is.na(DxDataFile)])[1] && CCSLevel>=3){
    warning("'NA means icd10 CCS multiple levels are 1~2",call. = F)
  }
  DxDataFile
}

