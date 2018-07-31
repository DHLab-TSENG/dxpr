#' Get the comorbidity meseaures of ICD
#'
#' This can be used to select the comorbidity methods
#' based on ICD code in clinical diagnostic data,
#' return comorbidity meseaures based on ICD
#'
#' @import stringr
#' @import icd
#' @import plyr
#' @import dplyr
#' @import rlist
#' @import tidyr
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate icd 10 using date
#' @param comorbidMethod  Three comorbidity method can be choose: AHRQ, Charlson and Elix Comorbidity 
#' @param NumericOrBinary  Member have one (or more) diagnostic comorbidities
#' @examples
#' groupICDBasedOnComorbid(DxDataFile, ID, ICD, Date, "2016-01-01", ahrq, N)

#' @export
groupICDBasedOnComorbid<-function(DxDataFile,idColName,icdColName,dateColName,icd10usingDate,comorbidMethod,NumericOrBinary=B){ 
  DxDataFile<-DxDataFile[ ,c(deparse(substitute(idColName)),deparse(substitute(icdColName)),deparse(substitute(dateColName)) )]
  names(DxDataFile)<-c("ID","ICD","Date")
  DxDataFile$ICD<-convertIcdDecimaltoShort(DxDataFile$ICD)
  
  icd9 <-DxDataFile[DxDataFile$Date< icd10usingDate, ]
  comorbidMap9<-eval(parse(text=paste("icd9_map_",tolower(deparse(substitute(comorbidMethod))),sep="")))
  comorbidDf9 <-as.data.frame(matrix(c(T),nrow=nrow(icd9),ncol=length(comorbidMap9)))
  names(comorbidDf9)<-names(comorbidMap9)
  icd10<-DxDataFile[DxDataFile$Date>=icd10usingDate, ]
  comorbidMap10<-eval(parse(text=paste("icd10_map_",tolower(deparse(substitute(comorbidMethod))),sep="")))
  comorbidDf10 <-as.data.frame(matrix(c(T),nrow=nrow(icd10),ncol=length(comorbidMap10)))
  names(comorbidDf10)<-names(comorbidMap10)
  
  for(i in 1:nrow(comorbidDf9)){comorbidDf9[i,]<-grepl(icd9$ICD[i],comorbidMap9)}
  comorbidDf9<-comorbidDf9 %>% mutate(ID=icd9$ID) %>% mutate(ICD=icd9$ICD)
  for(i in 1:nrow(comorbidDf10)){comorbidDf10[i,]<-grepl(icd10$ICD[i],comorbidMap10)}
  comorbidDf10<-comorbidDf10 %>% mutate(ID=icd10$ID) %>% mutate(ICD=icd10$ICD)
  
  comorbidDf<-full_join(comorbidDf9,comorbidDf10)
  
  if(toupper(deparse(substitute(NumericOrBinary)))=="N"){
    NumOrBin<-eval(parse(text="sum"))
  }else{
    NumOrBin<-eval(parse(text="any"))
  }
  comorbidDf<-comorbidDf %>% 
    group_by(ID) %>%
    summarise(CHF=NumOrBin(CHF),Valvular=NumOrBin(Valvular),PHTN=NumOrBin(PHTN),PVD=NumOrBin(PVD),HTN=NumOrBin(HTN),HTNcx=NumOrBin(HTNcx),
              Paralysis=NumOrBin(Paralysis),NeuroOther=NumOrBin(NeuroOther),Pulmonary=NumOrBin(Pulmonary),DM=NumOrBin(DM),DMcx=NumOrBin(DMcx),
              Hypothyroid=NumOrBin(Hypothyroid),Renal=NumOrBin(Renal),Liver=NumOrBin(Liver),PUD=NumOrBin(PUD),HIV=NumOrBin(HIV),
              Lymphoma=NumOrBin(Lymphoma),Mets=NumOrBin(Mets),Tumor=NumOrBin(Tumor),Rheumatic=NumOrBin(Rheumatic),Coagulopathy=NumOrBin(Coagulopathy),
              Obesity=NumOrBin(Obesity),WeightLoss=NumOrBin(WeightLoss),FluidsLytes=NumOrBin(FluidsLytes),BloodLoss=NumOrBin(BloodLoss),
              Anemia=NumOrBin(Anemia),Alcohol=NumOrBin(Alcohol),Drugs=NumOrBin(Drugs),Psychoses=NumOrBin(Psychoses),Depression=NumOrBin(Depression))
  comorbidDf
}
