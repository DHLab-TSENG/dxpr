#' Get the condition era 
#'
#' A Condition Era is defined as a span of time when the member is assumed to have a given condition.
#' based on ICD code in clinical diagnostic data,
#' return CCS single and multiple category/ description based on ICD9/ 10
#' @import icd
#' @import dplyr
#' @import lubridate
#' @import data.table
#' @param icdList An icd code list
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate icd 10 using date
#' @param gapDate default is 30 days
#' @param icdorCCS default is CCS
#' @param isCCSCategoryDescription  Clinical Classifications Software (CCS) single level categories (False) and description (True) for icd9/ 10, default is False

#' @examples
#' getConditionEra(DxDataFile, ID, ICD, Date, "2016-01-01", 30, ccs, F)
#' @export
getConditionEra <-function(DxDataFile,idColName,icdColName,dateColName,icd10usingDate,gapDate=30,icdorCCS=CCS,isCCSDescription=F){
  DxDataFile<-DxDataFile[ ,c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile)<-c("ID","ICD","Date")

  if(toupper(deparse(substitute(icdorCCS)))=="CCS"){
    DxDataFile <- DxDataFile %>%
      mutate(CCS=groupICDBasedOnCCS(DxDataFile,ID,ICD,Date,icd10usingDate,isCCSDescription)) %>%
      arrange(ID,CCS,Date) %>%
      group_by(ID,CCS) %>%
      mutate(LastDate=lag(Date)) %>%
      mutate(Gap=Date-LastDate)
  }else if(toupper(deparse(substitute(icdorCCS)))=="ICD"){
    DxDataFile$ICD<-convertIcdDecimaltoShort(DxDataFile$ICD)
    DxDataFile<- DxDataFile %>%
      arrange(ID,ICD,Date) %>%
      group_by(ID,ICD) %>%
      mutate(LastDate=lag(Date)) %>%
      mutate(Gap=Date-LastDate)
  }else{
    stop("'please enter icd or ccs",call.=FALSE)
  }
  DxDataFile<-as.data.table(DxDataFile)
  DxDataFile$episode <- DxDataFile$Gap > gapDate
  DxDataFile[is.na(DxDataFile$episode)]$episode<-T
  
  if(toupper(deparse(substitute(icdorCCS)))=="CCS"){
    DxDataFile[,Era:=cumsum(episode),by=list(ID,CCS)]
  }else if(toupper(deparse(substitute(icdorCCS)))=="ICD"){
    DxDataFile[,Era:=cumsum(episode),by=list(ID,ICD)]
  }
  DxDataFile<-subset(DxDataFile, select = c(-LastDate, -Gap, -episode)) 
  DxDataFile
}
