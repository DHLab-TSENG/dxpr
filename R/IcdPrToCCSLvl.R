#' Get the Multi-level Clinical Classifications Software (CCS) categories and description for ICD-9 and ICD-10 codes on procedures.
#'
#'  Multi-leve Clinical Classifications Software (CCS) for ICD-9 and ICD-10 procedure codes in clinical procedures data is a procedure categorization scheme. Four levels exist in the multi-level procedure CCS for ICD-9-CM codes, and two levels exist in the multi-level procedure CCS for ICD-10-CM codes
#'
#' return Multi-leve Clinical Classifications Software (CCS) categories or description based on ICD-9 and ICD-10 codes
#'
#' @import dplyr
#' @param PrDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", "Date"
#' @param idColName A column for MemberID of PrDataFile
#' @param icdColName A column for ICD of PrDataFile
#' @param dateColName A column for Date of PrDataFile
#' @param icd10usingDate icd 10 using date
#' @param CCSLevel Clinical Classifications Software (CCS) multiple level
#' @param CCSLvlLabel Clinical Classifications Software (CCS) multiple level categories/description for icd9/10, default is True
#' @export
#' @source ICD-9-PCS CCS (2015)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip}
#' @source ICD-10-PCS CCS (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_pr_icd10pcs_2019_1.zip}
#' @examples
#' PrDataFile <- data.frame(ID=c("A","A","A","B"),
#'                          ICD=c("0101","8838","00870ZZ","00920ZZ"),
#'                          Date=as.Date(c("2013-03-31","2013-01-29","2016-03-10","2016-03-10")),
#'                          stringsAsFactors = FALSE)
#' IcdPrToCCSLvl(PrDataFile, ID, ICD, Date, "2016-01-01",2, TRUE)
#'
IcdPrToCCSLvl <- function(PrDataFile, idColName, icdColName, dateColName, icd10usingDate, CCSLevel = 1, CCSLvlLabel = TRUE){
  PrDataFile <- PrDataFile[ , c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(PrDataFile) <- c("ID", "ICD", "Date")
  PrDataFile <- PrDataFile %>% mutate(Number =  1:nrow(PrDataFile))
  Conversion <- IcdPrDecimalToShort(PrDataFile$ICD)
  PrDataFile$Short <- Conversion$Short

  icd9 <- PrDataFile[as.Date(PrDataFile$Date) < icd10usingDate,]
  icd10 <- PrDataFile[as.Date(PrDataFile$Date) >= icd10usingDate,]

  if(CCSLevel <= 2){
    icd9ToCCSLvl <- left_join(icd9, select(ccsPrICD9, ICD, CCS_LVL_1, CCS_LVL_1_LABEL, CCS_LVL_2, CCS_LVL_2_LABEL), by = c("Short"="ICD"))
    icd10ToCCSLvl <- left_join(icd10, select(ccsPrICD10, ICD, CCS_LVL_1, CCS_LVL_1_LABEL, CCS_LVL_2, CCS_LVL_2_LABEL), by = c("Short"="ICD"))
    CCSLvl_combine <- rbind(icd9ToCCSLvl, icd10ToCCSLvl)
  }else{
    icd9ToCCSLvl <- left_join(icd9, select(ccsPrICD9, ICD, CCS_LVL_1, CCS_LVL_1_LABEL, CCS_LVL_2, CCS_LVL_2_LABEL,
                                           CCS_LVL_3, CCS_LVL_3_LABEL), by = c("Short"="ICD"))
    icd10ToCCSLvl <- left_join(icd10, select(ccsPrICD10, ICD, CCS_LVL_1, CCS_LVL_1_LABEL, CCS_LVL_2, CCS_LVL_2_LABEL), by = c("Short"="ICD"))

    CCSLvl_combine <- left_join(icd9ToCCSLvl, icd10ToCCSLvl, by = names(icd10ToCCSLvl))
  }

  if(CCSLvlLabel == T){
    CCSLevelcol <- as.character(parse(text = paste("CCS_LVL_", CCSLevel, "_LABEL", sep = "")))
  }else{
    CCSLevelcol <- as.character(parse(text = paste("CCS_LVL_", CCSLevel, sep = "")))
  }
  IcdToCCSLevel <- CCSLvl_combine[, CCSLevelcol]

  WrongFormat <- Conversion$Error
  error_ICD <- anti_join(data.frame(ICD = PrDataFile$ICD[is.na(IcdToCCSLevel)], stringsAsFactors= FALSE),WrongFormat, "ICD")
  if(anyNA(IcdToCCSLevel)){
    if(nrow(WrongFormat) > 0){
      message(paste0("wrong Format: ", unique(WrongFormat$ICD), sep = "\t\n"))
    }
    if(sum(is.na(IcdToCCSLevel)) > nrow(WrongFormat)){
      message(paste0("wrong ICD version: ", unique(error_ICD$ICD), sep = "\t\n"))
      message("\n")
    }
    warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = F)
    warning('"wrong Format" means the ICD has wrong format', call. = F)
    warning('"wrong ICD version" means the ICD classify to wrong ICD version (cause the "icd10usingDate"), ICD-10  CCS multiple levels are 1~2 or other issues', call. = F)
  }
  IcdToCCSLevel
}
