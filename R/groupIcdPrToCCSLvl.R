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
#' @examples
#' PrDataFile <- data.frame(ID=c("A","A","A","B"),
#'                          ICD=c("0101","8838","00870ZZ","00920ZZ"),
#'                          Date=as.Date(c("2013-03-31","2013-01-29","2016-03-10","2016-03-10")),
#'                          stringsAsFactors = FALSE)
#' groupIcdPrToCCSLvl(PrDataFile, ID, ICD, Date, "2016-01-01",2, TRUE)
#'
groupIcdPrToCCSLvl <- function(PrDataFile, idColName, icdColName, dateColName, icd10usingDate, CCSLevel = 1, CCSLvlLabel = TRUE){
  PrDataFile <- PrDataFile[ , c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(PrDataFile) <- c("ID", "ICD", "Date")
  Format <- ifelse(any(grepl("[.]", PrDataFile$ICD)), "Decimal", "Short")
  PrDataFile$ICD <- convertIcdPrDecimaltoShort(PrDataFile$ICD)$Short

  icd9 <- PrDataFile[PrDataFile$Date < icd10usingDate,]
  icd10 <- PrDataFile[PrDataFile$Date >= icd10usingDate,]

  if(CCSLevel <= 2){
    icd9ToCCSLvl <- left_join(icd9, select(ccsPrICD9, ICD, CCS_LVL_1, CCS_LVL_1_LABEL, CCS_LVL_2, CCS_LVL_2_LABEL), by = "ICD") %>% unique()
    icd10ToCCSLvl <- left_join(icd10, select(ccsPrICD10, ICD, CCS_LVL_1, CCS_LVL_1_LABEL, CCS_LVL_2, CCS_LVL_2_LABEL), by = "ICD") %>% unique()
    CCSLvl_combine <- rbind(icd9ToCCSLvl, icd10ToCCSLvl)
  }else{
    icd9ToCCSLvl <- left_join(icd9, select(ccsPrICD9, ICD, CCS_LVL_1, CCS_LVL_1_LABEL, CCS_LVL_2, CCS_LVL_2_LABEL,
                                           CCS_LVL_3, CCS_LVL_3_LABEL), by = "ICD") %>% unique()
    icd10ToCCSLvl <- left_join(icd10, select(ccsPrICD10, ICD, CCS_LVL_1, CCS_LVL_1_LABEL, CCS_LVL_2, CCS_LVL_2_LABEL), by = "ICD") %>% unique()

    CCSLvl_combine <- full_join(icd9ToCCSLvl, icd10ToCCSLvl, by = names(icd10ToCCSLvl))
  }
  CCSLvl_combine_with_originalFile <- left_join(PrDataFile,CCSLvl_combine, by = names(PrDataFile))

  if(CCSLvlLabel == T){
    CCSLevelcol <- as.character(parse(text = paste("CCS_LVL_", CCSLevel, "_LABEL", sep = "")))
  }else{
    CCSLevelcol <- as.character(parse(text = paste("CCS_LVL_", CCSLevel, sep = "")))
  }
  IcdToCCSLevel <- CCSLvl_combine_with_originalFile[, CCSLevelcol]

  WrongFormat <- convertIcdPrDecimaltoShort(PrDataFile$ICD)$Error
  error_ICD <- anti_join(data.frame(ICD = CCSLvl_combine_with_originalFile$ICD[is.na(IcdToCCSLevel)], stringsAsFactors= FALSE),
                         data.frame(ICD = WrongFormat, stringsAsFactors= FALSE), "ICD") %>% unique
  if(anyNA(IcdToCCSLevel)){
    if(length(WrongFormat) > 0){
      message(paste0("wrong Format: ", unique(WrongFormat), sep = "\t\n"))
    }
    if(sum(is.na(IcdToCCSLevel)) > length(WrongFormat)){
      if(Format == "Decimal"){
        message(paste0("warning ICD: ", convertIcdPrShortToDecimal(error_ICD$ICD)$Decimal, sep = "\t\n"))
      }else{
        message(paste0("warning ICD: ", error_ICD, sep = "\t\n"))
      }
      message("\n")
    }
    warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = F)
    warning('"wrong Format" means the ICD has wrong format', call. = F)
    warning('"warning ICD" means the ICD classify to wrong ICD version (cause the "icd10usingDate"), ICD-10  CCS multiple levels are 1~2 or other issues', call. = F)
  }
  IcdToCCSLevel
}
