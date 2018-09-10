if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "ccsDxICD9",
  "ccsDxICD10",
  "CCS_LVL_1",
  "CCS_LVL_1_LABEL",
  "CCS_LVL_2",
  "CCS_LVL_2_LABEL",
  "CCS_LVL_3",
  "CCS_LVL_3_LABEL",
  "CCS_LVL_4",
  "CCS_LVL_4_LABEL"))
#' Get the Multi-level Clinical Classifications Software (CCS) categories and description for ICD-9 and ICD-10 codes on diagnoses.
#'
#'  Multi-leve Clinical Classifications Software (CCS) for ICD-9 and ICD-10 diagnosis codes in clinical diagnostic data is a diagnosis categorization scheme. Four levels exist in the multi-level diagnosis CCS for ICD-9-CM codes, and two levels exist in the multi-level diagnosis CCS for ICD-10-CM codes
#'
#' return Multi-leve Clinical Classifications Software (CCS) categories or description based on ICD-9 and ICD-10 codes
#'
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
#'                          stringsAsFactors = FALSE)
#' groupIcdToCCSLvl(DxDataFile, ID, ICD, Date, "2016-01-01", 2, TRUE)
#'
groupIcdToCCSLvl <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, CCSLevel = 1, CCSLvlLabel = TRUE){
  DxDataFile <- DxDataFile[ , c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile) <- c("ID", "ICD", "Date")

  icd9 <- DxDataFile[DxDataFile$Date < icd10usingDate,]
  icd9$ICD <- convertIcdDecimaltoShort(icd9$ICD, icd9)
  icd9ToCCSLvl <- left_join(data.frame(ICD = icd9$ICD, stringsAsFactors = F),
                    select(ccsDxICD9, ICD, CCS_LVL_1, CCS_LVL_1_LABEL, CCS_LVL_2, CCS_LVL_2_LABEL, CCS_LVL_3, CCS_LVL_3_LABEL, CCS_LVL_4, CCS_LVL_4_LABEL),
                    by = "ICD") %>% mutate(ID = icd9$ID) %>% mutate(Date = icd9$Date) %>% unique()

  if(CCSLevel < 3){
    icd10 <- DxDataFile[DxDataFile$Date >= icd10usingDate,]
    icd10$ICD <- convertIcdDecimaltoShort(icd10$ICD, icd10)
    icd10ToCCSLvl <- left_join(data.frame(ICD = icd10$ICD, stringsAsFactors = F),
                       select(ccsDxICD10, ICD, CCS_LVL_1, CCS_LVL_1_LABEL, CCS_LVL_2, CCS_LVL_2_LABEL),
                       by = "ICD") %>% mutate(ID = icd10$ID) %>% mutate(Date = icd10$Date) %>% unique()
    DxDataFile_combine <- full_join(icd9ToCCSLvl, icd10ToCCSLvl, by = c("ID", "ICD", "Date", "CCS_LVL_1", "CCS_LVL_1_LABEL", "CCS_LVL_2", "CCS_LVL_2_LABEL"))
  }else{
    DxDataFile_combine <- icd9ToCCSLvl
  }

  DxDataFile_combine_with_originalFile <- left_join(DxDataFile,DxDataFile_combine, by = c("ID", "ICD", "Date"))

  if(CCSLvlLabel == T){
    CCSLevelcol <- as.character(parse(text = paste("CCS_LVL_", CCSLevel, "_LABEL", sep = "")))
  }else{
    CCSLevelcol <- as.character(parse(text = paste("CCS_LVL_", CCSLevel, sep = "")))
  }
  IcdToCCSLevel <- DxDataFile_combine_with_originalFile[, CCSLevelcol]

  if(anyNA(IcdToCCSLevel)){
    message(paste0("warning ICD: ", unique(DxDataFile_combine_with_originalFile$ICD[is.na(IcdToCCSLevel)]), sep = "\t\n"))
    warning("'NA' means icd10 CCS multiple levels are 1~2 or the data does not match the format", call. = F)
  }
  IcdToCCSLevel
}
