if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "CCS",
  "ID",
  "Date",
  "Gap",
  "episode",
  "Era"))
#' Get the condition era
#'
#' A Condition Era is defined as a span of time when the member is assumed to have a given condition.
#' Condition Eras are periods of Condition Occurrence.
#' Combining individual Condition Occurrences into a single Condition Era based on ICD code in clinical diagnostic data.
#' Condition Eras are built with a Persistence Window,deafault is 30 days, meaning, if no occurence of the same member id happens within 30 days of any one occurrence, it will be considered the end date of the last condition occurrence.
#'
#' return DxDataFile with new column, condition era.
#'
#' @import dplyr
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate Icd 10 using date
#' @param gapDate Length of condition era, default is 30 days
#' @param conditionSelect Four Stratified method can be chosen: CCS (or CCS levels), phecode (only for icd-9), comorbidities, grepICD or customICD , type `ccs`,`ccslvl1`,`ccslvl2`,`ccslvl3`,`ccslvl4`, `phecode`, `ahrq`, `charlson`, `elix` `customGrepIcdGroup`, or `customIcdGroup`, default is CCS
#' @param isDescription  CCS/Phecode categories or description for ICD-CM codes, default is True
#' @param CustomGroupingTable Table is for groupedICDMethod:`grepICD` and `customGroup`
#' @export
#' @examples
#' groupingTable <- data.frame(group = rep("Cardiac dysrhythmias",6),
#'                             ICD = c("427.1","427.2","427.31","427.61","427.81","427.89"),
#'                             stringsAsFactors = FALSE)
#' grepTable <- data.frame(group = c("Cardiac dysrhythmias"),
#'                         grepIcd = c("^427|^I48"),
#'                         stringsAsFactors = FALSE)
#' getConditionEra(sampleDxFile, ID, ICD, Date, "2015-10-01", 30, ccs, FALSE)
#' getConditionEra(sampleDxFile, ID, ICD, Date, "2015-10-01", 30, ccslvl3, FALSE)
#' getConditionEra(sampleDxFile, ID, ICD, Date, "2015-10-01", 30, ICD)
#' getConditionEra(sampleDxFile, ID, ICD, Date, "2015-10-01", 30, phecode, FALSE)
#' getConditionEra(sampleDxFile, ID, ICD, Date, "2015-10-01", 30, ahrq)
#' getConditionEra(sampleDxFile, ID, ICD, Date, "2015-10-01", 30, charlson)
#' getConditionEra(sampleDxFile, ID, ICD, Date, "2015-10-01", 30, elix)
#' getConditionEra(sampleDxFile, ID, ICD, Date, "2015-10-01", 30, customGrepIcdGroup,
#'                 CustomGroupingTable = grepTable)
#' getConditionEra(sampleDxFile, ID, ICD, Date, "2015-10-01", 30, customIcdgroup,
#'                 CustomGroupingTable = groupingTable)
#'
getConditionEra <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, gapDate = 30, conditionSelect = CCS, isDescription = TRUE,CustomGroupingTable){
  DxDataFile <- DxDataFile[ ,c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile$Date <- as.Date(DxDataFile$Date)

  conditionSelect <- toupper(deparse(substitute(conditionSelect)))
  if(conditionSelect == "CCS"){
    DxDataFile <- IcdDxToCCS(DxDataFile, ID, ICD, Date, icd10usingDate, isDescription)$groupedDf
  }else if(grepl("CCSLVL", conditionSelect)){
    CCSLevel <- as.numeric(sub("[A-Za-z]+","",conditionSelect))
    DxDataFile <- IcdDxToCCSLvl(DxDataFile, ID, ICD, Date, icd10usingDate, CCSLevel, isDescription)$groupedDf
  }else if(conditionSelect == "PHECODE"){
    DxDataFile <- IcdDxToPhecode(DxDataFile, ID, ICD, Date, icd10usingDate, isDescription)$groupedDf
  }else if(conditionSelect == "AHRQ"){
    DxDataFile <- IcdDxToComorbid(DxDataFile, ID, ICD, Date, icd10usingDate, ahrq)$groupedDf
  }else if(conditionSelect == "CHARLSON"){
    DxDataFile <- IcdDxToComorbid(DxDataFile, ID, ICD, Date, icd10usingDate, charlson)$groupedDf
  }else if(conditionSelect == "ELIX"){
    DxDataFile <- IcdDxToComorbid(DxDataFile, ID, ICD, Date, icd10usingDate, elix)$groupedDf
  }else if(conditionSelect == "CUSTOMGREPICDGROUP"){
    DxDataFile <- IcdToCustomGrep(DxDataFile, ID, ICD, Date, CustomGroupingTable)$groupedDf
  }else if(conditionSelect == "CUSTOMICDGROUP"){
    DxDataFile <- IcdDxToCustom(DxDataFile, ID, ICD, Date, CustomGroupingTable)$groupedDf
  }else if(conditionSelect == "ICD"){
    conversion <- IcdDxDecimalToShort(DxDataFile$ICD)
    DxDataFile$Short <- conversion$Short
    wrongFormat <- conversion$Error

    if(nrow(wrongFormat) > 0){
      message(paste0("wrong Format: ", unique(wrongFormat$ICD), sep = "\t\n"))
      message("\n")
      warning('"wrong Format" means the ICD has wrong format', call. = F)
    }
  }else{
    stop("'please enter `ccs`,`ccslvl`, `phecode`, `ahrq`, `charlson`, `elix` `customgrepicdgroup`, `customicdgroup` for 'conditionSelect'", call. = FALSE)
  }

  if(conditionSelect == "ICD"){
    conditionSelect <-  "ICD"
  }else{
    conditionSelect <- names(DxDataFile)[ncol(DxDataFile)]
  }
  conditionEra <- DxDataFile[(nchar(DxDataFile[,conditionSelect])>0 & !is.na(DxDataFile[,conditionSelect])),] %>%
    group_by_("ID", conditionSelect) %>%
    arrange_("ID", conditionSelect, "Date") %>%
    mutate(Gap = Date - lag(Date),
           episode = Gap >gapDate)

  conditionEra$episode[is.na(conditionEra$episode)] <- TRUE

  conditionEra <- conditionEra %>%
    mutate(Era = cumsum(episode)) %>%
    summarise(firstCaseDate = min(Date),
              endCaseDate = max(Date),
              period = endCaseDate - firstCaseDate,
              count = n(),
              Era = max(Era))
  conditionEra
}
