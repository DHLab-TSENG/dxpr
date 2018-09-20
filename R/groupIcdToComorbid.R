if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "icd9_ahrq",
  "icd10_ahrq",
  "icd9_charlson",
  "icd10_charlson",
  "icd9_elix",
  "icd10_elix",
  "Comorbidity",
  "B"))
#' Grouping comorbid method comorbidities (AHRQ, Charlson and Elixhauser Comorbidity) infers whether to use ICD-9 or ICD-10 codes
#'
#' Get comorbidities using the comorbidity methods based on ICD code in clinical diagnostic data.
#'
#' return comorbidity meseaures based on ICD diagnosis codes
#'
#' @import reshape2
#' @import dplyr
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate Icd 10 using date
#' @param comorbidMethod  Three comorbidity method: AHRQ, Charlson and Elixhauser Comorbidity, type `ahrq`,`charlson`, or`elix`
#' @param NumericOrBinary  Member have one (or more) diagnostic comorbidities, type `N` or `B`, default is `B` (Binary)
#' @param groupByDate Default is True.
#' @export
#' @examples
#' DxDataFile <- data.frame(ID=c("A","A","B","B"),
#'                          ICD=c("40201","42577","I350","K289"),
#'                          Date=as.Date(c("2013-03-31","2013-01-29","2016-03-10","2016-03-10")),
#'                          stringsAsFactors = FALSE)
#'
#' groupIcdToComorbid(DxDataFile, ID, ICD, Date, "2016-01-01", ahrq, N, TRUE)
#' groupIcdToComorbid(DxDataFile, ID, ICD, Date, "2016-01-01", charlson, B, TRUE)
#' groupIcdToComorbid(DxDataFile, ID, ICD, Date, "2016-01-01", elix, N, TRUE)
#'
groupIcdToComorbid <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, comorbidMethod, NumericOrBinary = B, groupByDate = TRUE){
  DxDataFile <- DxDataFile[ , c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  Format <- ifelse(any(grepl("[.]", DxDataFile$ICD)), "Decimal", "Short")
  DxDataFile$ICD <- convertIcdDecimaltoShort(DxDataFile$ICD)$Short

  comorbidMethod <- tolower(deparse(substitute(comorbidMethod)))
  if (grepl("ahrq", comorbidMethod)){
    comorbidMap9 <- `icd9_ahrq`
    comorbidMap10 <- `icd10_ahrq`
  }else if(grepl("charlson", comorbidMethod)){
    comorbidMap9 <- `icd9_charlson`
    comorbidMap10 <- `icd10_charlson`
  }else if(grepl("elix", comorbidMethod)){
    comorbidMap9 <- `icd9_elix`
    comorbidMap10 <- `icd10_elix`
  }
  icd9 <- data.frame(DxDataFile[DxDataFile$Date < icd10usingDate,])
  icd10 <- data.frame(DxDataFile[DxDataFile$Date >= icd10usingDate,])

  comorbidDf9 <- left_join(icd9, comorbidMap9,by = "ICD")
  comorbidDf10 <- left_join(icd10, comorbidMap10, by = "ICD")
  comorbidDf_combine <- rbind(comorbidDf9, comorbidDf10)

  comorbidDf_combine$ICD <- NULL
  if(groupByDate ==T){
    comorbidDf_combine <- comorbidDf_combine %>% group_by(ID, Date, Comorbidity) %>% unique()
  }
  comorbidDf_combine_wide <- dcast(comorbidDf_combine, ID~Comorbidity, value.var = c("Value"), sum)

  all_comorbidity_measures <- data.frame(matrix(nrow = length(comorbidDf_combine_wide$ID), ncol = length(unique(comorbidMap9$Comorbidity))))
  names(all_comorbidity_measures) <- unique(comorbidMap9$Comorbidity)
  all_comorbidity_measures <- mutate(all_comorbidity_measures, ID = comorbidDf_combine_wide$ID)

  JoiningBycol <- names(comorbidDf_combine_wide[ names(comorbidDf_combine_wide) != "NA"])
  combine <- right_join(all_comorbidity_measures, comorbidDf_combine_wide,by = JoiningBycol)
  combine <- combine[, names(combine) != "NA"]

  combine_Numeric <- combine[, c(ncol(combine), 1:(ncol(combine)-1))]
  combine_Numeric[is.na(combine_Numeric)] <- 0L
  if(toupper(deparse(substitute(NumericOrBinary))) == "B"){
    combine_Binary <-as.data.frame(combine_Numeric >= 1L)
    combine_Binary$ID <- unique(DxDataFile$ID)
    return(combine_Binary)
  }else if(toupper(deparse(substitute(NumericOrBinary))) == "N"){
    return(combine_Numeric)
  }else{
    stop("'please enter N or B for 'comorbidMethod'", call. = FALSE)
  }
  WrongFormat <- convertIcdDecimaltoShort(DxDataFile$ICD)$Error
  if(length(WrongFormat) > 0){
    message(paste0("wrong Format: ", unique(WrongFormat), sep = "\t\n"))
  }
}
