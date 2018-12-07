if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "icd9_ahrq",
  "icd10_ahrq",
  "icd9_charlson",
  "icd10_charlson",
  "icd9_elix",
  "icd10_elix",
  "Comorbidity"))
#' Grouping comorbid method comorbidities (AHRQ, Charlson and Elixhauser Comorbidity) infers whether to use ICD-9 or ICD-10 codes
#'
#' Get comorbidities using the comorbidity methods based on ICD code in clinical diagnostic data.
#'
#' return comorbidity meseaures based on ICD diagnosis codes
#'
#' @import dplyr
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate ICD 10 using date
#' @param comorbidMethod  Three comorbidity method: AHRQ, Charlson and Elixhauser Comorbidity, type `ahrq`,`charlson`, or`elix`
#' @export
#' @source AHRQ

#' @source Charlson

#' @source Elixhauser
#' @source ICD-9-CM Elixhauser (2012-2015)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp#references}
#' @source ICD-10-CM Elixhauser (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp}
#' @examples
#'
#' IcdDxToComorbid(testDxFile, ID, ICD, Date, "2015-10-01", charlson)
#' IcdDxToComorbid(testDxFile, ID, ICD, Date, "2015-10-01", elix)
#' IcdDxToComorbid(testDxFile, ID, ICD, Date, "2015-10-01", ahrq)
#'
IcdDxToComorbid <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, comorbidMethod){
  DxDataFile <- DxDataFile[ , c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile$Date <- as.Date(DxDataFile$Date)
  DxDataFile <- DxDataFile %>% mutate(Number =  1:nrow(DxDataFile))
  conversion <- IcdDxDecimalToShort(DxDataFile$ICD)
  DxDataFile$Short <- conversion$Short

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
  }else{
    stop("'please enter AHRQ, Charlson or Elix for 'comorbidMethod'", call. = FALSE)
  }

  IcdToComorbid <- rbind(left_join(data.frame(DxDataFile[DxDataFile$Date < icd10usingDate,]),
                                   select(comorbidMap9,ICD,Comorbidity),by = c("Short"="ICD")),
                         left_join(data.frame(DxDataFile[DxDataFile$Date  >= icd10usingDate,]),
                                   select(comorbidMap10,ICD,Comorbidity), by = c("Short"="ICD"))) %>% arrange(Number)

  IcdToComorbidLong <- IcdToComorbid[!is.na(IcdToComorbid$Comorbidity),] %>%
    group_by(ID,Comorbidity) %>%
    summarise(firstCaseDate = min(Date),
              endCaseDate = max(Date),
              period = endCaseDate - firstCaseDate,
              count = n())

  wrongFormat <- conversion$Error
  error_ICD <- anti_join(data.frame(ICD = IcdToComorbid$ICD[is.na(IcdToComorbid$Comorbidity)],stringsAsFactors = F), wrongFormat, "ICD")
  if(anyNA(IcdToComorbid)){
    if(nrow(wrongFormat) > 0){
      message(paste0("wrong Format: ", unique(wrongFormat$ICD), sep = "\t\n"))
    }
    if(sum(is.na(IcdToComorbid)) > nrow(wrongFormat)){
      message(paste0("wrong ICD version: ", unique(error_ICD$ICD), sep = "\t\n"))
      message("\n")
    }
    warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = F)
    warning('"wrong Format" means the ICD has wrong format', call. = F)
    warning('"wrong ICD version" means the ICD classify to wrong ICD version (cause the "icd10usingDate" or other issues)', call. = F)
  }
  return(list(groupedDf = IcdToComorbid,
              groupedData_Long = IcdToComorbidLong))
}
