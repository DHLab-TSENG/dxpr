#'
#' Get the categories of ICD-9 and ICD-10 codes on diagnoses, the grouping rules are based on your standards.
#'
#' This can be used to select the first diagnosis record
#' based on ICD code (grepl language) in DxDataFile,
#' return first diagnosis record based on factIcd Data
#'
#' @import data.table
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", and "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param CustomGroupingTable Grouping rules of clustering the ICD is based on yourself! There are two column in the dataframe/datatable: "group" and "ICD"
#' @export
#' @examples
#' head(sampleDxFile)
#' groupingTable <- data.frame(group = rep("Chronic kidney disease",6),
#'                             ICD = c("N181","5853","5854","5855","5856","5859"),
#'                             stringsAsFactors = FALSE)
#' IcdDxToCustom(sampleDxFile, ID, ICD, Date,
#'               CustomGroupingTable = groupingTable)
#'
IcdDxToCustom <- function(DxDataFile, idColName, icdColName, dateColName, CustomGroupingTable){
  customICD <- as.data.table(DxDataFile)
  CustomGroupingTable <- as.data.table(CustomGroupingTable)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  customICD <- customICD[,DataCol,with = FALSE]
  names(customICD) <- c("ID", "ICD", "Date")
  customICD[,c("Date", "Number") := list(as.Date(Date), 1:nrow(customICD))]


  groupedICD <- merge(customICD, CustomGroupingTable, by = "ICD", all.x = T)[order(Number),-"Number"]

  if(sum(!is.na(groupedICD$group)) > 0){
    groupedICDLong <- groupedICD[!is.na(group),
                                 list(firstCaseDate = min(Date),
                                      endCaseDate = max(Date),
                                      count = .N),
                                 by = list(ID, group)][,period := (endCaseDate - firstCaseDate),][order(ID),]
    return(list(groupedDT = groupedICD,
                summarised_groupedDT = groupedICDLong))
  }else{
    warning("There is no match diagnostic code with the groupingTable")
    return(groupedDT = groupedICD)
  }
}
