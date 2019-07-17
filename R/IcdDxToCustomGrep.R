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
#' @param CustomGroupingTable grouping rules of clustering the ICD is based on yourself! There are two column in the dataframe/datatable: "group", "grepIcd"
#' @export
#' @examples
#' head(sampleDxFile)
#' grepTable <- data.frame(Group = "Chronic kidney disease",
#'                         grepIcd = "^585|^N18",
#'                         stringsAsFactors = FALSE)
#' IcdDxToCustomGrep(DxDataFile = sampleDxFile,
#'                   ID, ICD, Date,
#'                   CustomGroupingTable = grepTable)
#'
IcdDxToCustomGrep <- function(DxDataFile, idColName, icdColName, dateColName, CustomGroupingTable){
  GrepedIcd <- as.data.table(DxDataFile)
  CustomGroupingTable <- as.data.table(CustomGroupingTable)
  DataCol  <-c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  GrepedIcd <- GrepedIcd[,DataCol,with = FALSE]
  names(GrepedIcd) <- c("ID", "ICD", "Date")
  GrepedIcd[,c("Date", "Number", "Group") := list(as.Date(Date), 1:nrow(GrepedIcd), NA)]

  for (rule in 1:nrow(CustomGroupingTable)){
    GrepedIcd$Group<-ifelse(grepl(CustomGroupingTable[rule,"grepIcd"],GrepedIcd$ICD), CustomGroupingTable[rule,Group], GrepedIcd$Group)
  }

  if(sum(!is.na(GrepedIcd$Group)) > 0){
    summarisedGrepedIcd <- GrepedIcd[nchar(Group)>0,
                                     list(firstCaseDate = min(Date),
                                          endCaseDate = max(Date),
                                          count = .N),by = list(ID,Group)][,period := (endCaseDate - firstCaseDate),][order(ID),]

    return(list(groupedDT = GrepedIcd[order(Number),-"Number"],
                summarised_groupedDT = summarisedGrepedIcd))
  }else{
    warning("There is no match diagnostic code with the grepTable")
    return(groupedDT = GrepedIcd[order(Number),-"Number"])
  }
}
