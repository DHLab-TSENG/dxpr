#' @rdname DxCustom
#' @export
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
