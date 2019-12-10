#' @rdname plotError
#' @export
#'
plot_errorICD <- function(errorFile, icdVersion = all, wrongICDType = all, groupICD = FALSE, others = TRUE, topN = 10){
  icdVersion <- tolower(deparse(substitute(icdVersion)))
  wrongICDType <- tolower(deparse(substitute(wrongICDType)))
  title <- paste0("Error ICD: Top ",topN)
  Xlabel <- "Error ICD"

  if(icdVersion == "9" | icdVersion == "10"){
    version <- paste0("ICD ", icdVersion)
    errorICD <- errorFile[IcdVersionInFile == version]
    title <- paste0("Error ", version, ": Top ",topN)
    Xlabel <- paste0("Error ", version)
  }else if(icdVersion != "9" && icdVersion != "10" && icdVersion != "all"){
    stop("'please enter `9`,`10`, `all` for 'icdVersion'", call. = FALSE)
  }

  if(wrongICDType == "format" | wrongICDType == "version"){
    wrongType <- paste0("Wrong ", wrongICDType)
    if(exists("errorICD")){errorICD <- errorICD[WrongType == wrongType,]}
    else{errorICD <- errorFile[WrongType == wrongType,]}
    title <- paste0(title," (",wrongType,")")
  }else if (wrongICDType != "format" && wrongICDType != "version" && wrongICDType != "all"){
    stop("'please enter `format`,`version`, `all` for 'wrongICDType'", call. = FALSE)
  }

  if(exists("errorICD")){errorFile <- errorICD}

  if(groupICD){
    if(version == "ICD 9"){
      errorData <- errorFile[,ICDGroup := substr(ICD,1,1),][,c("groupCount","maxICD") := list(sum(count),max(count)), by = "ICDGroup"][count == maxICD,][order(groupCount,decreasing = TRUE),]
      errorData <-errorData[,Number :=  1:nrow(errorData),][Number > topN, c("ICDGroup","groupCount") := list("others",sum(groupCount)),][!duplicated(ICDGroup),][,c("CumCount","ICDPercInGroup") := list(cumsum(groupCount),paste0(round((count/groupCount)*100,2),"%")),][,-"count"]

      if(!others){
        errorData <- errorData[!ICDGroup == "others",]
      }
      errorData <- errorData[,c("CumCountPerc","ICDGroup") :=
                               list(paste0(round(CumCount/max(CumCount)*100,2),"%"),factor(ICDGroup,levels = unique(ICDGroup))),]

      graph_col <- c("ICDGroup","groupCount")
      setnames(errorData,"ICD","MostICDInGroup")
      Xlabel <- paste0(Xlabel," (grouped)")
      ICD <- errorData[,c("ICDGroup","groupCount","CumCountPerc","MostICDInGroup","ICDPercInGroup","WrongType")]
    }else{
      stop("ICD 10 already has unique alphanumeric codes to identify known diseases")
    }
  }else{
    FileSize <- nrow(errorFile)
    errorData <- errorFile[, c("CumCount", "Number") :=
                             list(cumsum(count), 1:FileSize),][Number > topN, c("CumCount", "count", "ICD") :=
                                                                 list(max(CumCount), sum(count),"others"),][!duplicated(ICD),]
    if(!others){
      errorData <- errorData[!ICD == "others",]
    }
    errorData <- errorData[, c("CumCountPerc","ICD") := list(paste0(round(CumCount/max(CumCount)*100,2),"%"),factor(ICD,levels = unique(ICD))),]

    ICD <- errorData[,c("ICD","count","CumCountPerc","IcdVersionInFile","WrongType","Suggestion")]
    graph_col <- c("ICD","count")

  }
  Max <- errorData[,max(CumCount)]
  errorICDgraph <- ggplot(as.data.frame(errorData),
                          aes(x= eval(parse(text = paste(graph_col[1]))),
                              y = eval(parse(text = paste(graph_col[2]))))) +
    geom_bar(stat="identity", aes(fill = Number)) +
    guides(fill = FALSE, color = FALSE) +
    geom_line(aes(x= Number, y = CumCount, color = Number)) +
    geom_point(aes(x= Number, y = CumCount, color = Number), pch = 19) +
    scale_x_discrete(breaks = errorData[,eval(parse(text = paste(graph_col[1])))]) +
    annotate("rect", xmin = nrow(errorData) + .55, xmax =  nrow(errorData) + 1,
             ymin = -.02 * Max, ymax = Max * 1.02, fill = "white") +
    annotate("text", x = nrow(errorData) + .8, y = seq(0, Max, Max/10),
             label = c("  0%", " 10%", " 20%", " 30%", " 40%", " 50%", " 60%", " 70%", " 80%", " 90%", "100%"),
             size = 2.8) +
    geom_segment(x = Max + .55, xend = Max + .55, y = -.02 * Max, yend = Max * 1.02) +
    xlab(Xlabel) + ylab("count") +
    ggtitle(title) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 20))

  return(list(graph = errorICDgraph,
              ICD = ICD))
}
