#'
#' Pareto Plot of error ICD Codes
#'
#' @import data.table
#' @import ggplot2
#' @importFrom stats reorder
#' @param errorFile error file from ICD uniform function (`IcdDxDecimalToShort` or `IcdDxShortToDecimal`)
#' @param ICDVersion ICD version
#' @param wrongICDType Wrong ICD type
#' @param groupICD default is FALSE
#' @param Others default is TRUE
#' @param OthersThreshold Others threshold
#' @source \url{http://sape.inf.usi.ch/quick-reference/ggplot2/colour}
#' @export
#' @examples
#' head(sampleDxFile)
#' error <- IcdDxDecimalToShort(sampleDxFile, ICD, Date, "2015/10/01")
#' plot_errorICD(error$Error, ICDVersion = all,
#'                            wrongICDType = all,
#'                            groupICD = FALSE,
#'                            Others = FALSE,
#'                            OthersThreshold = 1)
#'
plot_errorICD <- function(errorFile, ICDVersion = all, wrongICDType = all, groupICD = FALSE, Others = TRUE, OthersThreshold){
  ICDVersion <- tolower(deparse(substitute(ICDVersion)))
  wrongICDType <- tolower(deparse(substitute(wrongICDType)))
  title <- "Error ICD: Top 10"
  Xlabel <- "Error ICD"

  if(ICDVersion == "9" | ICDVersion == "10"){
    version <- paste0("ICD ", ICDVersion)
    errorICD <- errorFile[IcdVersionInFile == version]
    title <- paste0("Error ", version, ": Top 10")
    Xlabel <- paste0("Error ", version)
  }else if(ICDVersion != "9" && ICDVersion != "10" && ICDVersion != "all"){
    stop("'please enter `9`,`10`, `all` for 'ICDVersion'", call. = FALSE)
  }

  if(wrongICDType == "format" | wrongICDType == "version"){
    wrongType <- paste0("Wrong ", wrongICDType)
    if(exists("errorICD")){errorICD <- errorICD[WrongType == wrongType,]}
    else{errorICD <- errorFile[WrongType == wrongType,]}
    title <- paste0(title," (",wrongType,")")
  }else if (wrongICDType != "format" && wrongICDType != "version" && wrongICDType != "all"){
    stop("'please enter `format`,`version`, `all` for 'wrongICDType'", call. = FALSE)
  }

  if(exists("errorICD")){
    errorFile <- errorICD
  }

  if(groupICD){
    if(version == "ICD 9"){
      errorData <- errorFile[,ICDGroup := substr(ICD,1,1),][,c("groupCount","maxICD") := list(sum(count),max(count)), by = "ICDGroup"][count == maxICD,][groupCount <= OthersThreshold, ICDGroup := "Others"][order(groupCount,decreasing = T),][!duplicated(ICDGroup),][,c("CumCount","ICDPercinGroup") := list(cumsum(groupCount),round((count/groupCount)*100,2)),][,-c("maxICD","count")]

      if(!Others){
        errorData <- errorData[-nrow(errorData),]
      }
      errorData <- errorData[,c("CumCountPerc","Number","ICDGroup") :=
                               list(round(CumCount/max(CumCount)*100,2),1:nrow(errorData),factor(ICDGroup,levels = errorData$ICDGroup)),]

      ICD <- errorData[,c(5:6,9,1,8,2:4)]
      graph_col <- c("ICDGroup","groupCount")
      setnames(errorData,"ICD","MostICDinGroup")
      Xlabel <- paste0(Xlabel," (grouped)")
    }else{
      stop("ICD 10 no group!!")
    }
  }else{ #Top 10
    FileSize <- nrow(errorFile)
    errorData <- errorFile[, c("CumCount", "Number") :=
                             list(cumsum(count), 1:FileSize),][Number > 10, c("Number", "CumCount", "count", "ICD") :=
                                                                              list(11, max(CumCount), sum(count),"Others"),][!duplicated(Number),]
    if(!Others){
      errorData <- head(errorData, 10)
    }
    errorData <- errorData[, c("CumCountPerc","ICD") := list(round(CumCount/max(CumCount)*100,2),factor(ICD,levels = errorData$ICD)),]

    ICD <- errorData[,-c("Number","CumCount")]
    graph_col <- c("ICD","count")

  }
  Max <- errorData[,max(CumCount)]
  errorICDgraph <- ggplot(as.data.frame(errorData),
                          aes(x= eval(parse(text = paste(graph_col[1]))),
                              y = eval(parse(text = paste(graph_col[2]))))) +
    geom_bar(stat="identity", aes(fill = Number)) +
    guides(fill = FALSE, color = FALSE) +
    geom_line(aes(x= Number, y = CumCount, color = Number)) +
    geom_point(aes(x= Number, y = CumCount, color = Number), pch = 10) +
    scale_x_discrete(breaks = errorData[,eval(parse(text = paste(graph_col[1])))]) +
    annotate("rect", xmin = nrow(errorData) + .55, xmax =  nrow(errorData) + 1,
             ymin = -.02 * Max, ymax = Max * 1.02, fill = "white") +
    annotate("text", x = nrow(errorData) + .8, y = seq(0, Max, Max/10),
             label = c("  0%", " 10%", " 20%", " 30%", " 40%", " 50%", " 60%", " 70%", " 80%", " 90%", "100%"),
             size = 2.8) +
    geom_segment(x = Max + .55, xend = Max + .55, y = -.02 * Max, yend = Max * 1.02, color = "gray70") +
    xlab(Xlabel) + ylab("count") +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 20))

  return(list(graph = errorICDgraph,
              ICD = ICD))
}
