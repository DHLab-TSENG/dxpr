#'
#' Pareto Plot of error ICD Codes
#'
#' @import data.table
#' @import ggplot2
#' @importFrom stats reorder
#' @param errorFile error file from ICD uniform function (`IcdDxDecimalToShort` or `IcdDxShortToDecimal`)
#' @param ICDVersion ICD version: ICD9 (\code{'9'}), ICD10 (\code{'10'}, and all version \code{'all'}
#' @param wrongICDType Wrong ICD type: wrong version (\code{'version'}), wrong format (\code{'format'}, and all wrong type \code{'all'}
#' @param groupICD Only ICD-9 codes can be grouped, because ICD 10 already has unique alphanumeric codes to identify known diseases. Default is FALSE
#' @param Others Default is TRUE
#' @param TopN Default is Top "10"
#' @export
#' @examples
#' head(sampleDxFile)
#' error <- IcdDxDecimalToShort(sampleDxFile, ICD, Date, "2015/10/01")
#' plot_errorICD(errorFile = error$Error,
#'               ICDVersion = 9,
#'               wrongICDType = all,
#'               groupICD = TRUE,
#'               Others = TRUE,
#'               TopN = 3)
#'
#' plot_errorICD(errorFile = error$Error,
#'               ICDVersion = all,
#'               wrongICDType = all,
#'               groupICD = FALSE,
#'               Others = TRUE)
#'
plot_errorICD <- function(errorFile, ICDVersion = all, wrongICDType = all, groupICD = FALSE, Others = TRUE, TopN = 10){
  ICDVersion <- tolower(deparse(substitute(ICDVersion)))
  wrongICDType <- tolower(deparse(substitute(wrongICDType)))
  title <- paste0("Error ICD: Top ",TopN)
  Xlabel <- "Error ICD"

  if(ICDVersion == "9" | ICDVersion == "10"){
    version <- paste0("ICD ", ICDVersion)
    errorICD <- errorFile[IcdVersionInFile == version]
    title <- paste0("Error ", version, ": Top ",TopN)
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

  if(exists("errorICD")){errorFile <- errorICD}

  if(groupICD){
    if(version == "ICD 9"){
      errorData <- errorFile[,ICDGroup := substr(ICD,1,1),][,c("groupCount","maxICD") := list(sum(count),max(count)), by = "ICDGroup"][count == maxICD,][order(groupCount,decreasing = TRUE),]
      errorData <-errorData[,Number :=  1:nrow(errorData),][Number > TopN, c("ICDGroup","groupCount") := list("Others",sum(groupCount)),][!duplicated(ICDGroup),][,c("CumCount","ICDPercInGroup") := list(cumsum(groupCount),paste0(round((count/groupCount)*100,2),"%")),][,-"count"]

      if(!Others){
        errorData <- errorData[!ICDGroup == "Others",]
      }
      errorData <- errorData[,c("CumCountPerc","ICDGroup") :=
                               list(paste0(round(CumCount/max(CumCount)*100,2),"%"),factor(ICDGroup,levels = unique(ICDGroup))),]

      graph_col <- c("ICDGroup","groupCount")
      setnames(errorData,"ICD","MostICDInGroup")
      Xlabel <- paste0(Xlabel," (grouped)")
      ICD <- errorData[,c(5,6,11,1,10,2)]
    }else{
      stop("ICD 10 already has unique alphanumeric codes to identify known diseases")
    }
  }else{
    FileSize <- nrow(errorFile)
    errorData <- errorFile[, c("CumCount", "Number") :=
                             list(cumsum(count), 1:FileSize),][Number > TopN, c("CumCount", "count", "ICD") :=
                                                                 list(max(CumCount), sum(count),"Others"),][!duplicated(ICD),]
    if(!Others){
      errorData <- errorData[!ICD == "Others",]
    }
    errorData <- errorData[, c("CumCountPerc","ICD") := list(paste0(round(CumCount/max(CumCount)*100,2),"%"),factor(ICD,levels = unique(ICD))),]

    ICD <- errorData[,c(1,2,8,3:5)]
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
