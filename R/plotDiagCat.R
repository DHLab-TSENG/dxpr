#' @rdname plotDiagCat
#' @export

plotDiagCat <- function(groupedDataWide, idColName, groupColName = NULL, topN = 10, limitFreq = 0.01, pvalue = 0.05){
  Test_pvalue <- c()
  plot_title <- "Diagnostic category"
  groupedDataWide <- as.data.table(groupedDataWide)
  setnames(groupedDataWide, deparse(substitute(idColName)), "ID")
  groupedDataWide$ID <- NULL

  if(deparse(substitute(groupColName)) != "NULL"){
    setnames(groupedDataWide, deparse(substitute(groupColName)), "Group")
    setcolorder(groupedDataWide, "Group")

    groupedDataWide <- groupedDataWide[ ,c(2:(ncol(groupedDataWide))) := lapply(.SD, function(x) ifelse(x >= 1L, 1L, 0L)), .SDcols = 2:(ncol(groupedDataWide))]

    groupedDataLong <- melt(groupedDataWide, id.vars = "Group",variable.name = "DiagnosticCategory", value.name = "count")
    groupedDataLong <- as.data.table(groupedDataLong)[,list(N = sum(count)), by = list(Group, DiagnosticCategory)]
    caseN <- sum(!grepl("non|[*]",groupedDataWide$Group))
    ctrlN <- sum(grepl("non",groupedDataWide$Group))
    caseDataLong <- groupedDataLong[!grepl("non|[*]",groupedDataLong$Group),][,Percentage := round((N/caseN)*100,2)]
    ctrlDataLong <- groupedDataLong[grepl("non",groupedDataLong$Group),][,Percentage := round((N/ctrlN)*100,2)]

    for(cat in 1:nrow(caseDataLong)){
      if(caseDataLong$Percentage[cat] >= limitFreq | ctrlDataLong$Percentage[cat] >= limitFreq){
        Table <- matrix(c(caseDataLong$N[cat], ctrlDataLong$N[cat],
                          caseN - caseDataLong$N[cat], ctrlN - ctrlDataLong$N[cat]), 2)

        if(sum(Table < 5) < 1 && sum((chisq.test(Table, simulate.p.value = TRUE)$expected) < 5) < 1){
          Test_pvalue[[length(Test_pvalue)+1]] <- chisq.test(Table, correct = FALSE)$p.value < pvalue
        }else{
          Test_pvalue[[length(Test_pvalue)+1]] <- fisher.test(Table, alternative = "greater")$p.value < pvalue
        }
      }else{
        Test_pvalue[[length(Test_pvalue)+1]] <- FALSE
      }
    }
    if(sum(Test_pvalue) == 0){
      return(message("There is no significant category between case and control"))
    }else{
      groupedDataLong <- groupedDataLong[,list(sum = sum(N)),by = DiagnosticCategory][order(sum,decreasing = TRUE)]
      groupedDataLong <- groupedDataLong[1:topN,][order(sum),"DiagnosticCategory"]
      groupedDataLong[,"DiagnosticCategory" := factor(DiagnosticCategory, levels = groupedDataLong$DiagnosticCategory),]

      dignosticCate <- merge(groupedDataLong, rbind(caseDataLong[Test_pvalue,],ctrlDataLong[Test_pvalue,]),all.x = TRUE)[!is.na(Group),]
      dignosticCate[,c("Group","Percentage") := list(factor(Group, levels = unique(dignosticCate$Group)),
                                                   Percentage),]
      dignosticCate <- dignosticCate[order(DiagnosticCategory, Group, N, decreasing = TRUE),]

      g <- ggplot(dignosticCate, aes(fill =  Group, y = Percentage, x = DiagnosticCategory, group = Group)) +
        geom_text(aes(label = paste("n =", N)), hjust = -.2, size = 3, position = position_dodge(width = 1)) +
        geom_bar(position="dodge", stat="identity")
    }
  }else{
    groupedDataWide <- groupedDataWide[ ,c(1:(ncol(groupedDataWide))) := lapply(.SD, function(x) ifelse(x >= 1L, 1L, 0L)), .SDcols = 1:(ncol(groupedDataWide))]

    groupedDataLong <<- melt(groupedDataWide, measure.vars = 1:ncol(groupedDataWide), variable.name = "DiagnosticCategory", value.name = "count")
    groupedDataLong <- as.data.table(groupedDataLong)[,list(N = sum(count)), by = .(DiagnosticCategory)][order(N)][,Percentage := round(N/nrow(groupedDataWide)*100,2)][Percentage >= limitFreq,]
    groupedDataLong <- groupedDataLong[][,c("DiagnosticCategory","Number","Percentage") :=
                                         list(factor(DiagnosticCategory, levels = DiagnosticCategory),
                                              nrow(groupedDataLong):1,
                                              Percentage),][Number <= topN,]
    dignosticCate <- groupedDataLong[,-"Number"][order(N, decreasing = TRUE),]

    g <- ggplot(groupedDataLong, aes(y = Percentage, x = DiagnosticCategory)) +
      geom_bar(position="dodge", stat="identity") +
      geom_text(aes(label = paste("n =", N)), hjust = -.2, size = 3, position = position_dodge(width = 1))
  }
  plot_title <- paste0(plot_title,": Top ", topN)
  dignosticCate_graph <- g + coord_flip() +
    xlab("Diagnostic category") + ylab("Diagnostic category, %") + ggtitle(plot_title) +
    theme_bw() +
    theme(axis.text.y = element_text(size = 10,face = "bold"),
          axis.text.x = element_text(size = 10,face = "bold"))

  return(list(graph = dignosticCate_graph,
              sigCate = dignosticCate))
}
