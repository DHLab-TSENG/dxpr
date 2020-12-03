#' Common argument of diagnostic functions
#'
#' Common argument of diagnostic functions
#'
#' Common argument of diagnostic functions
#' @name common_DxArg
#' @import data.table
#' @param dxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", and "Date"
#' @param idColName A column for MemberID of dxDataFile.
#' @param icdColName A column for ICD of dxDataFile
#' @param dateColName A column for Date of dxDataFile
#' @param icdVerColName A column for ICD-9/10 version. The format should be numeric 9L or 10L.
#' @param icd10usingDate ICD-10 using date
#' @param isDescription Category or description of standard classification methods for ICD diagnostic codes. By default, it set to \code{True} (standard description).
#' @param groupDataType  Four Stratified methods can chosen: CCS (\code{'ccs'}), CCS levels (\code{'ccslvl1'}, \code{'ccslvl2'}, \code{'ccslvl3'}, \code{'ccslvl4'}), PheWAS (\code{'PheWAS'}), comorbidities (\code{'ahrq'},\code{'charlson'}, \code{'elix'}), precise or fuzzy customized  method (\code{'customGrepIcdGroup'}, \code{'customIcdGroup'}). Change it to any of the other possible variables, default it set to \code{"ccs"}.
#' @param customGroupingTable Define the grouping categories. `icdDxToCustom` needs a dataset with two columns: "Group", "ICD"; "Group" defines one or more disease categories. "ICD" defines a list of disease-related ICD codes. `icdDxToCustomGrep`needs a dataset with two columns: "Group", "grepIcd"; "Group" defines one or more disease categories. "grepICD" defines disease-related ICD code character strings containing a regular expression.
#' @param selectedCaseFile Table for selectedCases. Default is \code{'NULL'}
NULL

#' Code format transformation
#'
#' Convert the ICD diagnostic codes to a uniform format
#'
#' `icdDxShortToDecimal` used for grouping diagnostic code to PheWAS classification (icdDxToPheWAS). `icdDxDecimalToShort` used for grouping to the others classification (icdDxToCCS, icdDxToCCSLvl, and icdDxToComorbid). The transformative function is not only convert the ICD to uniform format code but also check potential coding error of the ICD codes’ format or version.
#'
#'
#' @name dxUniform
#' @inherit common_DxArg
#' @param dxDataFile A file of clinical diagnostic data with at least 2 columns: "ICD" and "Date"
#' @importFrom utils head
#' @return Two new \code{data.table}s. 1) \code{ICD}: Uniform format diagnostic codes. 2) \code{Error}: Potential error codes.
#' @examples
#' # sample file for example
#'
#' head(sampleDxFile)
#'
#' # convert the diagnostic codes to the decimal format
#'
#' icdDxShortToDecimal(sampleDxFile,ICD,Date, icd10usingDate = "2015/10/01")
#'
#' # convert the diagnostic codes to the short format
#'
#' icdDxDecimalToShort(sampleDxFile,ICD,Date, icd10usingDate = "2015/10/01")
NULL

#' Code classification for CCS
#'
#' The CCS classification for ICD-9 and ICD-10 codes is a diagnostic categorization scheme that can employ in many types of projects analyzing data on diagnoses.
#'
#' These CCS functions (icdDxToCCS and icdDxToCCSLvl) collapse ICD codes into a smaller number of clinically meaningful categories that are more useful for presenting descriptive statistics than are individual ICD diagnostic codes.
#'
#' @name dxCCS
#' @inherit common_DxArg
#' @param CCSLevel By default it set to \code{1}. CCS multiple level:1~4, CCS for ICD-10-CM has only 1~2 multiple levels.
#' @return Three new \code{data.table}s. 1) \code{groupedDT}: Based on \code{dxDataFile} with two new columns for uniform format diagnostic codes and classified standard categories. 2) \code{summarised_groupedDT}: Summarized the dataset  \code{groupedDT} and sorted by memberID. 3) \code{Error}: Potential error codes from \code{\link{dxUniform}}.
#' @seealso Other code classification functions: \code{\link{dxPheWAS}}, \code{\link{dxCustom}}, \code{\link{dxComorbid}}
#' @examples
#' # sample file for example
#'
#' head(sampleDxFile)
#'
#' # Group diagnostic codes into single level of CCS classification
#'
#' icdDxToCCS(sampleDxFile, ID, ICD, Date, icd10usingDate =  "2015-10-01", isDescription = TRUE)
#'
#' # Group diagnostic codes into multiple levels of CCS classification
#'
#' icdDxToCCSLvl(sampleDxFile, ID, ICD, Date, icdVerColName = NULL, "2015-10-01", 2, TRUE)
NULL

#' Code classification for PheWAS
#'
#' The PheWAS classification for ICD-9-CM codes is a diagnostic categorization scheme that can employ in many types of projects analyzing data on diagnoses.
#'
#' Collapse ICD codes into a smaller number of clinically meaningful categories that are more useful for presenting descriptive statistics than are individual ICD diagnostic codes.
#'
#' @name dxPheWAS
#' @inherit common_DxArg
#' @return Three new \code{data.table}s. 1) \code{groupedDT}: Based on \code{dxDataFile} with two new columns for uniform format diagnostic codes and classified standard categories. 2) \code{summarised_groupedDT}: Summarized the dataset  \code{groupedDT} and sorted by memberID. 3) \code{Error}: Potential error codes from \code{\link{DxUniform}}.
#' @seealso Other code classification functions: \code{\link{dxCustom}}, \code{\link{dxComorbid}}, \code{\link{DxCCS}}
#' @examples
#' # sample file for example
#'
#' head(sampleDxFile)
#'
#' # Group diagnostic codes into PheWAS
#'
#' icdDxToPheWAS(sampleDxFile, ID, ICD, Date, icdVerColName = NULL, "2015-10-01", FALSE)
NULL

#' Code classification for customized group
#'
#' Researches can define the grouping categories and therefore have more flexible for grouping ICD diagnostic codes.
#'
#' There are two functions for customized defined grouping method, the customized category grouping is based on precise (`icdDxToCustom`) and fuzzy (`icdDxToCustomGrep`) grouping method, respectively.
#'
#' @name dxCustom
#' @inherit common_DxArg
#' @return Two new \code{data.table}s. 1) \code{groupedDT}: Based on \code{dxDataFile} with two new columns for uniform format diagnostic codes and classified standard categories. 2) \code{summarised_groupedDT}: Summarized the dataset  \code{groupedDT} and sorted by memberID.
#' @seealso Other code classification functions: \code{\link{dxPheWAS}}, \code{\link{dxComorbid}}, \code{\link{DxCCS}}
#' @examples
#' # sample file for example
#'
#' head(sampleDxFile)
#'
#' # Group diagnostic codes into "Chronic kidney disease" with precise grouping method
#'
#' groupingTable <- data.frame(Group = rep("Chronic kidney disease",6),
#'                             ICD = c("N181","5853","5854","5855","5856","5859"),
#'                             stringsAsFactors = FALSE)
#'
#' icdDxToCustom(sampleDxFile, ID, ICD, Date, customGroupingTable = groupingTable)
#'
#' # Group diagnostic codes into "Chronic kidney disease" with fuzzy grouping method
#'
#' grepTable <- data.frame(Group = "Chronic kidney disease",
#'                         grepIcd = "^585|^N18",
#'                         stringsAsFactors = FALSE)
#'
#' icdDxToCustomGrep(sampleDxFile, ID, ICD, Date, customGroupingTable = grepTable)
#'
NULL

#' Code classification for Comorbidity
#'
#' The comorbidities classification (AHRQ, Charlson ,and Elixhauser Comorbidity) for ICD diagnostic codes is a diagnostic categorization scheme that can employ in many types of projects analyzing data on diagnoses.
#'
#' Collapse ICD codes into a smaller number of clinically meaningful categories that are more useful for presenting descriptive statistics than are individual ICD diagnostic codes.
#'
#' @name dxComorbid
#' @inherit common_DxArg
#' @param comorbidMethod Three comorbidity methods: AHRQ, Charlson and Elixhauser Comorbidity. Change it to any of the other possible variables (\code{ahrq},\code{charlson}, and \code{elix}).
#' @param isDescription Category or description of standard classification methods for ICD diagnostic codes. By default it is set to \code{FALSE} ( Comorbidity categories).
#'
#' @return Three new \code{data.table}s. 1) \code{groupedDT}: Based on \code{dxDataFile} with two new columns for uniform format diagnostic codes and classified standard categories. 2) \code{summarised_groupedDT}: Summarized the dataset  \code{groupedDT} and sorted by memberID. 3) \code{Error}: Potential error codes from \code{\link{DxUniform}}.
#' @seealso Other code classification functions: \code{\link{dxPheWAS}}, \code{\link{dxCustom}}, \code{\link{DxCCS}}
#' @examples
#' # sample file for example
#'
#' head(sampleDxFile)
#'
#' # Group diagnostic codes into charlson comorbidity categories
#'
#' icdDxToComorbid(sampleDxFile, ID, ICD, Date, icdVerColName = NULL, "2015-10-01", charlson)
NULL

#' Data integration for case selection
#'
#' This query function can select the cases matching defined conditions for analyses.
#'
#' User can select cases by diagnostic categories, such as CCS category, ICD codes, etc. The function also provides the options to set the minimum number of diagnoses within a specific duration. The output dataset can be passed to `groupedDataLongToWide` to create tables in wide format for statistical analytic usage.
#'
#' @name selectCases
#' @inherit common_DxArg
#' @param caseCondition Certain diseases of standard groups.
#' @param caseCount Minimum number of diagnoses.
#' @param PeriodRange Determine days of interest for performing the case selection. By default, it set from 30 to 365 days.
#' @param CaseName Name of selected cases. By default, it set to \code{"selected"}.
#' @return A new \code{data.table} based on standard classification dataset with a new column: \code{selectedCase}
#' @seealso Other data integration functions: \code{\link{dataSplit}}, \code{\link{recordPeriod}}, \code{\link{era}}
#' @examples
#' # sample file for example
#'
#' head(sampleDxFile)
#'
#' #select case with "Diseases of the urinary system" by level 2 of CCS classification
#'
#' selectCases(dxDataFile = sampleDxFile,
#'             ID, ICD, Date,
#'             icdVerColName = NULL,
#'             groupDataType = ccslvl2,
#'             icd10usingDate = "2015/10/01",
#'             caseCondition = "Diseases of the urinary system",
#'             caseCount = 1)
NULL

#' Data integration for data split
#'
#' Splitting data by the date of the clinical event and shows the data recorded before or after the clinical event and the window counts, which is the gap between the record date and index date.
#'
#' In most condition, users need to extract data by a specific clinical event (e.g., first diagnosis dates of chronic diseases). Users can define a table of clinical index dates of each patient. The date can be generated by `selectCases` function or first/last admission date by `patientRecordDate` function.
#'
#' @name dataSplit
#' @inherit common_DxArg
#' @param Gap Gap length of the window. Default set to \code{30}.
#' @param indexDateFile Diagnostic dates for each patient in an observed period.
#' @return A new \code{data.table} based on \code{dxDataFile} and classified by \code{indexDateFile} for each patient.
#' @seealso Other data integration functions: \code{\link{selectCases}}, \code{\link{recordPeriod}}, \code{\link{era}}
#' @examples
#' # sample file for example
#'
#' SampleforCertainPatient <- sampleDxFile[grepl("A0|B0|C0|D0",ID),]
#'
#' head(SampleforCertainPatient)
#'
#' # Defined index date of patient A0,B0,C0 and D0
#'
#' indexDateTable <- data.frame(ID = c("A0","B0","C0","D0"),
#'                              indexDate = c("2023-08-12", "2024-02-12",
#'                                            "2015-12-05", "2017-01-29"),
#'                              stringsAsFactors = FALSE)
#' indexDateTable
#' # Split data by index date for each patient
#'
#' splitedData <- splitDataByDate(SampleforCertainPatient, ID, ICD, Date,
#'                                indexDateFile = indexDateTable,
#'                                gap = 30)
#' splitedData[15:19,]
NULL

#' Data integration for patient record period
#'
#' Function for researchers used for finding the first and last clinical event for a given patient as index date.
#'
#' The function queries the earliest and latest admission date for each patient.
#'
#' @name eligiblePeriod
#' @inherit common_DxArg
#' @return A new \code{data.table} based on \code{dxDataFile} with the earliest and latest admission date for each patient.
#' @seealso Other data integration functions: \code{\link{selectCases}}, \code{\link{dataSplit}}, \code{\link{era}}
#' @examples
#' # sample file for example
#'
#' head(sampleDxFile)
#'
#' # Earliest and latest admission date for each patient
#'
#' record <- getEligiblePeriod(sampleDxFile, ID, Date)
#' head(record)
NULL

#' Data integration for condition era calculation
#'
#' Conditions era is used to integrate distributed data of clinical records into a single progression record
#'
#' Calculate condition era by grouped categories of each patient. Conditions era is used to integrate distributed data of clinical records into a single progression record when the interval of admission data is smaller than the length of condition gap, and these admission data are considered same condition era.
#'
#' @name era
#' @importFrom stats na.omit
#' @inherit common_DxArg
#' @param gapDate Length of condition gap, By default, it set to 30 days \code{"30"}.
#' @return A new \code{data.table} based on classifying \code{dxDataFile} and calculated condition era by \code{groupDataType} for each patient.
#' @seealso Other data integration functions: \code{\link{selectCases}}, \code{\link{dataSplit}}, \code{\link{recordPeriod}}
#' @examples
#' # sample file for example
#'
#' head(sampleDxFile)
#'
#' # Select case with "Diseases of the urinary system" by level 2 of CCS classification
#'
#' selectedCaseFile <- selectCases(sampleDxFile, ID, ICD, Date,
#'                                 icdVerColName = NULL,
#'                                 icd10usingDate = "2015/10/01",
#'                                 groupDataType = ccslvl2,
#'                                 caseCondition = "Diseases of the urinary system",
#'                                 caseCount = 1)
#'
#' # Condition era calculation with case selection
#'
#' Era1 <- getConditionEra(sampleDxFile, ID, ICD, Date, icdVerColName = NULL, "2015-10-01",
#'                         groupDataType = CCSlvl3,
#'                         selectedCaseFile = selectedCaseFile)
#'
#' # Define the grouping categories
#'
#' grepTable <- data.frame(Group = "Chronic kidney disease",
#'                         grepIcd = "^58|^N18",
#'                         stringsAsFactors = FALSE)
#'
#' # Condition era calculation with grouping custom method of code standardization
#'
#' Era2 <- getConditionEra(sampleDxFile, ID, ICD, Date, icdVerColName = NULL, "2015-10-01",
#'                         groupDataType = customGrepIcdGroup,
#'                         customGroupingTable = grepTable)
#' head(Era1)
#' head(Era2)
NULL

#' Data format transformation
#'
#' Convert the long format of grouped data into a wide format which is fit to others analytical and plotting packages.
#'
#' The output dataset of this function can be numeric or binary wide format
#'
#' @name dataWide
#' @inherit common_DxArg
#' @param numericOrBinary Members have same diagnostic categories, type `N` or `B`, default is Binary \code{'B'}.
#' @return A new \code{data.table} based on classifying \code{dxDataFile} and converted the dataset into a wide format dataset.
#' @examples
#'
#' # Create a grouped data
#'
#' ELIX <- icdDxToComorbid(dxDataFile = sampleDxFile,
#'                        idColName = ID,
#'                        icdColName = ICD,
#'                        dateColName = Date,
#'                        icd10usingDate = "2015/10/01",
#'                        comorbidMethod = elix)
#'
#' head(ELIX$groupedDT)
#'
#' # Select case with "Diseases of the urinary system" by level 2 of CCS classification
#'
#' selectedCaseFile <- selectCases(dxDataFile = sampleDxFile,
#'                                 idColName = ID,
#'                                 icdColName = ICD,
#'                                 dateColName = Date,
#'                                 icdVerColName = NULL,
#'                                 icd10usingDate = "2015/10/01",
#'                                 groupDataType = ccslvl2,
#'                                 caseCondition = "Diseases of the urinary system",
#'                                 caseCount = 1)
#'
#'# Convert the long format of grouped data into a wide binary format with selected case
#'
#' groupedDataWide <- groupedDataLongToWide(ELIX$groupedDT,
#'                                          idColName = ID,
#'                                          categoryColName = Comorbidity,
#'                                          dateColName = Date,
#'                                          selectedCaseFile = selectedCaseFile)
#' groupedDataWide
NULL

#' Plot for error ICD list
#'
#' Pareto chart of error ICD list
#'
#' Through first phase function, code standardization, which generates a data of diagnosis codes with potential error. The Pareto chart includes bar plot and line chart to visualize individual possible error ICD codes represented in descending order and cumulative total.
#'
#' @import data.table
#' @import ggplot2
#' @importFrom stats reorder
#' @name plotError
#' @param errorFile Error file from ICD uniform function (`icdDxDecimalToShort` or `icdDxShortToDecimal`)
#' @param ICDVersion ICD version: ICD9 (\code{'9'}), ICD10 (\code{'10'}, and all version \code{'all'}
#' @param wrongICDType Wrong ICD type: wrong version (\code{'version'}), wrong format (\code{'format'}, and both wrong type \code{'all'}
#' @param groupICD Only ICD-9 codes can group, because ICD 10 already has unique alphanumeric codes to identify known diseases. Default is FALSE
#' @param Others Default is TRUE
#' @param TopN Default is Top "10"
#' @return A Pareto plot and a \code{data.table} for error codes.
#' @seealso other plot function: \code{\link{PlotGroupedData}}
#' @examples
#' # sample file for example
#' head(sampleDxFile)
#'
#' # Data of diagnosis codes with potential error
#'
#' error <- icdDxDecimalToShort(sampleDxFile, ICD, Date, icdVerColName = NULL, "2015/10/01")
#'
#' # Plot of top 3 common error ICD-9 codes and a list of the detail of error ICD codes
#'
#' plotICDError(errorFile = error$Error,
#'               icdVersion = 9,
#'               wrongICDType = all,
#'               groupICD = TRUE,
#'               others = TRUE,
#'               topN = 3)
#'
#' # Plot of top 10 common error ICD codes and a list of the detail of error ICD codes
#'
#' plotICDError(errorFile = error$Error,
#'               icdVersion = all,
#'               wrongICDType = all,
#'               groupICD = FALSE,
#'               others = TRUE)
#'
NULL

#' Plot of diagnostic categories
#'
#' Histogram plot of diagnostic categories
#'
#'An overview of grouping category of the diagnostic code summarizes the percentage of result in histogram plot. User can observe the number of diagnostic categories in their dataset through this function. This function also verifies by Chi-square test and Fisher’s exact test statistical significantly different diagnostic categories between case and control. The default level of significance considered at 5% (p = 0.05).
#'
#' @import data.table
#' @import ggplot2
#' @importFrom stats chisq.test
#' @importFrom stats fisher.test
#' @name plotDiagCat
#' @param groupedDataWide GroupedData file from functions of code classification (four stPercentagegies)
#' @param TopN Default is Top "10"
#' @param limitFreq The minimum frequency set to "0.01"; In other words, the limit at the same diagnostic category must have 1 percent patient in the total patient.
#' @param pvalue p value of chisq.test
#' @return A histogram plot and a \code{data.table} for classifying data.
#' @seealso other plot function: \code{\link{plotError}}
#' @examples
#' # sample file for example
#' head(sampleDxFile)
#'
#' # Create a grouped data
#'
#' ELIX <- icdDxToComorbid(dxDataFile = sampleDxFile,
#'                        idColName = ID,
#'                        icdColName = ICD,
#'                        dateColName = Date,
#'                        icd10usingDate = "2015/10/01",
#'                        comorbidMethod = elix)
#'
#' head(ELIX$groupedDT)
#'
#' # Convert long format of grouped data into wide binary format
#'
#' groupedDataWide <- groupedDataLongToWide(ELIX$groupedDT,
#'                                          idColName = ID,
#'                                          categoryColName = Comorbidity,
#'                                          dateColName = Date)
#'
#' # plot of top 10 common grouped categories and a list of the detail of grouped categories
#'
#' plot1 <- plotDiagCat(groupedDataWide = groupedDataWide,
#'                           idColName = ID,
#'                           topN = 10,
#'                           limitFreq = 0.01)
#' plot1
#'
#' # Select case with "Diseases of the urinary system" by level 2 of CCS classification
#'
#' selectedCaseFile <- selectCases(dxDataFile = sampleDxFile,
#'                                 idColName = ID,
#'                                 icdColName = ICD,
#'                                 dateColName = Date,
#'                                 icdVerColName = NULL,
#'                                 icd10usingDate = "2015/10/01",
#'                                 groupDataType = ccslvl2,
#'                                 caseCondition = "Diseases of the urinary system",
#'                                 caseCount = 1)
#'
#'# Convert the long format of grouped data into a wide binary format with selected case
#'
#' groupedDataWide <- groupedDataLongToWide(ELIX$groupedDT,
#'                                          idColName = ID,
#'                                          categoryColName = Comorbidity,
#'                                          dateColName = Date,
#'                                          selectedCaseFile = selectedCaseFile)
#'
#' # plot of top 10 common grouped categories and a list of the detail of grouped categories
#'
#' plot2 <- plotDiagCat(groupedDataWide = groupedDataWide,
#'                           idColName = ID,
#'                           topN = 10,
#'                           limitFreq = 0.01,
#'                           pvalue = 0.05)
#' plot2
NULL

#' Common argument of procedure functions
#'
#' Common argument of procedure functions
#'
#' Common argument of procedure functions
#' @name common_PrArg
#' @import data.table
#' @param prDataFile A file of clinical procedure data with at least 3 columns: "MemberID", "ICD", and "Date"
#' @param idColName A MemberID column of prDataFile
#' @param icdColName A ICD column of prDataFile
#' @param dateColName A Date column of prDataFile
#' @param icd10usingDate ICD-10 using date
#' @param isDescription Category or description of standard classification methods for ICD procedure codes. By default, it set to \code{True} (standard description).
NULL
NULL

#' Code format transformation
#'
#' Converts the ICD procedure codes to a uniform format
#'
#' These transformative function does not only convert the ICD to uniform format code but also check the potential coding error of the ICD codes’ format or version.
#'
#' @inherit common_PrArg
#' @name prUniform
#' @param prDataFile A file of clinical procedure data with at least 2 columns: "ICD" and "Date"
#' @return Two new \code{data.table}s. 1) \code{ICD}: Uniform format procedure codes. 2) \code{Error}: Potential error codes.
#' @examples
#' # sample file for example
#'
#' head(samplePrFile)
#'
#' # convert the procedure codes to the short format
#'
#' icdPrDecimalToShort(samplePrFile,ICD,Date, icdVerColName = NULL, "2015/10/01")
#'
#' # convert the procedure codes to the decimal format
#'
#' icdPrShortToDecimal(samplePrFile,ICD,Date, icdVerColName = NULL, "2015/10/01")
NULL

#' Code classification for CCS
#'
#' The CCS classification for ICD-9 and ICD-10 codes is a procedure categorization scheme that can employ in many types of projects analyzing data on procedures
#'
#' These CCS functions collapse ICD codes into a smaller number of clinically meaningful categories that are more useful for presenting descriptive statistics than are individual ICD procedure codes.
#'
#' @importFrom stats complete.cases
#' @name prCCS
#' @inherit common_PrArg
#' @param CCSLevel By default, it set to \code{1}. CCS multiple level:1~3, CCS for ICD-10-CM only has 1~2 multiple levels.
#' @return Two new \code{data.table}s. 1) \code{groupedDT}: Based on \code{prDataFile} with two new columns for uniform format procedure codes and classified standard categories. 2) \code{Error}: Potential error codes from \code{\link{PrUniform}}.
#' @seealso see other code classification: \code{\link{PC}}
#' @examples
#' # sample file for example
#'
#' head(samplePrFile)
#'
#' # Group procedure codes into single level of CCS classification
#'
#' icdPrToCCS(samplePrFile, ID, ICD, Date, icdVerColName = NULL, "2015-10-01", TRUE)
#'
#' # Group procedure codes into multiple levels of CCS classification
#'
#' icdPrToCCSLvl(samplePrFile, ID, ICD, Date, icdVerColName = NULL, "2015-10-01", 2, TRUE)
#'
NULL

#' Code classification for procedure code
#'
#' The Procedure Class classification for ICD-9 and ICD-10 codes is a procedure categorization scheme that can employ in many types of projects analyzing data
#'
#' Collapse ICD codes into a smaller number of clinically meaningful categories that are more useful for presenting descriptive statistics than are individual ICD procedure codes.
#'
#' @importFrom stats complete.cases
#' @name PC
#' @inherit common_PrArg
#' @return Two new \code{data.table}s. 1) \code{groupedDT}: Based on \code{prDataFile} with two new columns for uniform format procedure codes and classified standard categories. 2) \code{Error}: Potential error codes from \code{\link{PrUniform}}.
#' @seealso see other code classification: \code{\link{PrCCS}}
#' @examples
#' # sample file for example
#'
#' head(samplePrFile)
#'
#' # Group procedure codes into procedure class classification
#'
#' icdPrToProcedureClass(samplePrFile, ID, ICD, Date, icdVerColName = NULL, "2015-10-01", TRUE)
#'
NULL
