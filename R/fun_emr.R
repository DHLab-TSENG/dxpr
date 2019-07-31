#' Common argument of diagnostic functions
#'
#' Common argument of diagnostic functions
#'
#' Common argument of diagnostic functions
#' @name common_DxArg
#' @import data.table
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", and "Date"
#' @param idColName A column for MemberID of DxDataFile.
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate ICD-10 using date
#' @param isDescription Category or description of standard classification methods for ICD diagnostic codes. By default, it set to \code{True} (standard description).
#' @param groupDataType  Four Stratified methods can chosen: CCS (\code{'ccs'}), CCS levels (\code{'ccslvl1'}, \code{'ccslvl2'}, \code{'ccslvl3'}, \code{'ccslvl4'}), PheWAS (\code{'PheWAS'}), comorbidities (\code{'ahrq'},\code{'charlson'}, \code{'elix'}), precise or fuzzy customized  method (\code{'customGrepIcdGroup'}, \code{'customIcdGroup'}). Change it to any of the other possible variables, default it set to \code{"ccs"}.
#' @param CustomGroupingTable Define the grouping categories. `IcdDxToCustom` needs a dataset with two columns: "Group", "ICD"; "Group" defines one or more disease categories. "ICD" defines a list of disease-related ICD codes. `IcdDxToCustomGrep`needs a dataset with two columns: "Group", "grepIcd"; "Group" defines one or more disease categories. "grepICD" defines disease-related ICD code character strings containing a regular expression.
#' @param selectedCaseFile Table for selectedCases. Default is \code{'NULL'}
NULL

#' Code format transformation
#'
#' Convert the ICD diagnostic codes to a uniform format
#'
#' `IcdDxShortToDecimal` used for grouping diagnostic code to PheWAS classification (IcdDxToPheWAS). `IcdDxDecimalToShort` used for grouping to the others classification (IcdDxToCCS, IcdDxToCCSLvl, and IcdDxToComorbid). The transformative function is not only convert the ICD to uniform format code but also check potential coding error of the ICD codes’ format or version.
#'
#'
#' @name DxUniform
#' @inherit common_DxArg
#' @param DxDataFile A file of clinical diagnostic data with at least 2 columns: "ICD" and "Date"
#' @importFrom utils head
#' @source \url{https://www.findacode.com/search/search.php}
#' @source ICD-9-diagnostic code
#' @source \url{https://www.cms.gov/Medicare/Quality-Initiatives-Patient-Assessment-Instruments/HospitalQualityInits/Downloads/HospitalAppendix_F.pdf}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
#' @source ICD 10-diagnostic code
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD10/2019-ICD-10-CM.html}
#' @examples
#' # sample file for example
#'
#' head(sampleDxFile)
#'
#' # convert the diagnostic codes to the decimal format
#'
#' IcdDxShortToDecimal(sampleDxFile,ICD,Date,"2015/10/01")
#'
#' # convert the diagnostic codes to the short format
#'
#' IcdDxDecimalToShort(sampleDxFile,ICD,Date, "2015/10/01")
NULL

#' Code classification for CCS
#'
#' The CCS classification for ICD-9 and ICD-10 codes is a diagnostic categorization scheme that can employ in many types of projects analyzing data on diagnoses.
#'
#' These CCS functions (IcdDxToCCS and IcdDxToCCSLvl) collapse ICD codes into a smaller number of clinically meaningful categories that are more useful for presenting descriptive statistics than are individual ICD diagnostic codes.
#'
#' @name DxCCS
#' @inherit common_DxArg
#' @param CCSLevel By default it set to \code{1}. CCS multiple level:1~4, CCS for ICD-10-CM has only 1~2 multiple levels.
#' @source ICD-9-CM CCS (2015)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip}
#' @source ICD-10-CM CCS (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_dx_icd10cm_2019_1.zip}
#' @examples
#' # sample file for example
#'
#' head(sampleDxFile)
#'
#' # Group diagnostic codes into single level of CCS classification
#'
#' IcdDxToCCS(sampleDxFile, ID, ICD, Date, "2015-10-01", TRUE)
#'
#' # Group diagnostic codes into multiple levels of CCS classification
#'
#' IcdDxToCCSLvl(sampleDxFile, ID, ICD, Date, "2015-10-01", 2, TRUE)
NULL

#' Code classification for PheWAS
#'
#' The PheWAS classification for ICD-9-CM codes is a diagnostic categorization scheme that can employ in many types of projects analyzing data on diagnoses.
#'
#' Collapse ICD codes into a smaller number of clinically meaningful categories that are more useful for presenting descriptive statistics than are individual ICD diagnostic codes.
#'
#' @name DxPheWAS
#' @inherit common_DxArg
#' @source ICD-9-PheWAS (version 1.2, 2015)
#' @source \url{https://phewascatalog.org/PheWASs}
#' @examples
#' # sample file for example
#'
#' head(sampleDxFile)
#'
#' # Group diagnostic codes into PheWAS
#'
#' IcdDxToPheWAS(sampleDxFile, ID, ICD, Date, "2015-10-01", FALSE)
NULL

#' Code classification for customized group
#'
#' Researches can define the grouping categories and therefore have more flexible for grouping ICD diagnostic codes.
#'
#' There are two functions for customized defined grouping method, the customized category grouping is based on precise (`IcdDxToCustom`) and fuzzy (`IcdDxToCustomGrep`) grouping method, respectively.
#'
#' @name DxCustom
#' @inherit common_DxArg
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
#' IcdDxToCustom(sampleDxFile, ID, ICD, Date, CustomGroupingTable = groupingTable)
#'
#' # Group diagnostic codes into "Chronic kidney disease" with fuzzy grouping method
#'
#' grepTable <- data.frame(Group = "Chronic kidney disease",
#'                         grepIcd = "^585|^N18",
#'                         stringsAsFactors = FALSE)
#'
#' IcdDxToCustomGrep(sampleDxFile,ID, ICD, Date,CustomGroupingTable = grepTable)
#'
NULL

#' Code classification for Comorbidity
#'
#' The comorbidities classification (AHRQ, Charlson ,and Elixhauser Comorbidity) for ICD diagnostic codes is a diagnostic categorization scheme that can employ in many types of projects analyzing data on diagnoses.
#'
#' Collapse ICD codes into a smaller number of clinically meaningful categories that are more useful for presenting descriptive statistics than are individual ICD diagnostic codes.
#'
#' @name DxComorbid
#' @inherit common_DxArg
#' @param comorbidMethod Three comorbidity method: AHRQ, Charlson and Elixhauser Comorbidity. Change it to any of the other possible variables (\code{ahrq},\code{charlson}, and \code{elix}).
#' @param isDescription Category or description of standard classification methods for ICD diagnostic codes. By default it is set to \code{FALSE} ( Comorbidity categories).
#'
#' @source AHRQ
#' @source ICD-9-CM Elixhauser (2012-2015)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp#references}
#' @source ICD-10-CM Elixhauser (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp}
#' @source Charlson
#' @source ICD-9-CM Charlson (2006)
#' @source \url{http://mchp-appserv.cpe.umanitoba.ca/Upload/SAS/ICD9_E_Charlson.sas.txt}
#' @source ICD-10-CM Charlson (2006)
#' @source \url{http://mchp-appserv.cpe.umanitoba.ca/Upload/SAS/ICD10_Charlson.sas.txt}
#' @source Elixhauser
#' @source ICD-9-CM Elixhauser (2012-2015)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp#references}
#' @source ICD-10-CM Elixhauser (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp}
#' @examples
#' # sample file for example
#'
#' head(sampleDxFile)
#'
#' # Group diagnostic codes into charlson comorbidity categories
#'
#' IcdDxToComorbid(sampleDxFile, ID, ICD, Date, "2015-10-01", charlson)
NULL

#' Data integration for Case Selection
#'
#' This query function can select the cases matching defined conditions for analyses.
#'
#' User can select cases by diagnostic categories, such as CCS category, ICD codes, etc. The function also provides the options to set the minimum number of diagnoses within a specific duration. The output dataset can be passed to `groupedDataLongToWide` to create tables in wide format for statistical analytic usage.
#'
#' @name selectCase
#' @inherit common_DxArg
#' @param caseCondition Certain diseases of standard groups
#' @param caseCount Minimum number of diagnoses
#' @param PeriodRange Determine days of interest for performing the case selection. By default, it set from 30 to 365 days
#' @param CaseName Aggregation of selected cases name. By default, it set to \code{"selected"}.
#' @examples
#' # sample file for example
#'
#' head(sampleDxFile)
#'
#' #select case with "Diseases of the urinary system" by level 2 of CCS classification
#'
#' selectCases(DxDataFile = sampleDxFile,
#'             ID, ICD, Date,
#'             groupDataType = ccslvl2,
#'             icd10usingDate = "2015/10/01",
#'             caseCondition = "Diseases of the urinary system",
#'             caseCount = 1)
NULL

#' Data integration for Data Split
#'
#' Splitting data by the date of the clinical event and shows the data recorded before or after the clinical event and the window counts, which is the gap between the record date and index date.
#'
#' In most condition, users need to extract data by a specific clinical event (e.g., first diagnosis dates of chronic diseases). Users can define a table of clinical index dates of each patient. The date can be generated by `selectCases` function or first/last admission date by `patientRecordDate` function.
#'
#' @name DataSplit
#' @inherit common_DxArg
#' @param Gap Gap length of the window. Default set to \code{30}.
#' @param indexDateFile Diagnostic dates for each patient in an observed period
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
#'                              indexDate = c("2023-08-12", "2015-12-26",
#'                                            "2015-12-05", "2017-01-29"),
#'                              stringsAsFactors = FALSE)
#'
#' # Split data by index date for each patient
#'
#' splitedData <- splitDataByDate(SampleforCertainPatient, ID, ICD, Date,
#'                                indexDateFile = indexDateTable,
#'                                Gap = 30)
NULL

#' Data integration for Patient Record Period
#'
#' Function for researchers used for finding the first and last clinical event for a given patient as index date.
#'
#' The function queries the earliest and latest admission date for each patient.
#'
#' @name recordPeriod
#' @inherit common_DxArg
#' @examples
#' # sample file for example
#'
#' head(sampleDxFile)
#'
#' # Earliest and latest admission date for each patient
#'
#' record <- patientRecordDate(sampleDxFile, ID, ICD, Date)
#' head(record)
NULL

#' Data integration for Condition Era calculation
#'
#' Conditions era is used to integrate distributed data of clinical records into a single progression record
#'
#' Calculate condition era by grouped categories of each patient. Conditions era is used to integrate distributed data of clinical records into a single progression record when the interval of admission data is smaller than the length of condition gap, and these admission data are considered same condition era.
#'
#' @name era
#' @inherit common_DxArg
#' @param gapDate Length of condition gap, By default, it set to 30 days \code{"30"}.
#' @examples
#' # sample file for example
#'
#' head(sampleDxFile)
#'
#' # Select case with "Diseases of the urinary system" by level 2 of CCS classification
#'
#' selectedCaseFile <- selectCases(sampleDxFile, ID, ICD, Date,
#'                                 icd10usingDate = "2015/10/01",
#'                                 groupDataType = ccslvl2,
#'                                 caseCondition = "Diseases of the urinary system",
#'                                 caseCount = 1)
#'
#' # Condition era calculation with case selection
#'
#' Era1 <- getConditionEra(sampleDxFile, ID, ICD, Date, "2015-10-01",
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
#' Era2 <- getConditionEra(sampleDxFile, ID, ICD, Date, "2015-10-01",
#'                         groupDataType = customGrepIcdGroup,
#'                         CustomGroupingTable = grepTable)
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
#' @examples
#' # sample file for example
#'
#' head(sampleDxFile)
#'
#' # Select case with "Diseases of the urinary system" by level 2 of CCS classification
#'
#' selectedCaseFile <- selectCases(sampleDxFile, ID, ICD, Date,
#'                                 icd10usingDate = "2015/10/01",
#'                                 groupDataType = ccslvl2,
#'                                 caseCondition = "Diseases of the urinary system",
#'                                 caseCount = 1)
#'
#' # Convert long format grouped data into a wide numeric format with selected case
#'
#' groupedData_Wide <- groupedDataLongToWide(sampleDxFile, ID, ICD, Date,
#'                                           "2015-10-01", elix,
#'                                           numericOrBinary = N,
#'                                           isDescription = FALSE,
#'                                           selectedCaseFile = selectedCaseFile)
#' head(groupedData_Wide)
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
#' @param errorFile Error file from ICD uniform function (`IcdDxDecimalToShort` or `IcdDxShortToDecimal`)
#' @param ICDVersion ICD version: ICD9 (\code{'9'}), ICD10 (\code{'10'}, and all version \code{'all'}
#' @param wrongICDType Wrong ICD type: wrong version (\code{'version'}), wrong format (\code{'format'}, and both wrong type \code{'all'}
#' @param groupICD Only ICD-9 codes can group, because ICD 10 already has unique alphanumeric codes to identify known diseases. Default is FALSE
#' @param Others Default is TRUE
#' @param TopN Default is Top "10"
#' @examples
#' # sample file for example
#' head(sampleDxFile)
#'
#' # Data of diagnosis codes with potential error
#'
#' error <- IcdDxDecimalToShort(sampleDxFile, ICD, Date, "2015/10/01")
#'
#' # Plot of top 3 common error ICD-9 codes and a list of the detail of error ICD codes
#'
#' plot_errorICD(errorFile = error$Error,
#'               ICDVersion = 9,
#'               wrongICDType = all,
#'               groupICD = TRUE,
#'               Others = TRUE,
#'               TopN = 3)
#'
#' # Plot of top 10 common error ICD codes and a list of the detail of error ICD codes
#'
#' plot_errorICD(errorFile = error$Error,
#'               ICDVersion = all,
#'               wrongICDType = all,
#'               groupICD = FALSE,
#'               Others = TRUE)
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
#' @name PlotGroupedData
#' @param groupedDataWide GroupedData file from functions of code classification (four stPercentagegies)
#' @param TopN Default is Top "10"
#' @param limitFreq The minimum frequency set to "0.01"; In other words, the limit at the same diagnostic category must have 1 percent patient in the total patient.
#' @param pvalue p value of chisq.test
#' @examples
#' # sample file for example
#' head(sampleDxFile)
#'
#' # Convert long format of grouped data into wide binary format
#'
#' groupedDataWide <- groupedDataLongToWide(sampleDxFile, ID, ICD, Date,
#'                                          icd10usingDate = "2015-10-01",
#'                                          groupDataType = elix,
#'                                          isDescription = FALSE)
#'
#' # plot of top 10 common grouped categories and a list of the detail of grouped categories
#'
#' plot1 <- plot_groupedData(groupedDataWide = groupedDataWide,
#'                           TopN = 10,
#'                           limitFreq = 0.01)
#' plot1
#'
#' # Select case with "Diseases of the urinary system" by level 2 of CCS classification
#'
#' selectedCaseFile <- selectCases(DxDataFile = sampleDxFile,
#'                                 idColName = ID,
#'                                 icdColName = ICD,
#'                                 dateColName = Date,
#'                                 icd10usingDate = "2015/10/01",
#'                                 groupDataType = ccslvl2,
#'                                 caseCondition = "Diseases of the urinary system",
#'                                 caseCount = 1)
#'
#'# Convert the long format of grouped data into a wide binary format with selected case
#'
#' groupedDataWide <- groupedDataLongToWide(sampleDxFile, ID, ICD, Date,
#'                                          icd10usingDate = "2015-10-01",
#'                                          groupDataType = elix,
#'                                          isDescription = FALSE,
#'                                          selectedCaseFile = selectedCaseFile)
#'
#' # plot of top 10 common grouped categories and a list of the detail of grouped categories
#'
#' plot2 <- plot_groupedData(groupedDataWide = groupedDataWide,
#'                           TopN = 10,
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
#' @param PrDataFile A file of clinical procedure data with at least 3 columns: "MemberID", "ICD", and "Date"
#' @param idColName A MemberID column of PrDataFile
#' @param icdColName A ICD column of PrDataFile
#' @param dateColName A Date column of PrDataFile
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
#' @name PrUniform
#' @param PrDataFile A file of clinical procedure data with at least 2 columns: "ICD" and "Date"
#' @source \url{https://www.findacode.com/search/search.php}
#' @source \url{https://www.cms.gov/Medicare/Quality-Initiatives-Patient-Assessment-Instruments/HospitalQualityInits/Downloads/HospitalAppendix_F.pdf}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD10/2019-ICD-10-PCS.html}
#' @examples
#' # sample file for example
#'
#' head(samplePrFile)
#'
#' # convert the procedure codes to the short format
#'
#' IcdPrDecimalToShort(samplePrFile,ICD,Date,"2015/10/01")
#'
#' # convert the procedure codes to the decimal format
#'
#' IcdPrShortToDecimal(samplePrFile,ICD,Date,"2015/10/01")
NULL

#' Code classification for CCS
#'
#' The CCS classification for ICD-9 and ICD-10 codes is a procedure categorization scheme that can employ in many types of projects analyzing data on procedures
#'
#' These CCS functions collapse ICD codes into a smaller number of clinically meaningful categories that are more useful for presenting descriptive statistics than are individual ICD procedure codes.
#'
#' @importFrom stats complete.cases
#' @name PrCCS
#' @inherit common_PrArg
#' @param CCSLevel By default, it set to \code{1}. CCS multiple level:1~3, CCS for ICD-10-CM only has 1~2 multiple levels.
#' @source ICD-9-PCS CCS (2015)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip}
#' @source ICD-10-PCS CCS (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_pr_icd10pcs_2019_1.zip}
#' @examples
#' # sample file for example
#'
#' head(samplePrFile)
#'
#' # Group procedure codes into single level of CCS classification
#'
#' IcdPrToCCS(samplePrFile, ID, ICD, Date, "2015-10-01", TRUE)
#'
#' # Group procedure codes into multiple levels of CCS classification
#'
#' IcdPrToCCSLvl(samplePrFile, ID, ICD, Date, "2015-10-01", 2, TRUE)
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
#' @source ICD-9-Procedure Class (2015)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/procedure/pc2015.csv}
#' @source ICD-10-Procedure Class (2018)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/procedureicd10/procedure_icd10.jsp}
#' @examples
#' # sample file for example
#'
#' head(samplePrFile)
#'
#' # Group procedure codes into procedure class classification
#'
#' IcdPrToProcedureClass(samplePrFile, ID, ICD, Date, "2015-10-01", TRUE)
#'
NULL
