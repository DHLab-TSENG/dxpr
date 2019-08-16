#' emr : A health data analysis tool of electric medical record
#'
#' emr provides mechanisms to analyze, integrate and visualize clinical data, including diagnosis and procedure records. Preparing a research-ready dataset from EHRs is a complex and time-consuming task and requires substantial data science skills.
#'
#' It has four main sections:
#'
#'\itemize{
#'  \item Code standardization:  Transform the diagnostic and procedure codes into uniform format before the integration process.
#'  \item Data integration: Group EHR diagnostic/procedure codes with different strategies, after code grouping, emr provide functions for querying matching cases, splitting data and calculating condtion era by grouped categories of each patients.
#'  \item Exploratory data analysis (EDA) preparation: Convert long format of grouped data into wide format which is fit to others analytical and plotting packages.
#'  \item Visualization: Provide overviews for diagnoses standardization and data integration, such as the differences of comorbidities between case and control groups, and the most common diagnoses which are fail to be grouped or standardized.
#'}
#'
#'  To learn more about emr, start with the vignettes:
#' `browseVignettes(package = "emr")`
#'
"_PACKAGE"
NULL


#' Datasets for code standardization
#'
#' These datasets are used for diagnostic code standardization
#'
#' These datasets are used for \code{IcdDxShortToDecimal}, and \code{IcdDxDecimalToShort}. Transform the diagnosis codes into uniform format before the integration process.
#' @name Dx_dataset_standardization
#' @format These datatable with ICD9 and ICD10 diagnosis codes.
"ICD9DxwithTwoFormat"
#' @rdname Dx_dataset_standardization
"ICD10DxwithTwoFormat"
#' @source \url{https://www.findacode.com/search/search.php}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
#' @source \url{https://www.cms.gov/Medicare/Quality-Initiatives-Patient-Assessment-Instruments/HospitalQualityInits/Downloads/HospitalAppendix_F.pdf}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD10/2019-ICD-10-CM.html}
NULL

#' Datasets for code classification
#'
#' These datasets are used for diagnostic code transformation.
#'
#' These datasets are used for \code{IcdDxToCCS}, \code{IcdDxToCCSLvl}, \code{IcdDxToPheWAS}, and \code{IcdDxToComorbid}
#'
#' @name Dx_dataset_classification
#' @format These datatable with ICD diagnostic codes and the specific Standardization categories.
"ccsDxICD9"
#' @rdname Dx_dataset_classification
"ccsDxICD10"
#' @rdname Dx_dataset_classification
"phecode_icd9_2"
#' @rdname Dx_dataset_classification
"icd9_ahrq"
#' @rdname Dx_dataset_classification
"icd10_ahrq"
#' @rdname Dx_dataset_classification
"icd9_charlson"
#' @rdname Dx_dataset_classification
"icd10_charlson"
#' @rdname Dx_dataset_classification
"icd9_elix"
#' @rdname Dx_dataset_classification
"icd10_elix"
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_dx_icd10cm_2019_1.zip}
#' @source \url{https://phewascatalog.org/phecodes}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp#references}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp}
#' @source \url{http://mchp-appserv.cpe.umanitoba.ca/Upload/SAS/ICD9_E_Charlson.sas.txt}
#' @source \url{http://mchp-appserv.cpe.umanitoba.ca/Upload/SAS/ICD10_Charlson.sas.txt}
NULL

#' Sample file for demo with diagnostic codes
#'
#' Demo for code standardization, data integration, exploratory data analysis (EDA) preparation and visualization.
#'
#' @docType data
#' @name sampleDxFile
#' @format A sample data table with 300 rows and 3 variables
NULL

#' Datasets for code standardization
#'
#' These datasets are used for procedure code standardization
#'
#' These datasets are used for \code{IcdPrShortToDecimal}, and \code{IcdPrDecimalToShort}
#' @name Pr_dataset_standardization
#' @format These datatable with ICD9 and ICD10 procedure codes.
"ICD9PrwithTwoFormat"
#' @rdname Pr_dataset_standardization
"prICD10"
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD10/2019-ICD-10-PCS.html}
#' @source \url{https://www.findacode.com/search/search.php}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
NULL

#' Datasets for code classification
#'
#' These datasets are used for procedure code transformation.
#'
#' These datasets are used for \code{IcdPrToCCS}, \code{IcdPrToCCSLvl}, and \code{IcdPrToProcedureClass}
#' @name Pr_dataset_classification
#' @format These datatable with ICD procedure codes and the specific Standardization categories.
"ccsPrICD9"
#' @rdname Pr_dataset_classification
"ccsPrICD10"
#' @rdname Pr_dataset_classification
"pcICD9"
#' @rdname Pr_dataset_classification
"pcICD10"
#' @source ICD-9-PCS CCS (2015)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip}
#' @source ICD-10-PCS CCS (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_pr_icd10pcs_2019_1.zip}
#' @source ICD-9-PCS procedure class (2015)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/procedure/pc2015.csv}
#' @source ICD-10-PCS procedure class (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/procedureicd10/pc_icd10pcs_2019_1.zip}
NULL

#' Sample file for demo with procedure codes
#'
#' Demo for code standardization, data integration.
#'
#' @docType data
#' @name samplePrFile
#' @format A data table with 170 rows and 3 variables
NULL

#' Sample file for patients with patent ductus arteriosus (PDA) from MIMIC-III.
#'
#' @docType data
#' @name sample_MIMICIII
#' @format A data table with 45674 rows and 8 variables
#' @source Pollard, T. J., & Johnson, A. E. W. The MIMIC-III Clinical Database {http://dx.doi.org/10.13026/C2XW26} (2016)
#'
NULL

if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "ID", "ICD", "Date", "ICDD", "Short", "Decimal", "Number", "Suggestion",
  "ccs", "icd10usingDate","Comorbidity", "ahrq", "charlson", "elix", "isDescription",
  "Group", "period", "firstCaseDate", "endCaseDate", "CustomGroupingTable", "greplICD",
  "ICD9DxwithTwoFormat", "ICD10DxwithTwoFormat","ccsDxICD9", "ccsDxICD10", "phecode_icd9_2",
  "icd9_ahrq", "icd10_ahrq", "icd9_charlson", "icd10_charlson", "icd9_elix", "icd10_elix",
  "ICD9PrwithTwoFormat", "ICD_DESCRIPTION", "prICD10", "ccsPrICD9", "ccsPrICD10", "pcICD9", "pcICD10",
  "selectedCase", "OutRange", "OutCount", "count", "InRange", "MostCommonICDCount",
  "Gap", "NextDate", "diffDay", "episodeCount", "episode", "era",
  "timeTag","indexDateTable","window","indexDate", "firstRecordDate","endRecordDate",
  "B", "N",
  "errorFile", "ICDVersion", "WrongICDType", "WrongType", "IcdVersionInFile", "ICDGroup", "groupICD",
  "groupCount", "maxICD", "MostICDinGroup", "CumCount", "ICDPercinGroup", "CumCountPerc", "Percentage","sigCate","DiagnosticCategory"))
