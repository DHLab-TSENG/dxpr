#' emr.
#'
#' @name emr
#' @docType package
NULL

#' ICD-9-CM
#'
#' A dataframe for ICD-9-CM, containing ICD-9-CM code and ICD description(Updated for codes valid through FY 2011.)
#'
#' \itemize{
#'   \item ICD. ICD-9-CM
#'   \item ICD_DESCRIPTION. ICD-9-CM description
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name dxICD9
#' @usage data(dxICD10)
#' @format A data frame with 14567 rows and 2 variables
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
#' @seealso \code{\link{dxICD10}}
NULL

#' ICD-10-CM
#'
#' A dataframe for ICD-10-CM, containing ICD-10-CM code and ICD description(Updated for codes valid through FY 2019.)
#'
#' \itemize{
#'   \item ICD. ICD-10-CM
#'   \item ICD_DESCRIPTION. ICD-10-CM description
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name dxICD10
#' @usage data(dxICD10)
#' @format A data frame with 71931 rows and 2 variables
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD10/2019-ICD-10-CM.html}
#' @seealso \code{\link{dxICD9}}
NULL

#' ICD-9-PCS
#'
#' A dataframe for ICD-9-PCS, containing ICD-9-PCS code and ICD description(Updated for codes valid through FY 2014.)
#'
#' \itemize{
#'   \item ICD. ICD-9-PCS
#'   \item ICD_DESCRIPTION. ICD-9-PCS description
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name prICD9
#' @usage data(prICD9)
#' @format A data frame with 3882 rows and 2 variables
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
#' @seealso \code{\link{prICD10}}
NULL

#' ICD-10-PCS
#'
#' A dataframe for ICD-10-PCS, containing ICD-10-PCS code and ICD description(Updated for codes valid through FY 2019.)
#'
#' \itemize{
#'   \item ICD. ICD-10-PCS
#'   \item ICD_DESCRIPTION. ICD-10-PCS description
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name prICD10
#' @usage data(prICD10)
#' @format A data frame with 78880 rows and 2 variables
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD10/2019-ICD-10-PCS.html}
#' @seealso \code{\link{prICD9}}
NULL

#' Clinical Classifications Software (CCS)  for ICD-9-CM
#'
#' A dataframe for ICD-9-CM CCS mapping, containing ICD description, CCS single-level diagnosis classification, and CCS multiple level, which can be used for aggregate statistical reporting of a variety of types. (Updated for codes valid through FY 2015.)
#'
#' \itemize{
#'   \item ICD. ICD-9-CM
#'   \item CCS_CATEGORY. single ccs category for ICD-9
#'   \item ICD_DESCRIPTION. ICD-9 description
#'   \item CCS_CATEGORY_DESCRIPTION. single ccs description for ICD-9
#'   \item CCS_LVL_1. multiple ccs level one category for ICD-9
#'   \item CCS_LVL_1_LABEL. multiple ccs level one description for ICD-9
#'   \item CCS_LVL_2. multiple ccs level two category for ICD-9
#'   \item CCS_LVL_2_LABEL. multiple ccs level two description for ICD-9
#'   \item CCS_LVL_3. multiple ccs level three category for ICD-9
#'   \item CCS_LVL_3_LABEL. multiple ccs level three description for ICD-9
#'   \item CCS_LVL_4. multiple ccs level four category for ICD-9
#'   \item CCS_LVL_4_LABEL. multiple ccs level four description for ICD-9
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name ccsDxICD9
#' @usage data(ccsDxICD9)
#' @format A data frame with 15072 rows and 12 variables
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip}
#' @seealso \code{\link{ccsDxICD10}}
NULL

#' Clinical Classifications Software (CCS)  for ICD-10-CM
#'
#' A dataframe for ICD-10-CM CCS mapping, containing ICD description, CCS single-level diagnosis classification, and CCS multiple level, which can be used for aggregate statistical reporting of a variety of types. (Updated for codes valid through FY 2019.)
#'
#' \itemize{
#'   \item ICD. ICD-10-CM
#'   \item CCS_CATEGORY. single ccs category for ICD-10
#'   \item ICD_DESCRIPTION. ICD-10 description
#'   \item CCS_CATEGORY_DESCRIPTION. single ccs description for ICD-10
#'   \item CCS_LVL_1. multiple ccs level one category for ICD-10
#'   \item CCS_LVL_1_LABEL. multiple ccs level one description for ICD-10
#'   \item CCS_LVL_2. multiple ccs level two category for ICD-10
#'   \item CCS_LVL_2_LABEL. multiple ccs level two description for ICD-10
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name ccsDxICD10
#' @usage data(ccsDxICD10)
#' @format A data frame with 72446 rows and 8 variables
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_dx_icd10cm_2019_1.zip}
#' @seealso \code{\link{ccsDxICD9}}
NULL

#' Clinical Classifications Software (CCS)  for ICD-9-PCS
#'
#' A dataframe for ICD-9-PCS CCS mapping, containing ICD description, CCS single-level diagnosis classification, and CCS multiple level, which can be used for aggregate statistical reporting of a variety of types. (Updated for codes valid through FY 2015.)
#'
#' \itemize{
#'   \item ICD. ICD-9-PCS
#'   \item CCS_CATEGORY. single ccs category for ICD-9
#'   \item ICD_DESCRIPTION. ICD-9 description
#'   \item CCS_CATEGORY_DESCRIPTION. single ccs description for ICD-9
#'   \item CCS_LVL_1. multiple ccs level one category for ICD-9
#'   \item CCS_LVL_1_LABEL. multiple ccs level one description for ICD-9
#'   \item CCS_LVL_2. multiple ccs level two category for ICD-9
#'   \item CCS_LVL_2_LABEL. multiple ccs level two description for ICD-9
#'   \item CCS_LVL_3. multiple ccs level three category for ICD-9
#'   \item CCS_LVL_3_LABEL. multiple ccs level three description for ICD-9
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name ccsPrICD9
#' @usage data(ccsPrICD9)
#' @format A data frame with 3948 rows and 10 variables
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip}
#' @seealso \code{\link{ccsPrICD10}}
NULL

#' Clinical Classifications Software (CCS)  for ICD-10-PCS
#'
#' A dataframe for ICD-10-PCS CCS mapping, containing ICD description, CCS single-level diagnosis classification, and CCS multiple level, which can be used for aggregate statistical reporting of a variety of types. (Updated for codes valid through FY 2019.)
#'
#' \itemize{
#'   \item ICD. ICD-10-PCS
#'   \item CCS_CATEGORY. single ccs category for ICD-10
#'   \item ICD_DESCRIPTION. ICD-10 description
#'   \item CCS_CATEGORY_DESCRIPTION. single ccs description for ICD-10
#'   \item CCS_LVL_1. multiple ccs level one category for ICD-10
#'   \item CCS_LVL_1_LABEL. multiple ccs level one description for ICD-10
#'   \item CCS_LVL_2. multiple ccs level two category for ICD-10
#'   \item CCS_LVL_2_LABEL. multiple ccs level two description for ICD-10
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name ccsPrICD10
#' @usage data(ccsPrICD10)
#' @format A data frame with 79758 rows and 8 variables
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_pr_icd10pcs_2019_1.zip}
#' @seealso \code{\link{ccsPrICD9}}
NULL

#' Procedure Class for ICD-9-PCS
#'
#' The Procedure Classes provide users an easy way to categorize procedure codes into one of four broad categories: Minor Diagnostic, Minor Therapeutic, Major Diagnostic, and Major Therapeutic.  (Updated for codes valid through FY 2015.)
#'
#' \itemize{
#'   \item ICD. ICD-9-CM procedure codes
#'   \item ICD_DESCRIPTION. Code Description (word label for the code)
#'   \item PROCEDURE_CLASS. 1=minor diagnostic / 2=minor therapeutic / 3 = major diagnostic / 4=major therapeutic
#'   \item PROCEDURE_CLASS_NAME. minor diagnostic, minor therapeutic, major diagnostic, and major therapeutic
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name pcICD9
#' @usage data(pcICD9)
#' @format A data frame with 3948 rows and 4 variables
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/procedure/pc2015.csv}
#' @seealso \code{\link{pcICD10}}
NULL


#' Procedure Class for ICD-10-PCS
#'
#' The Procedure Classes provide users an easy way to categorize procedure codes into one of four broad categories: Minor Diagnostic, Minor Therapeutic, Major Diagnostic, and Major Therapeutic.  (Updated for codes valid through FY 2019.)
#'
#' \itemize{
#'   \item ICD. ICD-10-CM procedure codes
#'   \item ICD_DESCRIPTION. Code Description (word label for the code)
#'   \item PROCEDURE_CLASS. 1=minor diagnostic / 2=minor therapeutic / 3 = major diagnostic / 4=major therapeutic
#'   \item PROCEDURE_CLASS_NAME. minor diagnostic, minor therapeutic, major diagnostic, and major therapeutic
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name pcICD10
#' @usage data(pcICD10)
#' @format A data frame with 79758 rows and 4 variables
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/procedureicd10/pc_icd10pcs_2019_1.zip}
#' @seealso \code{\link{pcICD9}}
NULL

#' AHRQ Comorbidity for ICD-9
#'
#' A dataframe for ICD-9 AHRQ comorbidity mapping
#'
#' \itemize{
#'   \item ICD. ICD-9
#'   \item Comorbidity. comorbidity measures for ICD-9 (CHF, Valvular, PHTN, PVD, HTN, HTNcx, Paralysis, NeuroOther, Pulmonary, DM, DMcx, Hypothyroid, Renal, Liver, PUD, HIV, Lymphoma, Mets, Tumor, Rheumatic, Coagulopathy, Obesity, WeightLoss, FluidsLytes, BloodLoss, Anemia, Alcohol, Drugs, Psychoses, Depression)
#'   \item Value. if ICD code has comorbidity measures, value is 1
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name icd9_ahrq
#' @usage data(icd9_ahrq)
#' @format A data frame with 14696 rows and 3 variables
#' @seealso \code{\link{icd10_ahrq}} \code{\link{icd9_charlson}} \code{\link{icd10_charlson}} \code{\link{icd9_elix}} \code{\link{icd10_elix}}
NULL

#' AHRQ Comorbidity for ICD-10
#'
#' A dataframe for ICD-10 AHRQ comorbidity mapping
#'
#' \itemize{
#'   \item ICD. ICD-10
#'   \item Comorbidity. comorbidity measures for ICD-10 (CHF, Valvular, PHTN, PVD, HTN, HTNcx, Paralysis, NeuroOther, Pulmonary, DM, DMcx, Hypothyroid, Renal, Liver, PUD, HIV, Lymphoma, Mets, Tumor, Rheumatic, Coagulopathy, Obesity, WeightLoss, FluidsLytes, BloodLoss, Anemia, Alcohol, Drugs, Psychoses, Depression)
#'   \item Value. if ICD code has comorbidity measures, value is 1
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name icd10_ahrq
#' @usage data(icd10_ahrq)
#' @format A data frame with 3217 rows and 3 variables
#' @seealso \code{\link{icd9_ahrq}} \code{\link{icd9_charlson}} \code{\link{icd10_charlson}} \code{\link{icd9_elix}} \code{\link{icd10_elix}}
NULL

#' Charlson Comorbidity for ICD-9
#'
#' A dataframe for ICD-9 Charlson comorbidity mapping
#'
#' \itemize{
#'   \item ICD. ICD-9
#'   \item Comorbidity. comorbidity measures for ICD-9 (MI, CHF, PVD, Stroke, Dementia, Pulmonary, Rheumatic, PUD, LiverMild, DM, DMcx, Paralysis, Renal, Cancer, LiverSevere, Mets, HIV)
#'   \item Value. if ICD code has comorbidity measures, value is 1
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name icd9_charlson
#' @usage data(icd9_charlson)
#' @format A data frame with 12617 rows and 3 variables
#' @seealso \code{\link{icd9_ahrq}} \code{\link{icd10_ahrq}} \code{\link{icd10_charlson}} \code{\link{icd9_elix}} \code{\link{icd10_elix}}
NULL

#' Charlson Comorbidity for ICD-10
#'
#' A dataframe for ICD-10 Charlson comorbidity mapping
#'
#' \itemize{
#'   \item ICD. ICD-10
#'   \item Comorbidity. comorbidity measures for ICD-10 (MI, CHF, PVD, Stroke, Dementia, Pulmonary, Rheumatic, PUD, LiverMild, DM, DMcx, Paralysis, Renal, Cancer, LiverSevere, Mets, HIV)
#'   \item Value. if ICD code has comorbidity measures, value is 1
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name icd10_charlson
#' @usage data(icd10_charlson)
#' @format A data frame with 3029 rows and 3 variables
#' @seealso \code{\link{icd9_ahrq}} \code{\link{icd10_ahrq}} \code{\link{icd9_charlson}} \code{\link{icd9_elix}} \code{\link{icd10_elix}}
NULL

#' Elixhauser Comorbidity for ICD-9
#'
#' A dataframe for ICD-9 Elixhauser comorbidity mapping
#'
#' \itemize{
#'   \item ICD. ICD-9
#'   \item Comorbidity_abbr. comorbidity measures abbreviation for ICD-9
#'   \item Comorbidity. comorbidity measures for ICD-9 (CHF, VALVE, PULMCIRC, PERIVASC, HTN_C (using HTN, HTNCX), PARA, NEURO, CHRNLUNG, DM, DMcx, HYPOTHY, RENLFAIL, LIVER, ULCER, AIDS, Lymph, METS, TUMOR, ARTH, Tumor, COAG, OBESE, WGHTLOSS, LYTES, BLDLOSS, ANEMDEF, ALCOHOL, DRUG, PSYCH, DEPESS)
#'   \item Value. if ICD code has comorbidity measures, value is 1
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name icd9_elix
#' @usage data(icd9_elix)
#' @format A data frame with 1474 rows and 4 variables
#' @seealso \code{\link{icd9_ahrq}} \code{\link{icd10_ahrq}} \code{\link{icd9_charlson}} \code{\link{icd10_charlson}} \code{\link{icd10_elix}}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp#references}
NULL

#' Elixhauser Comorbidity for ICD-10
#'
#' A dataframe for ICD-10 Elixhauser comorbidity mapping
#'
#' \itemize{
#'   \item ICD. ICD-10
#'   \item Comorbidity_abbr. comorbidity measures abbreviation for ICD-10
#'   \item Comorbidity. comorbidity measures for ICD-9 (CHF, VALVE, PULMCIRC, PERIVASC, HTN_C (using HTN, HTNCX), PARA, NEURO, CHRNLUNG, DM, DMcx, HYPOTHY, RENLFAIL, LIVER, ULCER, AIDS, Lymph, METS, TUMOR, ARTH, Tumor, COAG, OBESE, WGHTLOSS, LYTES, BLDLOSS, ANEMDEF, ALCOHOL, DRUG, PSYCH, DEPESS)
#'   \item Value. if ICD code has comorbidity measures, value is 1
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name icd10_elix
#' @usage data(icd10_elix)
#' @format A data frame with 3493 rows and 4 variables
#' @seealso \code{\link{icd9_ahrq}} \code{\link{icd10_ahrq}} \code{\link{icd9_charlson}} \code{\link{icd10_charlson}} \code{\link{icd9_elix}}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp}
NULL

#' Phecode for ICD-9
#'
#' A dataframe for ICD-9 Phecode mapping
#'
#' \itemize{
#'   \item ICD. ICD-9
#'   \item ICDString. ICD-9 description
#'   \item PheCode. phecode for ICD-9
#'   \item PheCodeDescription. phecode description for ICD-9
#'   \item ExclPhecodes. exclude range of PheCode
#'   \item ExclPhenotypes. exclude range of PheCodeDescription
#'   \item Rollup. whether or not ICD9s mapped to this code also map to this code's parents
#'   \item Leaf. if phecode has parent phecode, leaf is 1.
#'   \item IgnoreBool. this icd9 code is ignored in analysis if this valus is 1.
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name phecode_icd9_2
#' @usage data(phecode_icd9_2)
#' @format A data frame with 15558 rows and 9 variables
#' @source \url{https://phewascatalog.org/phecodes}
NULL

#' Two format of ICD-9-CM (2011): Short and Decimal
#'
#' A dataframe for ICD-9 format conversion
#'
#' \itemize{
#'   \item Short ICD-9 Short format
#'   \item Decimal ICD-9 Decimal format
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name ICD9DxwithTwoFormat
#' @usage data(ICD9DxwithTwoFormat)
#' @format A data frame with 14567 rows and 2 variables
#' @source \url{https://www.findacode.com/search/search.php}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
#' @source \url{https://www.cms.gov/Medicare/Quality-Initiatives-Patient-Assessment-Instruments/HospitalQualityInits/Downloads/HospitalAppendix_F.pdf}
#' @seealso \code{\link{ICD10DxwithTwoFormat}}
NULL

#' Two format of ICD-10-CM (2019): Short and Decimal
#'
#' A dataframe for ICD-10 format conversion
#'
#' \itemize{
#'   \item Short ICD-10 Short format
#'   \item Decimal ICD-10 Decimal format
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name ICD10DxwithTwoFormat
#' @usage data(ICD10DxwithTwoFormat)
#' @format A data frame with 71931 rows and 2 variables
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD10/2019-ICD-10-CM.html}
#' @seealso \code{\link{ICD9DxwithTwoFormat}}
NULL

#' Two format of ICD-9-PCS: Short and Decimal
#'
#' A dataframe for ICD-9 format conversion
#'
#' \itemize{
#'   \item Short ICD-9 Short format
#'   \item Decimal ICD-9 Decimal format
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name ICD9PrwithTwoFormat
#' @usage data(ICD9PrwithTwoFormat)
#' @format A data frame with 3882 rows and 2 variables
#' @source \url{https://www.findacode.com/search/search.php}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
NULL

#' Test File of ICD-CM codes for grouping method
#'
#' A test file for grouping method (CCS, CCSLvl, phecode, comorbidity...) and calculating condition era
#'
#' \itemize{
#'   \item ID  MemberID
#'   \item ICD ICD-9 and ICD-10-CM codes
#'   \item Date diagnosis date
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name sampleDxFile
#' @usage data(sampleDxFile)
#' @format A data frame with 4002 rows and 3 variables
NULL
