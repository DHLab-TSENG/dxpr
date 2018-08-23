#' emr.
#'
#' @name emr
#' @docType package
#' @import icd
NULL

#' Clinical Classifications Software (CCS)  for ICD-9-CM
#'
#' A dataframe for ICD-9-CM CCS mapping, containing ICD description, CCS single-level diagnosis classification, and CCS multiple level
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
#' \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip}
#' \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip}
NULL

#' Clinical Classifications Software (CCS)  for ICD-10-CM
#'
#' A dataframe for ICD-10-CM CCS mapping, containing ICD description, CCS single-level diagnosis classification, and CCS multiple level
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
#' @format A data frame with 72167 rows and 8 variables
#' \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_dx_icd10cm_2018_1.zip}
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
NULL

#' Elixhauser Comorbidity for ICD-9
#'
#' A dataframe for ICD-9 Elixhauser comorbidity mapping
#'
#' \itemize{
#'   \item ICD. ICD-9
#'   \item Comorbidity. comorbidity measures for ICD-9 (CHF, Arrhythmia, Valvular, PHTN, PVD, HTN, HTNcx, Paralysis, NeuroOther, Pulmonary, DM, DMcx, Hypothyroid, Renal, Liver, PUD, HIV, Lymphoma, Mets, Tumor, Rheumatic, Coagulopathy, Obesity, WeightLoss, FluidsLytes, BloodLoss, Anemia, Alcohol, Drugs, Psychoses, Depression)
#'   \item Value. if ICD code has comorbidity measures, value is 1
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name icd9_elix
#' @usage data(icd9_elix)
#' @format A data frame with 13742 rows and 3 variables
NULL

#' Elixhauser Comorbidity for ICD-10
#'
#' A dataframe for ICD-10 Elixhauser comorbidity mapping
#'
#' \itemize{
#'   \item ICD. ICD-10
#'   \item Comorbidity. comorbidity measures for ICD-10 (CHF, Arrhythmia, Valvular, PHTN, PVD, HTN, HTNcx, Paralysis, NeuroOther, Pulmonary, DM, DMcx, Hypothyroid, Renal, Liver, PUD, HIV, Lymphoma, Mets, Tumor, Rheumatic, Coagulopathy, Obesity, WeightLoss, FluidsLytes, BloodLoss, Anemia, Alcohol, Drugs, Psychoses, Depression)
#'   \item Value. if ICD code has comorbidity measures, value is 1
#' }
#'
#' @docType data
#' @keywords dataframe
#' @name icd10_elix
#' @usage data(icd10_elix)
#' @format A data frame with 399 rows and 3 variables
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
#' \url{https://phewascatalog.org/phecodes}


NULL