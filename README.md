## emr
# Introduction
A tool of `Electric Medical Record` for grouping with ICD-9 and ICD-10 codes by `ccs, comorbidities and phecodes`, and calculating `condition era`. 
There are some information about patients' diagnoses data: members' ID, diagnosis ICD-9/ICD-10 codes, and date of service started, etc...
This tool can be used to group the ICD's multitude of codes into a smaller number of clinically meaningful categories (CCS, phecode, comorbidity, and even grouping rule by your standards!). 

### Feature
- convert ICD code's format: Short <-> Decimal
- Get the Clinical Classifications Software (CCS) categories and description for ICD-9 and ICD-10 codes on diagnoses
- Get the Phecode or phenotype (phecode description) of ICD-9 diagnosis codes
- Get the categories for ICD-9 and ICD-10 codes on diagnoses, the grouping  rules are based on your standard
- Grouping comorbid method measures (AHRQ, Charlson and Elixhauser Comorbidity) infers whether to use ICD-9 or ICD-10 codes
- Get the qualified cases which based on the ICD code searching criteria and number of ICD code per patients in the inout factIcd dataset
- Get the condition era

### Install
```{R}
devtools::install_github("DHLab-CGU/emr")
```
### Generate ICD code Decimal Format for conversion function
ICD codes have two forms: decimal`E950.7` and short`E9507` format, first at all, unifing the ICD codes format.
There are two function to generate ICD codes' decimal format.
### Convert ICD-9-CM and ICD-10-CM code Format: Short <->Decimal
There are two functions to let user convert ICD code format  for the following function for grouping methods.
Second, according to the using format of function, these conversion function can be used to convert ICD codes between types.
### Grouping ICD codes by ...
classifying ICD-9-CM and ICD-10-CM diagnoses codes into clinically meaningful categories, which can be used for aggregate statistical reporting of a variety of types.
### Clinical Classifications Software (CCS)
It is based on the CCS for ICD-9-CM and attempts to map ICD-10-CM/PCS codes into the same categories. Get the CCS categories and description by ICD-9/ICD-10 code.
#### single-level category
#### multi-level
### Phecode
This can be used to group Phecode or description of phecode (phenotype) by ICD-9 diagnosis codes in clinical diagnostic data.
### Creating a grouping standards
### Comorbidities
The comorbidities measures from different sources are provided as lists.
#### AHRQ comorbidity classification
AHRQ comorbidity measure dataframe is based on `icd package`, the names of the comorbidities derived from ICD-9 and ICD-10 codes are the same.
The AHRQ comorbidity measure map format is dataframe;it is convert `icd9_map_ahrq` list into `icd9_ahrq` dataframe.The functions apply those mappings to join with user's ICD data.
#### Charlson comorbidity classification
Charlson comorbidity measure dataframe is based on `icd package`: icd9_map_charlson, icd10_map_charlson
#### Elixhauser comorbidity classification
Elixhauser comorbidity measure dataframe is based on `icd package`: icd9_map_elix, icd10_map_elix
### Selection Cases
This can be used to select qualified cases from factIcd data based on the ICD code searching criteria and number of ICD code per patients in the inout factIcd dataset.
### Condition Era
Get the condition era
### Report a bug at 
See the `GitHub issues page` (https://github.com/DHLab-CGU/emr/issues) to see open issues and feature requests. 
