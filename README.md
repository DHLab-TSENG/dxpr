## emr
# I. Introduction
A tool of `Electric Medical Record` for grouping with ICD-9 and ICD-10 codes by `ccs, comorbidities and phecodes`, and calculating `condition era`. 
There are some information about patients' diagnoses data: members' ID, diagnosis ICD-9/ICD-10 codes, and date of service started, etc...
This tool can be used to group the ICD's multitude of codes into a smaller number of clinically meaningful categories (CCS, phecode, comorbidity, and even grouping rule by your standards!). 

## Get started
- English introduction: https://dhlab-cgu.github.io/emr/articles/emr.html
- Chinese introduction: https://dhlab-cgu.github.io/emr/articles/ChineseVersion_Diagnosis.html

## Feature
- convert ICD code's format: Short <-> Decimal
- Get the Clinical Classifications Software (CCS) categories and description for ICD-9 and ICD-10 codes on diagnoses
- Get the Phecode or phenotype (phecode description) of ICD-9 diagnosis codes
- Get the categories for ICD-9 and ICD-10 codes on diagnoses, the grouping  rules are based on your standard
- Grouping comorbid method measures (AHRQ, Charlson and Elixhauser Comorbidity) infers whether to use ICD-9 or ICD-10 codes
- Get the qualified cases which based on the ICD code searching criteria and number of ICD code per patients in the inout factIcd dataset
- Get the condition era

## Install
```r
install.packages("devtools")
# Install development version from GitHub
devtools::install_github("DHLab-CGU/emr")
library(emr)
```
### Generate ICD-CM code Decimal Format for conversion function 
ICD codes have two forms: decimal`E950.7` and short`E9507` format, first at all, unifing the ICD codes format.
There are two function to generate ICD codes' decimal format.

### Convert ICD-CM code Format
There are two functions to let user convert ICD code format  for the following function for grouping methods.
Second, according to the using format of function, these conversion function can be used to convert ICD codes between types.

## II. Grouping ICD-CM codes
classifying ICD-9-CM and ICD-10-CM diagnoses codes into clinically meaningful categories, which can be used for aggregate statistical reporting of a variety of types.

### A. Clinical Classifications Software (CCS)
It is based on the CCS for ICD-9-CM and attempts to map ICD-10-CM/PCS codes into the same categories. Get the CCS categories and description by ICD-9/ICD-10 code.
#### a. CCS single-level category
ICD-9-CM and ICD-10-CM code contains same 260 CCS categories.
#### b. CCS multi-level
ICD-9-CM has four levels

ICD-10-CM has two levels

### B. Phecode
This can be used to group Phecode or description of phecode (phenotype) by ICD-9 diagnosis codes in clinical diagnostic data.

### C. Creating a grouping standards
User can identify the rule of grouping standards, and return matched ICD codes. For example, there is a ICD data frame icdFile whether is matched these group: Hypotension and Hypertension or not.

### D. Comorbidities
The comorbidities measures from different sources are provided as lists. There are three comorbidities sources: AHRQ, Charlson, and Elixhauser Comorbidity.

#### a. AHRQ comorbidity classification
AHRQ comorbidity measure dataset is based on Elixhauser Comorbidity Index

#### b. Charlson comorbidity classification
Charlson comorbidity measure dataframe is based on Quan's translations of the Charlson Comorbidity Index

#### c. Elixhauser comorbidity classification
Elixhauser comorbidity measure data table icd9_elix, icd10_elix.

The Elixhauser Comorbidity Software is one in a family of databases and software tools developed as part of the Healthcare Cost and Utilization Project (HCUP).

## III. Selection Cases
This can be used to select qualified cases from factIcd data based on the ICD code searching criteria and number of ICD code per patients in the inout factIcd dataset.
## IV. Condition Era
A Condition Era is defined as a span of time when the member is assumed to have a given condition.

Condition Eras are periods of Condition Occurrence. Combining individual Condition Occurrences into a single Condition Era based on ICD code in clinical diagnostic data. Condition Eras are built with a Persistence Window,deafault is 30 days, meaning, if no occurence of the same member id happens within 30 days of any one occurrence, it will be considered the end date of the last condition occurrence.
## Report a bug at 
See the `GitHub issues page` (https://github.com/DHLab-CGU/emr/issues) to see open issues and feature requests. 


