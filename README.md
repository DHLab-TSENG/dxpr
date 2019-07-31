
emr
---

I. Introduction
===============

The proposed open-source emr package is a software tool aimed at expediting an integrated analysis of electronic health records (EHRs). The emr package provides mechanisms to integrate, analyze, and visualize clinical data, including diagnosis and procedure records.

Feature
-------

-   **Code standardization** Transform codes into uniform format before the integration process.
-   **Data integration** Code classification and data integration.
-   **Exploratory data analysis (EDA) preparation** Transform data format which is fit to others analytical and plotting packages.
-   **Visualization** Provide overviews for diagnoses standardization and data integration.

Geting start
------------

-   Diagnostic part
    English: <https://dhlab-cgu.github.io/emr/articles/Eng_Diagnosis.html>
    Chinese: <https://dhlab-cgu.github.io/emr/articles/Chi_Diagnosis.html>
-   Procedure part
    English: <https://dhlab-cgu.github.io/emr/articles/Eng_Procedure.html>
    Chinese: <https://dhlab-cgu.github.io/emr/articles/Chi_Procedure.html>

Development version
-------------------

``` r
# install.packages("devtools")
devtools::install_github("DHLab-CGU/emr")
#> Skipping install of 'emr' from a github remote, the SHA1 (557d1197) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

Overview
--------

<img src="https://github.com/DHLab-CGU/dhlab-cgu.github.io/blob/master/emr/overview.jpg?raw=true" style="display:block; margin:auto; width:100%;">

Usage
-----

``` r
library(emr)  
 
head(sampleDxFile)  
#>     ID  ICD       Date
#> 1:  A2 Z992 2020-05-22
#> 2:  A5 Z992 2020-01-24
#> 3:  A8 Z992 2015-10-27
#> 4: A13 Z992 2020-04-26
#> 5: A13 Z992 2025-02-02
#> 6: A15 Z992 2023-05-12

short <- IcdDxDecimalToShort(sampleDxFile, ICD, Date, "2015/10/01")
head(short$ICD)
#>     ICD
#> 1: Z992
#> 2: Z992
#> 3: Z992
#> 4: Z992
#> 5: Z992
#> 6: Z992

head(short$Error)
#>       ICD count IcdVersionInFile     WrongType Suggestion
#> 1:  A0.11    20           ICD 10  Wrong format           
#> 2:  V27.0    18           ICD 10 Wrong version           
#> 3:   E114     8           ICD 10  Wrong format           
#> 4: A01.05     8            ICD 9 Wrong version           
#> 5:  42761     7           ICD 10 Wrong version           
#> 6:  Z9.90     6           ICD 10  Wrong format

ELIX <- IcdDxToComorbid(sampleDxFile, ID, ICD, Date, "2015/10/01", elix)
head(ELIX$groupedDT)
#>    Short  ID  ICD       Date   Description
#> 1:  Z992  A2 Z992 2020-05-22 Renal failure
#> 2:  Z992  A5 Z992 2020-01-24 Renal failure
#> 3:  Z992  A8 Z992 2015-10-27 Renal failure
#> 4:  Z992 A13 Z992 2020-04-26 Renal failure
#> 5:  Z992 A13 Z992 2025-02-02 Renal failure
#> 6:  Z992 A15 Z992 2023-05-12 Renal failure

head(ELIX$summarised_groupedDT)
#>     ID   Description firstCaseDate endCaseDate count    period
#> 1:  A0 Renal failure    2009-07-25  2013-12-20     5 1609 days
#> 2:  A1 Renal failure    2006-11-29  2014-09-24     5 2856 days
#> 3: A10 Renal failure    2007-11-04  2012-07-30     5 1730 days
#> 4: A11 Renal failure    2008-03-09  2011-09-03     5 1273 days
#> 5: A12 Renal failure    2006-05-14  2015-06-29     5 3333 days
#> 6: A13 Renal failure    2006-04-29  2025-02-02     5 6854 days

Era <- getConditionEra(sampleDxFile, ID, ICD, Date, "2015/10/01")       
head(Era)                      
#>     ID CCS_CATEGORY_DESCRIPTION firstCaseDate endCaseDate count era
#> 1:  A0   Chronic kidney disease    2009-07-25  2013-12-20     5   5
#> 2:  A1   Chronic kidney disease    2006-11-29  2014-09-24     5   5
#> 3: A10   Chronic kidney disease    2007-11-04  2012-07-30     5   4
#> 4: A11   Chronic kidney disease    2008-03-09  2011-09-03     5   4
#> 5: A12   Chronic kidney disease    2006-05-14  2015-06-29     5   5
#> 6: A13   Chronic kidney disease    2006-04-29  2025-02-02     5   5
#>       period
#> 1: 1609 days
#> 2: 2856 days
#> 3: 1730 days
#> 4: 1273 days
#> 5: 3333 days
#> 6: 6854 days

groupedData_Wide <- groupedDataLongToWide(sampleDxFile, ID, ICD, Date, "2015/10/01", groupDataType = Charlson)
head(groupedData_Wide[,1:4])
#>    ID Cancer Cerebrovascular Disease Chronic Pulmonary Disease
#> 1  A0  FALSE                   FALSE                     FALSE
#> 2  A1  FALSE                   FALSE                     FALSE
#> 3 A10  FALSE                   FALSE                     FALSE
#> 4 A11  FALSE                   FALSE                     FALSE
#> 5 A12  FALSE                   FALSE                     FALSE
#> 6 A13  FALSE                   FALSE                     FALSE

plot_errorICD(short$Error)  
#> $graph
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

    #> 
    #> $ICD
    #>        ICD count CumCountPerc IcdVersionInFile     WrongType Suggestion
    #>  1:  A0.11    20       18.35%           ICD 10  Wrong format           
    #>  2:  V27.0    18       34.86%           ICD 10 Wrong version           
    #>  3:   E114     8        42.2%           ICD 10  Wrong format           
    #>  4: A01.05     8       49.54%            ICD 9 Wrong version           
    #>  5:  42761     7       55.96%           ICD 10 Wrong version           
    #>  6:  Z9.90     6       61.47%           ICD 10  Wrong format           
    #>  7:    F42     6       66.97%           ICD 10  Wrong format           
    #>  8:  V24.1     6       72.48%           ICD 10 Wrong version           
    #>  9:  A0105     5       77.06%            ICD 9 Wrong version           
    #> 10:    001     5       81.65%            ICD 9  Wrong format       0019
    #> 11: Others    20         100%            ICD 9  Wrong format

    plot_groupedData(groupedData_Wide)
    #> $graph

![](README_files/figure-markdown_github/unnamed-chunk-3-2.png)

    #> 
    #> $sigCate
    #>                              DiagnosticCategory  N Percentage
    #>  1:                               Renal Disease 24     63.16%
    #>  2:                                      Cancer 10     26.32%
    #>  3:                 Diabetes with complications  7     18.42%
    #>  4: Connective Tissue Disease-Rheumatic Disease  3      7.89%
    #>  5:                  Periphral Vascular Disease  2      5.26%
    #>  6:                     Cerebrovascular Disease  1      2.63%
    #>  7:                   Chronic Pulmonary Disease  1      2.63%
    #>  8:            Moderate or Severe Liver Disease  1      2.63%
    #>  9:                   Paraplegia and Hemiplegia  1      2.63%
    #> 10:                        Peptic Ulcer Disease  1      2.63%

Getting help
------------

See the `GitHub issues page` (<https://github.com/DHLab-CGU/emr/issues>) to see open issues and feature requests.
