
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

# I. Code standardization
short <- IcdDxDecimalToShort(sampleDxFile, ICD, Date, "2015/10/01")
head(short$ICD)
#>     ICD
#> 1: Z992
#> 2: Z992
#> 3: Z992
#> 4: Z992
#> 5: Z992
#> 6: Z992

tail(short$Error)
#>       ICD count IcdVersionInFile     WrongType Suggestion
#> 1:  75.52     4            ICD 9  Wrong format           
#> 2:  E03.0     4            ICD 9 Wrong version           
#> 3:    650     4           ICD 10 Wrong version           
#> 4: 123.45     3           ICD 10  Wrong format           
#> 5:  755.2     3            ICD 9  Wrong format     755.29
#> 6:   7552     2            ICD 9  Wrong format      75529

# II. Data integration
ELIX <- IcdDxToComorbid(sampleDxFile, ID, ICD, Date, "2015/10/01", elix)
head(ELIX$groupedDT)
#>    Short  ID  ICD       Date Comorbidity
#> 1:  Z992  A2 Z992 2020-05-22    RENLFAIL
#> 2:  Z992  A5 Z992 2020-01-24    RENLFAIL
#> 3:  Z992  A8 Z992 2015-10-27    RENLFAIL
#> 4:  Z992 A13 Z992 2020-04-26    RENLFAIL
#> 5:  Z992 A13 Z992 2025-02-02    RENLFAIL
#> 6:  Z992 A15 Z992 2023-05-12    RENLFAIL

head(ELIX$summarised_groupedDT)
#>     ID Comorbidity firstCaseDate endCaseDate count    period
#> 1:  A0    RENLFAIL    2009-07-25  2013-12-20     5 1609 days
#> 2:  A1    RENLFAIL    2006-11-29  2014-09-24     5 2856 days
#> 3: A10    RENLFAIL    2007-11-04  2012-07-30     5 1730 days
#> 4: A11    RENLFAIL    2008-03-09  2011-09-03     5 1273 days
#> 5: A12    RENLFAIL    2006-05-14  2015-06-29     5 3333 days
#> 6: A13    RENLFAIL    2006-04-29  2025-02-02     5 6854 days

# III. EDA preparation
groupedData_Wide <- groupedDataLongToWide(sampleDxFile, ID, ICD, Date, "2015/10/01", groupDataType = Charlson)
head(groupedData_Wide[,1:4])
#>    ID Cancer Cerebrovascular Disease Chronic Pulmonary Disease
#> 1  A0  FALSE                   FALSE                     FALSE
#> 2  A1  FALSE                   FALSE                     FALSE
#> 3 A10  FALSE                   FALSE                     FALSE
#> 4 A11  FALSE                   FALSE                     FALSE
#> 5 A12  FALSE                   FALSE                     FALSE
#> 6 A13  FALSE                   FALSE                     FALSE

# IV. Visualization
plot_errorICD <- plot_errorICD(short$Error)  
plot_groupedData <- plot_groupedData(groupedData_Wide)

plot_errorICD
#> $graph
```

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

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

    plot_groupedData
    #> $graph

![](README_files/figure-markdown_github/unnamed-chunk-2-2.png)

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
