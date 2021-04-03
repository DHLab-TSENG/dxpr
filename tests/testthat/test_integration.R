context("integration")
#icdStD
df1 <- data.table("ICD" = "A0.11", "count" = 20, "IcdVersionInFile" = "ICD 10", "WrongType" = "Wrong format", "Suggestion" = "")
df1_t <- data.table("ICD" = "7552", "count" = 2, "IcdVersionInFile" = "ICD 9", "WrongType" = "Wrong format", "Suggestion" = "75529")


auto_df1 <- icdDxShortToDecimal(sampleDxFile, ICD, Date, icd10usingDate = "2015/10/01")

test_that("ICD short -> decimal Error",
          { expect_equal(auto_df1$Error[1], df1)})
test_that("ICD short -> decimal Error",
          { expect_equal(auto_df1$Error[16], df1_t)})
test_that("ICD short -> decimal Error",
          {expect_output(str(auto_df1$Error), "16 obs")})

test_that("ICD short -> decimal Type",
          { expect_equal(as.character(auto_df1$ICD[1]), "Z99.2")})


#icdDtS
df2 <- data.table("ICD" = "A0.11", "count" = 20, "IcdVersionInFile" = "ICD 10", "WrongType" = "Wrong format", "Suggestion" = "")
df2_t <- data.table("ICD" = "7552", "count" = 2, "IcdVersionInFile" = "ICD 9", "WrongType" = "Wrong format", "Suggestion" = "75529")

auto_df2 <- icdDxDecimalToShort(sampleDxFile, ICD, Date, icd10usingDate = "2015/10/01")

test_that("ICD decimal -> short Error",
          { expect_equal(auto_df2$Error[1], df2)})
test_that("ICD short -> decimal Error",
          { expect_equal(auto_df2$Error[16], df2_t)})
test_that("ICD decimal -> short Error",
          {expect_output(str(auto_df2$Error), "16 obs")})

test_that("ICD decimal -> short Type",
          { expect_equal(as.character(auto_df2$ICD[1]), "Z992")})


#icdCCS
df3 <- data.table("ID" = "A0", "CCS_CATEGORY_DESCRIPTION" = "Chronic kidney disease", "firstCaseDate" = as.Date("2009-07-25"), "endCaseDate" = as.Date("2013-12-20"), "count" = 5, "period" = as.difftime(1609, units = "days"))
df3_t <- data.table("ID" = "D8", "CCS_CATEGORY_DESCRIPTION" = "Schizophrenia and other psychotic disorders", "firstCaseDate" = as.Date("2007-01-30"), "endCaseDate" = as.Date("2007-01-30"), "count" = 1, "period" = as.difftime(0, units = "days"))

auto_df3 <- icdDxToCCS(sampleDxFile, ID, ICD, Date, icd10usingDate =  "2015-10-01", isDescription = TRUE)

test_that("ICD CCS",
          { expect_equal(auto_df3$summarised_groupedDT[1], df3)})
test_that("ICD CCS",
          { expect_equal(auto_df3$summarised_groupedDT[97], df3_t)})

test_that("ICD CCS",
          { expect_output(str(auto_df3$summarised_groupedDT), "97 obs")})
test_that("ICD CCS",
          { expect_output(str(auto_df3$groupedDT), "300 obs")})
test_that("ICD CCS",
          { expect_output(str(auto_df3$groupedDT), "5 variables")})


#icdComorbid
df4 <- data.table("ID" = "A0", "Comorbidity" = "RD", "firstCaseDate" = as.Date("2009-07-25"), "endCaseDate" = as.Date("2013-12-20"), "count" = 5, "period" = as.difftime(1609, units = "days"))
df4_t <- data.table("ID" = "D8", "Comorbidity" = "PARA", "firstCaseDate" = as.Date("2010-09-27"), "endCaseDate" = as.Date("2010-09-27"), "count" = 1, "period" = as.difftime(0, units = "days"))

auto_df4 <- icdDxToComorbid(sampleDxFile, ID, ICD, Date, icdVerColName = NULL, "2015-10-01", charlson)

test_that("ICD Comorbid",
          { expect_equal(auto_df4$summarised_groupedDT[1], df4)})
test_that("ICD Comorbid",
          { expect_equal(auto_df4$summarised_groupedDT[51], df4_t)})
test_that("ICD Comorbid",
          { expect_output(str(auto_df4$summarised_groupedDT), "51 obs")})
test_that("ICD Comorbid",
          { expect_output(str(auto_df4$groupedDT), "300 obs")})
test_that("ICD Comorbid",
          { expect_output(str(auto_df4$groupedDT), "5 variables")})

#icdPheWAS
df5 <- data.table("ID" = "A2", "PheCode" = "585.31", "firstCaseDate" = as.Date("2015-08-12"), "endCaseDate" = as.Date("2020-05-22"), "count" = 2, "period" = as.difftime(1745, units = "days"))
df5_t <- data.table("ID" = "C2", "PheCode" = "300.3", "firstCaseDate" = as.Date("2020-03-24"), "endCaseDate" = as.Date("2020-03-24"), "count" = 1, "period" = as.difftime(0, units = "days"))

auto_df5 <- icdDxToPheWAS(sampleDxFile, ID, ICD, Date, icdVerColName = NULL, "2015-10-01", FALSE)

test_that("ICD PheWAS",
          { expect_equal(auto_df5$summarised_groupedDT[1], df5)})
test_that("ICD PheWAS",
          { expect_equal(auto_df5$summarised_groupedDT[153], df5_t)})
test_that("ICD PheWAS",
          { expect_output(str(auto_df5$summarised_groupedDT), "153 obs")})
test_that("ICD PheWAS",
          { expect_output(str(auto_df5$groupedDT), "304 obs")})
test_that("ICD PheWAS",
          { expect_output(str(auto_df5$groupedDT), "5 variables")})
