context("wrangling")

#selectCases
sc <- selectCases(dxDataFile = sampleDxFile,
                  ID, ICD, Date,
                  icdVerColName = NULL,
                  groupDataType = ccslvl2,
                  icd10usingDate = "2015/10/01",
                  caseCondition = "Diseases of the urinary system",
                  caseCount = 1)

test_that("Select Cases",
          { expect_equal(sum(sc$selectedCase == "Selected"), 24L)})
test_that("Select Cases",
          { expect_output(str(sc), "38 obs")})
test_that("Select Cases",
          { expect_output(str(sc), "8 variables")})


#conditionEra
df6 <- data.table("ID" = "A0", "CCS_LVL_3_LABEL" = "Chronic kidney disease", "era" = 1L,  "firstCaseDate" = as.Date("2009-07-25"), "endCaseDate" = as.Date("2009-07-25"), "count" = 1, "period" = as.difftime(0, units = "days"))
df6_t <- data.table("ID" = "D8", "CCS_LVL_3_LABEL" = "Schizophrenia  other psychotic disorders", "era" = 1L,  "firstCaseDate" = as.Date("2007-01-30"), "endCaseDate" = as.Date("2007-01-30"), "count" = 1, "period" = as.difftime(0, units = "days"))

era <- getConditionEra(sampleDxFile, ID, ICD, Date, icdVerColName = NULL, "2015-10-01",
                       groupDataType = CCSlvl3)
test_that("Condition Era",
          { expect_equal(era[1], df6)})
test_that("Condition Era",
          { expect_equal(era[98], df6_t)})
test_that("Condition Era",
          { expect_output(str(era), "98 obs")})
test_that("Condition Era",
          { expect_output(str(era), "7 variables")})


#period
record <- getEligiblePeriod(sampleDxFile, ID, Date)

test_that("Period",
          { expect_equal(record$endRecordDate[1] - record$firstRecordDate[1], as.difftime(7028, units = "days"))})
test_that("Period",
          { expect_output(str(record), "38 obs")})
test_that("Period",
          { expect_output(str(record), "3 variables")})


#split
colnames(record)[2] <- "indexDate"
record$endRecordDate <- NULL
splitedData <- splitDataByDate(sampleDxFile, ID, ICD, Date, record, gap = 30)

df7 <- data.table("ID" = "A0", "ICD" = "5856", "Date" = as.Date("2009-07-25"), "indexDate" = as.Date("2009-07-25"), "timeTag" = "A", "window" = 1)
df7_t <- data.table("ID" = "D8", "ICD" = "C8582", "Date" = as.Date("2025-09-10"), "indexDate" = as.Date("2007-01-30"), "timeTag" = "A", "window" = 227)

test_that("Split",
          { expect_equal(splitedData[1], df7)})
test_that("Split",
          { expect_equal(splitedData[300], df7_t)})
test_that("Split",
          { expect_output(str(splitedData), "300 obs")})
test_that("Split",
          { expect_output(str(splitedData), "6 variables")})


#wideFormat
auto_df4 <- icdDxToComorbid(sampleDxFile, ID, ICD, Date, icdVerColName = NULL, "2015-10-01", charlson)

groupedDataWide <- groupedDataLongToWide(auto_df4$groupedDT,
                                         idColName = ID,
                                         categoryColName = Comorbidity,
                                         dateColName = Date)

test_that("Wide Format",
          {expect_output(str(groupedDataWide), "38 obs")})
test_that("Wide Format",
          {expect_output(str(groupedDataWide), "11 variables")})
