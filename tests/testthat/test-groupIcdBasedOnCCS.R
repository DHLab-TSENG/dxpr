context("Group")

test_that("Group ICD9 and ICD10 based on CCS category",{
  DxDataFile <- data.frame(ID=c("A","A","A"),
                           ICD=c("6929","V433","I350"),
                           Date=as.Date(c("2013-03-31","2013-01-29","2016-03-10")),
                           stringsAsFactors = F)
  expect_output(groupIcdBasedOnCCS (DxDataFile, ID, ICD, Date, "2016-01-01", T))
  })
