context("Group")

test_that("Group ICD9 and ICD10 based on CCS category",
          { expect_equal(length(IcdDxToCCS(sampleDxFile, ID, ICD, Date, "2016-01-01", FALSE)), 2)})
