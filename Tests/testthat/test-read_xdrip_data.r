library(testthat)

test_that("Data wrangling process works correctly", {
  
  # Assuming the function 'data_wrangling' returns your dataframe
  dat_out <- read_xdrip_data()
  
  expect_is(dat_out, "data.frame")
  
  # Check that dataframe has the correct columns
  expected_cols <- c("DAY", "TIME", "UDT_CGMS", "BG_LEVEL", "REMARK", "datetim", "glucose", "remark_factor", "delta_time_remark")
  #expect_equal(colnames(dat_out), expected_cols)
  
  # Check that certain columns are of the correct type
  expect_is(dat_out$DAY, "Date")
  expect_is(dat_out$datetim, "POSIXct")
  expect_is(dat_out$glucose, "numeric")
  expect_is(dat_out$delta_time_remark, "numeric")
  
  # More specific tests can be added based on your needs
  # For example, checking that numeric columns have reasonable ranges, no NA values, etc.
})