test_that("multiplication works", {
  result <- format_date(ymd("2019-11-01"))
  expect_equal(result, "11/01/2019")
})
