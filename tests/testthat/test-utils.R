test_that("Format date correctly", {
  result <- format_date(ymd("2019-11-01"))
  expect_equal(result, "11/01/2019")
})

test_that("Clean up text based numbers", {
  result <- format_yen("1,000")
  expect_equal(result, 1000)
})

test_that("Test that basic journey builder works", {
  result <- build_journey("in", "out")
  expect_equal(result, "in => out")
})
