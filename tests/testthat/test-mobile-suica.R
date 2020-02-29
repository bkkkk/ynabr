library(tidyverse)

test_that("Mobile suica data pull is working", {
  test_data <- tribble(
    ~date, ~in_type, ~in_station, ~out_type, ~out_station, ~balance, ~amount,
    "01/11", "Out", "A", "In", "B", 10, 10
  )
  expected <- tribble(
    ~Date, ~Payee, ~Memo, ~Amount,
    "01/11/2020", "", "A => B", 10.0
  )
  result <- parse_mobile_suica_data(test_data, 2020)
  expect_identical(result, expected)
})
