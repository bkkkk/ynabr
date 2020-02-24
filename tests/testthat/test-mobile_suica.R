library(tidyverse)
test_that("Yen formatting works with comma separated", {
  result <- format_yen("1,000")
  expect_equal(result, 1000)
})

test_that("Yen formatting works", {
  result <- format_yen("1000")
  expect_equal(result, 1000)
})

test_that("Mobile suica data pull is working", {
  test_data <- tribble(
    ~`月/日`, ~種別...2, ~利用場所...3, ~種別...4, ~利用場所...5, ~残額, ~差額,
    "01/11", "Out", "A", "In", "B", 10, 10
  )
  expected <- tribble(
    ~Date, ~Payee, ~Memo, ~Amount,
    "01/11/2020", "", "A => B", 10.0
  )
  result <- parse_mobile_suica_data(test_data, 2020)
  expect_identical(result, expected)
})
