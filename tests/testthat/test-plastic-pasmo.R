test_that("Check IC Reader memo generation works", {
  test <- tribble(~in_station, ~out_station,
                  NA, NA,
                  "IN", "OUT")
  result <- add_pasmo_memo(test)
  expected <- test %>% add_column(Memo = c("物販", "IN => OUT"))
  expect_equal(result, expected)
})

test_that("Check that data parsing works", {
  mock_data <- tribble(~id, ~date, ~amount, ~remaining, ~in_station, ~out_station, ~train_company,
                       100, "2019-01-01", 1000, 10000, "IN", "OUT", "COMPANY",
                       100, "2019-01-01", 1000, 10000, NA, NA, NA)

  expected <- tribble(~Date, ~Amount, ~Payee, ~Memo,
                      "2019-01-01", 1000, "COMPANY", "IN => OUT",
                      "2019-01-01", 1000, NA, "TEST")

  result <- parse_plastic_pasmo_card(mock_data, "TEST")

  expect_equal(result, expected)
})
