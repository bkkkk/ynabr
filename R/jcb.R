split_data <- function(.data) {
  .data %>%
    mutate(divider = if_else(id == "◆ご利用明細内訳（差額分・お振替未済分）", "split", NA_character_)) %>%
    fill(divider, .direction = "down") %>%
    mutate(divider = coalesce(divider, "first")) %>%
    split(.$divider)
}

clean_up_and_merge <- function(.data) {
  first <- .data$first %>%
    select(-divider, -ddd, -idd, -id)

  second <- .data$split %>%
    filter(!id %in% c("◆ご利用明細内訳（差額分・お振替未済分）", "ご利用者")) %>%
    select(-divider, -ddd, -idd, -id)

  bind_rows(first, second) %>%
    mutate(
      date = ymd(date),
      amount = as.numeric(gsub(",", "", amount))
    ) %>%
    transmute(
      Date = format(date, "%m/%d/%Y"),
      Payee = payee,
      Amount = -amount,
      Memo = memo
    )
}

create_clean_jcb_data <- function(path) {
  filename <- basename(path)
  dirname <- dirname(path)

  read_csv(path,
           skip = 6,
           locale = locale(encoding = "cp932"),
           col_names = c("id", "date", "payee", "amount", "memo", "idd", "ddd")) %>%
    split_data() %>%
    clean_up_and_merge() %>%
    write_csv(file.path(dirname, str_replace(filename, ".csv", "_clean.csv")))
}
