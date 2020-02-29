#' Import myJCB CSV data
#'
#' @param path path to csv file
#'
#' @return dataframe
#' @export
process_jcb <- function(path, encoding = "cp932") {
  filename <- basename(path)
  dirname <- dirname(path)

  dir_path <- dirname(path)

  read_csv(path,
           skip = 6,
           locale = locale(encoding = encoding),
           col_names = c("id", "date", "payee", "amount", "memo", "idd", "ddd")) %>%
    parse_jcb_data() %>%
    write_csv(glue("{dir_path}/{out_file}"))
}

#' Parse myJCB data
#'
#' @param .data dataframe with JCB data
#'
#' @return clean dataframe
#' @export
parse_jcb_data <- function(.data) {
  .data %>%
    split_data() %>%
    clean_up_and_merge()
}

split_data <- function(.data) {
  .data %>%
    mutate(divider = if_else(id == get_split_string(), "split", NA_character_)) %>%
    fill(divider, .direction = "down") %>%
    mutate(divider = coalesce(divider, "first")) %>%
    split(.$divider)
}

clean_up_and_merge <- function(.data) {
  first <- remove_extra_columns(.data$first)

  second <- .data$split %>%
    filter(!id %in% c(get_split_string(), "ご利用者")) %>%
    remove_extra_columns()

  bind_rows(first, second) %>%
    mutate(
      date = ymd(date),
      amount = as.numeric(gsub(",", "", amount))
    ) %>%
    transmute(
      Date = format_date(date),
      Payee = payee,
      Amount = -amount,
      Memo = memo
    )
}

remove_extra_columns <- function(.data) {
  select(.data, -divider, -ddd, -idd, -id)
}
