#' Read and cleanup Mobile SUICA data
#'
#' @param path path to CSV file to cleanup
#' @param year Year
#' @param out_file (optional) name of output file
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom readr write_csv read_tsv
process_mobile_suica <- function(path, year) {
  col_names <- c("date", "in_type", "in_station", "out_type", "out_station", "balance", "amount")

  dir_path <- dirname(path)

  path %>%
    read_csv(col_names = col_names) %>%
    parse_mobile_suica_data(year = year) %>%
    write_csv(glue("{dir_path}/mobile_suica_clean.csv"))
}

#' Parse Mobile SUICA data
#'
#' @param .data dataframe of data imported from Mobile SUICA
#' @param year year of data
#'
#' @return clean dataframe
#'
#' @export
#'
#' @importFrom lubridate today ymd
#' @importFrom dplyr rename mutate filter transmute
#' @importFrom lubridate ymd
#' @importFrom glue glue
parse_mobile_suica_data <- function(.data, year, purchase_label = "物販") {
  stopifnot(length(.data) == 7)
  stopifnot(nrow(.data) > 0)

  .data %>%
    mutate(
      date = ymd(glue::glue('{year}/{date}'))
    ) %>%
    filter(date <= today()) %>%
    add_mobile_suica_memo() %>%
    transmute(
      Date = format_date(date),
      Payee = "",
      Memo,
      Amount = format_yen(amount)
    )
}

add_mobile_suica_memo <- function(.data, purchase_label = "物販") {
  .data %>%
    mutate(Memo = if_else(in_type == purchase_label, purchase_label, build_journey(in_station, out_station)))
}
