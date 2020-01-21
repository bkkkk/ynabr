#' Parse Mobile SUICA data
#'
#' @param .data dataframe of data imported from Mobile SUICA
#' @param year year of data
#'
#' @return clean dataframe
#' @export
#
#' @importFrom lubridate today
#' @importFrom dplyr rename mutate filter transmute
parse_mobile_suica_data <- function(.data, year) {
  .data %>%
    rename(
      date = `月/日`,
      in_type = 種別...2,
      in_station = 利用場所...3,
      out_type = 種別...4,
      out_station = 利用場所...5,
      balance = 残額,
      amount = 差額
    ) %>%
    mutate(
      date = ymd(glue::glue('{year}/{date}'))
    ) %>%
    filter(date <= today()) %>%
    transmute(
      Date = format_date(date),
      Payee = "",
      Memo = format_journey_memo(in_type, in_station, out_station),
      Amount = format_yen(amount)
    )
}

format_journey_memo <- function(in_type, in_station, out_station) {
  build_journey <- function(in_station, out_station) {
    as.character(glue::glue("{in_station} => {out_station}"))
  }
  purchase_label <- "物販"
  if_else(in_type == purchase_label, purchase_label, build_journey(in_station, out_station))
}
