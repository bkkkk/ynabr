format_yen <- function(x) {
  as.numeric(gsub(",", "", x))
}

format_date <- function(x) {
  format(x, "%m/%d/%Y")
}

get_split_string <- function() {
  "◆ご利用明細内訳（差額分・お振替未済分）"
}

#' @importFrom glue glue
build_journey <- function(in_station, out_station) {
  as.character(glue("{in_station} => {out_station}"))
}
