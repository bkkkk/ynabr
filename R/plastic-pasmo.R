#' Read and cleanup Pasmo data from IC Reader
#'
#' @param path path to CSV file to cleanup
#' @param out_file (optional) name of output file
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom readr write_csv read_tsv
process_plastic_pasmo_card <- function(path, out_file = "pasmo_clean.csv") {
  col_names <- c("id", "date", "amount", "remaining", "in_station", "in_line", "train_company", "out_station", "out_line")

  dir_path <- dirname(path)

  path %>%
    read_csv(col_names = col_names) %>%
    parse_plastic_pasmo_card() %>%
    write_csv(glue("{dir_path}/pasmo_clean.csv"))
}

#' Clean up the raw data imported from IC Reader into YNAB format
#'
#' @param .data Data frame built from the output of IC Reader app
#' @param purchase_label (optional) generic item label
#'
#' @return Data frame containing Pasmo card transactions in YNAB format
parse_plastic_pasmo_card <- function(.data, purchase_label = "物販") {
  stopifnot(length(.data) == 9)
  stopifnot(nrow(.data) > 0)

  .data %>%
    select(-id, -remaining) %>%
    add_pasmo_memo(purchase_label) %>%
    select(
      Date = date,
      Payee = train_company,
      Amount = amount,
      Memo
    )
}

add_pasmo_memo <- function(.data, purchase_label = "物販") {
  .data %>%
    mutate(Memo = if_else(is.na(in_station), purchase_label, build_journey(in_station, out_station)))
}
