
#' Read stations meteo
#'
#' @param file
#'
#' @return a tibble
#' @export
#'
#' @examples
#' read_station_meteo(
#' file = "https://data.toulouse-metropole.fr/explore/dataset/1-station-meteopole/download?format=csv&timezone=Europe/Berlin&use_labels_for_header=true"
#' )
#'
read_station_meteo <- function(file) {
  readr::read_csv2(file = file) %>%
    tricky::set_standard_names() %>%
    dplyr::mutate(
      temperature = temperature_partie_entiere - 50 + temperature_partie_decimale / 10,
      annee = annee + 2019,
      date = lubridate::ymd(
        paste0(
          annee,
          stringr::str_pad(
            mois,
            width = 2,
            side = "left",
            pad = "0"),
          stringr::str_pad(
            jour,
            width = 2,
            side = "left",
            pad = "0")
        )
      )
    )
}


#' plot_stations_meteo
#'
#' @param file file location
#'
#' @return a graph
#' @export
#'
#' @examples
#' \dontrun{
#'plot_stations_meteo(file = "https://data.toulouse-metropole.fr/explore/dataset/1-station-meteopole/download?format=csv&timezone=Europe/Berlin&use_labels_for_header=true")
#' }
#'
plot_stations_meteo <- function(file) {
  read_station_meteo(
    file = file
  ) %>%
  ggplot2::ggplot() +
    ggplot2::geom_tile(
      mapping = ggplot2::aes(y = date, x = numero_de_message, fill = temperature),
      color = "white"
    ) +
    ggplot2::scale_fill_gradientn(
      colours =
        wesanderson::wes_palette("Zissou1", 21, type = "continuous")
    ) +
    ggthemes::theme_fivethirtyeight() +
    ggplot2::coord_equal()
}



