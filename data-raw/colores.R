morant <- readxl::read_excel("data-raw/morant.xlsx")

colores <- list(
  morant=morant
)
usethis::use_data(colores, overwrite = TRUE)
