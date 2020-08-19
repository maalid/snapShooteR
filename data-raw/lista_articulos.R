## code to prepare `lista_articulos` dataset goes here
lista_articulos <- readr::read_csv("Lista_Ejemplo.csv")
usethis::use_data(lista_articulos, overwrite = TRUE, internal = TRUE)
