
library(readxl)
library(purrr)

path_descriptores <- "./materiales/anexo_estadistico_y_descriptores.xlsx"

descriptores <- path %>%
    excel_sheets() %>%
    set_names() %>%
    map(~ read_excel(path = path_descriptores, sheet = .x))

library(janitor)
desc_letra <- descriptores[["DESC_LETRA"]] %>%
    select(1,3,4) %>%
    row_to_names(row_number = 2) %>%
    rename( letra = 1, descripcion = 2, sexualizacion = 3)

desc_r32 <- descriptores[["DESC_R32"]] %>%
    row_to_names(row_number = 2) %>%
    rename(descripcion = 2)

desc_r34 <- descriptores[["DESC_R34"]] %>%
    row_to_names(row_number = 2) %>%
    rename(descripcion = 2)

# saveRDS(desc_letra, file = "./materiales/desc_letra.rds")
# saveRDS(desc_r32, file = "./materiales/desc_r32.rds")
# saveRDS(desc_r34, file = "./materiales/desc_r34.rds")

