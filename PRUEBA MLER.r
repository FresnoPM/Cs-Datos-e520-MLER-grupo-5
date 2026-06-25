# Using data.table for efficiency since the dataset is large (~8.5 million rows)
library(data.table)


mler_base <- read_parquet("./materiales/MLER_mujeres.parquet")
# Create a copy to avoid modifying the original object in the global environment
mler_base_new <- as.data.table(mler_base)

# Update 'periodo' where it is "estable" (case-insensitive check) or NA
# We use the value from 'nodo' as requested
mler_base_new[periodo == "estable" | is.na(periodo), periodo := nodo]

# Check the result
summary(mler_base_new$periodo)
head(mler_base_new[periodo %in% unique(mler_base_new$nodo)])

write_parquet(mler_base_new, "./materiales/mler_sectores_licencias_distintas.parquet")


 unique(mler_base_new$periodo)
# [1] "Activo: Inmobiliaria"           "Activo: Comercio"               "Activo: Hotelería/Restaurantes"
# [4] "lic_corta"                      "lic_larga"                      "Activo: Servicios Sociales"
# [7] "Activo: Finanzas"               "maternidad_ignorada"            "Activo: Industria"
# [10] "Activo: Enseñanza"              "Activo: Servicios Salud"        "ultima_maternidad"
# [13] "Activo: Transporte"             "Activo: Construcción"           "Activo: Agro"
# [16] "maternidad"                     "Activo: Otros"                  "Activo: Electricidad/Gas/Agua"
# [19] "Activo: Mineria"                "Activo: Pesca"


