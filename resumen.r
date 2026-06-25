library(arrow)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(TraMineR)
library(ggseqplot)
library(ggplot2)
library(tidyverse) # incluye dplyr

transformar <- function(ds, cantidad = 0, debug = FALSE) {
    if(debug) inicio <- Sys.time()
    # -- Fase Arrow: reducir datos antes del collect ---------------------------
    if (cantidad != 0) {
        ids <- ds |>
            dplyr::distinct(id_trabajador) |>
            dplyr::collect() |>
            dplyr::pull(id_trabajador) |>
            sample(size = cantidad)
        ds <- ds |> dplyr::filter(id_trabajador %in% ids)
    }

    # Una fila por (id_trabajador, tiempo): mayor rem_tot_real; empates rotos arbitrariamente
    df <- ds |>
        dplyr::collect() |>
        dplyr::group_by(id_trabajador, tiempo) |>
        dplyr::slice_max(rem_tot_real, n = 1, with_ties = FALSE) |>
        dplyr::ungroup()
    # if(debug) message("Cantidad de líneas elimnadas: ", nrow(ds) - nrow(df))
    # -- Fase R: operaciones no soportadas por Arrow ---------------------------
    df_transformado <- df |>
        dplyr::arrange(id_trabajador, tiempo) |>
        dplyr::mutate(
            licencia = ifelse(
                rem_tot_real == 0 &
                    dplyr::lag(letra) == letra &
                    dplyr::lag(id_trabajador) == id_trabajador
                , 1
                , 0
            ),
            # Sector base a 1 dígito
            desc_letra = dplyr::case_when(
                letra == 1  ~ "Agro",
                letra == 2  ~ "Pesca",
                letra == 3  ~ "Mineria",
                letra == 4  ~ "Industria",
                letra == 5  ~ "Electricidad/Gas/Agua",
                letra == 6  ~ "Construcción",
                letra == 7  ~ "Comercio",
                letra == 8  ~ "Hotelería/Restaurantes",
                letra == 9  ~ "Transporte",
                letra == 10 ~ "Finanzas",
                letra == 11 ~ "Inmobiliaria",
                letra == 12 ~ "Enseñanza",
                letra == 13 ~ "Servicios Salud",
                letra == 14 ~ "Servicios Sociales",
                TRUE        ~ "Otros"
            ),
            # Si está en licencia, lo marcamos en el nodo
            nodo = dplyr::if_else(licencia == 1,
                                  paste0("Licencia: ", desc_letra),
                                  paste0("Activo: ", desc_letra))
        ) |>
        # agrega la columna duracion_letra para ver la duración de cada periodo de actividad o licencia en cada sector para cada id_trabajador
        dplyr::group_by(id_trabajador) |>
        dplyr::mutate(.run = cumsum(letra != dplyr::lag(letra, default = dplyr::first(letra)))) |>
        dplyr::add_count(.run, name = "duracion_letra") |>
        dplyr::select(-.run) |>

        dplyr::ungroup() |>
        dplyr::relocate(desc_letra, duracion_letra, .after = letra) |>
        dplyr::mutate(id = dplyr::row_number(), .before = 1)

    if (debug) {

        message("edad NA: ", sum(is.na(df_transformado$edad)))
        message("Duración procesamiento: ", difftime(Sys.time(), inicio, units = "mins"))
    }


    df_transformado
}

ds_original_muj <- open_dataset("./materiales/MLER_mujeres_INCOMPL.parquet")
df_transformado_muj <- transformar(ds_original_muj, debug = TRUE)
write_parquet(df_transformado_muj, "./materiales/MLER_mujeres.parquet")

############################


agregar_periodo <- function(df, edad_min = 20, edad_max = 34, meses_inicio = 12, meses_fin = 36) {
    dt <- as.data.table(df)
    setorder(dt, id_trabajador, tiempo)
    dt[, nodo_tipo := fcase(
        startsWith(nodo, "Activo:"),   "Activo",
        startsWith(nodo, "Licencia:"), "Licencia",
        default = "Otro"
    )]
    dt[, run_id := rleid(nodo_tipo), by = id_trabajador]
    dt[, run_length := .N, by = .(id_trabajador, run_id)]
    dt[, periodo := fcase(
        nodo_tipo == "Activo"   & run_length >= 12,          "estable",
        nodo_tipo == "Licencia" & run_length == 3,            "ultima_maternidad",
        nodo_tipo == "Licencia" & run_length %in% c(1L, 2L), "lic_corta",
        nodo_tipo == "Licencia" & run_length > 3,             "lic_larga",
        default = NA_character_
    )]
    # All "ultima_maternidad" spells except the last one per worker → "maternidad"
    last_mat_run <- dt[periodo == "ultima_maternidad",
                       .(last_run = max(run_id)),
                       by = id_trabajador]
    dt <- last_mat_run[dt, on = "id_trabajador"]
    dt[periodo == "ultima_maternidad" & run_id != last_run, periodo := "maternidad"]
    dt[, last_run := NULL]
    # Spells overlapping the first 12 or last 36 months of the panel → "maternidad_ignorada"
    fecha_min    <- min(dt$tiempo)
    fecha_max    <- max(dt$tiempo)
    corte_inicio <- fecha_min + months(meses_inicio)  # spell must start >= this
    corte_fin    <- fecha_max - months(meses_fin)     # spell must end   <= this
    spell_bounds <- dt[
        periodo %in% c("maternidad", "ultima_maternidad"),
        .(spell_start = min(tiempo), spell_end = max(tiempo)),
        by = .(id_trabajador, run_id)
    ]
    spell_bounds[, ignorada := spell_start < corte_inicio | spell_end > corte_fin]
    # dt <- spell_bounds[, .(id_trabajador, run_id, ignorada)][dt, on = .(id_trabajador, run_id)]
    # dt[ignorada == TRUE & periodo %in% c("maternidad", "ultima_maternidad"),
    #    periodo := "maternidad_ignorada"]
    # dt[, ignorada := NULL]
    # Spells where edad is outside [20, 34] → "maternidad_ignorada"
    spell_edad <- dt[
        periodo %in% c("maternidad", "ultima_maternidad"),
        .(min_edad = min(edad), max_edad = max(edad)),
        by = .(id_trabajador, run_id)
    ]
    spell_edad[, fuera_rango := min_edad < edad_min | max_edad > edad_max]
    dt <- spell_edad[, .(id_trabajador, run_id, fuera_rango)][dt, on = .(id_trabajador, run_id)]
    dt[fuera_rango == TRUE & periodo %in% c("maternidad", "ultima_maternidad"),
       periodo := "maternidad_ignorada"]
    dt[, fuera_rango := NULL]
    as_tibble(dt)
}

mler_base <- read_parquet("./materiales/MLER_mujeres.parquet")
mler_base <- agregar_periodo(mler_base)

mler_base_new <- as.data.table(mler_base)

# Update 'periodo' where it is "estable" (case-insensitive check) or NA
# We use the value from 'nodo' as requested
mler_base_new[periodo == "estable" | is.na(periodo), periodo := nodo]


#######################



reemplazar_fuera_con_pausa <- function(fila, relleno = "Fuera") { # recibe una fila a la vez
    no_relleno <- which(fila != relleno)
    if (length(no_relleno) < 2) return(fila)
    primer_activo <- min(no_relleno)
    ultimo_activo <- max(no_relleno)
    entre <- seq_along(fila) > primer_activo &
        seq_along(fila) < ultimo_activo &
        fila == relleno
    fila[entre] <- "Pausa"
    return(fila) # devuelve una fila que luego se intertara en la matriz
}
crear_secuencia <- function(ds,
                            muestra = 0,
                            edad_min = 15,
                            edad_max = 100,
                            debug = FALSE,
                            relleno = "Fuera",
                            tipo = "edad") {
    if(debug) inicio <- Sys.time()

    df_sectores <- ds %>%
        select(
            id_trabajador
            #, nodo
            , periodo
            , edad
            , tiempo
        ) %>%
        filter(
            edad <= edad_max
            & edad >= edad_min
            & !is.na(edad)
        ) %>%
        collect()
    if(tipo == "edad"){
        df_sectores<- df_sectores%>%
            select(
                id_trabajador
                # , nodo
                , periodo
                , {{tipo}}
            ) %>%
            distinct(id_trabajador, edad, .keep_all = TRUE)
    }else{
        df_sectores<- df_sectores%>%
            select(
                id_trabajador
                # , nodo
                , periodo
                , {{tipo}}
            ) %>%
            distinct(id_trabajador, tiempo, .keep_all = TRUE)

    }


    if (muestra != 0) {
        indices_muestra <- sample(unique(df_sectores$id_trabajador), size = muestra)
        df_sectores <- df_sectores %>% filter(.$id_trabajador %in% indices_muestra)
    }

    alfabeto <- c(relleno
                  , "Pausa"
                  # , sort(unique(df_sectores$nodo), decreasing = FALSE)
                  , sort(unique(df_sectores$periodo), decreasing = FALSE)
    )

    df_secuencias <- df_sectores %>%
        pivot_wider(            # tidyverse
            names_from = {{tipo}},
            # values_from = nodo,
            values_from = periodo,
            values_fill = relleno,
            names_sort = TRUE # Mantenemos la corrección de edades
        ) %>%
        arrange(id_trabajador)

    matriz_estados <- df_secuencias %>%
        select(-id_trabajador) %>%
        as.data.frame()
    rownames(matriz_estados) <- as.character(df_secuencias$id_trabajador)

    # Reemplazar "Fuera" entre estados activos/licencia con "Pausa"
    temp <- t(apply(matriz_estados, 1, reemplazar_fuera_con_pausa, relleno = relleno))
    colnames(temp) <- colnames(matriz_estados)
    matriz_estados <- as.data.frame(temp)

    secuencia_sectores <- seqdef(
        data = matriz_estados,
        alphabet = alfabeto,
        states = alfabeto,
        cpal = colores_sectores[c(1:length(alfabeto))],
        id = rownames(matriz_estados)
    )
    if (debug) {
        message("Duración procesamiento: ", difftime(Sys.time(), inicio, units = "mins"))
    }
    return(secuencia_sectores)

}



secuencia_sectores_muj_tiempo <- crear_secuencia(mler_base_new , debug = TRUE, muestra = 0, tipo = "tiempo")

write_parquet(secuencia_sectores_muj_tiempo, "./materiales/secuencia_sectores_mujeres_tiempo.parquet")
###################3
###################

if(!exists("colores_MLER", mode="function")) source("./colores_MLER.r")
colores_sectores <- colores_MLER[[1]]
colores_favoritos <- colores_MLER[[2]]
colores_estados <- colores_MLER[[3]]

sexo = "Mujeres"
secuencia_sectores <- secuencia_sectores_muj_tiempo
