library(data.table)
library(dplyr)

# Classify consecutive Activo/Licencia spells per worker into periodo types:
#   "estable"          : Activo spell of 12+ months
#   "ultima_maternidad": Licencia spell of exactly 3 months
#   "lic_corta"        : Licencia spell of 1-2 months
#   "lic_larga"        : Licencia spell of 4+ months

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

####################################
#### EXPLORACIÓN TRAMINER ####
####################################



library(data.table)
library(TraMineR)
library(ggseqplot)
library(ggplot2)

# --- Priority order for state deduplication ---
# When a worker has multiple states at the same age, keep the most specific one
priority_order <- c(
  "maternidad", "ultima_maternidad", "maternidad_ignorada",
  "lic_larga", "lic_corta", "estable"
)

# --- Deduplicate: keep highest-priority state per worker-edad ---
dt_edad <- as.data.table(mler_base)[, .(id_trabajador, edad, periodo)]
dt_edad[, priority := match(periodo, priority_order)]
dt_edad[is.na(priority), priority := 99L]
setkey(dt_edad, id_trabajador, edad, priority)
dt_edad <- dt_edad[, .SD[1L], by = .(id_trabajador, edad)]

# --- Pivot to wide: rows = worker, cols = age 20:38 ---
mat_edad <- dcast(dt_edad, id_trabajador ~ edad, value.var = "periodo")
setnames(mat_edad, as.character(edad_min:edad_max), paste0("edad_", edad_min:edad_max))

# --- Create TraMineR sequence object with edad as time axis ---
alphabet <- c("estable", "lic_corta", "lic_larga",
              "maternidad", "maternidad_ignorada", "ultima_maternidad")

seq_edad <- seqdef(
  mat_edad,
  var      = paste0("edad_", 20:38),
  alphabet = alphabet,
  id       = mat_edad$id_trabajador,
  missing  = NA,
  nr       = "NA",
  tlab     = as.character(20:38)
)

# --- Sort all non-empty sequences by descending length (white gaps float to top) ---
all_non_empty <- which(seqlength(seq_edad) > 0)
sort_all      <- -seqlength(seq_edad[all_non_empty, ])

# --- Build base ggseqiplot ---
p <- ggseqiplot(seq_edad[all_non_empty, ], sortv = sort_all, no.n = TRUE)

# --- Apply custom palette and deduplicate legend ---
# lic_corta/lic_larga share one color; maternidad/ultima_maternidad share another
# guides(colour = "none") removes the duplicate legend ggseqiplot creates
# by mapping both fill and colour to the same states variable
p +
  scale_fill_manual(
    values = c(
      "Missing"             = "#c9c9c9",
      "estable"             = "#0B476B",
      "lic_corta"           = "#C9570A",
      "lic_larga"           = "#C9570A",
      "maternidad"          = "#7e0089",
      "ultima_maternidad"   = "#7e0089",
      "maternidad_ignorada" = "#9c0000"
    ),
    breaks = c("estable", "lic_corta", "maternidad", "ultima_maternidad", "maternidad_ignorada", "Missing"),
    labels = c("Estable", "Lic. corta / Lic. larga", "Maternidad", "Última maternidad", "Mat. ignorada", "Sin dato"),
    name   = "Estado"
  ) +
  guides(colour = "none") +
  labs(
    title = "Secuencias individuales MLER por edad",
    x     = "Edad",
    y     = "Secuencias"
  )


