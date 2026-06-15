
# ==============================================================================
# 0. LIBRERÍAS
# ==============================================================================
library(ggplot2)
library(scales)


# ==============================================================================
# 1. CARGA DE DATOS
# ==============================================================================.

df <- readRDS("./materiales/df_mujer_real.rds")

# Variables de tiempo derivadas
df$anio <- as.integer(format(df$tiempo, "%Y"))
df$mes  <- as.integer(format(df$tiempo, "%m"))

# Snapshot de diciembre (stock de fin de año)
dic <- df[df$mes == 12L, ]

cat("✓ Datos cargados:", nrow(df), "filas |", nrow(dic), "filas en diciembre\n")


# ==============================================================================
# ETIQUETAS
# ==============================================================================

letra_labels <- c(
  "0"  = "0 · Sin clasif.",
  "1"  = "A · Agric./Pesca",
  "2"  = "B · Minería",
  "3"  = "C · Alim./Textil",
  "4"  = "D · Manufactura",
  "5"  = "E · Electr./Gas",
  "6"  = "F · Construcción",
  "7"  = "G · Comercio",
  "8"  = "H · Hoteles/Rest.",
  "9"  = "I · Transporte",
  "10" = "J · Finanzas",
  "11" = "K · Inmobil./Serv.",
  "12" = "L · Adm. Pública",
  "13" = "M · Educación",
  "14" = "N · Salud/Social"
)

# Top 20 r34 más frecuentes en hombres (distinto al de mujeres)
r34_desc <- c(
  "4520" = "Construcción de edif.",
  "6022" = "Transporte automotor cargas",
  "7492" = "Selección de personal",
  "8000" = "Adm. Pública",
  "6021" = "Transporte urbano pasaj.",
  "5211" = "Supermercados",
  "5521" = "Restaurantes/bares",
  "9100" = "Hospitales",
  "121"  = "Cría de ganado bovino",
  "8510" = "Enseñanza prim./sec.",
  "1511" = "Frigoríficos/cárnica",
  "111"  = "Cultivos agrícolas",
  "7410" = "Activ. jurídicas/contab.",
  "7499" = "Otras activ. empres.",
  "7020" = "Alquiler de inmuebles",
  "7010" = "Activ. inmobiliarias",
  "7500" = "Activ. veterinarias",
  "6420" = "Telecomunicaciones",
  "114"  = "Cultivos forrajeros",
  "2520" = "Fab. de plásticos"
)
top20_r34 <- names(r34_desc)
top15_r34 <- top20_r34[1:15]


# ==============================================================================
# GRÁFICO 1 — Edad de ingreso al empleo formal: 1996–2000 vs. 2017–2021
# Pregunta: ¿Cambió el perfil etario de los hombres que se incorporan al
#           mercado formal entre el inicio y el fin del período?
# ==============================================================================

# Primera observación de cada trabajador
df_ord    <- df[order(df$id_trabajador, df$tiempo), ]
first_idx <- !duplicated(df_ord$id_trabajador)

entrada <- data.frame(
  edad = df_ord$edad[first_idx],
  anio = df_ord$anio[first_idx]
)
rm(df_ord, first_idx)  # liberar memoria

entrada <- entrada[!is.na(entrada$edad) & entrada$edad >= 17 & entrada$edad <= 65, ]
entrada$periodo <- NA_character_
entrada$periodo[entrada$anio >= 1996 & entrada$anio <= 2000] <- "1996\u20132000"
entrada$periodo[entrada$anio >= 2017 & entrada$anio <= 2021] <- "2017\u20132021"
entrada <- entrada[!is.na(entrada$periodo), ]

n_96     <- sum(entrada$periodo == "1996\u20132000")
n_21     <- sum(entrada$periodo == "2017\u20132021")
medianas <- aggregate(edad ~ periodo, entrada, median)

paleta_p <- c("1996\u20132000" = "#05EE07",   # azul celeste
              "2017\u20132021" = "#f4a261")    # naranja (distinto al de mujeres)

p1 <- ggplot(entrada, aes(x = edad, fill = periodo, color = periodo)) +
  geom_density(alpha = 0.35, linewidth = 0.9, adjust = 1.2) +
  geom_vline(data = medianas,
             aes(xintercept = edad, color = periodo),
             linetype = "dashed", linewidth = 0.85) +
  geom_label(
    data = medianas %>%
      mutate(
        x_pos = edad + ifelse(periodo == "1996-2000", 0.5, 2),
        y_pos = ifelse(periodo == "1996-2000", 0.065, 0.055)
      ),
    aes(
      x     = x_pos,
      y     = y_pos,
      label = sprintf("Mediana: %d a.", round(edad)),
      color = periodo
    ),
    fill        = "white",
    size        = 3,
    fontface    = "bold",
    hjust       = 0,
    show.legend = FALSE
  ) +

  scale_fill_manual(
    values = paleta_p,
    labels = c(sprintf("1996\u20132000  (n\u00a0=\u00a0%s)", format(n_96, big.mark = ".")),
               sprintf("2017\u20132021  (n\u00a0=\u00a0%s)", format(n_21, big.mark = ".")))
  ) +
  scale_color_manual(values = paleta_p, guide = "none") +
  scale_x_continuous(breaks = seq(17, 65, 4)) +
  labs(
    title    = "\u00bfA qu\u00e9 edad ingresan los hombres al empleo formal? \u2014 MLER",
    subtitle = "Distribuci\u00f3n de la edad en la primera observaci\u00f3n \u00b7 Comparaci\u00f3n 1996\u20132000 vs. 2017\u20132021",
    x        = "Edad al ingresar por primera vez al sistema formal",
    y        = "Densidad",
    fill     = "Per\u00edodo de ingreso",
    caption  = "Fuente: MLER \u00b7 Primera observaci\u00f3n de cada trabajador en la muestra"
  )

print(p1)
ggsave("g1_edad_entrada_hombres.png", p1, width = 13, height = 7, dpi = 150, bg = "#0a0a1a")



# ==============================================================================
# GRÁFICO 2 — Heatmap duración de relaciones laborales por LETRA
# Pregunta: ¿En qué sectores los hombres tienen vínculos más cortos o más largos?
# ==============================================================================

rel_key_all <- paste(df$id_trabajador, df$id_relacion)
dur_tab_all <- tapply(df$tiempo, rel_key_all,
                      function(x) length(unique(x)))
let_tab_all <- tapply(df$letra, rel_key_all,
                      function(x) as.integer(names(which.max(table(x)))))

dur_letra_df <- data.frame(
  dur   = as.integer(dur_tab_all),
  letra = as.character(let_tab_all[names(dur_tab_all)])
)
rm(rel_key_all, dur_tab_all, let_tab_all)

dur_letra_df$dur_grp <- cut(
  dur_letra_df$dur,
  breaks = c(0, 3, 6, 12, 24, 48, 300),
  labels = c("1\u20133 m", "4\u20136 m", "7\u201312 m", "13\u201324 m", "25\u201348 m", "48 m+"),
  right  = TRUE
)

tab_dur_letra <- as.data.frame(table(LETRA = dur_letra_df$letra, DUR = dur_letra_df$dur_grp))
tab_dur_letra$total <- ave(tab_dur_letra$Freq, tab_dur_letra$LETRA, FUN = sum)
tab_dur_letra$prop  <- tab_dur_letra$Freq / tab_dur_letra$total
tab_dur_letra$LETRA_LAB <- factor(
  tab_dur_letra$LETRA,
  levels = as.character(0:14),
  labels = letra_labels
)

p2 <- ggplot(tab_dur_letra, aes(x = DUR, y = LETRA_LAB, fill = prop)) +
  geom_tile(color = "grey10", linewidth = 0.45) +
  geom_text(aes(label = sprintf("%.0f%%", prop * 100)),
            size = 2.8, color = "white", fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#03001C","#150050","#3F0071","#CB0C8B","#FF6000","#FFD700"),
    name   = "% de\nrelaciones",
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title    = "Duraci\u00f3n de Relaciones Laborales por Sector (letra) \u2014 Hombres (MLER 1996\u20132021)",
    subtitle = "Cada fila suma 100% \u00b7 Celda m\u00e1s brillante = tramo donde se concentra el sector",
    x        = "Duraci\u00f3n del v\u00ednculo laboral",
    y        = NULL,
    caption  = "Duraci\u00f3n = meses con observaci\u00f3n registrada en la relaci\u00f3n laboral \u00b7 Fuente: MLER"
  ) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

print(p2)
ggsave("g2_duracion_letra_hombres.png", p2, width = 12, height = 8, dpi = 150, bg = "#0a0a1a")



# ==============================================================================
# GRÁFICO 3 — Stock de hombres por grupo etario × año (área apilada)
# Pregunta: ¿Cómo cambió la composición etaria del empleo formal masculino?
# ==============================================================================



dic_ed      <- dic[!is.na(dic$edad), ]
dic_ed$grp  <- cut(
  dic_ed$edad,
  breaks = c(17, 25, 30, 35, 40, 45, 50, 55, 85),
  labels = c("17-24","25-29","30-34","35-39","40-44","45-49","50-54","55+"),
  right  = FALSE
)

tab_ea      <- as.data.frame(table(GRP = dic_ed$grp, ANIO = dic_ed$anio))
tab_ea$ANIO <- as.integer(as.character(tab_ea$ANIO))

total_max   <- max(aggregate(Freq ~ ANIO, tab_ea, sum)$Freq)

hitos <- data.frame(
  x     = c(1999, 2001, 2002, 2009, 2018, 2020),
  label = c("Recesi\u00f3n\n98\u201399", "Crisis\n2001", "Post\ncrisis",
            "Crisis\nglobal", "FMI\n2018", "COVID\n2020")
)

paleta_edad <- c(
  "17-24" = "#f4a261",
  "25-29" = "#e76f51",
  "30-34" = "#d62828",
  "35-39" = "#9b2226",
  "40-44" = "#6a3d9a",
  "45-49" = "#3a86ff",
  "50-54" = "#0077b6",
  "55+"   = "#00b4d8"
)

p3 <- ggplot(tab_ea, aes(x = ANIO, y = Freq, fill = GRP)) +
  geom_area(alpha = 0.88, color = NA) +
  geom_vline(xintercept = hitos$x,
             color = "black", linetype = "dashed",
             linewidth = 0.45, alpha = 0.6) +
  annotate("text",
           x = hitos$x + 0.1,
           y = total_max * 1.05,
           label     = hitos$label,
           color     = "black",
           size      = 2.1,
           hjust     = 0,
           vjust     = 0,
           lineheight = 0.85) +
  scale_fill_manual(values = paleta_edad, name = "Grupo\netario") +
  scale_x_continuous(breaks = seq(1996, 2021, 3),
                     expand = expansion(mult = c(0.01, 0.06))) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.13))) +
  labs(
    title    = "Evoluci\u00f3n del Stock de Hombres en Empleo Formal por Grupo Etario",
    subtitle = "Observaciones de diciembre de cada a\u00f1o \u00b7 MLER 1996\u20132021",
    x        = NULL,
    y        = "Trabajadores en stock (dic)",
    caption  = "Fuente: MLER \u00b7 Elaboraci\u00f3n propia"
  )

print(p3)
ggsave("g3_stock_edad_hombres.png", p3, width = 13, height = 7, dpi = 150, bg = "#0a0a1a")



# ==============================================================================
# GRÁFICO 4 — Heatmap: ¿qué edades concentra cada r34?
# Pregunta: ¿Existe segregación etaria entre ramas de actividad masculina?
# ==============================================================================

df_r34_ed <- df[df$r34 %in% as.integer(top20_r34) &
                  !is.na(df$edad) &
                  df$edad >= 17 &
                  df$edad <= 70, ]

df_r34_ed$grp_edad <- cut(
  df_r34_ed$edad,
  breaks = c(17, 22, 27, 32, 37, 42, 47, 52, 57, 70),
  labels = c("17-21","22-26","27-31","32-36","37-41","42-46","47-51","52-56","57+"),
  right  = FALSE
)

med_edad_r34 <- tapply(df_r34_ed$edad, df_r34_ed$r34, median, na.rm = TRUE)
r34_ord_edad <- names(sort(med_edad_r34))  # orden creciente por edad mediana

tab_re       <- as.data.frame(table(R34 = df_r34_ed$r34, EDAD_GRP = df_r34_ed$grp_edad))
tab_re$total <- ave(tab_re$Freq, tab_re$R34, FUN = sum)
tab_re$prop  <- tab_re$Freq / tab_re$total

tab_re$R34_LAB <- factor(
  tab_re$R34,
  levels = r34_ord_edad,
  labels = sprintf("%s  (med.\u00a0%s\u00a0a.)",
                   r34_desc[r34_ord_edad],
                   round(med_edad_r34[r34_ord_edad]))
)

p4 <- ggplot(tab_re, aes(x = EDAD_GRP, y = R34_LAB, fill = prop)) +
  geom_tile(color = "grey10", linewidth = 0.4) +
  geom_text(aes(label = ifelse(prop >= 0.04, sprintf("%.0f%%", prop * 100), "")),
            size = 2.4, color = "white", fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#03001C","#0d1b2a","#1565C0","#0288D1","#26C6DA","#FFF176","#FF6F00"),
    name   = "% del sector\nen ese rango",
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title    = "\u00bfQu\u00e9 edades concentra cada rama de actividad? \u2014 Hombres (MLER 1996\u20132021)",
    subtitle = "Distribuci\u00f3n etaria dentro de cada r34 \u00b7 Ordenado por edad mediana creciente \u2191",
    x        = "Grupo etario",
    y        = NULL,
    caption  = "Top 20 ramas r34 por frecuencia \u00b7 Fuente: MLER"
  )
print(p4)
ggsave("g4_edad_r34_hombres.png", p4, width = 13, height = 10, dpi = 150, bg = "#0a0a1a")


# ==============================================================================
# GRÁFICO 5 — Crecimiento del stock por r34 × año (índice base 1996 = 100)
# Pregunta: ¿Qué ramas crecieron más y cuáles colapsaron en cada crisis?
# ==============================================================================


dic_r34       <- dic[dic$r34 %in% as.integer(top15_r34), ]
tab_r34_a     <- as.data.frame(table(R34 = dic_r34$r34, ANIO = dic_r34$anio))
tab_r34_a$ANIO <- as.integer(as.character(tab_r34_a$ANIO))
tab_r34_a     <- tab_r34_a[order(tab_r34_a$R34, tab_r34_a$ANIO), ]

base_96        <- tab_r34_a[tab_r34_a$ANIO == 1996, c("R34","Freq")]
names(base_96)[2] <- "base"
tab_r34_a      <- merge(tab_r34_a, base_96, by = "R34")
tab_r34_a$idx          <- tab_r34_a$Freq / tab_r34_a$base * 100
tab_r34_a$idx_clamped  <- pmax(pmin(tab_r34_a$idx, 400), 20)

library(stringr)

tab_r34_a$R34_LAB <- factor(
  tab_r34_a$R34,
  levels = top15_r34,
  labels = str_wrap(r34_desc[top15_r34], width = 20)
)

p5 <- ggplot(tab_r34_a, aes(x = ANIO, y = R34_LAB, fill = idx_clamped)) +
  geom_tile(color = "grey10", linewidth = 0.35) +
  geom_text(aes(label = sprintf("%.0f", idx)),
            size = 2.0, color = "white", fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#8B0000","#C62828","#E53935","#1a1a2e",
               "#1B5E20","#2E7D32","#66BB6A","#B9F6CA"),
    name   = "\u00cdndice\n(1996=100)",
    limits = c(20, 400)
  ) +
  scale_x_continuous(breaks = seq(1996, 2021, 3)) +
  annotate("rect", xmin = 2001.5, xmax = 2002.5, ymin = 0.5, ymax = 15.5,
           fill = NA, color = "yellow", linewidth = 0.9) +
  annotate("rect", xmin = 2019.5, xmax = 2020.5, ymin = 0.5, ymax = 15.5,
           fill = NA, color = "cyan", linewidth = 0.9) +
  annotate("text", x = 2002, y = 15.7, label = "Crisis\n2002",
           color = "yellow", size = 2.0, hjust = 0.5, vjust = 0) +
  annotate("text", x = 2020, y = 15.7, label = "COVID\n2020",
           color = "cyan",   size = 2.0, hjust = 0.5, vjust = 0) +
  labs(
    title    = "Crecimiento del Stock de Hombres por Rama de Actividad (1996\u20132021)",
    subtitle = "\u00cdndice base 1996 = 100 \u00b7 Verde = expansi\u00f3n \u00b7 Rojo = contracci\u00f3n \u00b7 Obs. diciembre",
    x        = NULL,
    y        = NULL,
    caption  = "Fuente: MLER \u00b7 Top 15 ramas r34 m\u00e1s frecuentes en hombres"
  ) +
  theme(
    axis.text.y = element_text(
      size = 9,
      face = "bold"
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8
    )
  )

print(p5)
ggsave("g5_crecimiento_r34_hombres.png", p5, width = 15, height = 8, dpi = 150, bg = "#0a0a1a")


# ==============================================================================
# GRÁFICO 6 — Duración de relaciones laborales por r34
# Pregunta: ¿Hay ramas masculinas con vínculos más precarios (cortos) que otras?
# ==============================================================================


df_r34_dur  <- df[df$r34 %in% as.integer(top20_r34), ]
rel_key_r34 <- paste(df_r34_dur$id_trabajador, df_r34_dur$id_relacion)

dur_r34_tab <- tapply(df_r34_dur$tiempo, rel_key_r34,
                      function(x) length(unique(x)))
r34_rel_tab <- tapply(df_r34_dur$r34, rel_key_r34,
                      function(x) as.character(names(which.max(table(x)))))
rm(df_r34_dur, rel_key_r34)

dur_r34_df <- data.frame(
  dur = as.integer(dur_r34_tab),
  r34 = r34_rel_tab[names(dur_r34_tab)]
)
rm(dur_r34_tab, r34_rel_tab)

dur_r34_df$dur_grp <- cut(
  dur_r34_df$dur,
  breaks = c(0, 3, 6, 12, 24, 48, 300),
  labels = c("1\u20133 m","4\u20136 m","7\u201312 m","13\u201324 m","25\u201348 m","48 m+"),
  right  = TRUE
)

tab_dr        <- as.data.frame(table(R34 = dur_r34_df$r34, DUR = dur_r34_df$dur_grp))
tab_dr$total  <- ave(tab_dr$Freq, tab_dr$R34, FUN = sum)
tab_dr$prop   <- tab_dr$Freq / tab_dr$total

dur_med_r34  <- tapply(dur_r34_df$dur, dur_r34_df$r34, median)
r34_ord_dur  <- names(sort(dur_med_r34))   # menor → mayor duración mediana

tab_dr$R34_LAB <- factor(
  tab_dr$R34,
  levels = r34_ord_dur,
  labels = sprintf("%s  (med.\u00a0%s\u00a0m.)",
                   r34_desc[r34_ord_dur],
                   round(dur_med_r34[r34_ord_dur]))
)

p6 <- ggplot(tab_dr, aes(x = DUR, y = R34_LAB, fill = prop)) +
  geom_tile(color = "grey10", linewidth = 0.45) +
  geom_text(aes(label = ifelse(prop >= 0.03, sprintf("%.0f%%", prop * 100), "")),
            size = 2.5, color = "white", fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#03001C","#150050","#3F0071","#CB0C8B","#FF6000","#FFD700"),
    name   = "% de\nrelaciones",
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title    = "Duraci\u00f3n de Relaciones Laborales por Rama (r34) \u2014 Hombres (MLER 1996\u20132021)",
    subtitle = "Cada fila = 100% de relaciones de esa rama \u00b7 Ordenado por duraci\u00f3n mediana creciente \u2191",
    x        = "Duraci\u00f3n del v\u00ednculo laboral",
    y        = NULL,
    caption  = "Duraci\u00f3n = meses con observaci\u00f3n registrada \u00b7 Fuente: MLER \u00b7 Top 20 r34 en hombres"
  ) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

print(p6)
ggsave("g6_duracion_r34_hombres.png", p6, width = 13, height = 10, dpi = 150, bg = "#0a0a1a")


