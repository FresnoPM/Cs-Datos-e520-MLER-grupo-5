

# Extraemos los flujos con la misma lógica, pero ahora detectando la PAUSA
flujos_santi <- df_red_santi %>%
    select(id_trabajador, meses_relativos, nodo_final) %>%
    distinct(id_trabajador, meses_relativos, .keep_all = TRUE) %>%
    pivot_wider(names_from = meses_relativos, values_from = nodo_final, names_prefix = "mes_") %>%
    rename(Origen = mes_0, Destino = mes_4) %>%
    mutate(
        # Aplicamos la lógica de Lu para el destino:
        # 1. Si el registro desaparece -> Fuera del sistema (9999)
        # 2. Si el registro sigue, pero su rem_tot es cero (sigue de licencia en el mes 4) -> Pausa (999)
        Destino = case_when(
            is.na(Destino) ~ "Fuera del Sistema (9999)",
            grepl("Licencia", Destino) ~ "Pausa Excedencia (999)",
            TRUE ~ Destino
        )
    ) %>%
    filter(!is.na(Origen)) %>%
    count(Origen, Destino, name = "Cantidad") %>%
    arrange(desc(Cantidad))

# Miramos los principales flujos de expulsión
print("Flujos de expulsión por sector de origen:")
print(head(flujos_santi, 10))