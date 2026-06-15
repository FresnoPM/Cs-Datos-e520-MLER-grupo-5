library(dplyr)
library(lubridate)

# 1. Prepare the Index Dataset (Assuming variables are 'fecha' and 'ipc')
ipc <- ipc %>%
  mutate(
    fecha = floor_date(parse_date_time(fecha, orders = c("ymd", "dmy", "ym", "my")), "month"),
    ipc = as.numeric(as.character(ipc_general))
  )

ipc_target <- ipc %>%
  filter(fecha == as.Date("2026-05-01")) %>%
  pull(ipc_general)

# 2. Update df_genero0 dataset using 'tiempo'
df_hombre_real <- df_hombre %>%
  mutate(
    # Standardize 'tiempo' into a recognized monthly date format
    tiempo = floor_date(parse_date_time(tiempo, orders = c("ymd", "dmy", "ym", "my")), "month"),
    rem_tot = as.numeric(rem_tot)
  ) %>%
  # Join using the updated variable name
  left_join(ipc, by = c("tiempo" = "fecha")) %>%
  mutate(
    rem_tot_real = rem_tot * (ipc_target / ipc)
  )
saveRDS(df_hombre_real,file='df_hombre_real')
