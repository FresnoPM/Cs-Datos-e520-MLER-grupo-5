library(tidyverse)
library(readxl)
library(lubridate)
#ACA PONES EL ARCHIVO DE INIDICE DE INFLACION
x<-read_excel('C:/Users/Ivan/Desktop/Ciencia de Datos para Economía y Negocios/Projecto R/Cs-Datos-e520-MLER-grupo-5/Indice-FACPCE-Res.-JG-539-18-2026-4.xlsx-kBC1DWYCtZ(1).xlsx')
#SACAR CLUMNAS
names(x)

inflation_df <- x |> 
  
  mutate(
    # ARREGLAR FORMATO DE EXCEL Y PASARLO A R
    true_date = as.Date(as.numeric(MES), origin = "1899-12-30"),
    
    year = year(true_date),
    month = month(true_date)
  ) |> 
  #EL PERIODO QUE USAMOS
  filter(year >= 1996 & year <= 2021) |> 
  
  select(year, month, `IPC NACIONAL EMPALME IPIM`)

# SACAS INDICE QUE 2021
ipc_dec_2021 <- inflation_df |> 
  filter(year == 2021 & month == 12) |> 
  pull(`IPC NACIONAL EMPALME IPIM`) |> 
  as.numeric()

# CALCULO PARA df_mujer. Puede ser otro
df_mujer_real <- df_mujer |> 
  mutate(
    year = year(tiempo),
    month = month(tiempo)
  ) |> 
  
  # Unir con las fechas y meses especificos
  left_join(inflation_df, by = c("year", "month")) |> 
  
  # Calcular con base de diciembre 2021
  mutate(
    rem_tot_real = (rem_tot / as.numeric(`IPC NACIONAL EMPALME IPIM`)) * ipc_dec_2021 
  ) |> 
  
  # solo quedarte con rem_tot_real
  select(-year, -month, -`IPC NACIONAL EMPALME IPIM`)

library(readr)

# Save as an R binary file
write_rds(df_mujer_real, "df_mujer_real.rds")
