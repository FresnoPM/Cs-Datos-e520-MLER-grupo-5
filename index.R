library(data.table)
MLER_csv <- fread("./materiales/MLER.csv")
library(haven)
MLER_dta <- read_dta("./materiales/MLER.dta")
# A tibble: 6 × 13
library(dplyr)
head(MLER_csv)

df <- as.data.frame(MLER_csv)

df_mujer <- df %>% filter(sexo==1)
nrow(df_mujer) # 15.346.369

df_hombre <- df %>% filter(sexo==2)
nrow(df_hombre) # 33.788.884

# write.csv(df_mujer, "materiales/df_mujer.csv", row.names = FALSE)
# write.csv(df_hombre, "materiales/df_hombre.csv", row.names = FALSE)
