library(data.table)
MLER_csv <- fread("./materiales/MLER.csv")
library(haven)
MLER_dta <- read_dta("./materiales/MLER.dta")
# A tibble: 6 × 13
library(dplyr)
summary(MLER_csv)
df <- as.data.frame(MLER_csv, what = c("edges", "vertices", "both"))
