library(TraMineR)
library(pheatmap)
df_MLER_licencias <- readRDS("./materiales/df_MLER_licencias.rds")

# Assume 'seq_data' is your defined state sequence object
# Calculate transversal state distributions

statd <- seqstatd(secuencia_sectores)

# Extract the frequency matrix (states in rows, time points in columns)
freq_matrix <- statd$freq

# Plot the heatmap
pheatmap(freq_matrix, cluster_rows = FALSE, cluster_cols = FALSE)
