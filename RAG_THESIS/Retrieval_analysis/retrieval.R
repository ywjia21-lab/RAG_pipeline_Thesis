library(dplyr)
library(tidyr)
library(ggplot2)

#lectura
files <- c(
  "df_ranks_350-100.csv",
  "df_ranks_350-150.csv",
  "df_ranks_450-100.csv",
  "df_ranks_450-150.csv",
  "df_ranks_500-100.csv",
  "df_ranks_500-150.csv",
  "df_ranks_550-100.csv",
  "df_ranks_550-150.csv"
)

col_names <- c(
  "id",
  "all-MiniLM-L6-v2",
  "all-MiniLM-L12-v2",
  "bge-base-en-v1.5",
  "bge-small-en-v1.5",
  "multilingual-e5-large",
  "gte-Qwen2-1.5B-instruct"
)

df_rank <- list()

for (f in files) {
  df <- read.csv(f)
  names(df) <- col_names
  df_rank[[f]] <- df
  rm(df)
}
######################### pasar en un sol data
df_rank <- bind_rows(
  lapply(names(df_rank), function(nombre_archivo) {
    config_raw <- gsub("df_ranks_|\\.csv", "", nombre_archivo)
    config <- config_raw
    
    # Añadir la columna config al data frame correspondiente
    df_rank[[nombre_archivo]] %>%
      mutate(config = config)
  })
)


#funcion para calular HR I MRR
calculate_metrics <- function(df) {
  
  df <- df[, colnames(df) != "id", drop = FALSE]
  
  results <- data.frame(
    Model = character(),
    HR1 = numeric(),
    HR3 = numeric(),
    HR5 = numeric(),
    MRR3 = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (model in colnames(df)) {
    
    ranks <- df[[model]]
    
    hr1 <- mean(ranks <= 1)
    hr3 <- mean(ranks <= 3)
    hr5 <- mean(ranks <= 5)
    
    mrr3 <- mean(
      ifelse(ranks <= 3, 1 / ranks, 0)
    )
    
    results <- rbind(
      results,
      data.frame(
        Model = model,
        HR1 = round(hr1, 3),
        HR3 = round(hr3, 3),
        HR5 = round(hr5, 3),
        MRR3 = round(mrr3, 3)
      )
    )
  }
  results
}

configs <- unique(df_rank$config)

lista_metricas <- list()

for (cfg in configs) {
  # Filtrar filas de esta configuración
  df_cfg <- df_rank %>%
    filter(config == cfg) %>%
    select(-id, -config)   # dejar solo las columnas de los modelos
  
  metricas_cfg <- calculate_metrics(df_cfg)
  
  lista_metricas[[cfg]] <- metricas_cfg
}

rm(df_cfg)
rm(metricas_cfg)

df_metricas <- bind_rows(
  lapply(names(lista_metricas), function(cfg) {
    lista_metricas[[cfg]] %>%
      mutate(config = cfg) %>%
      pivot_longer(cols = c(HR1, HR3, HR5, MRR3),
                   names_to = "metric",
                   values_to = "value")
  })
)


ggplot(df_metricas,
       aes(x = config, y = value, color = Model, group = Model)) +
  geom_line() +
  geom_point() +
  facet_wrap(~metric, scales = "free_y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Retrieval performance across embedding models and chunk configurations",
    x = "Chunk size - Overlap",
    y = "Score"
  )

####################################### bootstrap

# Filtrar el modelo y las configuraciones, crear hit3 binaria
df_boot <- df_rank %>%
  filter(config %in% c("450-100", "550-100")) %>%
  select(id, config, `multilingual-e5-large`) %>%
  rename(rank = `multilingual-e5-large`) %>%
  mutate(hit3 = as.integer(rank <= 3))

df_boot <- df_boot %>%
  select(id, config, hit3) %>%
  pivot_wider(id_cols = id, names_from = config, values_from = hit3) %>%
  na.omit()

library(boot)

boot_diff <- function(data, indices) {
  mean(data[indices])
}

# 450-100 i 550-100
df_dif<-df_boot[,2]-df_boot[,3]
set.seed(123)
res <- boot(
  data = df_dif[[1]],
  statistic = boot_diff,
  R = 1500
)

boot.ci(res, type = "perc", conf = 0.95)
quantile(res$t, c(0.025, 0.975))

# ==========================================
# ANÁLISIS DE ESTABILIZACIÓN DEL BOOTSTRAP
# ==========================================
library(boot)
library(ggplot2)
library(dplyr)

# Definir la estadística
boot_diff <- function(data, indices) {
  mean(data[indices])
}

R_values <- c(50, 100, 200, 500, 1000, 1500, 2000,3000,4000,5000)

resultados <- data.frame()

set.seed(123)  

for (R in R_values) {
  # Ejecutar el bootstrap con R remuestras
  res <- boot(data = df_dif[[1]], statistic = boot_diff, R = R)
  
  # Calcular el intervalo de confianza percentil
  ci <- boot.ci(res, type = "perc", conf = 0.95)
  
  # Guardar los resultados
  temp <- data.frame(
    R = R,
    mean_resample = mean(res$t),     # Media de las medias bootstrap
    se = sd(res$t),                  # Error estándar bootstrap
    lower = ci$perc[4],              # Límite inferior del IC
    upper = ci$perc[5]               # Límite superior del IC
  )
  resultados <- rbind(resultados, temp)
  
  cat("R =", R, " | SE =", round(temp$se, 4), " | IC = [", round(temp$lower, 4), ", ", round(temp$upper, 4), "]\n")
}

# ==========================================
# GRÁFICOS DE ESTABILIZACIÓN
# ==========================================

#Límites del IC al 95% vs. Número de remuestras
ggplot(resultados, aes(x = R)) +
  geom_line(aes(y = lower), color = "red", linewidth = 1) +
  geom_point(aes(y = lower), color = "red", size = 2) +
  geom_line(aes(y = upper), color = "blue", linewidth = 1) +
  geom_point(aes(y = upper), color = "blue", size = 2) +
  labs(
    title = "Bootstrap 95% Confidence Interval Stabilization",
    x = "Number of Bootstrap Resamples (R)",
    y = "Confidence Interval Bounds"
  ) +
  theme_minimal()

###############################
library(pheatmap)

ls_rank <- list()

for (f in files) {
  df <- read.csv(f)
  names(df) <- col_names
  ls_rank[[f]] <- df
  rm(df)
}

# Elige una configuración (por ejemplo, la primera o la mejor)
cfg_name <- names(ls_rank)[3] 
df_cfg <- ls_rank[[cfg_name]]

# Matriz de rangos: filas = preguntas (id), columnas = modelos
mat_ranks <- df_cfg %>%
  dplyr::select(-id) %>%
  as.matrix()
rownames(mat_ranks) <- df_cfg$id
mat_success <- ifelse(mat_ranks <= 3, 1, 0)
136-colSums(mat_success)
table(rowSums(mat_success))
row_counts <- c(0, 1, 2, 3, 4, 5, 6)
freq <- c(1, 4, 7, 4, 9, 51, 60)

bp <- barplot(
  freq,
  names.arg = row_counts,
  xlab = "Number of successful models",
  ylab = "Number of queries",
  main = "Distribution of retrieval success per query",
  col = "skyblue",
  ylim = c(0, max(freq) * 1.1)
)
text(
  x = bp,
  y = freq,
  label = freq,
  pos = 3
)

mat_success[rowSums(mat_success) == 0, ,drop=FALSE]

########### heatmap
# Copiar los nombres originales
modelos <- colnames(mat_success)

# Calcular fallos por modelo
fallos <- colSums(mat_success == 0)

# Crear nuevos nombres con el número de fallos
nuevos_nombres <- paste0(modelos, " (", fallos, ")")

# Asignar los nuevos nombres a la matriz
colnames(mat_success) <- nuevos_nombres

# Generar el heatmap
pheatmap(mat_success,
         color = c("red", "darkgreen"),
         breaks = c(-0.5, 0.5, 1.5),
         main = "Hit@3 heatmap (450–100)",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         show_rownames = FALSE,
         fontsize_col = 10,
         legend_breaks = c(0, 1),
         legend_labels = c("0", "1"),
         filename = "row_heat_450_100.png")

######### por filas para ver donde no recupera, identificar patron

# requiere la lectura de los ficheros metricas de los LMM's de ragas
df_comp<-data.frame(id=1:136,retrieval=mat_success[,5],qwen_rag_LA=resultados_RAG_qwen$answer_correctness_LA,qwen_norag_LA=resultados_NORAG_qwen$answer_correctness_LA)

library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Transformar a formato largo
df_long <- df_comp %>%
  pivot_longer(
    cols = c(qwen_rag_LA, qwen_norag_LA),
    names_to = "condition",
    values_to = "score"
  ) %>%
  mutate(
    rag = ifelse(condition == "qwen_rag_LA", "RAG", "NoRAG"),
    retrieval_label = ifelse(retrieval == 1, "Context retrieved", "Context NOT retrieved")
  )

# 2. Gráfico de líneas por pregunta
ggplot(df_long, aes(x = rag, y = score, group = id, color = as.factor(retrieval))) +
  geom_line(alpha = 0.3, linewidth = 0.3) +
  geom_point(alpha = 0.6, size = 1.5) +
  facet_wrap(~ retrieval_label) +
  labs(
    title = "Qwen Answer Correctness (LA) with and without RAG",
    x = "Condition",
    y = "Answer Correctness (0-1)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.background = element_rect(fill = "lightgray", color = NA),
    legend.position = "none"
  )

sum(df_comp$qwen_rag_LA < df_comp$qwen_norag_LA & df_comp$retrieval == 1)