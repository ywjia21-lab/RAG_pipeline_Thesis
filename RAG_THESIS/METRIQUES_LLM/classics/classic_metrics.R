###########################################################3
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(patchwork)

# ------------------------------------------------------------
# 1. CARGA DE DATOS
# ------------------------------------------------------------
files <- c(
  "flan_xl_classic_metrics.csv",
  "gemma2_classic_metrics.csv",
  "large_classic_metrics.csv",
  "qwen15_classic_metrics.csv",
  "tiny_classic_metrics.csv"
)

df_classic <- lapply(files, read.csv)

names(df_classic) <- c(
  "FLAN_XL",
  "Gemma2",
  "FLAN_Large",
  "Qwen15",
  "Tiny"
)

# ------------------------------------------------------------
# 2. COMBINACIONES
# ------------------------------------------------------------
combinaciones <- expand.grid(
  rag = c("con_rag", "sin_rag"),
  resp = c("SA", "LA"),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 3. FUNCIÓN DE MÉTRICAS
# ------------------------------------------------------------
calcular_medias <- function(datasets, rag, resp) {
  sufijo <- paste0(rag, "_", resp)
  
  data.frame(
    Model = names(datasets),
    R1_P = sapply(datasets, function(x) mean(x[[paste0("rouge1_precision_respuesta_", sufijo)]], na.rm = TRUE)),
    R1_R = sapply(datasets, function(x) mean(x[[paste0("rouge1_recall_respuesta_", sufijo)]], na.rm = TRUE)),
    R1_F1 = sapply(datasets, function(x) mean(x[[paste0("rouge1_f1_respuesta_", sufijo)]], na.rm = TRUE)),
    R2_P = sapply(datasets, function(x) mean(x[[paste0("rouge2_precision_respuesta_", sufijo)]], na.rm = TRUE)),
    R2_R = sapply(datasets, function(x) mean(x[[paste0("rouge2_recall_respuesta_", sufijo)]], na.rm = TRUE)),
    R2_F1 = sapply(datasets, function(x) mean(x[[paste0("rouge2_f1_respuesta_", sufijo)]], na.rm = TRUE)),
    RL_P = sapply(datasets, function(x) mean(x[[paste0("rougeL_precision_respuesta_", sufijo)]], na.rm = TRUE)),
    RL_R = sapply(datasets, function(x) mean(x[[paste0("rougeL_recall_respuesta_", sufijo)]], na.rm = TRUE)),
    RL_F1 = sapply(datasets, function(x) mean(x[[paste0("rougeL_f1_respuesta_", sufijo)]], na.rm = TRUE)),
    BLEU = sapply(datasets, function(x) mean(x[[paste0("bleu_respuesta_", sufijo)]], na.rm = TRUE))
  )
}

# ------------------------------------------------------------
# 4. GENERAR DATASET LARGO
# ------------------------------------------------------------
lista_tablas <- list()

for (i in 1:nrow(combinaciones)) {
  rag_i <- combinaciones$rag[i]
  resp_i <- combinaciones$resp[i]
  
  df_temp <- calcular_medias(df_classic, rag_i, resp_i)
  df_temp$RAG <- rag_i
  df_temp$Response <- resp_i
  
  lista_tablas[[i]] <- df_temp
}

df_total <- bind_rows(lista_tablas)

df_largo <- df_total %>%
  pivot_longer(
    cols = c(R1_P, R1_R, R1_F1,
             R2_P, R2_R, R2_F1,
             RL_P, RL_R, RL_F1,
             BLEU),
    names_to = "Metric",
    values_to = "Value"
  )
rm(list = c("df_temp", "df_total", "lista_tablas", "combinaciones"))
# ------------------------------------------------------------
# 5. FACTORES
# ------------------------------------------------------------
df_largo$Model <- factor(df_largo$Model,
                          levels = c("FLAN_XL", "FLAN_Large", "Qwen15", "Gemma2", "Tiny"))

df_largo$RAG <- factor(df_largo$RAG,
                       levels = c("con_rag", "sin_rag"),
                       labels = c("RAG", "NO_RAG"))

df_largo$Response <- factor(df_largo$Response,
                            levels = c("SA", "LA"))

df_largo$Metric <- factor(df_largo$Metric,
                          levels = c("BLEU",
                                     "R1_F1", "R2_F1", "RL_F1",
                                     "R1_R", "R2_R", "RL_R",
                                     "R1_P", "R2_P", "RL_P"))

# ------------------------------------------------------------
# 6. FUNCIÓN HEATMAP
# ------------------------------------------------------------
heatmap_metrics_x <- function(datos, title, y_var,
                              lim_min = 0, lim_max = 0.85) {
  
  ggplot(datos, aes(x = Metric, y = !!sym(y_var), fill = Value)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "steelblue",
                        limits = c(lim_min, lim_max)) +
    theme_minimal() +
    labs(
      title = title,
      x = "Metric",
      y = y_var
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# recall: Qué proporción de la referencia ha sido cubierta por la respuesta generada
#comun/ referencia
# presicion: Qué proporción de lo generado aparece realmente en la referencia. 
#comun/ generado
# ------------------------------------------------------------
# 7. BLOQUE A: RAG × RESPONSE (Model on Y)
# ------------------------------------------------------------
for (r in levels(df_largo$RAG)) {
  for (resp in levels(df_largo$Response)) {
    
    datos_filt <- filter(df_largo, RAG == r, Response == resp)
    title <- paste(r, resp, sep = "-")
    
    print(heatmap_metrics_x(datos_filt, title, y_var = "Model"))
  }
}

#### los 4 en 1
library(patchwork)
plots <- list()

i <- 1

for (r in levels(df_largo$RAG)) {
  for (resp in levels(df_largo$Response)) {
    
    datos_filt <- dplyr::filter(
      df_largo,
      RAG == r,
      Response == resp
    )
    
    title <- paste(r, resp, sep = "-")
    
    plots[[i]] <- heatmap_metrics_x(
      datos_filt,
      title,
      y_var = "Model"
    )
    
    i <- i + 1
  }
}

(plots[[1]] | plots[[2]]) /
  (plots[[3]] | plots[[4]])

############## ANALISIS

#### añadir Model Y PREPARAR DATA SET

df_classic <- lapply(names(df_classic), function(nm) {
  df <- df_classic[[nm]]
  df$Model <- nm
  df
})

df_classic <- bind_rows(df_classic)

df_long <- df_classic %>%
  pivot_longer(
    cols = -c(ID, Q, SA, LA, Model),
    names_to = "Variable",
    values_to = "Value"
  )
df_long <- df_long %>%
  mutate(
    
    RAG = case_when(
      grepl("con_rag", Variable) ~ "RAG",
      grepl("sin_rag", Variable) ~ "NO_RAG"
    ),
    
    Response = case_when(
      grepl("_SA", Variable) ~ "SA",
      grepl("_LA", Variable) ~ "LA"
    ),
    
    Metric = case_when(
      grepl("rouge1_f1", Variable) ~ "R1_F1",
      grepl("rouge1_precision", Variable) ~ "R1_P",
      grepl("rouge1_recall", Variable) ~ "R1_R",
      
      grepl("rouge2_f1", Variable) ~ "R2_F1",
      grepl("rouge2_precision", Variable) ~ "R2_P",
      grepl("rouge2_recall", Variable) ~ "R2_R",
      
      grepl("rougeL_f1", Variable) ~ "RL_F1",
      grepl("rougeL_precision", Variable) ~ "RL_P",
      grepl("rougeL_recall", Variable) ~ "RL_R",
      
      grepl("bleu", Variable) ~ "BLEU"
    )
  ) %>%

  dplyr::select(ID, Model, RAG, Response, Metric, Value) %>%
  
  filter(!is.na(Metric), !is.na(RAG), !is.na(Response))

cols <- c("ID", "Model", "RAG", "Response", "Metric")

df_long[cols] <- lapply(df_long[cols], as.factor)

############################# 
# ANALIZAR ROUGE1 RECALL

library(car)
df<- df_long %>% filter(Metric == "R1_R")

medias <- df %>%
  group_by(Model, Response, RAG) %>%
  summarise(media = mean(Value), .groups = "drop")

ggplot(df, aes(x = Model, y = Value, fill = Response)) +
  geom_boxplot() +
  facet_wrap(~RAG) +
  scale_fill_manual(values = c("red", "skyblue")) +
  geom_point(
    data = medias,
    aes(x = Model, y = media, fill = Response),
    shape = 23,
    size = 3,
    color = "black",
    position = position_dodge(width = 0.75)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

hist(df$Value)

#####################################################

library(gamlss)
df$ID <- as.factor(df$ID)
df$Model <- as.factor(df$Model)
df$RAG <- as.factor(df$RAG)
df$Response <- as.factor(df$Response)
df_limpio <- na.omit(df[, c("Value", "Model", "RAG", "Response", "ID")])

# Ajustar el modelo ZOIB 
modelo_zoib <- gamlss(
  Value ~ Model * RAG * Response + re(random = ~ 1 | ID), 
  sigma.formula = ~ 1,                                    
  nu.formula = ~ Model * RAG * Response,                 
  tau.formula = ~ Model * RAG * Response,                 
  family = BEINF, 
  data = df_limpio
)
summary(modelo_zoib)
#reduccion
drop1(modelo_zoib, parameter = "nu", parallel = "multicore", ncpus = 4)

modelo_reducido <- gamlss(
  Value ~ (Model + RAG + Response)^2 +
    re(random = ~1|ID),
  
  sigma.formula = ~1,
  
  nu.formula  = ~(Model + RAG + Response)^2,
  
  tau.formula = ~(Model + RAG + Response)^2,
  
  family = BEINF,
  data = df_limpio
)
plot(modelo_reducido)
wp(modelo_reducido)

drop1(modelo_reducido, parameter = "nu", parallel = "multicore", ncpus = 4)

modelo_reducido2 <- gamlss(
  Value ~ (Model + RAG + Response)^2 +
    re(random = ~1|ID),
  
  sigma.formula = ~1,
  
  nu.formula = ~ Model + RAG + Response +
    Model:RAG + Model:Response,
  
  tau.formula = ~ Model + RAG + Response +
    Model:RAG + Model:Response,
  
  family = BEINF,
  data = df_limpio
)

#validacion
plot(modelo_reducido2)

residuos <- residuals(modelo_reducido2)
fitted_mu <- fitted(modelo_reducido2, parameter = "mu")

plot(fitted_mu, residuos,
     xlab = "Fitted values (mu)",
     ylab = "Quantile residuals",
     main = "Residuals vs Fitted",
     pch = 20, col = "darkgreen")

abline(h = 0, col = "red", lty = 2, lwd = 2)

qqnorm(residuos, 
       main = "Q-Q Plot of Residuals",
       pch = 20, col = "darkgreen")

qqline(residuos, col = "red", lty = 2, lwd = 2)

hist(residuos, 
     probability = TRUE, 
     main = "Residual Density",
     xlab = "Quantile residuals",
     border = "white", col = "lightblue")

lines(density(residuos), col = "darkgreen", lwd = 2)

curve(dnorm(x, mean = mean(residuos), sd = sd(residuos)), 
      add = TRUE, col = "red", lty = 2, lwd = 2)
wp(modelo_reducido2)

summary(modelo_reducido2)
