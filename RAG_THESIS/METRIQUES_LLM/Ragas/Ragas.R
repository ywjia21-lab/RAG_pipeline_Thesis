library(dplyr)
# lectura de datos

resultados_RAG_tiny   <- read.csv("resultados_RAG_tiny.csv")
resultados_RAG_qwen   <- read.csv("resultados_RAG_qwen.csv")
resultados_RAG_large  <- read.csv("resultados_RAG_large.csv")
resultados_RAG_gemma2 <- read.csv("resultados_RAG_gemma2.csv")
resultados_RAG_flan_xl <- read.csv("resultados_RAG_flan_xl.csv")

resultados_NORAG_tiny   <- read.csv("resultados_NORAG_tiny.csv")
resultados_NORAG_qwen   <- read.csv("resultados_NORAG_qwen.csv")
resultados_NORAG_large  <- read.csv("resultados_NORAG_large.csv")
resultados_NORAG_gemma2 <- read.csv("resultados_NORAG_gemma2.csv")
resultados_NORAG_flan_xl <- read.csv("resultados_NORAG_flan_xl.csv")

resultados_RAG_tiny   <- resultados_RAG_tiny   %>% mutate(id = 1:136)
resultados_RAG_qwen   <- resultados_RAG_qwen   %>% mutate(id = 1:136)
resultados_RAG_large  <- resultados_RAG_large  %>% mutate(id = 1:136)
resultados_RAG_gemma2 <- resultados_RAG_gemma2 %>% mutate(id = 1:136)
resultados_RAG_flan_xl <- resultados_RAG_flan_xl %>% mutate(id = 1:136)

resultados_NORAG_tiny   <- resultados_NORAG_tiny   %>% mutate(id = 1:136)
resultados_NORAG_qwen   <- resultados_NORAG_qwen   %>% mutate(id = 1:136)
resultados_NORAG_large  <- resultados_NORAG_large  %>% mutate(id = 1:136)
resultados_NORAG_gemma2 <- resultados_NORAG_gemma2 %>% mutate(id = 1:136)
resultados_NORAG_flan_xl <- resultados_NORAG_flan_xl %>% mutate(id = 1:136)

datos <- bind_rows(
  resultados_RAG_tiny  %>% mutate(model="tiny", metodo="RAG"),
  resultados_RAG_qwen  %>% mutate(model="qwen", metodo="RAG"),
  resultados_RAG_large %>% mutate(model="large", metodo="RAG"),
  resultados_RAG_gemma2 %>% mutate(model="gemma2", metodo="RAG"),
  resultados_RAG_flan_xl %>% mutate(model="flan_xl", metodo="RAG"),
  
  resultados_NORAG_tiny  %>% mutate(model="tiny", metodo="NORAG"),
  resultados_NORAG_qwen  %>% mutate(model="qwen", metodo="NORAG"),
  resultados_NORAG_large %>% mutate(model="large", metodo="NORAG"),
  resultados_NORAG_gemma2 %>% mutate(model="gemma2", metodo="NORAG"),
  resultados_NORAG_flan_xl %>% mutate(model="flan_xl", metodo="NORAG")
)

datos$faithfulness<-NULL

df_faith <- bind_rows(
  resultados_RAG_tiny   %>% mutate(model = "tiny"),
  resultados_RAG_qwen   %>% mutate(model = "qwen"),
  resultados_RAG_large  %>% mutate(model = "large"),
  resultados_RAG_gemma2 %>% mutate(model = "gemma2"),
  resultados_RAG_flan_xl %>% mutate(model = "flan_xl")
) %>%
  dplyr::select(model, faithfulness,id)

########################################## estadistica descriptiva

datos %>%
  group_by(model, metodo) %>%
  summarise(
    relevancy_mean = mean(answer_relevancy, na.rm=TRUE),
    corr_SA_mean = mean(answer_correctness_SA, na.rm=TRUE),
    corr_LA_mean = mean(answer_correctness_LA, na.rm=TRUE)
  )
# boxplot de las metricas
library(ggplot2)

ggplot(datos,
       aes(x=model,
           y=answer_relevancy,
           fill=metodo)) +
  geom_boxplot()

ggplot(datos,
       aes(x=model,
           y=answer_correctness_SA,
           fill=metodo)) +
  geom_boxplot()

ggplot(datos,
       aes(x=model,
           y=answer_correctness_LA,
           fill=metodo)) +
  geom_boxplot()

ggplot(df_faith,
       aes(x = model,
           y = faithfulness)) +
  geom_boxplot()

ggplot(df_faith, aes(x = faithfulness)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ model, scales = "fixed")

df_faith$faith_bin <- cut(df_faith$faithfulness,
                          breaks = seq(0, 1, by = 0.2),
                          include.lowest = TRUE)
table(df_faith$model, df_faith$faith_bin)

############  histograma

#### asnwer relevancy
#junto
ggplot(datos, aes(x = answer_relevancy)) +
  geom_histogram(bins = 20)
# por grupo
ggplot(datos, aes(x = answer_relevancy)) +
  geom_histogram(bins = 20) +
  facet_grid(metodo ~ model)
# normal?
ggplot(datos, aes(sample = answer_relevancy)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(metodo ~ model)

########### asnwer correctness SA

ggplot(datos, aes(x = answer_correctness_SA)) +
  geom_histogram(bins = 20)

ggplot(datos, aes(x = answer_correctness_SA)) +
  geom_histogram(bins = 20) +
  facet_grid(metodo ~ model)

ggplot(datos, aes(sample = answer_correctness_SA)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(metodo ~ model)

############## ASNWER CORRECNESS la

ggplot(datos, aes(x = answer_correctness_LA)) +
  geom_histogram(bins = 20)

ggplot(datos, aes(x = answer_correctness_LA)) +
  geom_histogram(bins = 20) +
  facet_grid(metodo ~ model)

ggplot(datos, aes(sample = answer_correctness_LA)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(metodo ~ model)

#### PARACE QUE MUY MAL la normalidad

#####################################

tiny   <- resultados_RAG_tiny   %>% select(id, answer_correctness_SA)
qwen   <- resultados_RAG_qwen   %>% select(id, answer_correctness_SA)
large  <- resultados_RAG_large  %>% select(id, answer_correctness_SA)
gemma2 <- resultados_RAG_gemma2 %>% select(id, answer_correctness_SA)
flan   <- resultados_RAG_flan_xl %>% select(id, answer_correctness_SA)

tiny   <- rename(tiny, tiny = answer_correctness_SA)
qwen   <- rename(qwen, qwen = answer_correctness_SA)
large  <- rename(large, large = answer_correctness_SA)
gemma2 <- rename(gemma2, gemma2 = answer_correctness_SA)
flan   <- rename(flan, flan = answer_correctness_SA)

df_rag_sa <- tiny %>%
  full_join(qwen,   by = "id") %>%
  full_join(large,  by = "id") %>%
  full_join(gemma2, by = "id") %>%
  full_join(flan,   by = "id") %>%
  arrange(id)

mat_rag_sa <- as.matrix(df_rag_sa[,-1])

friedman.test(mat_rag_sa)

df <- bind_rows(
  resultados_RAG_tiny   %>% select(id, answer_correctness_SA) %>% mutate(model="Tiny"),
  resultados_RAG_qwen   %>% select(id, answer_correctness_SA) %>% mutate(model="Qwen"),
  resultados_RAG_large  %>% select(id, answer_correctness_SA) %>% mutate(model="Flan_large"),
  resultados_RAG_gemma2 %>% select(id, answer_correctness_SA) %>% mutate(model="Gemma2"),
  resultados_RAG_flan_xl%>% select(id, answer_correctness_SA) %>% mutate(model="Flan_XL")
)

library(PMCMRplus)

frdAllPairsExactTest(
  y = df$answer_correctness_SA,
  groups = df$model,
  blocks = df$id,
  p.adjust.method = "holm"
)
# funcion general per a tos
run_friedman <- function(data_list, metric_name) {
  
  df <- bind_rows(
    data_list$tiny   %>% select(id, all_of(metric_name)) %>% mutate(model="Tiny"),
    data_list$qwen   %>% select(id, all_of(metric_name)) %>% mutate(model="Qwen"),
    data_list$large  %>% select(id, all_of(metric_name)) %>% mutate(model="Flan_Large"),
    data_list$gemma2 %>% select(id, all_of(metric_name)) %>% mutate(model="Gemma2"),
    data_list$flan   %>% select(id, all_of(metric_name)) %>% mutate(model="Flan_XL")
  )
  
  colnames(df)[2] <- "value"
  
  cat("\n====================\n")
  cat("METRIC:", metric_name, "\n")
  
  # -------------------------
  # 1. FRIEDMAN GLOBAL TEST
  # -------------------------
  fried <- friedman.test(value ~ model | id, data = df)
  print(fried)
  
  # -------------------------
  # 2. POST-HOC SI SIGNIFICATIVO
  # -------------------------
  if (fried$p.value < 0.05) {
    
    cat("\nPOST-HOC (Holm corrected)\n")
    
    post <- frdAllPairsExactTest(
      y = df$value,
      groups = df$model,
      blocks = df$id,
      p.adjust.method = "holm"
    )
    
    print(post)
  }
}

rag <- list(
  tiny   = resultados_RAG_tiny,
  qwen   = resultados_RAG_qwen,
  large  = resultados_RAG_large,
  gemma2 = resultados_RAG_gemma2,
  flan   = resultados_RAG_flan_xl
)

run_friedman(rag, "answer_relevancy")
run_friedman(rag, "answer_correctness_SA")
run_friedman(rag, "answer_correctness_LA")

norag <- list(
  tiny   = resultados_NORAG_tiny,
  qwen   = resultados_NORAG_qwen,
  large  = resultados_NORAG_large,
  gemma2 = resultados_NORAG_gemma2,
  flan   = resultados_NORAG_flan_xl
)

run_friedman(norag, "answer_relevancy")
run_friedman(norag, "answer_correctness_SA")
run_friedman(norag, "answer_correctness_LA")

###################################
# dentro de asnwer relevancy entre rag y no rag sewgun modelo

library(dplyr)
library(tidyr)

compare_rag_norag_model <- function(metric_name) {
  
  models <- c("tiny", "qwen", "large", "gemma2", "flan_xl")
  
  p_values <- c()
  
  for (m in models) {
    
    rag_data <- get(paste0("resultados_RAG_", m))
    norag_data <- get(paste0("resultados_NORAG_", m))
    
    df <- bind_rows(
      rag_data   %>% select(id, all_of(metric_name)) %>% mutate(method = "RAG"),
      norag_data %>% select(id, all_of(metric_name)) %>% mutate(method = "NORAG")
    ) %>%
      rename(value = all_of(metric_name)) %>%
      filter(!is.na(value))
    
    wide <- df %>%
      pivot_wider(names_from = method, values_from = value) %>%
      drop_na()
    
    test <- wilcox.test(wide$RAG, wide$NORAG, paired = TRUE)
    
    p_values <- c(p_values, test$p.value)
  }
  
  # corrección Holm
  p_adjusted <- p.adjust(p_values, method = "holm")
  
  # tabla final
  results <- data.frame(
    model = models,
    p_value = p_values,
    p_adjusted_holm = p_adjusted,
    significant = p_adjusted < 0.05
  )
  
  return(results)
}
compare_rag_norag_model("answer_relevancy")
#####################################

# correlacion de las metriacs entre modelos y escenarios
library(tidyr)
library(dplyr)

analizar_correlacion <- function(data, metrica, metodo_name) {
  
  wide <- data %>%
    dplyr::filter(metodo == metodo_name) %>%
    dplyr::select(id, model, all_of(metrica)) %>%
    tidyr::pivot_wider(names_from = model,
                       values_from = all_of(metrica))
  
  corr <- cor(wide[, -1], method = "spearman", use = "complete.obs")
  
  print(corr)
  
  corrplot::corrplot(corr,
                     method = "color",
                     tl.cex = 0.8,
                     title = paste(metrica, "-", metodo_name))
}

analizar_correlacion(datos, "answer_correctness_SA", "RAG")
analizar_correlacion(datos, "answer_correctness_SA", "NORAG")
analizar_correlacion(datos, "answer_correctness_LA", "RAG")
analizar_correlacion(datos, "answer_correctness_LA", "NORAG")
analizar_correlacion(datos, "answer_relevancy", "RAG")
analizar_correlacion(datos, "answer_relevancy", "NORAG")