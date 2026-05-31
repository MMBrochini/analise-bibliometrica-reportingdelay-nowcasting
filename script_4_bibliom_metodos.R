# ==================================================================
# ANÁLISE DE MÉTODOS ESTATÍSTICOS NOS RESUMOS DOS ARTIGOS
# Evolução das abordagens metodológicas por período (1982-2026)
# ==================================================================

library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)

# ==================================================================
# 1. DEFINIR PERÍODOS E TERMOS PARA CLASSIFICAÇÃO DOS MÉTODOS
# ==================================================================

# Definir os períodos
dado$periodo <- NA
dado$periodo[dado$PY >= 1982 & dado$PY <= 1995] <- "1982-1995"
dado$periodo[dado$PY >= 1996 & dado$PY <= 2005] <- "1996-2005"
dado$periodo[dado$PY >= 2006 & dado$PY <= 2015] <- "2006-2015"
dado$periodo[dado$PY >= 2016 & dado$PY <= 2019] <- "2016-2019"
dado$periodo[dado$PY >= 2020 & dado$PY <= 2026] <- "2020-2026"

# Verificar distribuição
table(dado$periodo)

# ==================================================================
# 2. DICIONÁRIO DE TERMOS PARA CLASSIFICAÇÃO METODOLÓGICA
# ==================================================================

# Cada termo será buscado nos resumos (AB) e títulos (TI)

metodos_dict <- list(
 # Métodos Bayesianos
 bayesian = c("bayesian", "bayes", "hierarchical bayesian", "markov chain monte carlo", "mcmc", 
              "posterior", "prior", "bayes theorem", "inla", "integrated nested laplace",
              "dirichlet", "gibbs sampling"),
 
 # Nowcasting
 nowcasting = c("nowcasting", "nowcast", "now-casting", "now cast", "right truncation",
                "backfill", "back-filling", "occurred-but-not-yet-reported"),
 
 # Splines e suavização
 splines = c("p-spline", "pspline", "smoothing", "bivariate smoothing", "penalized spline",
             "constrained spline", "b-spline", "loess", "kernel smoothing"),
 
 # Modelos de séries temporais
 time_series = c("time series", "autoregressive", "arima", "sarima", "moving average",
                 "kalman filter", "state space", "seasonal", "trend"),
 
 # Modelos de regressão
 regression = c("regression", "linear model", "generalized linear", "glm", "logistic",
                "negative binomial", "poisson regression", "multivariate", "log-linear"),
 
 # Modelos de sobrevivência
 survival = c("survival", "cox", "proportional hazards", "incubation period",
              "kaplan-meier", "weibull"),
 
 # Machine Learning / Deep Learning
 machine_learning = c("machine learning", "neural network", "deep learning", "random forest",
                      "support vector", "svm", "gradient boosting", "xgboost", "lstm",
                      "attention", "ensemble", "adaboost", "decision tree"),
 
 # Modelos epidemiológicos (compartimentais)
 compartmental = c("sir", "seir", "compartmental", "deterministic model", "susceptible",
                   "transmission model", "reproduction number", "r0", "reproduction"),
 
 # Back-calculation (método específico)
 backcalc = c("back-calculation", "backcalculation", "back projection", "back-projection")
)

# ==================================================================
# 3. FUNÇÃO PARA CLASSIFICAR ARTIGOS POR MÉTODO
# ==================================================================

# Combinar AB e TI para busca
dado$texto_busca <- paste(dado$AB, dado$TI, sep = " ")
dado$texto_busca <- tolower(dado$texto_busca)

# Inicializar colunas de métodos
for(metodo in names(metodos_dict)) {
 dado[[paste0("metodo_", metodo)]] <- FALSE
}

# Classificar artigos
for(i in 1:nrow(dado)) {
 texto <- dado$texto_busca[i]
 if(is.na(texto)) next
 
 for(metodo in names(metodos_dict)) {
  # Verificar se algum termo do dicionário aparece no texto
  termos <- metodos_dict[[metodo]]
  for(termo in termos) {
   if(grepl(termo, texto, fixed = TRUE)) {
    dado[[paste0("metodo_", metodo)]][i] <- TRUE
    break
   }
  }
 }
}

# ==================================================================
# 4. ESTATÍSTICAS POR PERÍODO
# ==================================================================

# Função para calcular proporção de cada método por período
estatisticas_por_periodo <- function(dado, periodo_nome) {
 subset_df <- dado[dado$periodo == periodo_nome & !is.na(dado$periodo), ]
 n_artigos <- nrow(subset_df)
 
 if(n_artigos == 0) return(NULL)
 
 resultados <- data.frame(
  periodo = periodo_nome,
  n_artigos = n_artigos,
  metodo = names(metodos_dict),
  proporcao = sapply(names(metodos_dict), function(m) {
   sum(subset_df[[paste0("metodo_", m)]], na.rm = TRUE) / n_artigos * 100
  })
 )
 return(resultados)
}  # Chave adicionada

# Aplicar para todos os períodos
periodos <- c("1982-1995", "1996-2005", "2006-2015", "2016-2019", "2020-2026")
lista_resultados <- lapply(periodos, function(p) estatisticas_por_periodo(dado, p))

# Combinar resultados
resultados_metodos <- do.call(rbind, lista_resultados)


# ==================================================================
# 5. GRÁFICO DE EVOLUÇÃO DOS MÉTODOS (BARRAS LADO A LADO COM PERCENTUAIS)
# ==================================================================

# Criar pasta de resultados ANTES de salvar
if(!dir.exists("resultadosmetodos")) {
 dir.create("resultadosmetodos")
}

# Ordenar períodos
resultados_metodos$periodo <- factor(resultados_metodos$periodo, 
                                     levels = c("1982-1995", "1996-2005", "2006-2015", "2016-2019", "2020-2026"))

# Selecionar métodos para visualização (os mais relevantes)
metodos_principais <- c("bayesian", "nowcasting", "machine_learning", "time_series", 
                        "regression", "compartmental", "splines", "backcalc")

resultados_filtrados <- resultados_metodos %>%
 filter(metodo %in% metodos_principais)

# Criar coluna de rótulo: só aparece se proporção > 0
resultados_filtrados$rotulo <- ifelse(resultados_filtrados$proporcao > 0, 
                                      sprintf("%.1f%%", resultados_filtrados$proporcao), 
                                      "")
 # Rótulos dentro da barra, no topo (não sobrepõe)
 ggplot(resultados_filtrados, aes(x = periodo, y = proporcao, fill = metodo, label = rotulo)) +
 geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
 geom_text(position = position_dodge(width = 0.9), vjust = 1.2, size = 3, 
           fontface = "bold", color = "white") +  # vjust positivo = dentro
  
 geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
 geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5, 
           fontface = "bold", color = "black") +
 labs(title = "Evolução dos Métodos Estatísticos na Literatura (1982-2026)",
      subtitle = "Proporção de artigos que mencionam cada abordagem nos resumos",
      x = "Período", 
      y = "Proporção de artigos (%)",
      fill = "Método") +
 scale_fill_manual(values = c("bayesian" = "#1B4F72", 
                              "nowcasting" = "#2E86C1", 
                              "machine_learning" = "#D4AC0D",
                              "time_series" = "#28B463",
                              "regression" = "#E74C3C",
                              "compartmental" = "#8E44AD",
                              "splines" = "#D35400",
                              "backcalc" = "#5D6D7E")) +
 scale_y_continuous(limits = c(0, max(resultados_filtrados$proporcao) * 1.15)) +
 theme_minimal() +
 theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
       legend.position = "right",
       plot.title = element_text(hjust = 0.5, face = "bold"),
       plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray40"))

ggsave("resultadosmetodos/evolucao_metodos_estatisticos.png", width = 14, height = 8, dpi = 300)


# ==================================================================
# 6. GRÁFICO DE LINHAS DA EVOLUÇÃO TEMPORAL
# ==================================================================

ggplot(resultados_filtrados, aes(x = periodo, y = proporcao, color = metodo, group = metodo)) +
 geom_line(size = 1.2) +
 geom_point(size = 3) +
 labs(title = "Tendência Temporal dos Métodos Estatísticos",
      subtitle = "Crescimento de métodos bayesianos e nowcasting nas últimas décadas",
      x = "Período", 
      y = "Proporção de artigos (%)",
      color = "Método") +
 scale_color_manual(values = c("bayesian" = "#1B4F72", 
                               "nowcasting" = "#2E86C1", 
                               "machine_learning" = "#D4AC0D",
                               "time_series" = "#28B463",
                               "regression" = "#E74C3C",
                               "compartmental" = "#8E44AD",
                               "splines" = "#D35400",
                               "backcalc" = "#5D6D7E")) +
 theme_minimal() +
 theme(axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "right",
       plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("resultadosmetodos/tendencia_metodos_estatisticos.png", width = 12, height = 7, dpi = 300)

# ==================================================================
# 7. TABELA DE RESULTADOS (PARA O ARTIGO)
# ==================================================================

# Formatar tabela
tabela_metodos <- resultados_metodos %>%
 filter(metodo %in% metodos_principais) %>%
 tidyr::pivot_wider(id_cols = periodo, names_from = metodo, values_from = proporcao) %>%
 mutate(across(where(is.numeric), ~ round(., 1)))

# Salvar tabela
write.csv(tabela_metodos, "resultadosmetodos/tabela_metodos_por_periodo.csv", row.names = FALSE)

# ==================================================================
# 8. ANÁLISE DE PERFIS SIMILARES (CLUSTERING DOS MÉTODOS)
# ==================================================================

# Criar matriz de perfis metodológicos (artigos x métodos)
matriz_metodos <- dado[, paste0("metodo_", metodos_principais)]

# Calcular distância entre artigos
dist_artigos <- dist(matriz_metodos, method = "binary")

# Clustering hierárquico (identificar perfis semelhantes)
hc <- hclust(dist_artigos, method = "ward.D2")

# Identificar 4 clusters de perfis metodológicos
clusters <- cutree(hc, k = 4)

# Adicionar cluster ao dataframe
dado$cluster_metodologico <- clusters

# Ver distribuição dos clusters por período
tabela_clusters <- table(dado$periodo, dado$cluster_metodologico)
print(tabela_clusters)

# ==================================================================
# 9. TERMOS MAIS FREQUENTES POR PERÍODO (MÉTODOS)
# ==================================================================

# Tokenizar e contar termos dos métodos
palavras_titulo <- dado %>%
 filter(!is.na(TI)) %>%
 select(periodo, TI) %>%
 unnest_tokens(word, TI) %>%
 filter(word %in% unlist(metodos_dict)) %>%
 group_by(periodo, word) %>%
 summarise(freq = n(), .groups = "drop") %>%
 group_by(periodo) %>%
 slice_max(freq, n = 5)

cat("\n📊 TERMOS METODOLÓGICOS MAIS FREQUENTES POR PERÍODO:\n")
print(palavras_titulo)

# ==================================================================
# 10. EXPORTAR RESULTADOS COMPLETOS
# ==================================================================

# Salvar dados classificados
write.csv(dado[, c("PY", "TI", "periodo", paste0("metodo_", metodos_principais), "cluster_metodologico")],
          "resultadosmetodos/classificacao_metodos_artigos.csv", row.names = FALSE)

# Criar relatório resumo
sink("resultadosmetodos/resumo_metodos_estatisticos.txt")

cat("RELATÓRIO DE ANÁLISE DE MÉTODOS ESTATÍSTICOS\n")
cat("=============================================\n\n")
cat("Data da análise:", Sys.Date(), "\n\n")

cat("DISTRIBUIÇÃO DOS ARTIGOS POR PERÍODO:\n")
print(table(dado$periodo))

cat("\n\nPROPORÇÃO DE MÉTODOS POR PERÍODO (%):\n")
print(tabela_metodos)

cat("\n\nDISTRIBUIÇÃO DOS CLUSTERS METODOLÓGICOS POR PERÍODO:\n")
print(tabela_clusters)

sink()

cat("\n✅ ARQUIVOS GERADOS:\n")
cat("   📊 evolucao_metodos_estatisticos.png - Gráfico de barras\n")
cat("   📊 tendencia_metodos_estatisticos.png - Gráfico de linhas\n")
cat("   📄 tabela_metodos_por_periodo.csv - Tabela de proporções\n")
cat("   📄 classificacao_metodos_artigos.csv - Classificação por artigo\n")
cat("   📄 resumo_metodos_estatisticos.txt - Relatório completo\n")

