#===================================
# Análise de sentimento Acadêmico
#===================================

#------------------------------------
#Processamento dos dados com AB e PY
#------------------------------------
library(tidyverse)
library(tidytext)
library(stringr)
library(stopwords)

# Filtrar dados válidos
dados_texto <- dado %>%
 filter(!is.na(AB), AB != "") %>%
 select(PY, AB)

# Tokenizar (quebrar em palavras)
tokens <- dados_texto %>%
 unnest_tokens(word, AB)

#stopwords
stopwords_finais <- unique(c(
 stopwords("en"),
 "study", "studies", "paper", "articles", "research",
 "analysis", "using", "based", "approach", "method",
 "methods", "results", "data", "model", "models"
))

# Limpeza básica
tokens <- tokens %>%
 filter(str_length(word) >= 4) %>%
 filter(!word %in% stopwords_finais)

#----------------------
# Dicionário adaptado
#----------------------
# Termos positivos (avanço, eficiência, melhora)
positivo <- c(
 "accurate", "robust", "efficient", "improved", "reliable",
 "timely", "effective", "real-time", "predictive", "optimized",
 "early", "enhanced", "better", "strong", "consistent"
)

# Termos negativos (problemas, limitações)
negativo <- c("error", "errors", "limitation", "limitations",
 "underreporting", "missing", "incomplete", "lag",
 "instability", "noise", "poor"
)

# Criar dicionário
dicionario <- data.frame(
 word = c(positivo, negativo),
 sentimento = c(rep("positivo", length(positivo)),
                rep("negativo", length(negativo)))
)
 
#--------------------------
# Análise de sentimento
#--------------------------
tokens_sentimento <- tokens %>%
 inner_join(dicionario, by = "word")

# Contagem por artigo/ano
sentimento_ano <- tokens_sentimento %>%
 count(PY, sentimento) %>%
 pivot_wider(names_from = sentimento, values_from = n, values_fill = 0)

# Criar índice de tom científico
sentimento_ano <- sentimento_ano %>%
 mutate(
  score = positivo - negativo,
  proporcao = score / (positivo + negativo + 1)
 )

#------------------------------
# Evolução de tom científico
#------------------------------

grafico_sentimento <- ggplot(sentimento_ano, aes(x = PY, y = proporcao)) +
 geom_line(linewidth = 1, color = "steelblue")+
 geom_point(size = 2) +
 labs(
  title = "Evolução do Tom Científico na Literatura",
  subtitle = "Baseado em abstracts (1982–2026)",
  x = "Ano",
  y = "Índice de Tom (positivo - negativo)"
 ) +
 theme_minimal()

print(grafico_sentimento)

ggsave("evolucao_sentimento.png", grafico_sentimento,
       width = 10, height = 6, dpi = 300)

#gráfico suavizado

library(ggplot2)
library(zoo)

# 1. Garantir ordenação por ano
sentimento_ano <- sentimento_ano[order(sentimento_ano$PY), ]

# 2. Criar média móvel (mantendo bordas)
sentimento_ano$media_movel <- rollapply(
 sentimento_ano$proporcao,
 width = 5,
 FUN = mean,
 fill = NA,
 partial = TRUE
)

# 3. Criar gráfico
grafico_sentimento_suavizado <- ggplot(sentimento_ano, aes(x = PY)) +
 
 # Linha original (transparente - mostra ruído)
 geom_line(aes(y = proporcao),
           linewidth = 0.6,
           alpha = 0.4,
           color = "gray50") +
 
 # Linha suavizada (principal)
 geom_line(aes(y = media_movel),
           linewidth = 1.2,
           color = "steelblue") +
 
 # Pontos da média móvel
 geom_point(aes(y = media_movel),
            size = 2,
            color = "steelblue") +
 
 labs(
  title = "Evolução do Tom Científico na Literatura",
  subtitle = "Série suavizada (média móvel de 5 anos)",
  x = "Ano",
  y = "Índice de Tom (positivo - negativo)"
 ) +
 
 theme_minimal() +
 theme(
  plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
  plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"),
  axis.text = element_text(size = 10),
  axis.title = element_text(face = "bold"),
  panel.grid.minor = element_blank()
 )

# 4. Exibir gráfico
print(grafico_sentimento_suavizado)

# 5. Salvar em alta qualidade
ggsave("sentimento_suavizado.png",
       grafico_sentimento_suavizado,
       width = 10,
       height = 6,
       dpi = 300)

# ==================================================================
# ANÁLISE DE SENTIMENTO ACADÊMICO
# Baseada no léxico de Bastos et al. (2019) - Statistics in Medicine
# ==================================================================

library(dplyr)
library(tidytext)
library(ggplot2)
library(writexl)  # Para exportar direto para Excel (sem problemas de decimal)

# ==================================================================
# 1. DICIONÁRIO DE SENTIMENTOS
# ==================================================================

palavras_positivas <- c(
 "flexible", "reliable", "accurate", "robust", "efficient",
 "faster", "earlier", "timeliness", "fast", "rapid",
 "useful", "valuable", "important", "crucial", "successful",
 "beneficial", "advantage", "promising",
 "predictive", "nowcasting", "forecasting", "warning",
 "novel", "innovative"
)

palavras_negativas <- c(
 "censoring", "missing", "unreported", "underreported",
 "limitation", "difficulty", "problem", "challenge", "uncertainty",
 "poor", "worse", "misinformed", "misclassification",
 "complex", "difficult", "saturation",
 "uncertain", "unknown", "unobserved", "approximate"
)

sentimentos_academicos <- tibble(
 word = c(palavras_positivas, palavras_negativas),
 sentiment = c(rep("positive", length(palavras_positivas)),
               rep("negative", length(palavras_negativas)))
)

cat("✅ Dicionário carregado:", nrow(sentimentos_academicos), "termos\n")
cat("   - Positivos:", length(palavras_positivas), "\n")
cat("   - Negativos:", length(palavras_negativas), "\n")

# ==================================================================
# 2. PREPARAR TEXTOS
# ==================================================================

textos <- data.frame(
 id = 1:nrow(dado),
 ano = dado$PY,
 resumo = dado$AB,
 stringsAsFactors = FALSE
) %>% filter(!is.na(resumo) & resumo != "")

cat("📄 Artigos analisados:", nrow(textos), "\n")

# ==================================================================
# 3. TOKENIZAR E ANALISAR
# ==================================================================

palavras <- textos %>%
 unnest_tokens(word, resumo) %>%
 inner_join(sentimentos_academicos, by = "word")

if(nrow(palavras) > 0) {
 
 # ==================================================================
 # 4. SCORE POR ARTIGO
 # ==================================================================
 
 score_artigo <- palavras %>%
  group_by(id, ano) %>%
  summarise(
   positive = sum(sentiment == "positive"),
   negative = sum(sentiment == "negative"),
   .groups = "drop"
  ) %>%
  mutate(
   score = positive - negative,
   tom = case_when(
    score > 0 ~ "Positivo",
    score < 0 ~ "Negativo",
    TRUE ~ "Neutro"
   )
  )
 
 # ==================================================================
 # 5. EVOLUÇÃO ANUAL (CORRIGIDA)
 # ==================================================================
 
 sentimento_anual <- score_artigo %>%
  group_by(ano) %>%
  summarise(
   indice_tom = round(mean(score, na.rm = TRUE), 3),
   prop_positivo = round(mean(tom == "Positivo", na.rm = TRUE), 3),
   prop_negativo = round(mean(tom == "Negativo", na.rm = TRUE), 3),
   n_artigos = n(),
   .groups = "drop"
  ) %>%
  filter(ano >= 1982, ano <= 2026) %>%
  arrange(ano)
 
 # ==================================================================
 # 6. DISTRIBUIÇÃO POR PERÍODO
 # ==================================================================
 
 score_artigo$periodo <- cut(score_artigo$ano, 
                             breaks = c(1981, 1995, 2005, 2015, 2026),
                             labels = c("1982-1995", "1996-2005", "2006-2015", "2016-2026"))
 
 sentimento_periodo <- score_artigo %>%
  group_by(periodo) %>%
  summarise(
   N = n(),
   indice_medio = round(mean(score, na.rm = TRUE), 3),
   prop_positivo = round(mean(tom == "Positivo", na.rm = TRUE) * 100, 1),
   prop_negativo = round(mean(tom == "Negativo", na.rm = TRUE) * 100, 1),
   prop_neutro = round(mean(tom == "Neutro", na.rm = TRUE) * 100, 1),
   .groups = "drop"
  )
 
 # ==================================================================
 # 7. TESTE DE TENDÊNCIA
 # ==================================================================
 
 correlacao <- cor.test(score_artigo$ano, score_artigo$score, method = "spearman")
 
 # ==================================================================
 # 8. GRÁFICO PRINCIPAL
 # ==================================================================
 
 p <- ggplot(sentimento_anual, aes(x = ano, y = indice_tom)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkred", size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "orange", 
              linetype = "dotted", size = 0.8, alpha = 0.3) +
  labs(title = "Índice de Tom Acadêmico (1982-2026)",
       subtitle = paste("Baseado no léxico de Bastos et al. (2019) | ρ =", 
                        round(correlacao$estimate, 3), "| p < 0,001"),
       x = "Ano", 
       y = "Índice de Tom (positivo - negativo)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray40"))
 
 print(p)
 
 # ==================================================================
 # 9. GRÁFICO DE BARRAS POR PERÍODO
 # ==================================================================
 
 p2 <- ggplot(sentimento_periodo, aes(x = periodo, y = indice_medio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(indice_medio, 2)), vjust = -0.5, size = 5) +
  labs(title = "Evolução do Índice de Tom por Período",
       x = "Período", 
       y = "Índice de Tom (médio)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
 
 print(p2)
 
 # ==================================================================
 # 10. TERMOS MAIS FREQUENTES
 # ==================================================================
 
 termos_freq <- palavras %>%
  group_by(sentiment, word) %>%
  summarise(freq = n(), .groups = "drop") %>%
  group_by(sentiment) %>%
  slice_max(freq, n = 10) %>%
  arrange(sentiment, desc(freq))
 
 cat("\n📊 TERMOS MAIS FREQUENTES:\n")
 print(termos_freq)
 
 # ==================================================================
 # 11. SAÍDA NO CONSOLE
 # ==================================================================
 
 cat("\n", paste(rep("=", 60), collapse = ""), "\n")
 cat("📊 RESULTADOS DA ANÁLISE DE SENTIMENTO\n")
 cat(paste(rep("=", 60), collapse = ""), "\n")
 
 cat("\n📈 ESTATÍSTICAS GERAIS:\n")
 cat("   Total de artigos analisados:", nrow(score_artigo), "\n")
 cat("   Total de termos analisados:", nrow(palavras), "\n")
 cat("   Média de termos por artigo:", round(mean(score_artigo$positive + score_artigo$negative), 2), "\n")
 
 cat("\n📊 TESTE DE TENDÊNCIA:\n")
 cat("   Correlação de Spearman:", round(correlacao$estimate, 3), "\n")
 cat("   p-valor:", format(correlacao$p.value, scientific = TRUE), "\n")
 cat("   ", if(correlacao$p.value < 0.05) "✅ Tendência significativa (p < 0,05)" else "❌ Tendência não significativa", "\n")
 
 cat("\n📊 DISTRIBUIÇÃO POR PERÍODO:\n")
 print(sentimento_periodo)
 
 # ==================================================================
 # 12. EXPORTAR RESULTADOS (FORMATO CORRETO)
 # ==================================================================
 
 # Criar pasta
 if(!dir.exists("resultados")) dir.create("resultados")
 
 # Salvar gráficos
 ggsave("resultados/indice_tom_academico.png", p, width = 12, height = 6, dpi = 300)
 ggsave("resultados/indice_tom_periodo.png", p2, width = 8, height = 6, dpi = 300)
 
 # Exportar para EXCEL (sem problemas de decimal)
 write_xlsx(
  list(
   sentimento_por_artigo = score_artigo,
   sentimento_anual = sentimento_anual,
   sentimento_periodo = sentimento_periodo,
   termos_frequentes = termos_freq,
   estatisticas = data.frame(
    indicador = c("Total_artigos", "Total_termos", "Media_termos_por_artigo",
                  "Correlacao_Spearman", "p_valor", "Tendencia_significativa"),
    valor = c(nrow(score_artigo), nrow(palavras),
              round(mean(score_artigo$positive + score_artigo$negative), 2),
              round(correlacao$estimate, 3), format(correlacao$p.value, scientific = TRUE),
              ifelse(correlacao$p.value < 0.05, "Sim", "Nao"))
   )
  ),
  "resultados/analise_sentimento_COMPLETA.xlsx"
 )
 
 # Exportar também como CSV (com separador correto para o Brasil)
 write.csv2(score_artigo, "resultados/sentimento_por_artigo.csv", row.names = FALSE)
 write.csv2(sentimento_anual, "resultados/sentimento_anual.csv", row.names = FALSE)
 write.csv2(sentimento_periodo, "resultados/sentimento_periodo.csv", row.names = FALSE)
 write.csv2(termos_freq, "resultados/termos_frequentes.csv", row.names = FALSE)
 
 cat("\n", paste(rep("=", 60), collapse = ""), "\n")
 cat("✅ ARQUIVOS GERADOS NA PASTA 'resultados/':\n")
 cat(paste(rep("=", 60), collapse = ""), "\n")
 cat("📊 analise_sentimento_COMPLETA.xlsx - TODAS as tabelas em Excel\n")
 cat("📄 sentimento_por_artigo.csv - Score por artigo\n")
 cat("📄 sentimento_anual.csv - Média anual\n")
 cat("📄 sentimento_periodo.csv - Média por período\n")
 cat("📄 termos_frequentes.csv - Termos mais frequentes\n")
 cat("🖼️ indice_tom_academico.png - Gráfico de tendência\n")
 cat("🖼️ indice_tom_periodo.png - Gráfico de barras por período\n")
 
} else {
 cat("❌ Nenhum termo do dicionário encontrado nos resumos.\n")
 cat("   Verifique se os resumos estão em inglês.\n")
}
