# ============================================
# SCRIPT - ANÁLISES BIBLIOMÉTRICAS - artigo 1
# ============================================

# Carregando pacotes
library(ggplot2)
library(dplyr)
library(bibliometrix)
library(igraph)
library(reshape2)
library(tm)
library(pheatmap)
library(tidyr)
library(ggraph)
library(tidygraph)
library(patchwork)

# ============================================
# 1. CARREGANDO E CONVERTENDO OS DADOS
# ============================================

# WOS
data_1 <- ("savedrecs_web_of_science.bib")
wos <- convert2df(file = data_1, dbsource = "isi", format = "bibtex")

# Scopus (CSV)
data_2 <- ("scopus_export_Apr_26-2026_1_100.csv")
data_2b <- ("scopus_export_Apr_26-2026_101_134.csv")
scop <- convert2df(file = data_2, dbsource = "scopus", format = "csv")
scop_b <- convert2df(file = data_2b, dbsource = "scopus", format = "csv")

# Pubmed
data_3 <- ("pubmed-reportingd-set_17.txt")
data_3b <- ("pubmed-reportingd-set_29.txt")
data_3c <- ("pubmed-reportingd-set.txt")
pubm <- convert2df(file = data_3, dbsource = "pubmed", format = "pubmed")
pubm_b <- convert2df(file = data_3b, dbsource = "pubmed", format = "pubmed")
pubm_c <- convert2df(file = data_3c, dbsource = "pubmed", format = "pubmed")

# Merge das bases
dado <- bibliometrix::mergeDbSources(wos, scop, scop_b, pubm, pubm_b, pubm_c,
                                     remove.duplicated = TRUE)
cat("✅ Merge concluído! Duplicatas removidas.\n")

# Salvar o banco
saveRDS(dado, "resultados/dados_classificados.rds")

# ============================================
# 2. ANÁLISE BIBLIOMÉTRICA BÁSICA
# ============================================

results <- biblioAnalysis(dado, sep = ";")
print(results)

# Verificar campo de referências
names(dado)
cat("Artigos com referências:", sum(!is.na(dado$CR) & dado$CR != ""), "\n")

# ============================================
# 3. REDES
# ============================================

# Rede de palavras-chave
NetMatrix_keywords <- biblioNetwork(dado, 
                                    analysis = "co-occurrences", 
                                    network = "keywords", 
                                    sep = ";")

# Rede de autores
NetMatrix_authors <- biblioNetwork(dado, 
                                   analysis = "collaboration", 
                                   network = "authors")

# Rede de afiliações
NetMatrix_aff <- biblioNetwork(dado, 
                               analysis = "collaboration", 
                               network = "universities")

# Plot rede de palavras-chave (top 20)
net <- networkPlot(NetMatrix_keywords, 
                   n = 20,
                   Title = "Co-occurrence of Keywords", 
                   type = "fruchterman", 
                   size.cex = TRUE,
                   remove.multiple = TRUE)

# ============================================
# 4. ANÁLISE TEMPORAL
# ============================================

# Verificar distribuição por ano
min(dado$PY, na.rm = TRUE)  # 1982
max(dado$PY, na.rm = TRUE)  # 2026

# Anos com poucos artigos
anos_fracos <- names(table(dado$PY)[table(dado$PY) < 5])
if(length(anos_fracos) > 0) {
 cat("Anos com menos de 5 artigos:", paste(anos_fracos, collapse=", "), "\n")
}

#sequência de anos

sort(unique(dado$PY))

# Gráfico de distribuição anual
barplot(table(dado$PY), 
        main = "Artigos por Ano (1982-2026)",
        xlab = "Ano", 
        ylab = "Nº de artigos",
        col = "steelblue")

# ============================================
# 5. DEFINIÇÃO DE PERÍODOS
# ============================================

dado$periodo <- NA
dado$periodo[dado$PY >= 1982 & dado$PY <= 1995] <- "1982_1995"
dado$periodo[dado$PY >= 1996 & dado$PY <= 2005] <- "1996_2005"
dado$periodo[dado$PY >= 2006 & dado$PY <= 2015] <- "2006_2015"
dado$periodo[dado$PY >= 2016 & dado$PY <= 2019] <- "2016_2019"
dado$periodo[dado$PY >= 2020 & dado$PY <= 2026] <- "2020_2026"

cat("\n=== DISTRIBUIÇÃO DOS PERÍODOS ===\n") 
print(table(dado$periodo))

# ============================================
# 6. ANÁLISE DE EVOLUÇÃO TEMÁTICA (RESUMOS)
# ============================================

# Stopwords
stopwords_ab <- c(
 "the", "and", "for", "with", "this", "that", "these", "those", 
 "are", "was", "were", "been", "can", "will", "may", "also",
 "from", "have", "has", "had", "but", "not", "all", "any",
 "they", "their", "them", "she", "he", "it", "its", "we", "our",
 "you", "your", "what", "which", "who", "whom", "where", "when",
 "why", "how", "then", "than", "such", "both", "each", "other",
 "some", "more", "most", "few", "very", "just", "only", "about",
 "after", "before", "into", "through", "during", "without", "upon",
 "then", "now", "first", "second", "third", "well",
 "use", "used", "using", "study", "studies", "research", "paper", 
 "article", "result", "results", "method", "methods", "analysis",
 "data", "conclusion", "objective", "purpose", "background",
 "introduction", "discussion", "section", "figure", "table",
 "appendix", "material", "provides", "providing", "provided",
 "describe", "describes", "described", "show", "shows", "showed",
 "shown", "demonstrate", "demonstrates", "demonstrated", "suggest",
 "suggests", "suggested", "indicate", "indicates", "indicated",
 "find", "finds", "found", "observed", "observe",
 "based", "approach", "time", "model", "health", "public",
 "estimate", "estimates", "level", "levels", "rate", "rates", "case",
 "disease", "number", "between", "among", "states", "cancer", "trends",
 "years"
)

# Função para extrair termos
get_terms_from_abstracts <- function(df, periodo_nome, stopwords, n_terms = 30) {
 subset_df <- df[df$periodo == periodo_nome, ]
 if(nrow(subset_df) == 0) return(NULL)
 
 abstracts <- subset_df$AB[!is.na(subset_df$AB) & subset_df$AB != ""]
 if(length(abstracts) == 0) return(NULL)
 
 textos <- paste(abstracts, collapse = " ")
 textos <- tolower(textos)
 textos <- gsub("[[:punct:]]", " ", textos)
 textos <- gsub("[0-9]", " ", textos)
 textos <- gsub("\\s+", " ", textos)
 
 palavras <- unlist(strsplit(textos, " "))
 palavras <- palavras[nchar(palavras) >= 4]
 palavras <- palavras[!palavras %in% stopwords]
 palavras <- palavras[palavras != ""]
 
 freq <- sort(table(palavras), decreasing = TRUE)
 freq <- head(freq, n_terms)
 
 return(data.frame(
  Periodo = periodo_nome,
  Termo = names(freq),
  Frequencia = as.numeric(freq),
  stringsAsFactors = FALSE
 ))
}

# Extrair termos por período
cat("\n=== EXTRAINDO TERMOS DOS RESUMOS ===\n")
termos_1982_1995 <- get_terms_from_abstracts(dado, "1982_1995", stopwords_ab, 30)
termos_1996_2005 <- get_terms_from_abstracts(dado, "1996_2005", stopwords_ab, 30)
termos_2006_2015 <- get_terms_from_abstracts(dado, "2006_2015", stopwords_ab, 30)
termos_2016_2019 <- get_terms_from_abstracts(dado, "2016_2019", stopwords_ab, 30)
termos_2020_2026 <- get_terms_from_abstracts(dado, "2020_2026", stopwords_ab, 30)

# Agrupar sinônimos
agrupar_sinonimos <- function(df) {
 if(is.null(df)) return(NULL)
 df$Termo[df$Termo == "delays"] <- "delay"
 aggregate(Frequencia ~ Periodo + Termo, data = df, FUN = sum)
}

termos_1982_1995 <- agrupar_sinonimos(termos_1982_1995)
termos_1996_2005 <- agrupar_sinonimos(termos_1996_2005)
termos_2006_2015 <- agrupar_sinonimos(termos_2006_2015)
termos_2016_2019 <- agrupar_sinonimos(termos_2016_2019)
termos_2020_2026 <- agrupar_sinonimos(termos_2020_2026)

# Selecionar top 15 termos
todos_termos <- c()
for(df in list(termos_1982_1995, termos_1996_2005, termos_2006_2015, termos_2016_2019, termos_2020_2026)) {
 if(!is.null(df)) todos_termos <- c(todos_termos, as.character(df$Termo))
}
freq_total <- sort(table(todos_termos), decreasing = TRUE)
termos_principais <- names(head(freq_total, 15))

# Criar matriz de evolução
matriz_evolucao <- data.frame(
 Termo = termos_principais,
 `1982_1995` = integer(15),
 `1996_2005` = integer(15),
 `2006_2015` = integer(15),
 `2016_2019` = integer(15),
 `2020_2026` = integer(15)
)

for(i in 1:15) {
 termo <- matriz_evolucao$Termo[i]
 if(!is.null(termos_1982_1995) && termo %in% termos_1982_1995$Termo)
  matriz_evolucao$`1982_1995`[i] <- termos_1982_1995$Frequencia[termos_1982_1995$Termo == termo]
 if(!is.null(termos_1996_2005) && termo %in% termos_1996_2005$Termo)
  matriz_evolucao$`1996_2005`[i] <- termos_1996_2005$Frequencia[termos_1996_2005$Termo == termo]
 if(!is.null(termos_2006_2015) && termo %in% termos_2006_2015$Termo)
  matriz_evolucao$`2006_2015`[i] <- termos_2006_2015$Frequencia[termos_2006_2015$Termo == termo]
 if(!is.null(termos_2016_2019) && termo %in% termos_2016_2019$Termo)
  matriz_evolucao$`2016_2019`[i] <- termos_2016_2019$Frequencia[termos_2016_2019$Termo == termo]
 if(!is.null(termos_2020_2026) && termo %in% termos_2020_2026$Termo)
  matriz_evolucao$`2020_2026`[i] <- termos_2020_2026$Frequencia[termos_2020_2026$Termo == termo]
}

# Heatmap
matriz_melt <- melt(matriz_evolucao, id.vars = "Termo")
colnames(matriz_melt) <- c("Termo", "Periodo", "Frequencia")

heatmap_original <- ggplot(matriz_melt, aes(x = Periodo, y = Termo, fill = Frequencia)) +
 geom_tile(color = "white", linewidth = 0.5) +
 scale_fill_gradient(low = "white", high = "darkred", name = "Frequência") +
 geom_text(aes(label = Frequencia), size = 3.5) +
 labs(title = "Evolução de Termos-Chave (Baseada nos Resumos)",
      x = "", y = "") +
 theme_minimal() +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("heatmap_original.png", heatmap_original, width = 12, height = 8, dpi = 300)

# ============================================
# 7. EXTRAÇÃO E LIMPEZA DE PAÍSES
# ============================================

# Extrair países
if(!"AU_CO" %in% names(dado)) {
 dado <- metaTagExtraction(dado, Field = "AU_CO", sep = ";")
}

extrair_pais <- function(c1) {
 if(is.na(c1) || c1 == "") return(NA)
 partes <- unlist(strsplit(c1, ";"))
 paises <- sapply(partes, function(x) {
  partes2 <- unlist(strsplit(x, ","))
  trimws(tail(partes2, 1))
 })
 paises <- paises[paises != ""]
 return(paste(unique(paises), collapse = "; "))
}

dado$AU_CO_manual <- sapply(dado$C1, extrair_pais)
dado$AU_CO_final <- ifelse(is.na(dado$AU_CO) | dado$AU_CO == "", dado$AU_CO_manual, dado$AU_CO)

# Limpeza automática dos países
paises_para_limpar <- c(
 "MARYLAND 20802" = "USA", "MASSACHUSETTS 02115" = "USA", 
 "MARYLAND 21205" = "USA", "PROC.NC 27709" = "USA",
 "JOHNS HOPKINS U." = "USA", "SAN FRANCISCO 94143-0840" = "USA",
 "GEORGIA 30341-3724" = "USA", "GAGA 30333" = "USA",
 "BERN30333" = "SWITZERLAND", "MADRID" = "SPAIN",
 "FIOCRUZ" = "BRAZIL", "BRASIL" = "BRAZIL",
 "UK" = "UNITED KINGDOM", "VANCOUVER" = "CANADA",
 "BR" = "BRAZIL", "DEMARK" = "DENMARK"
)

for(pais_sujo in names(paises_para_limpar)) {
 dado$AU_CO_final <- gsub(pais_sujo, paises_para_limpar[pais_sujo], dado$AU_CO_final)
}

dado$AU_CO_final <- gsub("United States|USA|U.S.A.", "USA", dado$AU_CO_final)
dado$AU_CO_final <- gsub("England|Scotland|Wales", "UK", dado$AU_CO_final)

# ============================================
# 8. ANÁLISE POR REGIÃO GEOGRÁFICA
# ============================================

regioes <- list(
 "América do Norte" = c("USA", "CANADA"),
 "Europa" = c("UNITED KINGDOM", "GERMANY", "SPAIN", "FRANCE", "ITALY", 
              "PORTUGAL", "SWEDEN", "NETHERLANDS", "SWITZERLAND", "DENMARK"),
 "Ásia" = c("CHINA", "JAPAN", "KOREA", "INDIA", "INDONESIA", "THAILAND", "NEPAL"),
 "América Latina" = c("BRAZIL", "GUATEMALA"),
 "Oceania" = c("AUSTRALIA")
)

dado$REGIAO <- NA
for(regiao in names(regioes)) {
 for(pais in regioes[[regiao]]) {
  dado$REGIAO[grepl(pais, dado$AU_CO_final)] <- regiao
 }
}

# Gráfico por região
dados_regiao <- dado %>%
 filter(!is.na(REGIAO), !is.na(periodo)) %>%
 group_by(periodo, REGIAO) %>%
 summarise(N = n(), .groups = "drop")

grafico_regiao <- ggplot(dados_regiao, aes(x = periodo, y = N, fill = REGIAO)) +
 geom_bar(stat = "identity", position = "dodge") +
 labs(title = "Evolução da Produção por Região",
      x = "Período", y = "Número de Publicações") +
 theme_minimal() +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("evolucao_por_regiao.png", grafico_regiao, width = 10, height = 6, dpi = 300)
print(grafico_regiao)

# ============================================
# 9. SÉRIE TEMPORAL DOS TERMOS
# ============================================

termos_serie <- head(termos_principais, 5)
anos <- 2011:2026
dados_anuais <- data.frame(Ano = anos)

for(termo in termos_serie) {
 freq_anual <- sapply(anos, function(ano) {
  artigos_ano <- which(dado$PY == ano)
  sum(grepl(termo, dado$TI[artigos_ano], ignore.case = TRUE)) +
   sum(grepl(termo, dado$AB[artigos_ano], ignore.case = TRUE))
 })
 dados_anuais[[termo]] <- freq_anual
}

dados_long <- melt(dados_anuais, id.vars = "Ano", 
                   variable.name = "Termo", value.name = "Frequencia")

grafico_serie <- ggplot(dados_long, aes(x = Ano, y = Frequencia, color = Termo)) +
 geom_line(linewidth = 1.2) +
 geom_point(size = 2) +
 labs(title = "Evolução Temporal dos Termos-Chave (2011-2026)",
      y = "Frequência") +
 theme_minimal()

ggsave("serie_temporal_termos.png", grafico_serie, width = 10, height = 6, dpi = 300)
print(grafico_serie)


# ============================================
# 10. MATRIZ DE CO-OCORRÊNCIA
# ============================================

termos_cooc <- head(termos_principais, 10)
cooc_mat <- matrix(0, nrow = length(termos_cooc), ncol = length(termos_cooc))
rownames(cooc_mat) <- colnames(cooc_mat) <- termos_cooc

for(i in 1:length(termos_cooc)) {
 for(j in 1:length(termos_cooc)) {
  if(i <= j) {
   cooc <- sum(grepl(termos_cooc[i], dado$AB, ignore.case = TRUE) & 
                grepl(termos_cooc[j], dado$AB, ignore.case = TRUE))
   cooc_mat[i, j] <- cooc
   cooc_mat[j, i] <- cooc
  }
 }
}

png("matriz_coocorrencia.png", width = 10, height = 8, units = "in", res = 300)
pheatmap(cooc_mat,
         main = "Matriz de Co-ocorrência de Termos",
         display_numbers = TRUE,
         fontsize_number = 8,
         color = colorRampPalette(c("white", "orange", "darkred"))(50))


# =================================================
# 11. REDE DE COLABORAÇÃO - EXECUÇÃO PASSO A PASSO
# =================================================

library(igraph)
library(bibliometrix)

# Criar pasta resultados
if(!dir.exists("resultados")) {
 dir.create("resultados")
 cat("📁 Pasta 'resultados' criada\n")
}

# PASSO 1: Construir matriz de colaboração
cat("\n1. Construindo matriz de colaboração...\n")
country_collab <- biblioNetwork(dado, 
                                analysis = "collaboration", 
                                network = "countries",
                                sep = ";")

cat("   Dimensão da matriz:", dim(country_collab), "\n")
cat("   Exemplo:\n")
print(country_collab[1:min(5, nrow(country_collab)), 1:min(5, ncol(country_collab))])

# PASSO 2: Converter para grafo
cat("\n2. Convertendo para objeto grafo...\n")
grafo_paises <- graph_from_adjacency_matrix(country_collab, 
                                            mode = "undirected",
                                            weighted = TRUE,
                                            diag = FALSE)

cat("   Vértices (países):", vcount(grafo_paises), "\n")
cat("   Arestas (colaborações):", ecount(grafo_paises), "\n")

# PASSO 3: Remover vértices isolados
cat("\n3. Removendo países sem colaboração...\n")
grafo_paises <- delete_vertices(grafo_paises, degree(grafo_paises) == 0)

cat("   Vértices após remoção:", vcount(grafo_paises), "\n")
cat("   Arestas após remoção:", ecount(grafo_paises), "\n")

# PASSO 4: Verificar se há países para plotar
if(vcount(grafo_paises) == 0) {
 cat("\n❌ NENHUM PAÍS COM COLABORAÇÃO ENCONTRADO!\n")
 cat("   Isso pode ocorrer se todos os artigos forem de um único país.\n")
} else {
 
 # PASSO 5: Salvar o gráfico
 cat("\n4. Salvando gráfico...\n")
 arquivo_saida <- "resultados/rede_colaboracao_tese.png"
 
 png(arquivo_saida, width = 14, height = 10, units = "in", res = 300)
 
 V(grafo_paises)$color <- "#2166ac"
 V(grafo_paises)$size <- degree(grafo_paises) * 3 + 8
 E(grafo_paises)$width <- E(grafo_paises)$weight / 3 + 0.5
 
 plot(grafo_paises,
      layout = layout_with_fr,
      vertex.color = V(grafo_paises)$color,
      vertex.size = V(grafo_paises)$size,
      vertex.label = V(grafo_paises)$name,
      vertex.label.cex = 0.9,
      edge.width = E(grafo_paises)$width,
      edge.color = "gray50",
      main = "Rede de Colaboração Científica entre Países")
 
 dev.off()
 
 # PASSO 6: Verificar se o arquivo foi criado
 if(file.exists(arquivo_saida)) {
  cat("\n✅ SUCESSO! Arquivo salvo:\n")
  cat("   📁", file.path(getwd(), arquivo_saida), "\n")
  cat("   📊 Tamanho:", file.size(arquivo_saida), "bytes\n")
 } else {
  cat("\n❌ ERRO: O arquivo não foi criado!\n")
  cat("   Verifique permissões de escrita na pasta 'resultados'\n")
 }
 
 # PASSO 7: Estatísticas da rede
 cat("\n=== ESTATÍSTICAS DA REDE ===\n")
 cat("Países na rede:", vcount(grafo_paises), "\n")
 cat("Colaborações:", ecount(grafo_paises), "\n")
 cat("Densidade:", edge_density(grafo_paises), "\n")
 
 # PASSO 8: Países mais centrais
 grau <- degree(grafo_paises)
 cat("\n=== TOP 10 PAÍSES MAIS CENTRAIS ===\n")
 top_paises <- head(sort(grau, decreasing = TRUE), 10)
 print(top_paises)
}

# PASSO 9: Listar todos os arquivos na pasta resultados
cat("\n📁 Conteúdo da pasta 'resultados':\n")
print(list.files("resultados"))

 
# ============================================
# 12. LEI DE BRADFORD E KEYWORD GROWTH
# ============================================

# Bradford's Law
bradford_result <- bradford(dado)
print(bradford_result)

# Keyword Growth
kw_growth <- KeywordGrowth(dado, Tag = "DE", top = 15, cdf = TRUE)
print(kw_growth)

# ============================================
# 13. DIVERSIDADE TEMÁTICA (ÍNDICE DE SHANNON)
# ============================================

calcular_diversidade <- function(periodo_nome) {
 subset_df <- dado[dado$periodo == periodo_nome, ]
 if(nrow(subset_df) == 0) return(NULL)
 
 textos <- tolower(paste(subset_df$AB[!is.na(subset_df$AB)], collapse = " "))
 textos <- gsub("[[:punct:][:digit:]]", " ", textos)
 palavras <- unlist(strsplit(textos, "\\s+"))
 palavras <- palavras[nchar(palavras) >= 4]
 palavras <- palavras[!palavras %in% stopwords_ab]
 
 freq <- table(palavras)
 if(length(freq) == 0) return(NULL)
 
 p <- freq / sum(freq)
 shannon <- -sum(p * log(p))
 
 return(data.frame(
  Periodo = periodo_nome,
  Shannon = shannon,
  Riqueza = length(freq),
  Artigos = nrow(subset_df)
 ))
}

diversidade <- do.call(rbind, lapply(unique(dado$periodo[!is.na(dado$periodo)]), calcular_diversidade))

if(!is.null(diversidade)) {
 ggplot(diversidade, aes(x = Periodo, y = Shannon, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1.5) +
  geom_point(size = 4, color = "darkred") +
  labs(title = "Diversidade Temática por Período (Índice de Shannon)",
       y = "Diversidade de Shannon") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 ggsave("diversidade_tematica.png", width = 8, height = 5, dpi = 300)
 write.csv(diversidade, "diversidade_por_periodo.csv", row.names = FALSE)
}



# ============================================
 # 16. Peiódicos
#=============================================

dado %>%
 dplyr::mutate(SO = gsub('(.{1,30})(\\s|$)', 
                         '\\1\n', SO )
 ) %>%
 dplyr::count(SO) %>%
 dplyr::arrange(desc(n)) %>% 
 dplyr::slice(1:10) %>% 
 ggplot() +
 geom_col(aes(x = reorder(SO, n),
              y = n),
          fill = "#4b689c") +
 coord_flip() +
 theme_bw() +
 labs(x = "Periódico", 
      y = "Quantidade de artigos")

#============================================
#gerar imagem única para gráficos
#============================================

grafico_ano <- ggplot(dado, aes(x = PY)) +
 geom_bar(fill = "steelblue") +
 scale_x_continuous(
  breaks = seq(1982, 2026, by = 5),
  expand = c(0, 0)
 ) +
 coord_cartesian(xlim = c(1982, 2026)) +  # <- AQUI (no final)
 labs(title = "Artigos por Ano (1982–2026)",
      x = "Ano",
      y = "Número de artigos") +
theme_minimal() +
 theme(
  panel.grid = element_blank(),
  axis.line = element_line(color = "black")
 )
 

#gráfico por região mantido
dados_regiao <- dado %>%
 filter(!is.na(REGIAO), !is.na(periodo)) %>%
 group_by(periodo, REGIAO) %>%
 summarise(N = n(), .groups = "drop")

grafico_regiao <- ggplot(dados_regiao, aes(x = periodo, y = N, fill = REGIAO)) +
 geom_bar(stat = "identity", position = "dodge") +
 labs(title = "Evolução da Produção por Região",
      x = "Período", y = "Número de Publicações") +
theme_minimal() +
 theme(
  panel.grid = element_blank(),
  axis.line = element_line(color = "black")
 )

#colocar os dois lado a lado
install.packages("patchwork")  # se não tiver
library(patchwork)

grafico_final <- grafico_ano + grafico_regiao

ggsave("artigos_ano_e_regiao.png",
       grafico_final,
       width = 14,   # maior porque são dois gráficos
       height = 6,
       dpi = 300)
print(grafico_final)


sum(is.na(dado$REGIAO))
sum(is.na(dado$periodo))

# =================================================
# 17. MAPA CONCEITUAL - USANDO APENAS TÍTULOS (TI)
# =================================================

library(FactoMineR)
library(factoextra)
library(tm)

# Usar apenas títulos (mais limpos que DE)
titulos <- dado$TI[!is.na(dado$TI) & dado$TI != ""]

# Criar corpus com limpeza AGGRESSIVA
corpus <- Corpus(VectorSource(titulos))

# Limpeza pesada
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)

# Stopwords ampliadas (remover tudo que não é técnico)
stopwords_personalizadas <- c(
 # Genéricos
 stopwords("en"),
 "study", "studies", "paper", "articles", "research", "analysis",
 "using", "based", "approach", "method", "methods", "results",
 "data", "model", "models", "evaluation", "assessment",
 "comparison", "evaluation", "validation", "application",
 "development", "system", "systems", "tool", "tools",
 "framework", "review", "literature", "finding", "findings",
 # Termos que poluem
 "effect", "impact", "association", "risk", "factor", "factors",
 "population", "prevalence", "united", "states"
)

corpus <- tm_map(corpus, removeWords, stopwords_personalizadas)

# Criar matriz documento-termo
dtm <- DocumentTermMatrix(corpus)

# Remover termos que aparecem em menos de 3 documentos
dtm <- dtm[, slam::col_sums(dtm > 0) >= 3]

# Selecionar TOP 15 termos mais frequentes
freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
top_termos <- names(head(freq, 15))

cat("✅ Termos selecionados dos Títulos:\n")
print(freq[1:15])

# Criar matriz de co-ocorrência
cooc_mat <- matrix(0, nrow = length(top_termos), ncol = length(top_termos))
rownames(cooc_mat) <- colnames(cooc_mat) <- top_termos

matriz_binaria <- as.matrix(dtm[, top_termos]) > 0

for(i in 1:ncol(matriz_binaria)) {
 for(j in 1:ncol(matriz_binaria)) {
  if(i < j) {
   cooc <- sum(matriz_binaria[, i] & matriz_binaria[, j])
   cooc_mat[i, j] <- cooc
   cooc_mat[j, i] <- cooc
  }
 }
}

# Verificar se matriz tem dados válidos
if(sum(cooc_mat) == 0) {
 cat("\n⚠️ Matriz vazia! Tente aumentar o número de documentos ou reduzir stopwords.\n")
} else {
 # CA e clusters
 res.ca <- CA(cooc_mat, graph = FALSE)
 set.seed(123)
 clust <- kmeans(res.ca$col$coord[, 1:2], centers = 3)
 
 # GRÁFICO LIMPO
 # Aumentar o tamanho das elipses e ajustar transparência
 p <- fviz_ca_biplot(res.ca,
                     col.var = as.factor(clust$cluster),
                     palette = c("#2166ac", "#d73027", "#fdae61"),  # Azul, Vermelho, Laranja
                     repel = TRUE,
                     title = "Estrutura Conceitual - Clusters Temáticos",
                     addEllipses = TRUE,
                     ellipse.level = 0.7,
                     ellipse.alpha = 0.15,  # Transparência da elipse
                     ellipse.type = "norm",  # Tipo de elipse
                     geom.var = c("point", "text"),
                     geom.ind = "none",
                     labelsize = 5,
                     pointsize = 5,
                     ggtheme = theme_minimal(base_size = 12) +
                      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                            axis.title = element_text(face = "bold"),
                            legend.position = "bottom")) +
  labs(color = "Clusters Temáticos") +
  xlab(paste0("Dimensão 1 (29.3%)")) +
  ylab(paste0("Dimensão 2 (18.5%)"))
 
 # Salvar com resolução maior
 ggsave("clusters_nitidos.png", p, width = 12, height = 9, dpi = 300)

 # Mostrar clusters
 cat("\n📊 CLUSTERS TEMÁTICOS (pelos títulos):\n")
 for(i in 1:3) {
  termos <- top_termos[clust$cluster == i]
  if(length(termos) > 0) {
   cat(paste0("\n🔵 Cluster ", i, ":\n"))
   cat(paste("   ", paste(termos, collapse = ", ")), "\n")
  }
 }
}
