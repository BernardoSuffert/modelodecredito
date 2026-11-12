# =============================================================================
# 05_avaliacao.R — avaliação final no teste, scorecard e gráficos
# =============================================================================
library(ggplot2)
source("R/00_funcoes.R")
source("R/02_features.R")

dados  <- criar_features(readRDS("data/processed/dados.rds"))
pipe   <- readRDS("data/processed/pipeline_final.rds")
treino <- dados[dados$base == "treino", ]
teste  <- dados[dados$base == "teste", ]

treino$p_mau <- prever(pipe, treino)
teste$p_mau  <- prever(pipe, teste)

# ---- 1. Métricas: treino x teste ----------------------------------------------
met <- rbind(treino = metricas(treino$p_mau, treino$mau),
             teste  = metricas(teste$p_mau,  teste$mau))

# Intervalo de 95% por bootstrap no teste: com 300 linhas o número pontual
# engana; o intervalo mostra o quanto ele pode variar.
set.seed(2026)
boot <- t(replicate(2000, {
  i <- sample(nrow(teste), replace = TRUE)
  metricas(teste$p_mau[i], teste$mau[i])
}))
ic <- apply(boot, 2, quantile, probs = c(0.025, 0.975))
met_out <- data.frame(base = rownames(met), met, row.names = NULL)
met_out$AUC_ic95 <- c(NA, sprintf("%.3f a %.3f", ic[1, "AUC"], ic[2, "AUC"]))
met_out$KS_ic95  <- c(NA, sprintf("%.3f a %.3f", ic[1, "KS"],  ic[2, "KS"]))
print(met_out, digits = 3)
write.csv(met_out, "output/tabelas/metricas.csv", row.names = FALSE)

# ---- 2. Scorecard: probabilidade -> pontos --------------------------------------
# Convenção de mercado: 600 pontos = chance de 50 bons para 1 mau;
# a cada +20 pontos a chance de ser bom dobra (PDO = 20).
pdo <- 20; score_base <- 600; odds_base <- 50
fator  <- pdo / log(2)
offset <- score_base - fator * log(odds_base)
para_score <- function(p) offset + fator * log((1 - p) / p)
treino$score <- para_score(treino$p_mau)
teste$score  <- para_score(teste$p_mau)

# Pontos de cada faixa de cada variável (somando tudo dá o score do cliente)
b0 <- coef(pipe$modelo)[1]; nv <- length(pipe$vars)
scorecard <- do.call(rbind, lapply(pipe$vars, function(v) {
  t <- pipe$woe[[v]]$tabela
  data.frame(variavel = v, faixa = t$faixa, n_treino = t$n,
             taxa_maus = round(t$taxa_maus, 3), woe = round(t$woe, 3),
             pontos = round(-(coef(pipe$modelo)[v] * t$woe + b0 / nv) * fator +
                              offset / nv))
}))
write.csv(scorecard, "output/tabelas/scorecard.csv", row.names = FALSE)

psi <- calc_psi(treino$score, teste$score)
cat(sprintf("PSI treino x teste: %.3f\n", psi))

# ---- 3. Tabela por faixa de score (teste) ---------------------------------------
# Faixas definidas no treino; um bom modelo ordena: taxa de maus cai conforme
# o score sobe.
cortes <- unique(quantile(treino$score, seq(0, 1, 0.2)))
cortes[1] <- -Inf; cortes[length(cortes)] <- Inf
teste$faixa <- cut(teste$score, cortes, dig.lab = 4)
faixas <- do.call(rbind, lapply(split(teste, teste$faixa), function(d)
  data.frame(faixa_score = d$faixa[1], n = nrow(d), maus = sum(d$mau),
             taxa_maus = mean(d$mau), score_medio = mean(d$score))))
faixas$pct_maus_acum <- cumsum(faixas$maus) / sum(faixas$maus)
faixas$pct_bons_acum <- cumsum(faixas$n - faixas$maus) / sum(faixas$n - faixas$maus)
rownames(faixas) <- NULL
print(faixas, digits = 3)
write.csv(faixas, "output/tabelas/faixas_score_teste.csv", row.names = FALSE)
write.csv(data.frame(psi_treino_teste = psi), "output/tabelas/psi.csv", row.names = FALSE)

# ---- 4. Gráficos ----------------------------------------------------------------
tema <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot",
        legend.position = "top")
cor_bom <- "#2E7D6B"; cor_mau <- "#C8102E"

# 4a. Curva KS
s <- sort(unique(teste$score))
d_ks <- data.frame(score = s,
                   maus = ecdf(teste$score[teste$mau == 1])(s),
                   bons = ecdf(teste$score[teste$mau == 0])(s))
i_ks <- which.max(abs(d_ks$maus - d_ks$bons))
g_ks <- ggplot(d_ks, aes(score)) +
  geom_line(aes(y = maus, colour = "Maus"), linewidth = 1) +
  geom_line(aes(y = bons, colour = "Bons"), linewidth = 1) +
  annotate("segment", x = d_ks$score[i_ks], xend = d_ks$score[i_ks],
           y = d_ks$bons[i_ks], yend = d_ks$maus[i_ks], linetype = "dashed") +
  annotate("label", x = d_ks$score[i_ks], y = mean(c(d_ks$bons[i_ks], d_ks$maus[i_ks])),
           label = sprintf("KS = %.1f%%", 100 * met["teste", "KS"]), hjust = -0.1) +
  scale_colour_manual(values = c(Maus = cor_mau, Bons = cor_bom), name = NULL) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Curva KS (base de teste)",
       subtitle = "Proporção acumulada de maus e de bons até cada score",
       x = "Score", y = "Proporção acumulada") + tema
ggsave("output/figuras/03_ks.png", g_ks, width = 8, height = 5, dpi = 150)

# 4b. Curva ROC
r <- pROC::roc(teste$mau, teste$p_mau, quiet = TRUE, direction = "<")
d_roc <- data.frame(fpr = 1 - r$specificities, tpr = r$sensitivities)
d_roc <- d_roc[order(d_roc$fpr, d_roc$tpr), ]
g_roc <- ggplot(d_roc, aes(fpr, tpr)) +
  geom_abline(linetype = "dashed", colour = "grey50") +
  geom_line(colour = cor_mau, linewidth = 1) +
  coord_equal() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = sprintf("Curva ROC (teste): AUC = %.3f", met["teste", "AUC"]),
       x = "Bons classificados como maus (falso positivo)",
       y = "Maus capturados (sensibilidade)") + tema
ggsave("output/figuras/04_roc.png", g_roc, width = 6, height = 6, dpi = 150)

# 4c. Taxa de maus por faixa de score
g_fx <- ggplot(faixas, aes(faixa_score, taxa_maus)) +
  geom_col(fill = cor_mau, width = 0.65) +
  geom_text(aes(label = sprintf("%.0f%%\n(n=%d)", 100 * taxa_maus, n)),
            vjust = -0.2, size = 3.6) +
  geom_hline(yintercept = mean(teste$mau), linetype = "dashed") +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Taxa de maus por faixa de score (teste)",
       subtitle = "Faixas definidas pelos quintis do treino. Tracejado: taxa média de maus",
       x = "Faixa de score (maior = menos risco)", y = "Taxa de maus") + tema
ggsave("output/figuras/05_taxa_maus_por_faixa.png", g_fx, width = 8, height = 5, dpi = 150)

# 4d. Distribuição do score por classe
teste$classe <- ifelse(teste$mau == 1, "Maus", "Bons")
g_dist <- ggplot(teste, aes(score, fill = classe)) +
  geom_density(alpha = 0.55, colour = NA) +
  scale_fill_manual(values = c(Maus = cor_mau, Bons = cor_bom), name = NULL) +
  labs(title = "Distribuição do score por classe (teste)",
       x = "Score", y = "Densidade") + tema
ggsave("output/figuras/06_distribuicao_score.png", g_dist, width = 8, height = 5, dpi = 150)
