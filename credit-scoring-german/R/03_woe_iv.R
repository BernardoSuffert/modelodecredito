# =============================================================================
# 03_woe_iv.R — poder preditivo de cada variável (WoE e IV), só no treino
# =============================================================================
library(ggplot2)
source("R/00_funcoes.R")
source("R/02_features.R")

dados  <- criar_features(readRDS("data/processed/dados.rds"))
treino <- dados[dados$base == "treino", ]

woe_fit <- ajustar_woe(treino, treino$mau, c(vars_originais, vars_criadas))

iv <- tabela_iv(woe_fit)
iv$origem <- ifelse(iv$variavel %in% vars_criadas, "criada", "original")
print(iv, row.names = FALSE, digits = 3)

dir.create("output/tabelas", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figuras", recursive = TRUE, showWarnings = FALSE)
write.csv(iv, "output/tabelas/iv.csv", row.names = FALSE)

woe_todas <- do.call(rbind, lapply(names(woe_fit), function(v)
  cbind(variavel = v, woe_fit[[v]]$tabela)))
write.csv(woe_todas, "output/tabelas/woe_por_faixa.csv", row.names = FALSE)

# ---- Gráfico 1: ranking de IV -------------------------------------------------
tema <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot",
        legend.position = "top")
cores <- c(original = "#8A8F98", criada = "#C8102E")

g_iv <- ggplot(iv, aes(x = iv, y = reorder(variavel, iv), fill = origem)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = c(0.02, 0.1, 0.3), linetype = "dashed", colour = "grey40") +
  scale_fill_manual(values = cores, name = NULL) +
  labs(title = "Information Value por variável (base de treino)",
       subtitle = "Linhas tracejadas: 0,02 (mínimo útil), 0,10 (média) e 0,30 (forte)",
       x = "IV", y = NULL) + tema
ggsave("output/figuras/01_iv.png", g_iv, width = 8, height = 6.5, dpi = 150)

# ---- Gráfico 2: WoE por faixa das 6 variáveis mais fortes ---------------------
top <- head(iv$variavel, 6)
d_top <- woe_todas[woe_todas$variavel %in% top, ]
d_top$variavel <- factor(d_top$variavel, levels = top)
d_top$faixa_curta <- ifelse(nchar(d_top$faixa) > 34,
                            paste0(substr(d_top$faixa, 1, 32), "…"), d_top$faixa)
d_top$id <- factor(seq_len(nrow(d_top)))

g_woe <- ggplot(d_top, aes(x = woe, y = id, fill = woe > 0)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_vline(xintercept = 0, colour = "grey30") +
  scale_y_discrete(labels = setNames(d_top$faixa_curta, d_top$id)) +
  scale_fill_manual(values = c(`TRUE` = "#2E7D6B", `FALSE` = "#C8102E")) +
  facet_wrap(~variavel, scales = "free_y", ncol = 2) +
  labs(title = "WoE por faixa das 6 variáveis de maior IV",
       subtitle = "À direita do zero: faixa concentra bons pagadores. À esquerda: concentra maus",
       x = "WoE = ln(% bons / % maus)", y = NULL) + tema
ggsave("output/figuras/02_woe_top6.png", g_woe, width = 11, height = 8, dpi = 150)
