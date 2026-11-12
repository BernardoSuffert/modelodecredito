# =============================================================================
# 04_modelo.R — validação cruzada (as variáveis criadas ajudaram?) e modelo final
# =============================================================================
source("R/00_funcoes.R")
source("R/02_features.R")

dados  <- criar_features(readRDS("data/processed/dados.rds"))
treino <- dados[dados$base == "treino", ]

# ---- 1. Validação cruzada repetida, só dentro do treino ----------------------
# Pergunta: as variáveis criadas melhoram o modelo ou é impressão?
# Com 700 linhas, um único corte treino/validação é ruído puro. Por isso
# 5 dobras x 10 repetições = 50 medições por cenário. Em cada dobra o
# pipeline INTEIRO (faixas, WoE, seleção, regressão) é refeito do zero.
cenarios <- list(
  "A: só variáveis originais"  = vars_originais,
  "B: originais + criadas"     = c(vars_originais, vars_criadas)
)

set.seed(2026)
k <- 5; repeticoes <- 10
cv <- do.call(rbind, lapply(seq_len(repeticoes), function(r) {
  dobra <- dobras_estratificadas(treino$mau, k)   # mesmas dobras para A e B
  do.call(rbind, lapply(seq_len(k), function(f) {
    tr <- treino[dobra != f, ]; va <- treino[dobra == f, ]
    do.call(rbind, lapply(names(cenarios), function(cn) {
      pipe <- treinar_pipeline(tr, cenarios[[cn]])
      m    <- metricas(prever(pipe, va), va$mau)
      data.frame(cenario = cn, rep = r, dobra = f, AUC = m["AUC"], KS = m["KS"],
                 n_vars = length(pipe$vars), row.names = NULL)
    }))
  }))
}))

resumo_cv <- do.call(rbind, lapply(split(cv, cv$cenario), function(d)
  data.frame(cenario = d$cenario[1],
             AUC_media = mean(d$AUC), AUC_dp = sd(d$AUC),
             KS_media  = mean(d$KS),  KS_dp  = sd(d$KS),
             n_vars_medio = mean(d$n_vars))))
rownames(resumo_cv) <- NULL
print(resumo_cv, digits = 3)

# Diferença pareada (B - A) na mesma dobra: é o que responde a pergunta.
a <- cv[cv$cenario == names(cenarios)[1], ]
b <- cv[cv$cenario == names(cenarios)[2], ]
# média por repetição (as 5 dobras de uma repetição não são independentes)
dif_rep <- tapply(b$AUC - a$AUC, a$rep, mean)
dif_ks  <- tapply(b$KS  - a$KS,  a$rep, mean)
dif <- data.frame(
  metrica = c("AUC", "KS"),
  diferenca_media = c(mean(dif_rep), mean(dif_ks)),
  minimo_entre_repeticoes = c(min(dif_rep), min(dif_ks)),
  maximo_entre_repeticoes = c(max(dif_rep), max(dif_ks)),
  repeticoes_em_que_B_venceu = c(sum(dif_rep > 0), sum(dif_ks > 0)))
print(dif, digits = 3)

write.csv(cv,        "output/tabelas/cv_detalhe.csv",   row.names = FALSE)
write.csv(resumo_cv, "output/tabelas/cv_resumo.csv",    row.names = FALSE)
write.csv(dif,       "output/tabelas/cv_diferenca.csv", row.names = FALSE)

# ---- 2. Modelo final: treino inteiro -------------------------------------------
# Regra definida ANTES de olhar o teste: as variáveis criadas só entram se o
# cenário B vencer em AUC em pelo menos 8 das 10 repetições. Empate ou ganho
# dentro do ruído = fica o modelo mais simples (A).
escolhido <- if (sum(dif_rep > 0) >= 8) 2 else 1
cat("\nCenário escolhido para o modelo final ->", names(cenarios)[escolhido], "\n")
pipe <- treinar_pipeline(treino, cenarios[[escolhido]])
pipe$cenario <- names(cenarios)[escolhido]
print(summary(pipe$modelo))

coefs <- as.data.frame(summary(pipe$modelo)$coefficients)
names(coefs) <- c("coeficiente", "erro_padrao", "z", "p_valor")
coefs <- cbind(termo = rownames(coefs), coefs)
coefs$iv <- pipe$iv[coefs$termo]
write.csv(coefs, "output/tabelas/coeficientes.csv", row.names = FALSE)

saveRDS(pipe, "data/processed/pipeline_final.rds")
