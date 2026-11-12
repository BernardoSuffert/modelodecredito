# =============================================================================
# 00_funcoes.R — funções reutilizadas em todo o projeto
#
# WoE, IV e KS foram escritos "na mão" de propósito: são poucas linhas e
# assim cada número do projeto pode ser explicado sem depender de um pacote.
# =============================================================================

# ---- 1. Binning (categorização) --------------------------------------------
# Regra de qualidade de uma faixa: ter pelo menos `min_prop` da amostra e
# conter bons E maus. Faixas que não cumprem isso são fundidas com a faixa
# de taxa de maus mais parecida (vizinha, no caso numérico).
# Variáveis numéricas ainda passam por uma checagem de monotonicidade.

ajustar_bin_num <- function(x, y, n_bins = 5, min_prop = 0.05, monotono = TRUE) {
  vals <- sort(unique(x))
  if (length(vals) <= n_bins) {
    # variável discreta com poucos valores: cada valor começa como uma faixa
    cortes <- c(-Inf, (head(vals, -1) + tail(vals, -1)) / 2, Inf)
  } else {
    # variável contínua: faixas de mesmo tamanho (quantis)
    q <- unique(quantile(x, probs = seq(0, 1, length.out = n_bins + 1)))
    cortes <- c(-Inf, q[-c(1, length(q))], Inf)
  }
  cortes <- unname(unique(cortes))

  repeat {
    if (length(cortes) <= 2) break
    tab  <- table(cut(x, cortes), factor(y, levels = c(0, 1)))
    n    <- rowSums(tab)
    ruim <- which(n / length(x) < min_prop | tab[, 1] == 0 | tab[, 2] == 0)
    if (length(ruim) == 0) break
    i    <- ruim[which.min(n[ruim])]
    taxa <- tab[, 2] / pmax(n, 1)
    viz  <- c(i - 1, i + 1)
    viz  <- viz[viz >= 1 & viz <= nrow(tab)]
    j    <- viz[which.min(abs(taxa[viz] - taxa[i]))]
    cortes <- cortes[-max(i, j)]   # remove a fronteira entre as faixas i e j
  }

  # Monotonicidade: em variável numérica a taxa de maus deve só subir ou só
  # descer ao longo das faixas. Zigue-zague em amostra pequena é quase sempre
  # ruído, e uma regra de crédito que não é monótona não se explica ao cliente.
  # Enquanto houver zigue-zague, funde o par vizinho de taxas mais parecidas.
  if (monotono) repeat {
    if (length(cortes) <= 3) break
    tab  <- table(cut(x, cortes), factor(y, levels = c(0, 1)))
    taxa <- tab[, 2] / rowSums(tab)
    d    <- diff(taxa)
    if (all(d >= 0) || all(d <= 0)) break
    cortes <- cortes[-(which.min(abs(d)) + 1)]
  }
  list(tipo = "num", cortes = cortes)
}

ajustar_bin_cat <- function(x, y, min_prop = 0.05) {
  x     <- as.character(x)
  grupo <- setNames(unique(x), unique(x))      # categoria original -> grupo

  repeat {
    tab <- table(grupo[x], factor(y, levels = c(0, 1)))
    n   <- rowSums(tab)
    if (nrow(tab) <= 1) break
    ruim <- which(n / length(x) < min_prop | tab[, 1] == 0 | tab[, 2] == 0)
    if (length(ruim) == 0) break
    i      <- ruim[which.min(n[ruim])]
    taxa   <- tab[, 2] / n
    outros <- setdiff(seq_len(nrow(tab)), i)
    j      <- outros[which.min(abs(taxa[outros] - taxa[i]))]
    juntar <- rownames(tab)[c(i, j)]
    grupo[grupo %in% juntar] <- paste(sort(juntar), collapse = " + ")
  }
  padrao <- names(which.max(table(grupo[x])))  # usado para categoria inédita
  list(tipo = "cat", grupo = grupo, padrao = padrao)
}

aplicar_bin <- function(spec, x) {
  if (spec$tipo == "num") {
    as.character(cut(x, spec$cortes, dig.lab = 6))
  } else {
    g <- unname(spec$grupo[as.character(x)])
    g[is.na(g)] <- spec$padrao
    g
  }
}

# ---- 2. WoE e IV ------------------------------------------------------------
# WoE da faixa = ln( % dos bons na faixa / % dos maus na faixa )
#   WoE > 0: faixa concentra bons (menos risco); WoE < 0: concentra maus.
# IV = soma de (%bons - %maus) * WoE: resume a força da variável inteira.
# O 0,5 somado às contagens evita ln(0) em faixas muito pequenas.

tabela_woe <- function(faixa, y) {
  tab  <- table(faixa, factor(y, levels = c(0, 1)))
  bons <- as.numeric(tab[, 1])
  maus <- as.numeric(tab[, 2])
  k    <- nrow(tab)
  pct_bons <- (bons + 0.5) / (sum(bons) + 0.5 * k)
  pct_maus <- (maus + 0.5) / (sum(maus) + 0.5 * k)
  woe <- log(pct_bons / pct_maus)
  data.frame(
    faixa     = rownames(tab),
    n         = bons + maus,
    bons      = bons,
    maus      = maus,
    taxa_maus = maus / (bons + maus),
    woe       = woe,
    iv_faixa  = (pct_bons - pct_maus) * woe,
    stringsAsFactors = FALSE
  )
}

# Ajusta binning + WoE de várias variáveis usando APENAS a base informada
# (sempre o treino). Devolve um objeto que depois é aplicado a qualquer base.
ajustar_woe <- function(df, y, vars, n_bins = 5, min_prop = 0.05) {
  lapply(setNames(vars, vars), function(v) {
    x    <- df[[v]]
    spec <- if (is.numeric(x)) ajustar_bin_num(x, y, n_bins, min_prop)
            else               ajustar_bin_cat(x, y, min_prop)
    tab  <- tabela_woe(aplicar_bin(spec, x), y)
    if (spec$tipo == "num")       # faixas numéricas na ordem crescente
      tab <- tab[match(levels(cut(x, spec$cortes, dig.lab = 6)), tab$faixa), ]
    list(spec = spec, tabela = tab, iv = sum(tab$iv_faixa))
  })
}

# Troca cada variável pelo WoE da faixa em que a observação cai.
transformar_woe <- function(df, woe_fit, vars = names(woe_fit)) {
  out <- lapply(vars, function(v) {
    f   <- woe_fit[[v]]
    w   <- f$tabela$woe[match(aplicar_bin(f$spec, df[[v]]), f$tabela$faixa)]
    w[is.na(w)] <- 0            # faixa nunca vista no treino: WoE neutro
    w
  })
  as.data.frame(setNames(out, vars))
}

tabela_iv <- function(woe_fit) {
  iv <- sapply(woe_fit, function(f) f$iv)
  d  <- data.frame(variavel = names(iv), iv = unname(iv),
                   n_faixas = sapply(woe_fit, function(f) nrow(f$tabela)))
  d$forca <- cut(d$iv, c(-Inf, 0.02, 0.1, 0.3, 0.5, Inf), right = FALSE,
                 labels = c("inútil", "fraca", "média", "forte", "suspeita"))
  rownames(d) <- NULL
  d[order(-d$iv), ]
}

# ---- 3. Métricas ------------------------------------------------------------
# KS: maior distância entre as curvas acumuladas de maus e de bons ao longo
# do score. AUC: probabilidade de um mau ter score de risco maior que um bom.

calc_ks <- function(p, y) {
  s <- sort(unique(p))
  max(abs(ecdf(p[y == 1])(s) - ecdf(p[y == 0])(s)))
}

calc_auc <- function(p, y) {
  as.numeric(pROC::auc(pROC::roc(y, p, quiet = TRUE, direction = "<")))
}

metricas <- function(p, y) {
  auc <- calc_auc(p, y)
  c(AUC = auc, Gini = 2 * auc - 1, KS = calc_ks(p, y))
}

# PSI: mede se a distribuição do score mudou entre duas bases.
# < 0,10 estável | 0,10 a 0,25 atenção | > 0,25 mudança relevante
calc_psi <- function(esperado, observado, n_faixas = 10) {
  cortes <- unique(quantile(esperado, seq(0, 1, length.out = n_faixas + 1)))
  cortes[1] <- -Inf; cortes[length(cortes)] <- Inf
  e <- prop.table(table(cut(esperado,  cortes))) + 1e-4
  o <- prop.table(table(cut(observado, cortes))) + 1e-4
  sum((o - e) * log(o / e))
}

# ---- 4. Pipeline de modelagem -----------------------------------------------
# Tudo que "aprende" com os dados está aqui dentro, para poder ser refeito
# do zero em cada dobra da validação cruzada (sem vazamento de informação).
#
#   1. binning + WoE no treino
#   2. descarta variáveis com IV < iv_min
#   3. entre pares muito correlacionados, fica a de maior IV
#   4. regressão logística com eliminação para trás: sai a variável de maior
#      p-valor até todas terem p < p_max e coeficiente negativo
#      (com WoE = ln(bons/maus) e alvo = mau, o sinal correto é negativo;
#       sinal positivo indica variável redundante com outra)

treinar_pipeline <- function(treino, vars, alvo = "mau",
                             iv_min = 0.02, cor_max = 0.7, p_max = 0.05) {
  y       <- treino[[alvo]]
  woe_fit <- ajustar_woe(treino, y, vars)
  iv      <- sapply(woe_fit, function(f) f$iv)

  cand <- names(sort(iv[iv >= iv_min], decreasing = TRUE))
  X    <- transformar_woe(treino, woe_fit, cand)

  # filtro de correlação: percorre da maior IV para a menor
  manter <- character(0)
  for (v in cand) {
    if (length(manter) == 0 ||
        all(abs(cor(X[[v]], X[manter])) < cor_max)) manter <- c(manter, v)
  }

  dados <- cbind(mau = y, X[manter])
  repeat {
    fit  <- glm(mau ~ ., data = dados, family = binomial())
    co   <- summary(fit)$coefficients[-1, , drop = FALSE]
    ruim <- co[, "Pr(>|z|)"] > p_max | co[, "Estimate"] > 0
    if (!any(ruim) || nrow(co) == 1) break
    # prioridade: sinal invertido; depois o maior p-valor
    pior <- if (any(co[, "Estimate"] > 0)) names(which.max(co[, "Estimate"]))
            else names(which.max(co[, "Pr(>|z|)"]))
    dados[[pior]] <- NULL
  }
  list(woe = woe_fit, modelo = fit, vars = setdiff(names(dados), "mau"),
       iv = iv, descartadas_cor = setdiff(cand, manter))
}

# Probabilidade prevista de ser mau pagador.
prever <- function(pipe, df) {
  X <- transformar_woe(df, pipe$woe, pipe$vars)
  as.numeric(predict(pipe$modelo, newdata = X, type = "response"))
}

# Amostragem estratificada: mantém a proporção de maus em cada parte.
dobras_estratificadas <- function(y, k) {
  dobra <- integer(length(y))
  for (classe in unique(y)) {
    idx <- which(y == classe)
    dobra[idx] <- sample(rep_len(seq_len(k), length(idx)))
  }
  dobra
}
