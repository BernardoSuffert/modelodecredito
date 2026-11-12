# =============================================================================
# 02_features.R — criação de variáveis (feature engineering)
#
# Ponto importante: como o modelo usa faixas (binning + WoE), transformações
# monótonas de UMA variável (log, raiz, padronização) não mudam nada: as
# faixas ficam idênticas. Só agrega informação nova quem COMBINA variáveis
# (razões, interações) ou REAGRUPA categorias com conhecimento de negócio.
# =============================================================================

criar_features <- function(d) {
  # -- Razões -----------------------------------------------------------------
  # Parcela mensal aproximada: o que pesa no bolso todo mês.
  d$parcela_mensal  <- d$valor_credito / d$prazo_meses
  # Tamanho da dívida relativo à fase de vida (idade como proxy de renda/patrimônio).
  d$valor_por_idade <- d$valor_credito / d$idade
  # Prazo longo para quem é jovem = compromisso longo com pouco histórico.
  d$prazo_por_idade <- d$prazo_meses / d$idade
  # Comprometimento de renda (1 a 4) sustentado ao longo de todo o prazo.
  d$carga_total     <- d$taxa_parcela * d$prazo_meses

  # -- Interação --------------------------------------------------------------
  # Liquidez: saldo em conta e reserva olhados juntos. Quem tem conta
  # negativa E pouca poupança não tem colchão nenhum.
  conta <- ifelse(d$conta_corrente == "sem conta", "sem conta",
           ifelse(d$conta_corrente == "negativa", "conta negativa", "conta positiva"))
  reserva <- ifelse(d$poupanca %in% c("menos de 100 DM"), "reserva baixa",
             ifelse(d$poupanca == "desconhecida ou sem", "reserva desconhecida",
                    "com reserva"))
  d$liquidez <- paste(conta, reserva, sep = " | ")

  # -- Reagrupamento por conhecimento de negócio --------------------------------
  # 10 finalidades viram 5 grupos com lógica econômica parecida.
  d$finalidade_grupo <- with(d, ifelse(
    finalidade == "carro novo", "carro novo", ifelse(
    finalidade == "carro usado", "carro usado", ifelse(
    finalidade %in% c("móveis", "rádio/TV", "eletrodomésticos", "reparos"),
      "bens e casa", ifelse(
    finalidade %in% c("educação", "requalificação"), "educação", "negócios e outros")))))

  # Vínculos que sinalizam estabilidade: emprego longo + casa própria.
  d$estabilidade <- (d$tempo_emprego %in% c("4 a 7 anos", "7 anos ou mais")) +
                    (d$moradia == "própria") +
                    (d$tempo_residencia >= 3)
  d
}

# ---- Conjuntos de variáveis ---------------------------------------------------
# Excluídas por serem atributos protegidos: usar sexo, estado civil ou
# nacionalidade para negar crédito é discriminatório (e vedado em vários
# países). Um modelo de crédito real passa por esse filtro antes de qualquer
# métrica. Idade foi mantida por ser prática comum, mas é discutível.
vars_protegidas <- c("estado_civil_sexo", "trabalhador_estrangeiro")

vars_originais <- c("conta_corrente", "prazo_meses", "historico_credito",
                    "finalidade", "valor_credito", "poupanca", "tempo_emprego",
                    "taxa_parcela", "outros_devedores", "tempo_residencia",
                    "propriedade", "idade", "outros_parcelamentos", "moradia",
                    "n_creditos_banco", "ocupacao", "n_dependentes", "telefone")

vars_criadas <- c("parcela_mensal", "valor_por_idade", "prazo_por_idade",
                  "carga_total", "liquidez", "finalidade_grupo", "estabilidade")
