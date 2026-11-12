# =============================================================================
# 01_dados.R — leitura, decodificação e separação treino/teste
# =============================================================================
source("R/00_funcoes.R")

arquivo <- "data/raw/german.csv"
if (!file.exists(arquivo)) {
  dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
  # Fonte original: UCI Machine Learning Repository, Statlog (German Credit Data)
  bruto <- read.table(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data",
    stringsAsFactors = FALSE)
  write.table(bruto, arquivo, sep = ",", row.names = FALSE, col.names = FALSE,
              quote = FALSE)
}

nomes <- c("conta_corrente", "prazo_meses", "historico_credito", "finalidade",
           "valor_credito", "poupanca", "tempo_emprego", "taxa_parcela",
           "estado_civil_sexo", "outros_devedores", "tempo_residencia",
           "propriedade", "idade", "outros_parcelamentos", "moradia",
           "n_creditos_banco", "ocupacao", "n_dependentes", "telefone",
           "trabalhador_estrangeiro", "classe")

dados <- read.csv(arquivo, header = FALSE, col.names = nomes,
                  stringsAsFactors = FALSE)

# ---- Dicionário: códigos da UCI -> rótulos legíveis --------------------------
dic <- list(
  conta_corrente = c(A11 = "negativa", A12 = "0 a 200 DM", A13 = "200 DM ou mais",
                     A14 = "sem conta"),
  historico_credito = c(A30 = "sem créditos ou todos quitados",
                        A31 = "todos quitados neste banco",
                        A32 = "créditos atuais em dia",
                        A33 = "atraso no passado",
                        A34 = "conta crítica ou créditos em outros bancos"),
  finalidade = c(A40 = "carro novo", A41 = "carro usado", A42 = "móveis",
                 A43 = "rádio/TV", A44 = "eletrodomésticos", A45 = "reparos",
                 A46 = "educação", A47 = "férias", A48 = "requalificação",
                 A49 = "negócios", A410 = "outros"),
  poupanca = c(A61 = "menos de 100 DM", A62 = "100 a 500 DM",
               A63 = "500 a 1000 DM", A64 = "1000 DM ou mais",
               A65 = "desconhecida ou sem"),
  tempo_emprego = c(A71 = "desempregado", A72 = "menos de 1 ano",
                    A73 = "1 a 4 anos", A74 = "4 a 7 anos", A75 = "7 anos ou mais"),
  estado_civil_sexo = c(A91 = "homem divorciado/separado",
                        A92 = "mulher divorciada/separada/casada",
                        A93 = "homem solteiro", A94 = "homem casado/viúvo",
                        A95 = "mulher solteira"),
  outros_devedores = c(A101 = "nenhum", A102 = "co-requerente", A103 = "fiador"),
  propriedade = c(A121 = "imóvel", A122 = "seguro de vida ou poupança habitacional",
                  A123 = "carro ou outro", A124 = "nenhuma ou desconhecida"),
  outros_parcelamentos = c(A141 = "banco", A142 = "lojas", A143 = "nenhum"),
  moradia = c(A151 = "aluguel", A152 = "própria", A153 = "cedida"),
  ocupacao = c(A171 = "desempregado ou não qualificado não residente",
               A172 = "não qualificado residente", A173 = "qualificado",
               A174 = "gestão ou altamente qualificado"),
  telefone = c(A191 = "não", A192 = "sim"),
  trabalhador_estrangeiro = c(A201 = "sim", A202 = "não")
)
for (v in names(dic)) dados[[v]] <- unname(dic[[v]][dados[[v]]])
stopifnot(!anyNA(dados))

# ---- Variável alvo ----------------------------------------------------------
# Na base: classe 1 = bom pagador, 2 = mau pagador. Modelamos o evento "mau".
dados$mau    <- as.integer(dados$classe == 2)
dados$classe <- NULL

cat("Linhas:", nrow(dados), "| Taxa de maus:", mean(dados$mau), "\n")

# ---- Treino (70%) e teste (30%), estratificado pelo alvo --------------------
# O teste fica guardado e só é usado uma vez, na avaliação final.
set.seed(2026)
dobra <- dobras_estratificadas(dados$mau, 10)
dados$base <- ifelse(dobra <= 7, "treino", "teste")
print(with(dados, tapply(mau, base, function(m) c(n = length(m), taxa_maus = mean(m)))))

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(dados, "data/processed/dados.rds")
