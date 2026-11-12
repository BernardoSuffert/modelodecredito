# Roda o projeto inteiro, do dado bruto aos gráficos.
# Uso (na raiz do projeto):  Rscript run_all.R
# Pacotes necessários: install.packages(c("ggplot2", "pROC", "scales"))
for (script in c("R/01_dados.R", "R/03_woe_iv.R", "R/04_modelo.R", "R/05_avaliacao.R")) {
  cat("\n==========", script, "==========\n")
  source(script, echo = FALSE)
}
