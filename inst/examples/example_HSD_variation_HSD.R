# Exemplo: variação HSD no plot.group
#
# Demonstra a diferença entre as barras descritivas (padrão) e a semiamplitude
# de Tukey (variation = "HSD"):
#   - barras descritivas (SD, SE, etc.) refletem dispersão dos dados brutos
#   - variation = "HSD" mostra mean ± MSD/2, onde barras que não se sobrepõem
#     indicam diferença significativa pelo critério do próprio teste de Tukey
#
# ATENÇÃO: substitua os 5 valores marcados com NA (bloco 5) pelos reais.

library(agricolae)

# ------------------------------------------------------------
# Dados — DBC: 5 blocos × 6 genótipos
# ------------------------------------------------------------
dados_dbc <- data.frame(
  bloco    = rep(1:5, each = 6),
  genotipo = rep(paste0("T", 1:6), 5),
  producao = c(
    # bloco 1
    79.5, 80.7, 77.3, 78.4, 76.8, 78.4,
    # bloco 2
    81.9, 77.9, 77.6, 77.2, 79.1, 76.3,
    # bloco 3
    84.6, 79.1, 79.8, 78.2, 80.2, 80.5,
    # bloco 4
    80.3, 83.8, 77.1, 79.1, 80.2, 78.0,
    # bloco 5
    83.6, 86.1, 79.8, 79.9, 77.8, 76.9
  )
)

# ------------------------------------------------------------
# ANOVA
# ------------------------------------------------------------
modelo_dbc <- lm(producao ~ bloco + genotipo, data = dados_dbc)
anova(modelo_dbc)

# ------------------------------------------------------------
# Teste de Tukey
# ------------------------------------------------------------
tukey_dbc <- HSD.test(modelo_dbc, "genotipo", console = TRUE)
tukey_dbc$groups

# ------------------------------------------------------------
# Gráficos comparativos
# ------------------------------------------------------------
op <- par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

# 1. Padrão (range): amplitude total dos dados observados — estatística descritiva
plot(tukey_dbc,
     variation = "range",
     main      = "Barras: Range (padrão)\n[estatística descritiva]",
     ylab      = "Produção",
     las       = 2)

# 2. Desvio padrão: também descritivo, sem relação com o limiar do teste
plot(tukey_dbc,
     variation = "SD",
     main      = "Barras: Desvio padrão\n[estatística descritiva]",
     ylab      = "Produção",
     las       = 2)

# 3. Semiamplitude de Tukey: mean ± MSD/2
#    Barras que NÃO se sobrepõem = diferença significativa ao nível alpha do teste
plot(tukey_dbc,
     variation = "HSD",
     main      = "Barras: Semiamplitude Tukey (MSD/2)\n[critério do teste]",
     ylab      = "Produção",
     las       = 2)

par(op)

# ------------------------------------------------------------
# Valores de referência para inspeção
# ------------------------------------------------------------
cat("\nMSD (amplitude mínima significativa):", tukey_dbc$statistics$MSD, "\n")
cat("Semiamplitude plotada (MSD/2):       ", tukey_dbc$statistics$MSD / 2, "\n")
cat("q de Tukey (alpha =", tukey_dbc$parameters$alpha, "):",
    tukey_dbc$parameters$StudentizedRange, "\n")
