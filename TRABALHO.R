library (AER)
base <- read.csv(file.choose(), sep = ";")   
ncol(base)
names(base)
nrow(base)  

# variáveis usadas 
vars <- c("PARTO", "ESCMAE2010", "IDADEMAE", "RACACORMAE", "ESTCIVMAE",
          "CONSPRENAT", "SEMAGESTAC", "GRAVIDEZ", "QTDPARTCES",
          "QTDGESTANT", "TPAPRESENT", "STCESPARTO", "CODMUNRES",
          "CODMUNNATU")   
str(base)
base <- base[, vars]
summary(base)


# Limpeza
# ANTES
table(base$PARTO, useNA = "ifany")

base$esc_ignorada <- ifelse(is.na(base$ESCMAE2010) | base$ESCMAE2010 == 9, 1, 0)
tapply(base$esc_ignorada, substr(base$CODMUNRES, 1, 1), mean)   
base$esc_ignorada <- NULL

table(base$QTDPARTCES)
table(base$CONSPRENAT)

taxa_antes <- mean(base$PARTO[base$PARTO == 1 | base$PARTO == 2] == 2, na.rm = TRUE)

n <- nrow(base)

base <- subset(base, PARTO == 1 | PARTO == 2); n <- c(n, nrow(base))
base <- subset(base, ESCMAE2010 >= 0 & ESCMAE2010 <= 5); n <- c(n, nrow(base))
base <- subset(base, RACACORMAE >= 1 & RACACORMAE <= 5); n <- c(n, nrow(base))
base <- subset(base, ESTCIVMAE >= 1 & ESTCIVMAE <= 5); n <- c(n, nrow(base))
base <- subset(base, GRAVIDEZ >= 1 & GRAVIDEZ <= 3); n <- c(n, nrow(base))
base <- subset(base, TPAPRESENT >= 1 & TPAPRESENT <= 3); n <- c(n, nrow(base))
base <- subset(base, IDADEMAE >= 10 & IDADEMAE <= 55); n <- c(n, nrow(base))
base <- subset(base, SEMAGESTAC >= 22 & SEMAGESTAC <= 45); n <- c(n, nrow(base))
base <- subset(base, CONSPRENAT >= 0 & CONSPRENAT < 99); n <- c(n, nrow(base))
base <- subset(base, QTDPARTCES < 99 & QTDGESTANT < 99); n <- c(n, nrow(base))
base <- subset(base, QTDPARTCES <= QTDGESTANT); n <- c(n, nrow(base))

# TABELA 1 
retiradas <- c(0, -diff(n))
tabela1 <- data.frame(restantes = n, retiradas = retiradas)
tabela1
sum(retiradas)



# Variáveis
# variável dependente 
base$cesarea <- ifelse(base$PARTO == 2, 1, 0)
prop.table(table(base$cesarea)) 

# cesarea eletiva
base$STCESPARTO[base$STCESPARTO == 3 | base$STCESPARTO == 9] <- NA
base$eletiva <- ifelse(base$PARTO == 2 & base$STCESPARTO == 1, 1, 0)

#escolaridade da mãe
base$esc <- NA
base$esc[base$ESCMAE2010 == 0] <- "Sem_escolaridade"
base$esc[base$ESCMAE2010 == 1] <- "Fundamental_I"
base$esc[base$ESCMAE2010 == 2] <- "Fundamental_II"
base$esc[base$ESCMAE2010 == 3] <- "Medio"
base$esc[base$ESCMAE2010 == 4] <- "Superior_incompleto"
base$esc[base$ESCMAE2010 == 5] <- "Superior_completo"
base$esc <- factor(base$esc, levels = c("Sem_escolaridade", "Fundamental_I", "Fundamental_II",
                                        "Medio", "Superior_incompleto", "Superior_completo"))
table(base$esc)

#dummy para escolaridade do vi
base$superior   <- ifelse(base$ESCMAE2010 == 5, 1, 0)   
base$medio_mais <- ifelse(base$ESCMAE2010 >= 3, 1, 0) 

#Idade 
base$idade <- base$IDADEMAE

# raça da mãe
base$raca <- factor(base$RACACORMAE, levels = 1:5,
                    labels = c("Branca", "Preta", "Amarela", "Parda", "Indigena"))

# situação conjugal
base$casada <- ifelse(base$ESTCIVMAE == 2 | base$ESTCIVMAE == 5, 1, 0)

# pré natal, gestação e histórico clinico 
base$consultas <- base$CONSPRENAT
base$semanas   <- base$SEMAGESTAC
base$cesarea_anterior  <- ifelse(base$QTDPARTCES > 0, 1, 0)   
base$primeira_gestacao <- ifelse(base$QTDGESTANT == 0, 1, 0)   
base$gravidez_multipla <- ifelse(base$GRAVIDEZ > 1, 1, 0)      
base$nao_cefalico <- ifelse(base$TPAPRESENT != 1, 1, 0)  


#região e estado 
base$uf <- substr(base$CODMUNRES, 1, 2)  
base$regiao <- NA
base$regiao[substr(base$CODMUNRES, 1, 1) == "1"] <- "Norte"
base$regiao[substr(base$CODMUNRES, 1, 1) == "2"] <- "Nordeste"
base$regiao[substr(base$CODMUNRES, 1, 1) == "3"] <- "Sudeste"
base$regiao[substr(base$CODMUNRES, 1, 1) == "4"] <- "Sul"
base$regiao[substr(base$CODMUNRES, 1, 1) == "5"] <- "CentroOeste"
base$regiao <- factor(base$regiao, levels = c("Sudeste", "Norte", "Nordeste", "Sul", "CentroOeste"))
table(base$regiao)

# instrumentos da VI
summary(base$CODMUNNATU)  
base$CODMUNNATU <- as.numeric(substr(base$CODMUNNATU, 1, 6))
base$CODMUNNATU[base$CODMUNNATU < 110000 | base$CODMUNNATU > 530010] <- NA   
summary(base$CODMUNNATU)  

# numero de maes nascidas em cada municipio
base$n_mun <- ave(base$superior, base$CODMUNNATU, FUN = length)
base$soma_sup <- ave(base$superior,   base$CODMUNNATU, FUN = sum)
base$soma_med <- ave(base$medio_mais, base$CODMUNNATU, FUN = sum)

# proporcao entre as outras
base$z_sup <- (base$soma_sup - base$superior)   / (base$n_mun - 1)
base$z_med <- (base$soma_med - base$medio_mais) / (base$n_mun - 1)

# maes sem municipio
base$z_sup[is.na(base$CODMUNNATU)] <- NA
base$z_med[is.na(base$CODMUNNATU)] <- NA
base$n_mun[is.na(base$CODMUNNATU)] <- NA

# migrante
base$migrante <- ifelse(base$CODMUNNATU != base$CODMUNRES, 1, 0)

saveRDS(base, "base_final_sinasc2024.rds") 

# estatitisca descritiva
# tabela 2
vars_t2 <- c("cesarea", "eletiva", "idade", "casada", "consultas", "semanas",
             "cesarea_anterior", "primeira_gestacao", "gravidez_multipla", "nao_cefalico")
media <- colMeans(base[, vars_t2], na.rm = TRUE)
dp    <- apply(base[, vars_t2], 2, sd, na.rm = TRUE)          
n_obs <- colSums(!is.na(base[, vars_t2]))                    

tabela2 <- data.frame(media = round(media, 2), desvio_padrao = round(dp, 2), n = n_obs)
tabela2
table(base$esc)
table(base$regiao)
tapply(base$cesarea, base$esc, mean)
tapply(base$cesarea, base$regiao, mean)
prop.table(table(base$esc))
prop.table(table(base$raca))
summary(base[, c("z_sup", "z_med", "n_mun", "migrante")])
mean(base$migrante, na.rm = TRUE)   

# regressores, teste e grafico
# base inteira 
dados <- base

# FUNCOES E TESTESS
# mpl simples 
reg1 <- lm(cesarea ~ esc, data = dados)
summary(reg1)
tapply(dados$cesarea, dados$esc, mean)
ct1 <- coeftest(reg1, vcov = vcovHC(reg1, type = "HC1"))[2:6, ]
ct1
nobs(reg1); summary(reg1)$r.squared

# MPL com controles 
reg2 <- lm(cesarea ~ esc + idade + I(idade^2) + raca + casada, data = dados)
ct2 <- coeftest(reg2, vcov = vcovHC(reg2, type = "HC1"))[2:6, ]
ct2
nobs(reg2); summary(reg2)$r.squared

reg3 <- lm(cesarea ~ esc + idade + I(idade^2) + raca + casada +
             consultas + semanas + I(semanas^2) +
             cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico, data = dados)
ct3 <- coeftest(reg3, vcov = vcovHC(reg3, type = "HC1"))[2:6, ]
ct3
nobs(reg3); summary(reg3)$r.squared

reg4 <- lm(cesarea ~ esc + idade + I(idade^2) + raca + casada +
             consultas + semanas + I(semanas^2) +
             cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf, data = dados)
ct4 <- coeftest(reg4, vcov = vcovHC(reg4, type = "HC1"))[2:6, ]
ct4
nobs(reg4); summary(reg4)$r.squared

# Teste de heterocedasticidade
bptest(reg4)

# teste f dummy conjun relevante
reg4_sem_esc <- lm(cesarea ~ idade + I(idade^2) + raca + casada +
                     consultas + semanas + I(semanas^2) +
                     cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf, data = dados)
waldtest(reg4_sem_esc, reg4, vcov = vcovHC(reg4, type = "HC1"))   # teste F robusto

# probabilidades abaixo de 0 ou acima de 1 
mean(fitted(reg4) < 0 | fitted(reg4) > 1)   

#funcionais 
# idade ao quadrado
reg3_sem_quad <- lm(cesarea ~ esc + idade + raca + casada +
                      consultas + semanas + I(semanas^2) +
                      cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico, data = dados)
waldtest(reg3_sem_quad, reg3, vcov = vcovHC(reg3, type = "HC1"))

# Ponto de virada da idade: -b1 / (2*b2)
- coef(reg3)["idade"] / (2 * coef(reg3)["I(idade^2)"])

# semana gestao
coef(reg3)[c("semanas", "I(semanas^2)")]

# idade
reg3_ln <- lm(cesarea ~ esc + log(idade) + raca + casada +
                consultas + semanas + I(semanas^2) +
                cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico, data = dados)
summary(reg3_ln)

# ajuste
summary(reg3)$adj.r.squared
summary(reg3_ln)$adj.r.squared

# sem pre natal
reg3_sem_cons <- lm(cesarea ~ esc + idade + I(idade^2) + raca + casada +
                      semanas + I(semanas^2) +
                      cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico, data = dados)
coef(reg3)[2:6]
coef(reg3_sem_cons)[2:6]

# consulta ao quadrado
reg3_cons2 <- lm(cesarea ~ esc + idade + I(idade^2) + raca + casada +
                   consultas + I(consultas^2) + semanas + I(semanas^2) +
                   cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico,
                 data = dados)
waldtest(reg3, reg3_cons2, vcov = vcovHC(reg3_cons2, type = "HC1"))  
coef(reg3_cons2)[c("consultas", "I(consultas^2)")]

# TABELA 3 
tabela3 <- data.frame(coef_1 = round(ct1[, 1], 3), ep_1 = round(ct1[, 2], 4),
                      coef_2 = round(ct2[, 1], 3), ep_2 = round(ct2[, 2], 4),
                      coef_3 = round(ct3[, 1], 3), ep_3 = round(ct3[, 2], 4),
                      coef_4 = round(ct4[, 1], 3), ep_4 = round(ct4[, 2], 4))
tabela3

tabela3_ajuste <- data.frame(modelo = c("(1)", "(2)", "(3)", "(4)"),
                             observacoes = c(nobs(reg1), nobs(reg2), nobs(reg3), nobs(reg4)),
                             r2 = round(c(summary(reg1)$r.squared, summary(reg2)$r.squared,
                                          summary(reg3)$r.squared, summary(reg4)$r.squared), 3))
tabela3_ajuste

# interações 
# escolaridade e região
reg_reg <- lm(cesarea ~ esc + regiao + idade + I(idade^2) + raca + casada +
                consultas + semanas + I(semanas^2) +
                cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico, data = dados)
reg_int_reg <- lm(cesarea ~ esc * regiao + idade + I(idade^2) + raca + casada +
                    consultas + semanas + I(semanas^2) +
                    cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico, data = dados)
coeftest(reg_int_reg, vcov = vcovHC(reg_int_reg, type = "HC1"))

# teste f interação 
waldtest(reg_reg, reg_int_reg, vcov = vcovHC(reg_int_reg, type = "HC1"))

# comparar região
coef(lm(cesarea ~ esc + idade + I(idade^2) + raca + casada +
          consultas + semanas + I(semanas^2) +
          cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico,
        data = subset(dados, regiao == "Sudeste")))[2:6]
coef(lm(cesarea ~ esc + idade + I(idade^2) + raca + casada +
          consultas + semanas + I(semanas^2) +
          cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico,
        data = subset(dados, regiao == "Nordeste")))[2:6]

# escolaridade e primeira gestação
reg_int_prim <- lm(cesarea ~ esc * primeira_gestacao + idade + I(idade^2) + raca + casada +
                     consultas + semanas + I(semanas^2) +
                     cesarea_anterior + gravidez_multipla + nao_cefalico, data = dados)
coeftest(reg_int_prim, vcov = vcovHC(reg_int_prim, type = "HC1"))

# base muito grande, foi preciso para continuar rodando
rm(reg1, reg2, reg3_cons2, reg3_ln, reg3_sem_cons, reg3_sem_quad,
   reg4_sem_esc, reg_reg, reg_int_reg, reg_int_prim)
gc()

# logit e probit para comparar com o MPL
logit <- glm(cesarea ~ esc + idade + I(idade^2) + raca + casada +
               consultas + semanas + I(semanas^2) +
               cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
             family = binomial, data = dados)

probit <- glm(cesarea ~ esc + idade + I(idade^2) + raca + casada +
                consultas + semanas + I(semanas^2) +
                cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
              family = binomial(link = "probit"), data = dados)

summary(logit)
summary(probit)

# efeito marginal medio das dummies de escolaridade 
d <- dados
d$esc <- factor("Sem_escolaridade", levels = levels(dados$esc))
p0_logit  <- predict(logit,  newdata = d, type = "response")
p0_probit <- predict(probit, newdata = d, type = "response")
ame_logit  <- c()
ame_probit <- c()
for (nivel in levels(dados$esc)[-1]) {
  d$esc <- factor(nivel, levels = levels(dados$esc))
  ame_logit[nivel]  <- mean(predict(logit,  newdata = d, type = "response") - p0_logit)
  ame_probit[nivel] <- mean(predict(probit, newdata = d, type = "response") - p0_probit)
}
rm(d)
ame_logit
ame_probit
coef(reg4)[2:6]              

# razao logit
exp(coef(logit)[2:6])

# teste razao VM
logit_sem_esc <- glm(cesarea ~ idade + I(idade^2) + raca + casada +
                       consultas + semanas + I(semanas^2) +
                       cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
                     family = binomial, data = dados)
anova(logit_sem_esc, logit, test = "Chisq")

# pseudo R2
logit_nulo <- glm(cesarea ~ 1, family = binomial, data = dados)
1 - as.numeric(logLik(logit) / logLik(logit_nulo))

# percenual de acertos 
prev <- ifelse(fitted(logit) > 0.5, 1, 0)
table(Previsto = prev, Observado = dados$cesarea)
mean(prev == dados$cesarea)


#  robustez
# cesarea eletiva como variavel dependente
mpl_elet <- lm(eletiva ~ esc + idade + I(idade^2) + raca + casada +
                 consultas + semanas + I(semanas^2) +
                 cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
               data = dados)
ct_elet <- coeftest(mpl_elet, vcov = vcovHC(mpl_elet, type = "HC1"))[2:6, ]
ct_elet

# so maes sem cesarea anterior
mpl_semces <- lm(cesarea ~ esc + idade + I(idade^2) + raca + casada +
                   consultas + semanas + I(semanas^2) +
                   primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
                 data = subset(dados, cesarea_anterior == 0))
ct_semces <- coeftest(mpl_semces, vcov = vcovHC(mpl_semces, type = "HC1"))[2:6, ]
ct_semces

# so primeira gestacao
mpl_prim <- lm(cesarea ~ esc + idade + I(idade^2) + raca + casada +
                 consultas + semanas + I(semanas^2) +
                 gravidez_multipla + nao_cefalico + uf,
               data = subset(dados, primeira_gestacao == 1))
ct_prim <- coeftest(mpl_prim, vcov = vcovHC(mpl_prim, type = "HC1"))[2:6, ]
ct_prim

# guardar para grafico
coef_reg4 <- coef(reg4)[2:6]

rm(logit, probit, logit_sem_esc, logit_nulo, mpl_elet, mpl_semces, mpl_prim, reg3, reg4)
gc()

# variavel instrumental (MQ2E)
dados_iv <- subset(dados, !is.na(z_sup) & n_mun >= 30)
mig <- subset(dados_iv, migrante == 1)  # so migrante

# MQO com superior, para comparar
mqo <- lm(cesarea ~ superior + idade + I(idade^2) + raca + casada +
            consultas + semanas + I(semanas^2) +
            cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
          data = dados_iv)
ct_mqo <- coeftest(mqo, vcov = vcovCL(mqo, cluster = ~CODMUNNATU))[2, ]
ct_mqo

# MQO so com migrante
mqo_mig <- lm(cesarea ~ superior + idade + I(idade^2) + raca + casada +
                consultas + semanas + I(semanas^2) +
                cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
              data = mig)
ct_mqo_mig <- coeftest(mqo_mig, vcov = vcovCL(mqo_mig, cluster = ~CODMUNNATU))[2, ]
ct_mqo_mig

# primeiro estagio, relevancia (amostra completa)
pe <- lm(superior ~ z_sup + z_med + idade + I(idade^2) + raca + casada +
           consultas + semanas + I(semanas^2) +
           cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
         data = dados_iv)
pe_sem <- lm(superior ~ idade + I(idade^2) + raca + casada +
               consultas + semanas + I(semanas^2) +
               cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
             data = dados_iv)
ct_pe <- coeftest(pe, vcov = vcovCL(pe, cluster = ~CODMUNNATU))[2:3, ]  
ct_pe
f_pe <- waldtest(pe_sem, pe, vcov = vcovCL(pe, cluster = ~CODMUNNATU))  
f_pe
rm(pe, pe_sem); gc()

# primeiro estagio e relevancia
pe_mig <- lm(superior ~ z_sup + z_med + idade + I(idade^2) + raca + casada +
               consultas + semanas + I(semanas^2) +
               cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
             data = mig)
pe_mig_sem <- lm(superior ~ idade + I(idade^2) + raca + casada +
                   consultas + semanas + I(semanas^2) +
                   cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
                 data = mig)
f_pe_mig <- waldtest(pe_mig_sem, pe_mig, vcov = vcovCL(pe_mig, cluster = ~CODMUNNATU))  
f_pe_mig
rm(pe_mig, pe_mig_sem); gc()

# MQ2E 
iv <- ivreg(cesarea ~ superior + idade + I(idade^2) + raca + casada +
              consultas + semanas + I(semanas^2) +
              cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf |
              z_sup + z_med + idade + I(idade^2) + raca + casada +
              consultas + semanas + I(semanas^2) +
              cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
            data = dados_iv)
# erro-padrao por municipio de nascimento
ct_iv <- coeftest(iv, vcov = vcovCL(iv, cluster = dados_iv$CODMUNNATU))[2, ]
ct_iv

# MQ2E so migrante
iv_mig <- ivreg(cesarea ~ superior + idade + I(idade^2) + raca + casada +
                  consultas + semanas + I(semanas^2) +
                  cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf |
                  z_sup + z_med + idade + I(idade^2) + raca + casada +
                  consultas + semanas + I(semanas^2) +
                  cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
                data = mig)
ct_iv_mig <- coeftest(iv_mig, vcov = vcovCL(iv_mig, cluster = mig$CODMUNNATU))[2, ]
ct_iv_mig

# teste J de sobreidentificacao classico
dados_iv$u_iv <- residuals(iv)
j_reg <- lm(u_iv ~ z_sup + z_med + idade + I(idade^2) + raca + casada +
              consultas + semanas + I(semanas^2) +
              cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
            data = dados_iv)
j_sem <- lm(u_iv ~ idade + I(idade^2) + raca + casada +
              consultas + semanas + I(semanas^2) +
              cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
            data = dados_iv)
J_sargan <- 2 * waldtest(j_sem, j_reg)$F[2]
pchisq(J_sargan, df = 1, lower.tail = FALSE)   # p-valor classico (1 grau de liberdade = 2 instrumentos - 1 endogena)
rm(j_reg, j_sem); gc()

# robusto
dados_iv$sup_hat <- fitted(lm(superior ~ z_sup + z_med + idade + I(idade^2) + raca + casada +
                                consultas + semanas + I(semanas^2) +
                                cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
                              data = dados_iv))
r_med <- residuals(lm(z_med ~ sup_hat + idade + I(idade^2) + raca + casada +
                        consultas + semanas + I(semanas^2) +
                        cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
                      data = dados_iv))
ur  <- dados_iv$u_iv * r_med
um  <- rep(1, length(ur))
aux <- lm(um ~ ur - 1)
J_rob <- length(ur) - sum(residuals(aux)^2)
pchisq(J_rob, df = 1, lower.tail = FALSE)      
rm(r_med, ur, um, aux); gc()

# teste J de sobreidentificacao com migrantes
mig$u_iv <- residuals(iv_mig)
j_reg <- lm(u_iv ~ z_sup + z_med + idade + I(idade^2) + raca + casada +
              consultas + semanas + I(semanas^2) +
              cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
            data = mig)
j_sem <- lm(u_iv ~ idade + I(idade^2) + raca + casada +
              consultas + semanas + I(semanas^2) +
              cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
            data = mig)
J_sargan_mig <- 2 * waldtest(j_sem, j_reg)$F[2]
pchisq(J_sargan_mig, df = 1, lower.tail = FALSE)   
rm(j_reg, j_sem); gc()

# robusta
mig$sup_hat <- fitted(lm(superior ~ z_sup + z_med + idade + I(idade^2) + raca + casada +
                           consultas + semanas + I(semanas^2) +
                           cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
                         data = mig))
r_med <- residuals(lm(z_med ~ sup_hat + idade + I(idade^2) + raca + casada +
                        consultas + semanas + I(semanas^2) +
                        cesarea_anterior + primeira_gestacao + gravidez_multipla + nao_cefalico + uf,
                      data = mig))
ur  <- mig$u_iv * r_med
um  <- rep(1, length(ur))
aux <- lm(um ~ ur - 1)
J_rob_mig <- length(ur) - sum(residuals(aux)^2)
pchisq(J_rob_mig, df = 1, lower.tail = FALSE)      
rm(r_med, ur, um, aux); gc()

# comparar regressoes
coef(mqo)["superior"]
coef(iv)["superior"]
coef(iv_mig)["superior"]

# TABELA 4
tabela4 <- data.frame(amostra_completa = c(round(ct_mqo[1], 3), round(ct_mqo[2], 4),
                                           round(ct_iv[1], 3), round(ct_iv[2], 4),
                                           round(f_pe$F[2], 1),
                                           round(pchisq(J_sargan, df = 1, lower.tail = FALSE), 3),
                                           round(pchisq(J_rob, df = 1, lower.tail = FALSE), 3),
                                           nobs(iv)),
                      migrantes = c(round(ct_mqo_mig[1], 3), round(ct_mqo_mig[2], 4),
                                    round(ct_iv_mig[1], 3), round(ct_iv_mig[2], 4),
                                    round(f_pe_mig$F[2], 1),
                                    round(pchisq(J_sargan_mig, df = 1, lower.tail = FALSE), 3),
                                    round(pchisq(J_rob_mig, df = 1, lower.tail = FALSE), 3),
                                    nobs(iv_mig)))
rownames(tabela4) <- c("superior completo (MQO)", "erro-padrao MQO",
                       "superior completo (MQ2E)", "erro-padrao MQ2E", "F do 1o estagio",
                       "teste J classico (p-valor)", "teste J robusto (p-valor)", "observacoes")
tabela4

# MQO de referencia
round(ct_mqo[1:2], 4)

#gráficos 
nomes_esc <- c("Sem esc.", "Fund. I", "Fund. II", "Médio", "Sup. inc.", "Sup. comp.")

# grafico 1 
barplot(tapply(base$cesarea, base$esc, mean) * 100,
        names.arg = nomes_esc ,cex.names = 0.7,
        xlab = "Escolaridade da mãe", ylab = "Partos cesáreos (%)",
        col = "darkseagreen4", ylim = c(0, 100))

# grafico 2 
taxa_reg <- tapply(base$cesarea, base$regiao, mean)[c("Norte", "Nordeste", "Sudeste", "Sul", "CentroOeste")] * 100
barplot(taxa_reg,
        names.arg = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"),
        xlab = "Região", ylab = "Partos cesáreos (%)",
        col = "darkseagreen4", ylim = c(0, 100))

# grafico 3 
taxa_idade <- tapply(base$cesarea, base$idade, mean) * 100
plot(as.numeric(names(taxa_idade)), taxa_idade, type = "l",
     xlab = "Idade da mãe (anos)", ylab = "Partos cesáreos (%)",
     col = "darkolivegreen", lwd = 2, xlim = c(15, 45))   

# grafico 4 
taxa_sem <- tapply(base$cesarea, base$semanas, mean) * 100
plot(as.numeric(names(taxa_sem)), taxa_sem, type = "l",
     xlab = "Semanas de gestação", ylab = "Partos cesáreos (%)",
     col = "darkolivegreen", lwd = 2, xlim = c(28, 44))  

# grafico 5 
png("grafico5.png", width = 16, height = 6.5, units = "cm", res = 300, pointsize = 8)
efeitos <- rbind(MPL    = coef_reg4,
                 Logit  = ame_logit,
                 Probit = ame_probit) * 100
barplot(efeitos, beside = TRUE, legend.text = TRUE,
        names.arg = nomes_esc[-1], ylim = c(0, max(efeitos) * 1.3),
        args.legend = list(x = "topleft"),
        xlab = "Escolaridade da mãe",
        ylab = "Efeito (pontos percentuais)",
        col = c("darkolivegreen", "darkseagreen4", "darkseagreen"),
        cex.names = 0.8)
dev.off()

# grafico 6 
png("grafico6.png", width = 16, height = 6.5, units = "cm", res = 300, pointsize = 8)
robustez <- rbind("Cesárea total"        = ct4[, 1],
                  "Cesárea eletiva"      = ct_elet[, 1],
                  "Sem cesárea anterior" = ct_semces[, 1],
                  "Primeira gestação"    = ct_prim[, 1]) * 100
barplot(robustez, beside = TRUE, legend.text = TRUE,
        names.arg = nomes_esc[-1], cex.names = 0.8,
        ylim = c(min(0, min(robustez)) * 1.2, max(robustez) * 1.3),
        args.legend = list(x = "topleft", cex = 0.8),
        xlab = "Escolaridade da mãe",
        ylab = "Efeito (pontos percentuais)",
        col = c("darkolivegreen", "darkseagreen4", "darkseagreen", "honeydew3"))
abline(h = 0)
dev.off()