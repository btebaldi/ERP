#' Autor: Bruno Tebaldi Barbosa
#'
#' Data: 2023-01-18
#'
#' Processo de calculo do Equity Risk Premium (ERP). Baseado no documento: 
#' Antonio Zoratto Sanvicente (October 23, 2017) - ESTIMATIVAS DO EQUITY RISK PREMIUM PARA O MERCADO BRASILEIRO DE CAPITAIS
#'
#' Input: Planilha com dados do Economatica contendo: Last_Price, VPA, LPA e DPA
#'
#' Output: Arquivos txt

# Setup -------------------------------------------------------------------
rm(list = ls())

library(readxl)
library(dplyr)

# ininicializa a string de mensagem de log
str_log_message = "****** LOG DO PROCESSO DE CALCULO DO ERP ******";

log_message <- function(msg){
  str_log_message <<- sprintf("%s\n[%s] - %s",
                              str_log_message,
                              Sys.time(),
                              msg)  
}


# Data load ---------------------------------------------------------------

log_message("PROCESSO INICIADO")


base_ERP_full <- read_excel("Database/Import_base_ERP_economatica.xlsx", 
                            # range = cell_limits(ul = c(1,3), lr = c(NA, 11+2)),
                            na = "-")

# column  name regularization
# colnames(base_ERP_full) <- c("Nome", "Classe", "Bolsa", "Tipo_de_Ativo", "Ativo",
#                              "Ticker", "DPA", "LPA", "VPA", "Fechamento", 
#                              "Setor")

T10_Bond <- (3.5069)/100

log_message(sprintf("TBond: %f", T10_Bond))
log_message(sprintf("Lista de empresas consideradas: %s", paste(base_ERP_full$Ticker, collapse = ", ")))

# Database Preparation ----------------------------------------------------


Setor_levels <- c("Petróleo e Gas", "Agro e Pesca", "Energia Elétrica", "Finanças e Seguros", 
                  "Siderur & Metalur", "Máquinas Indust", "Outros", "Transporte Serviç", 
                  "Comércio", "Textil", "Construção", "Alimentos e Beb", "Telecomunicações", 
                  "Mineração", "Software e Dados", "Veiculos e peças", "Química", 
                  "Minerais não Met", "Eletroeletrônicos", "Papel e Celulose", "Fundos")


Setor_labels <- c("Petroleo e Gas", "Agro e Pesca", "Energia Eletrica", "Financas e Seguros", 
                  "Siderurgia e Metalurgia", "Maquinas Industriais", "Outros", "Transporte Servico", 
                  "Comercio", "Textil", "Construcao", "Alimentos e Bebidas", "Telecomunicacoes", 
                  "Mineracao", "Software e Dados", "Veiculos e pecas", "Quimica", 
                  "Minerais nao Metais", "Eletroeletronicos", "Papel e Celulose", "Fundos")


base_ERP_full$Setor <- factor(x = base_ERP_full$Setor, levels = Setor_levels,
                              labels = Setor_labels)

if(any(is.na(base_ERP_full$Setor))){
  stop("ERRO: SETOR COMO NA!")
}


# Logic layer - Calculo de k por acao -------------------------------------


# 1. São coletados os valores mais recentes de dividendos por ação,
# representados por DIV_0.

base_ERP <- base_ERP_full %>% 
  mutate(DIV_0 = DPA,
         P_0 = Fechamento)


# 2. A taxa de crescimento é estimada através da relação g = ROE * (1 - payout),
# onde ROE = return on equity = lucro líquido por ação/valor patrimonial da
# ação; por sua vez, payout = dividendo por ação/lucro líquido por ação. Os
# números necessários de lucro líquido e valor patrimonial por ação são
# extraídos de demonstrações financeiras publicadas trimestralmente por
# companhias abertas, mas os valores utilizados se referem a períodos móveis de
# 12 meses, tal como acontece com os dividendos por ação.

base_ERP <- base_ERP %>% 
  mutate(ROE = LPA/VPA,
         Payout = DPA/LPA,
         g = ROE * (1-Payout))


# 3. Uma vez estimada a taxa de crescimento, g, calcula-se o valor esperado do
# próximo dividendo por ação, DIV_1, o qual é dividido pelo preço P_0 observado.

base_ERP <- base_ERP %>% 
  mutate(DIV_1 = DIV_0 * (1+g),
         k = (DIV_1/P_0) + g)

# Logic layer - Filtro de empresas ----------------------------------------


# Exclui ações dos setores "Financas e Seguros", "Fundos"
Exclusao <- base_ERP %>%
  mutate(row_id = row_number()) %>% 
  filter(Setor %in% c("Financas e Seguros", "Fundos")) %>% 
  pull(row_id)

if(length(Exclusao) > 0){
  log_message(sprintf("Exclusao de emrpesas do setores 'Financas e Seguros', 'Fundos': %s", paste(base_ERP$Ticker[Exclusao], collapse = ", ")))
  base_ERP[Exclusao, ]
  base_ERP <- base_ERP[-Exclusao, ]
}


# 1. São excluídas as ações que não apresentaram cotação de fechamento no mês.
Exclusao <- base_ERP %>%
  mutate(row_id = row_number()) %>% 
  filter(is.na(Fechamento)) %>% 
  pull(row_id)

if(length(Exclusao) > 0){
  log_message(sprintf("Exclusao de emrpesas sem preço de fechamento: %s", paste(base_ERP$Ticker[Exclusao], collapse = ", ")))
  base_ERP[Exclusao, ]
  base_ERP <- base_ERP[-Exclusao, ]
}

# 2. São excluídas as ações de empresas para as quais houve prejuízo no
# período móvel de 12 meses (ou seja, lucro líquido por ação é negativo),
# bem como aquelas para as quais o valor patrimonial da ação é negativo.


# Exclui ações que tem LPA negativo
Exclusao <- base_ERP %>%
  mutate(row_id = row_number()) %>% 
  filter(LPA < 0) %>% 
  pull(row_id)

if(length(Exclusao) > 0){
  log_message(sprintf("Exclusao de emrpesas com LPA negativo: %s", paste(base_ERP$Ticker[Exclusao], collapse = ", ")))
  base_ERP[Exclusao, ]
  base_ERP <- base_ERP[-Exclusao, ]
}

# É possivel excluir o VPA negativo, porem o mesmo nao esta documentado no processo. Por essa razao o processo abaixo esta desligado.

# Exclui ações que tem VPA negativo
# Exclusao <- base_ERP %>%
#   mutate(row_id = row_number()) %>%
#   filter(VPA < 0) %>%
#   pull(row_id)
# 
# if(length(Exclusao) > 0){
#   log_message(sprintf("Exclusao de emrpesas com VPA negativo: %s", paste(base_ERP$Ticker[Exclusao], collapse = ", ")))
#   base_ERP[Exclusao, ]
#   base_ERP <- base_ERP[-Exclusao, ]
# }

# 3. São excluídas as ações para as quais o payout calculado é superior a 100%.
Exclusao <- base_ERP %>%
  mutate(row_id = row_number()) %>% 
  filter(Payout > 1) %>% 
  pull(row_id)

if(length(Exclusao) > 0){
  log_message(sprintf("Exclusao de emrpesas com Payout superior a 100%%: %s", paste(base_ERP$Ticker[Exclusao], collapse = ", ")))
  base_ERP[Exclusao, ]
  base_ERP <- base_ERP[-Exclusao, ]
}

# 4. São excluídas as ações para as quais DPA ou LPA ou VPA não foram informados
Exclusao <- base_ERP %>%
  mutate(row_id = row_number()) %>% 
  filter(is.na(DPA) |is.na(LPA) | is.na(VPA)) %>% 
  pull(row_id)

if(length(Exclusao) > 0){
  log_message(sprintf("Exclusao de emrpesas com DPA ou LPA ou VPA ausentes: %s", paste(base_ERP$Ticker[Exclusao], collapse = ", ")))
  base_ERP[Exclusao, ]
  base_ERP <- base_ERP[-Exclusao, ]
}

summary(base_ERP)


# Logic layer - Media aparada ---------------------------------------------

# A seguir, os valores de kj para as empresas restantes são ordenados e
# "winsorizados", excluindo-se os 10% de valores mais altos e os 10% de valores
# mais baixos, o que deixa uma amostra de aproximadamente 80 empresas, a qual
# varia de mês para mês, conforme a disponibilidade de dados e o atendimento das
# restrições descritas acima.

# Media aparada
E_k <- mean(base_ERP$k, trim = 0.1)

log_message(sprintf("E_k: %f", E_k))

ERP <- E_k - T10_Bond

cat(sprintf("ERP: %f", ERP), "\n")
log_message(sprintf("ERP: %f", ERP))

log_message("PROCESSO FINALIZADO")


# Escreve log de saida ----------------------------------------------------

file_name <- sprintf("%s ERP Log.txt", Sys.Date())
fileConn <- file(file_name, encoding = "UTF-8")
writeLines(str_log_message, fileConn)
close(fileConn)

