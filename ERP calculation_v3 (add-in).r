#' Autor: Bruno Tebaldi Barbosa
#'
#' Data: 2023-01-18
#'
#' Processo de calculo do Equity Risk Premium (ERP). Baseado no documento: 
#' Antonio Zoratto Sanvicente (October 23, 2017) - ESTIMATIVAS DO EQUITY RISK PREMIUM PARA O MERCADO BRASILEIRO DE CAPITAIS
#'
#' Input: Planilha com dados do Economatica contendo: Last_Price, VPA, LPA e DPA
#'
#' Output: Arquivos txt e planilha excel(atualizada)

# Setup -------------------------------------------------------------------
rm(list = ls())

library(readxl)
library(dplyr)
require(writexl)
require(stringr)
library(lubridate)


# User defined functions and variables ------------------------------------

source(file = "./fct_AtualizaSerieErp.R")

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

dataRef <- read_excel("Database/Add In Economatica.xlsx",
                            cell_limits(ul = c(1,2), lr = c(1, 2)),
                            na = "-",
                            col_names = c("dateRef"),
                            sheet = "Acoes Bovespa")

data_ref <- lubridate::floor_date(dataRef$dateRef, "month")


base_ERP_full <- read_excel("Database/Add In Economatica.xlsx",
                            cell_limits(ul = c(5,2), lr = c(NA, 11)),
                            na = "-",
                            col_names = c("id", "Nome", "Classe", "Ticker", "DPA", "LPA", "VPA", "Volume", "Setor", "Fechamento"),
                            sheet = "Acoes Bovespa")


Tbond <- read_excel("Database/Add In Economatica.xlsx",
                    cell_limits(ul = c(3,1), lr = c(NA, 2)),
                    na = "-",
                    col_names = c("date", "TBond"),
                    sheet = "TBond")



# Filtragem e preparacao inicial ------------------------------------------


Tbond %>% 
  mutate(date = as.Date(date), yearMonth = floor_date(date, "month"), TBond2 = zoo::na.locf(TBond)) %>% 
  group_by(yearMonth) %>% 
  filter(date == max(date)) %>%
  filter(yearMonth == as.Date(data_ref)) %>% pull(TBond2)/100 -> T10_Bond
  

base_ERP_full <- base_ERP_full %>% filter(!is.na(Ticker))


log_message(sprintf("TBond: %f", T10_Bond))
log_message(sprintf("Lista de empresas consideradas: %s", paste(base_ERP_full$Ticker, collapse = ", ")))

# Database Preparation ----------------------------------------------------


Setor_levels_labels <- c("Petróleo e Gas"          = "Petroleo e Gas",
                         "Agro e Pesca"            = "Agro e Pesca",
                         "Energia Elétrica"        = "Energia Eletrica",
                         "Finanças e Seguros"      = "Financas e Seguros",
                         "Siderur & Metalur"       = "Siderurgia e Metalurgia",
                         "Máquinas Indust"         = "Maquinas Industriais",
                         "Outros"                  = "Outros",
                         "Transporte Serviç"       = "Transporte Servico",
                         "Comércio"                = "Comercio",
                         "Textil"                  = "Textil",
                         "Construção"              = "Construcao",
                         "Alimentos e Beb"         = "Alimentos e Bebidas",
                         "Telecomunicações"        = "Telecomunicacoes",
                         "Mineração"               = "Mineracao",
                         "Software e Dados"        = "Software e Dados",
                         "Veiculos e peças"        = "Veiculos e pecas",
                         "Química"                 = "Quimica",
                         "Minerais não Met"        = "Minerais nao Metais",
                         "Eletroeletrônicos"       = "Eletroeletronicos",
                         "Papel e Celulose"        = "Papel e Celulose",
                         "Fundos"                  = "Fundos",
                         "Petroleo e Gas"          = "Petroleo e Gas",
                         "Agro e Pesca"            = "Agro e Pesca",
                         "Energia Eletrica"        = "Energia Eletrica",
                         "Financas e Seguros"      = "Financas e Seguros",
                         "Siderurgia e Metalurgia" = "Siderurgia e Metalurgia",
                         "Maquinas Industriais"    = "Maquinas Industriais",
                         "Outros"                  = "Outros",
                         "Transporte Servico"      = "Transporte Servico",
                         "Comercio"                = "Comercio",
                         "Textil"                  = "Textil",
                         "Construcao"              = "Construcao",
                         "Alimentos e Bebidas"     = "Alimentos e Bebidas",
                         "Telecomunicacoes"        = "Telecomunicacoes",
                         "Mineracao"               = "Mineracao",
                         "Software e Dados"        = "Software e Dados",
                         "Veiculos e pecas"        = "Veiculos e pecas",
                         "Quimica"                 = "Quimica",
                         "Minerais nao Metais"     = "Minerais nao Metais",
                         "Eletroeletronicos"       = "Eletroeletronicos",
                         "Papel e Celulose"        = "Papel e Celulose",
                         "Fundos"                  = "Fundos",
                         "Oil & Gas"               = "Petroleo e Gas",
                         "Agri & Fisheries"        = "Agro e Pesca",
                         "Electric Power"          = "Energia Eletrica",
                         "Finance and Insurance"   = "Financas e Seguros",
                         "Basic & Fab Metal"       = "Siderurgia e Metalurgia",
                         "Industrial Machin"       = "Maquinas Industriais",
                         "Other"                   = "Outros",
                         "Transportat Serv"        = "Transporte Servico",
                         "Trade"                   = "Comercio",
                         "Textile"                 = "Textil",
                         "Construction"            = "Construcao",
                         "Food & Beverage"         = "Alimentos e Bebidas",
                         "Telecommunication"       = "Telecomunicacoes",
                         "Mining"                  = "Mineracao",
                         "Software & Data"         = "Software e Dados",
                         "Vehicle & Parts"         = "Veiculos e pecas",
                         "Chemical"                = "Quimica",
                         "Nonmetallic Min"         = "Minerais nao Metais",
                         "Electric Electron"       = "Eletroeletronicos",
                         "Pulp & Paper"            = "Papel e Celulose",
                         "Funds"                   = "Fundos")

base_ERP_full$Setor <- factor(x = base_ERP_full$Setor,
                              levels = names(Setor_levels_labels),
                              labels = Setor_levels_labels)

if(any(is.na(base_ERP_full$Setor))){
  print(base_ERP_full[is.na(base_ERP_full$Setor), ])
  stop("ERRO: SETOR COMO 'NA'!")
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


# I. Não são utilizadas ações de instituições financeiras (setor Finanças, na
# base da Economática), porque suas estruturas de capital têm características
# bem distintas das estruturas de passivos de empresas industriais, comerciais e
# de prestação de serviços. Ou sejam, são altamente alavancadas por força da
# natureza de sua operação.
Exclusao <- base_ERP %>%
  mutate(row_id = row_number()) %>% 
  filter(Setor %in% c("Financas e Seguros", "Fundos")) %>% 
  pull(row_id)

if(length(Exclusao) > 0){
  log_message(sprintf("Exclusao de emrpesas do setores 'Financas e Seguros', 'Fundos': %s", paste(base_ERP$Ticker[Exclusao], collapse = ", ")))
  base_ERP[Exclusao, ]
  base_ERP <- base_ERP[-Exclusao, ]
}

# II. São usados preços de apenas uma das classes de ações de cada empresa
# incluída na amostra, para se evitar que haja dupla contagem.

Exclusao <- base_ERP %>%
  mutate(row_id = row_number()) %>%
  group_by(Nome) %>%
  mutate(K2 = Volume == max(Volume)) %>%
  filter(K2 == FALSE | Volume == 0) %>%
  pull(row_id)

if(length(Exclusao) > 0){
  log_message(sprintf("Exclusao de ativos da mesma empresa: %s", paste(base_ERP$Ticker[Exclusao], collapse = ", ")))
  base_ERP[Exclusao, ]
  base_ERP <- base_ERP[-Exclusao, ]
}

# 1. São excluídas as ações que não apresentaram cotação de fechamento no mês.
Exclusao <- base_ERP %>%
  mutate(row_id = row_number()) %>% 
  filter(is.na(Fechamento) | Fechamento <= 0) %>% 
  pull(row_id)

if(length(Exclusao) > 0){
  log_message(sprintf("Exclusao de emrpesas sem preço de fechamento: %s", paste(base_ERP$Ticker[Exclusao], collapse = ", ")))
  base_ERP[Exclusao, ]
  base_ERP <- base_ERP[-Exclusao, ]
}



base_ERP_check <- base_ERP %>%
  group_by(Nome) %>% 
  summarise(P_0 = mean(if_else(is.na(P_0), true = 0, false = P_0), na.rm = TRUE), n = n()) %>%
  filter(n > 1)

if(nrow(base_ERP_check)){ stop("ERRO: EMPRESA DUPLICADA!") }
rm("base_ERP_check")



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

# É possivel excluir o VPA negativo, porem o mesmo nao esta documentado no
# processo. Por essa razao o processo abaixo esta desligado.

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

# Security check ----------------------------------------------------------

checkDuplicatas <- base_ERP %>% group_by(Nome) %>% summarise(n = n()) %>% filter(n > 1) %>% nrow()

if(checkDuplicatas != 0){
  stop("ERRO: ACOES DUPLICADAS!")
}

summary_base_ERP <- summary(base_ERP)

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


# Escreve log de saida e arquivos em diretorio Historico ------------------


# diretorio para guarda dos aqruivos
dir <- file.path(".", "Historico", sprintf("ref %s", data_ref))

#  cria o diretorio
if(!dir.exists(dir)){ dir.create(dir) }

# Log de saida
file_name <- stringr::str_replace_all(string = sprintf("%s - ERP Log_v2.txt", Sys.time()), pattern = ":", replacement = "_")
fileConn <- file(file.path(dir, file_name), encoding = "UTF-8")
writeLines(str_log_message, fileConn)
close(fileConn)


# Copia a base de dados
detination_file <- file.path(dir, sprintf("%s - Base_ERP.xlsx", Sys.time()))
detination_file <- stringr::str_replace_all(string = detination_file, pattern = ":", replacement = "_")

# Escreve a base de dados
writexl::write_xlsx(x = base_ERP_full, path = detination_file)

# Copia estatisticas descritivas
summary_file_name <- stringr::str_replace_all(string = sprintf("%s - summary base_ERP.txt", Sys.time()), pattern = ":", replacement = "_")
write.table(x = summary(base_ERP), file = file.path(dir, summary_file_name) )


# Salvando dados na planilha do ERP ---------------------------------------
AtualizarSerieERP(data = as.Date(data_ref), ERP = ERP)

