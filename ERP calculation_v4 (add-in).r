#' Autor: Bruno Tebaldi Barbosa
#'
#' Create: 2023-01-18
#'
#' Modifications:
#'  - 20224-10-11 : Version 4.0 Created functions
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
source(file = "./fct_ERP_Logic.R")
source(file = "./fct_ERP_Padronization.R")


# User defined variables --------------------------------------------------

# dataRef <- "2024-08-14"
# T10_Bond <- 03.8020/100
# 
# file <- "C:/Users/bteba/Documents/GitHub/ERP/Historico/ref 2024-09-01/2024-10-13 11_06_26.239924 - Base_ERP.xlsx"


# Data load ---------------------------------------------------------------

log_message("PROCESSO INICIADO")

source(file = "./Process_ERP_LoadData.R")
# source(file = "./Process_ERP_LoadData(historical).R")

# Filtragem e preparacao inicial ------------------------------------------

log_message(sprintf("data de referencia: %s", data_ref))

base_ERP_full <- base_ERP_full %>% filter(!is.na(Ticker))


log_message(sprintf("TBond: %f", T10_Bond))
log_message(sprintf("Lista de empresas consideradas: %s", paste(base_ERP_full$Ticker, collapse = ", ")))
cat(sprintf("data ref: %s \nT10_Bond = %f", data_ref, T10_Bond))
# Database Preparation ----------------------------------------------------

base_ERP_full <- SectorPadronization(base_ERP_full)


# Logic layer - Calculo de k por acao -------------------------------------


base_ERP <- calculate_Columns(base_ERP_full)
base_ERP <- filter_NoFinantialInstitutions(base_ERP)
base_ERP <- filter_OneStockPerCompany(base_ERP)
base_ERP <- filter_OnlyTraded(base_ERP)
base_ERP <- filter_LPA(base_ERP)
base_ERP <- filter_VPA(base_ERP)
base_ERP <- filter_Payout(base_ERP)
base_ERP <- filter_DPA(base_ERP)
Sanity_Check(base_ERP)


summary_base_ERP <- summary(base_ERP)

# Logic layer - Media aparada ---------------------------------------------
ERP <- Calculate_ERP(base_ERP, T10_Bond)

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

