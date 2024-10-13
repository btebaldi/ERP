require(dplyr)


log_message <- function(msg, env = environment()){
  if(!exists(x = "str_log_message", envir = env)){
    str_log_message <<- "****** LOG DO PROCESSO DE CALCULO DO ERP ******";
  }
  
  str_log_message <<- sprintf("%s\n[%s] - %s",
                              str_log_message,
                              Sys.time(),
                              msg)
}

calculate_Columns <- function(database){
  
  # 1. São coletados os valores mais recentes de dividendos por ação,
  # representados por DIV_0.
  
  base_ERP <- database %>% mutate(DIV_0 = DPA, P_0 = Fechamento)
  
  
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
  
  return(base_ERP)
  
}

filter_NoFinantialInstitutions <- function(base_ERP){
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
  
  return(base_ERP)
}

filter_OneStockPerCompany <- function(base_ERP){
  
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
  
  return(base_ERP)
}

filter_OnlyTraded <- function(base_ERP){
  
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
  
  return(base_ERP)
}

filter_LPA <- function(base_ERP){
  
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
  
  return(base_ERP)
}

filter_VPA <- function(base_ERP){
  
  # É possivel excluir o VPA negativo, porem o mesmo nao esta documentado no
  # processo. Por essa razao o processo abaixo esta desligado.
  
  # Exclui ações que tem VPA negativo
  Exclusao <- base_ERP %>%
    mutate(row_id = row_number()) %>%
    filter(VPA <= 0) %>%
    pull(row_id)
  
  if(length(Exclusao) > 0){
    log_message(sprintf("Exclusao de emrpesas com VPA negativo: %s", paste(base_ERP$Ticker[Exclusao], collapse = ", ")))
    base_ERP[Exclusao, ]
    base_ERP <- base_ERP[-Exclusao, ]
  }
  return(base_ERP)
}

filter_Payout <- function(base_ERP){
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
  return(base_ERP)
}

filter_DPA <- function(base_ERP){
  
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
  
  return(base_ERP)
}

Sanity_Check <- function(base_ERP){
  
  checkDuplicatas <- base_ERP %>% 
    group_by(Nome) %>% 
    summarise(n = n()) %>% 
    filter(n > 1)
  
  if(nrow(checkDuplicatas) != 0){
    print(checkDuplicatas)
    stop("ERRO: ACOES DUPLICADAS!")
  }
  
}

Calculate_ERP <- function(base_ERP, T10_Bond){
  
  # A seguir, os valores de kj para as empresas restantes são ordenados e
  # "winsorizados", excluindo-se os 10% de valores mais altos e os 10% de valores
  # mais baixos, o que deixa uma amostra de aproximadamente 80 empresas, a qual
  # varia de mês para mês, conforme a disponibilidade de dados e o atendimento das
  # restrições descritas acima.
  
  
  # Media aparada
  E_k <- mean(base_ERP$k, trim = 0.1)
  
  log_message(sprintf("E_k: %f", E_k))
  
  if(is.na(E_k)){
    warning("NA values in ERP calculation.")
  }

  ERP <- E_k - T10_Bond
  return(ERP)
}
