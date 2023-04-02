library(openxlsx)
require(lubridate)

AtualizarSerieERP <- function(data, ERP){
  
  # validacoes
  if(!lubridate::is.Date(data)){
    stop("data must be a date.")
  } else if(!base::is.numeric(ERP)){
    stop("ERP must be numeric.")
  }
  
  # Cria variaveis para atualizacao
  value <- data.frame(X1 = data,
                      X2 = ERP)
  
  # Abre a planilha
  wb <- loadWorkbook(file = "Database/Serie de Equity Risk Premium.xlsx")
  
  #  Faz a leitura dos dados para determinar a ultima linha de escrita.
  tbl <- read.xlsx(wb)
  st_Row = nrow(tbl)+2
  
  # Escreve os dados na planilha
  writeData(wb, sheet = "Planilha1",
            x = value,
            startCol = "A",
            startRow = st_Row,
            colNames = FALSE)
  
  # Add some styles
  # addStyle(wb, sheet = "mysheet", rows = 2, cols = "B", 
  #          style = createStyle(textDecoration = "Bold", fgFill = "yellow"))
  
  # Salva os dados na planilha
  saveWorkbook(wb, file = "./Database/Serie de Equity Risk Premium.xlsx", overwrite = TRUE)
  
}