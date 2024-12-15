

# Data load ---------------------------------------------------------------

message("Loading data")

file_to_load <- file.path("./Database/Add In Economatica.xlsx")

dataRef <- read_excel(file_to_load,
                      cell_limits(ul = c(1,2), lr = c(1, 2)),
                      na = "-",
                      col_names = c("dateRef"),
                      sheet = "Acoes Bovespa")

data_ref <- lubridate::floor_date(dataRef$dateRef, "month")
message(sprintf("data_ref: %s", data_ref))


base_ERP_full <- read_excel("Database/Add In Economatica.xlsx",
                            cell_limits(ul = c(5,2), lr = c(NA, 11)),
                            na = "-",
                            col_names = c("id", "Nome", "Classe", "Ticker", "DPA", "LPA", "VPA", "Volume", "Setor", "Fechamento"),
                            sheet = "Acoes Bovespa")

base_ERP_full <- base_ERP_full %>% dplyr::select(c("id", "Nome", "Classe",
                                                   "Ticker",
                                                   "DPA", "LPA", "VPA",
                                                   "Volume", "Setor", "Fechamento"))


Tbond <- read_excel("Database/Add In Economatica.xlsx",
                    cell_limits(ul = c(3,1), lr = c(NA, 2)),
                    na = "-",
                    col_names = c("date", "TBond"),
                    sheet = "TBond")



# Filtragem e preparacao inicial ------------------------------------------

# Can be downloaded from
# https://finance.yahoo.com/quote/%5ETNX/history/?period1=1570976431&period2=1728824359
T10_Bond <- Tbond %>% 
  mutate(date = as.Date(date), yearMonth = floor_date(date, "month"), TBond2 = zoo::na.locf(TBond)) %>% 
  group_by(yearMonth) %>% 
  filter(date == max(date)) %>%
  filter(yearMonth == as.Date(data_ref)) %>% pull(TBond2)/100

message(sprintf("T10_Bond: %s", T10_Bond))

base_ERP_full <- base_ERP_full %>% filter(!is.na(Ticker))


rm(list = c("dataRef", "Tbond", "file_to_load"))











