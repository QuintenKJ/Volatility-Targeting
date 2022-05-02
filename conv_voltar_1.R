# Inladen pakketten 
  library(readxl) # Activeert het readxl pakket
  library(xts) # Activeert het xts pakket
  library(PerformanceAnalytics) # Activeert het performanceAnalytics pakkket
  library(writexl) # Activeert het writexl pakket 

# Inladen datasets van Datastream
  # Aandelen 
    data.equity <- read_excel("MSCI_PR_daily.xlsm", skip = 5,col_types = c("date","numeric", "numeric", "numeric", "numeric"), col_names = c("Date","MSCI.EU", "MSCI.PA","MSCI.NA","MSCI.EM"))
  # Obligaties 
    data.bonds <- read_excel("BB_TR_daily.xlsm", skip = 5,col_types = c("date","numeric"), col_names = c("Date","BB.USAGG"))
    # Datastream geeft de Bloomberg US Aggregate TR index als het verschil met de initiële investering uitgedrukt als een percentage. Om de oorspronkelijke index te bekomen moet 100 bijgeteld worden:
      data.bonds[,2] <- data.bonds[,2] + 100
  # Commodities
    data.commodity <- read_excel("GSCI_TR_daily.xlsm", skip = 5,col_types = c("date","numeric"), col_names = c("Date","GSCI"))
  # Risicovrij activa 
    return.rf <- read_excel("KFRENCH_RF_daily.xlsx", skip = 5,col_types = c("date","numeric"), col_names = c("Date","RF"))
    # K. French database geeft het rendement als een percentage. Om het percentage om te zetten in een decimaal wordt het gedeeld door 100.
      return.rf[,2] <- return.rf[,2]/100 

# Data omzetten in xts objecten (tijdreeksen)
  data.equity <- xts(data.equity[,-1], order.by = data.equity$Date)
  data.bonds <- xts(data.bonds[,-1], order.by = data.bonds$Date)
  data.commodity <- xts(data.commodity[,-1], order.by = data.commodity$Date)
  return.rf <- xts(return.rf[,-1], order.by = return.rf$Date)
  
# Prijzen omzetten in eenvoudige rendementen
  return.equity <- diff(data.equity, arithmetic = FALSE, na.pad = FALSE) - 1
  return.bonds <- diff(data.bonds, arithmetic = FALSE, na.pad = FALSE) - 1
  return.commodity <- diff(data.commodity, arithmetic = FALSE, na.pad = FALSE) - 1

# Rendementen samenvoegen in één xts object 
  returns <- merge(return.equity,return.bonds,return.commodity,return.rf, join ="inner") # Join = "inner" zorgt ervoor dat enkel data die voorkomen in alle datasets worden opgenomen in returns   
  returns <- returns["1989/2021"] # Data vanaf jan 1989 (laatste datum waarop dagelijkse data beschikbaar wordt) tot en met dec 2021 
  returns <- na.omit(returns) # Rijen met een na waarde worden geëlimineerd, dit zorgt ervoor dat de data overeenkomen met de data van de K. French database
  colnames(returns) <- c("MSCI.EU", "MSCI.PA","MSCI.NA","MSCI.EM","BB.USAGG","GSCI", "RF")
  
# Rendementen opslaan 
  #saveRDS(returns,"asset_returns.rds")
  
# Beschrijvende statistieken
  stats <- table.Stats(returns) # Geeft een tabel met beschrijvende statistieken voor de rendementen 
  write_xlsx(stats, "descriptives.xlsx") # Bewaart de tabel met beschrijvende statistieken in een Excel bestand    
