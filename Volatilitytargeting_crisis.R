# Dit script berekent de cumulatieve rendementen gedurende de kredietcrisis van 2008 an de coronacrisis. 
# Voor het uitvoeren van dit script is het belangrijk belangrijk om de plaats waarin de backtestresultaten zijn opgeslagen te wijzigen. 

# Inladen pakketten
  library(PerformanceAnalytics)
  library(xts)

# Map waarin backtestresultaten zijn opgeslagen - vergeet naam niet te wijzigen 
  file_map <- "Results/P1/L = inf/"

# Data inladen 
  unscaled.returns <- readRDS(paste0(file_map,"unscaled_returns.rds")) # Niet-geschaalde rendementen 
  scaled.returns <- readRDS(paste0(file_map,"scaled_returns.rds")) # Geschaalde rendementen 

# Crisis rendementen
  # Initialisatie tabel die crisisrendementen bijhoudt 
    crisis <- matrix(0, nrow = 2, ncol = 2)
    colnames(crisis) <- c("2008", "covid")
    rownames(crisis) <- c("unscaled", "scaled")
  # Berekenen rendementen gedurende crisis
    crisis[1,1] <- Return.cumulative(unscaled.returns["2007-10/2009-03"]) # Niet-geschaald cumulatief rendement gedurende kredietcrisis
    crisis[2,1] <- Return.cumulative(scaled.returns["2007-10/2009-03"]) # Geschaald cumulatief rendement gedurende kredietcrisis
    crisis[1,2] <- Return.cumulative(unscaled.returns["2020-02/2020-04"]) # Niet-geschaald cumulatief rendement gedurende coronacrisis
    crisis[2,2] <- Return.cumulative(scaled.returns["2020-02/2020-04"]) # Geschaald cumulatief rendement gedurende coronacrisis