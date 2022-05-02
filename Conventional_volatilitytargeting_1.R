# Dit script bepaalt met behulp van twee backtests de rendementen en portefeuillegewichten voor de niet-geschaalde strategie
# en de conventionele volatiliteitstargetingstrategie. 

# Pakketten activeren 
  library(xts) # Activeert het xts pakket 
  library(writexl) # Activeert het writexl pakket 
  library(PerformanceAnalytics) # Activeert het PerformanceAnalytics pakket 

# Rendementen inladen 
  returns <- readRDS("asset_returns.rds") # Zie Volatilitytargeting_1.R
  # Risicovolle activa rendementen 
    asset_returns <- returns[,-ncol(returns)] # Laatste kolom zijn risicovrije rendementen
  # Risicovrije rendementen
    rf_returns <- returns[,ncol(returns)]

# Vereiste inputs
  # Gewichten gebalanceerde portefeuille 
    w.MSCI.EU <-  0.25
    w.MSCI.PA <-  0.25
    w.MSCI.NA <-  0.25
    w.MSCI.EM <-  0.25
    w.USAGG <-  0
    w.GSCI <- 0
    weights.balanced <- c(w.MSCI.EU,w.MSCI.PA, w.MSCI.NA, w.MSCI.EM, w.USAGG, w.GSCI)
    weights.portfolio <- c(weights.balanced, 0) # Nul wordt toegevoegd voor het risicovrij activum
  # Plaats om resultaten op te slaan 
    file_map <- "Results/P1/L = inf/test/"  
  # Herbalanceringsfrequentie 
    rebalance.frequency = "months"  
  # Aantal jaren om de doelvolatiliteit te initialiseren 
    initialisation.years <-5
  # Leveragelimiet (zet op een groot getal (vb: 100) indien er geen limiet is)
    exposure_limit <- 100
  # EWMA (indien gebruikt)
    EWMA_used <- 0
    EWMA.window <- 250 # terugkijk window
    # Vervalfactor of halveringstijd moet gegeven worden bij gebruik van EWMA  
      EWMA.lamda <- 0.94 # Vervalfactor  
      #EWMA.half <- 10 # Halveringstijd 
  # Tabel die inputs bijhoudt voor latere controle
    inputs <- matrix(0, nrow = 5, ncol =6)
    row.names(inputs) <- c("Weights", "Frequency","Leverage limit", "EWMA", "Initialisation years")
    inputs[1,] <- weights.balanced
    inputs[2,1] <- rebalance.frequency
    inputs[3,1] <- exposure_limit
    inputs[4,1] <- EWMA_used
    inputs[5,1] <- initialisation.years

# Berekenen rendementen van niet geschaalde portefeuille
  unscaled.results <- Return.portfolio(returns, weights = weights.portfolio, rebalance_on = rebalance.frequency, verbose = TRUE) 
  # De Return.portfolio functie berekent dagelijkse rendementen voor de portefeuille die overeenstemt met de gekozen gewichten en de gekozen herbalanceringsfrequentie
  # Rendementen niet geschaalde portefeuille 
    unscaled.returns <- unscaled.results$returns
  # Portefeuillegewichten van de activa in de niet geschaalde portefeuille aan het begin van elke dag (Beginning Of Period - BOP)
    unscaled.bop_weight <- unscaled.results$BOP.Weight
  # Portefeuillegewichten van de activa in de niet geschaalde portefeuille aan het einde van elke dag (End Of Period - EOP)
    unscaled.eop_weight <- unscaled.results$EOP.Weight
  
# Berekenen gewichten voor volatiliteitstargeting 
  # Functie voor het berekenen van gerealiserde volatiliteit (voorspelde volatiliteit en de doelvolatiliteit)  
    realized_volatility <- function(returns){
      sqrt(mean(returns[-nrow(returns),]^2)) # Laatste rij (dag) wordt niet in rekening genomen om te verzekeren dat het 24u op voorhand gekend is
    }
  # Berekenen voorspelde volatiliteit (gerealiseerde volatiliteit)
    predicted_volatility <- period.apply(unscaled.returns, INDEX = endpoints(unscaled.returns, on = "months"), FUN = realized_volatility)
    colnames(predicted_volatility) <- "Predicted volatility"
  # Berekenen voorspelde volatiliteit (EWMA)
    # Bepalen lamda indien halveringstijd gegeven is 
      ##EWMA.lamda <- (0.5)^(1/EWMA.half)
    # Initialisatie van tabel die de gewichten bijhoudt van de verschillende observaties 
      #EWMA.weights <- matrix(0, nrow =EWMA.window, ncol = 1)
    # Bepalen gewichten 
      #for (i in 1:EWMA.window){
        #EWMA.weights[EWMA.window+1-i] <- ((1-EWMA.lamda)/(1-(EWMA.lamda^EWMA.window)))*(EWMA.lamda^(i-1))
      #}
    # Herbalanceringsmomenten 
      #rebalancing.moments <- endpoints(unscaled.returns, on = "months")[-1] # Eerste endpoint is 0 en wordt dus weggelaten
    # Initialisatie van tabel die voorspelde volatiliteit op ieder herbalanceringsmoment bijhoudt  
      #predicted_volatility <- xts(matrix(0, nrow = length(rebalancing.moments), ncol = 1), order.by =index(unscaled.returns[rebalancing.moments]))
    # Er zijn minstens 250 dagen data nodig (terugkijkperiode) voor het berekenen van de EWMA volatiliteit 
    #De volatiliteit kan berekend worden vanaf het 12e herbalanceringsmoment(dag 252)
      #for (i in rebalancing.moments[12:length(rebalancing.moments)]){
        #predicted_volatility[index(unscaled.returns[i])] <- sqrt(sum((unscaled.returns[(i-EWMA.window):(i-1)]^2)*EWMA.weights))
      #}
  # Berekenen doelvolatiliteit (gerealiseerde volatiliteit)
    # Initialisatie tabel die doelvolatiliteit bijhoudt   
      target_volatility <- xts(matrix(0, nrow = nrow(predicted_volatility),ncol = 1), order.by = index(predicted_volatility))
      colnames(target_volatility) <- "Target volatility"
    # Berekenen doelvolatilieit 
      for (d in endpoints(unscaled.returns, on = "months")[-1]){ # Eerste endpoint is 0 en wordt dus weggelaten 
        target_volatility[index(unscaled.returns[d])] <- realized_volatility(unscaled.returns[1:d])
      }
  # Berekenen blootstelling in gebalanceerde portefeuille of index 
    p.rebalance <- pmin(target_volatility/predicted_volatility, exposure_limit) # Indien doelvolatiliteit/ voorspelde volatiliteit hoger is dan de blootstellingslimiet, dan wordt de blootstelling gelijkgesteld aan de blootstellingslimiet  
    colnames(p.rebalance) <- "Exposure"
  # Gewichten bepalen
    # Initialisatie van tabel die de gewenste gewichten bijhoudt van de geschaalde portefeuille 
      weights.scaled <- xts(matrix(0,ncol = 7, nrow = nrow(p.rebalance)), order.by = index(p.rebalance))
      colnames(weights.scaled) <- c("MSCI EU", "MSCI PA", "MSCI NA", "MSCI EM", "BB US AGG", "GSCI", "RF")
    # Bepalen gewenste gewichten op ieder herbalanceringsmoment 
      for(r in 1:nrow(weights.scaled)){
        weights.scaled[r,7] <- 1 - p.rebalance[r,] # Gewicht in risicovrij activum = 1 - blootstelling in risicovolle activa 
        weights.scaled[r,c(1:6)] <- weights.balanced*rep(coredata(p.rebalance[r,]),6) # Gewicht risicovol activum = gewicht in oorspronkelijke portefeuille * blootstelling risicovolle activa 
      }

# Bepalen backtest periode 
  # Backtestperiode 
    # Laatste dag van de initialisatieperiode wordt bepaald 
      initialisation.last_day <- index(p.rebalance[initialisation.years*12])
    # Eerste dag van de backtestperiode wordt bepaald 
      backtest.first_day <- index(unscaled.returns[unscaled.returns[initialisation.last_day, which.i =TRUE]+1,])
    # Indexen in backtestperiode worden bepaald 
      backtest_period <- index(unscaled.returns[paste0(backtest.first_day,"/")])
    # Backtestperiode van tijdreeksen splitsen  
      backtest.returns <- returns[backtest_period]
      backtest.asset_returns <- asset_returns[backtest_period]
      backtest.rf_returns <- rf_returns[backtest_period]
      backtest.unscaled.returns <- unscaled.returns[backtest_period]
      backtest.unscaled.bop_weight <- unscaled.bop_weight[backtest_period]
      backtest.unscaled.eop_weight <- unscaled.eop_weight[backtest_period]
      backtest.p.rebalance <- p.rebalance[backtest_period]
      backtest.target_volatility <- target_volatility[backtest_period]
      backtest.predicted_volatility <- predicted_volatility[backtest_period]
    # Indexen voor gewichten 
      weights_period <- index(weights.scaled[paste0(initialisation.last_day,"/")]) # De gewenste gewichten beginnen op de laatste dag van de initialisatie periode, de functie neemt dit namelijk als het bop gewicht van de eerste dag 
    # Gewichten backtestperiode 
      backtest.weights_scaled <- weights.scaled[weights_period]
      #backtest.weights_scaled <- backtest.weights_scaled[-nrow(backtest.weights_scaled),]
  
# Berekenen geschaalde rendementen
  scaled.results <- Return.portfolio(backtest.returns, weights = backtest.weights_scaled, verbose = TRUE)
  # Rendementen van geschaalde portefeuille 
    scaled.returns <- scaled.results$returns
  # Portefeuillegewichten van de activa in de geschaalde portefeuille aan het begin van elke dag (Beginning Of Period - BOP)
    scaled.bop_weight <- scaled.results$BOP.Weight
  # Portefeuillegewichten van de activa in de geschaalde portefeuille aan het einde van elke dag (End Of Period - EOP)
    scaled.eop_weight <- scaled.results$EOP.Weight

# Resultaten opslaan
  #saveRDS(inputs, paste0(file_map,"inputs.rds"))
  #saveRDS(scaled.returns, paste0(file_map,"scaled_returns.rds"))
  #saveRDS(backtest.unscaled.returns, paste0(file_map,"unscaled_returns.rds"))
  #saveRDS(backtest.rf_returns, paste0(file_map,"rf_returns.rds"))
  #saveRDS(scaled.bop_weight, paste0(file_map,"scaled_bop_weights.rds"))
  #saveRDS(backtest.unscaled.bop_weight, paste0(file_map,"unscaled_bop_weights.rds"))
  #saveRDS(scaled.eop_weight, paste0(file_map,"scaled_eop_weights.rds"))
  #saveRDS(backtest.unscaled.eop_weight, paste0(file_map,"unscaled_eop_weights.rds"))
  #saveRDS(backtest.target_volatility, paste0(file_map,"target_volatility.rds"))
  #saveRDS(backtest.predicted_volatility, paste0(file_map,"predicted_volatility.rds"))
  #saveRDS(backtest.p.rebalance, paste0(file_map,"P_rebalance.rds"))
  #saveRDS(backtest.asset_returns, paste0(file_map,"asset_returns.rds"))
  
