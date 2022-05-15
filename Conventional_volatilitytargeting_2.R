# Dit script bepaalt met behulp van twee backtests de rendementen en portefeuillegewichten voor de niet-geschaalde strategie en de conventionele volatiliteitstargetingstrategie. 
# In dit script wordt er geen gebruik gemaakt van de Return.portfolio() functie (zoals in Conventional_volatilitytargeting_1.R), hierdoor is het uitvoeren van het script aanzienlijk trager. 
# De bedoeling van dit script is om de lezer een beter begrip te geven van de praktische werking van de conventionele volatiliteitstargetingstrategie. 
# Voor het uitvoeren van dit script is het belangrijk dat de working directory gelijk wordt gesteld aan de map waarin het RDS bestand met rendementen is opgeslagen (zie Volatilitytargeting_1.R).
# Vergeet niet om volgende zaken aan te passen: de gewenste portefeuillegewichten (als decimaal), de plaats om de resultaten op te slaan, de blootstellingslimiet.
# Indien de EWMA methode gebruikt wordt, dan moeten de volgende zaken aangepast worden: lamda of halveringstijd ingeven, de sectie "Berekenen voorspelde volatiliteit (gerealiseerde volatiliteit)" deactiveren 
# en de sectie "Berekenen voorspelde volatiliteit (EWMA)" activeren. 

# Pakketten activeren 
  library(xts)
  library(writexl)
  library(PerformanceAnalytics)

# Rendementen inladen 
  returns <- readRDS("asset_returns.rds") # Zie volatilitytargeting_1.R
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
  # Plaats om resultaten op te slaan 
    file_map <- "Results/P1/L = inf/"  
  # Herbalanceringsfrequentie 
    rebalance.frequency = "months"  
  # Aantal jaren om de doelvolatiliteit te initialiseren 
    initialisation.years <-5
  # Blootstellingslimiet (zet op een groot getal (vb: 100) indien er geen limiet is)
    exposure_limit <- 100
  # EWMA (indien gebruikt)
    EWMA_used <- 0
    EWMA.window <- 250 # Terugkijk window 
    # Vervalfactor of halveringstijd moet gegeven worden bij gebruik van EWMA 
      EWMA.lamda <- 0.94
      #EWMA.half <- 90
  # Inputs tabel (voor latere controle)
    inputs <- matrix(0, nrow = 5, ncol =6)
    row.names(inputs) <- c("Weights", "Frequency","Leverage limit", "EWMA", "Initialisation years")
    inputs[1,] <- weights.balanced
    inputs[2,1] <- rebalance.frequency
    inputs[3,1] <- exposure_limit
    inputs[4,1] <- EWMA_used
    inputs[5,1] <- initialisation.years

# Berekenen niet-geschaalde portefeuille rendementen 
  # Initialisatie resultaattabellen 
    # Tabel die de waarde bijhoudt van de activa in de niet-geschaalde portefeuille aan het begin van elke dag (beginning of period - bop) 
      unscaled.bop_value <- xts(matrix(0,nrow = nrow(asset_returns), ncol = (ncol(asset_returns) + 1)) , order.by = index(asset_returns)) # Elke rij is een dag, elke kolom is een activa  
      colnames(unscaled.bop_value) <- c(colnames(asset_returns), "Total")
    # Tabel die de waarde bijhoudt van de activa in de niet-geschaalde portefeuille aan het einde van elke dag (end of period - eop) 
      unscaled.eop_value <- unscaled.bop_value
    # Tabel die de gewichten bijhoudt van de activa in de niet-geschaalde portefeuille aan het begin van elke dag 
      unscaled.bop_weight <- xts(matrix(0,nrow = nrow(asset_returns), ncol = ncol(asset_returns)) , order.by = index(asset_returns)) # Elke rij is een dag, elke kolom is een activa
      colnames(unscaled.bop_weight) <- colnames(asset_returns)
    # Tabel die de gewichten bijhoudt van de activa in de niet-geschaalde portefeuille aan het einde van elke dag
      unscaled.eop_weight <- unscaled.bop_weight
    # Tabel die de niet-geschaalde portefeuillerendementen bijhoudt
      unscaled.returns <- xts(matrix(0,nrow = nrow(asset_returns), ncol = 1) , order.by = index(asset_returns)) # Elke rij is een dag
      colnames(unscaled.returns) <- "Return unscaled portfolio"
  
  # Initiele waarde bepalen
    # Initiele portefeuillewaarde wordt gelijkgesteld aan 100
      value_0 <- 100
    # Waarde van individuele activa aan het begin van de eerste dag is gelijk aan de initiele portefeuille waarde vermenigvuldigd met het overeenkomstig gewicht 
      unscaled.bop_value[1,-ncol(unscaled.bop_value)] <- value_0 * weights.balanced
    # Totale waarde van de portefeuille aan het begin van de eerste dag 
      unscaled.bop_value[1,ncol(unscaled.bop_value)] <- sum(unscaled.bop_value[1,-ncol(unscaled.bop_value)]) # Zou gelijk moeten zijn aan value_0
    # Gewichten van individuele activa aan het begin van de eerste dag 
      unscaled.bop_weight[1,] <- weights.balanced
  
  # Backtest uitvoeren 
    for (d in 1:(nrow(asset_returns)-1)){ # Lus over de dagen (behalve de laatste dag) 
      # Berekenen waarde van activa in portefeuille op het einde van de dag 
        unscaled.eop_value[d,-ncol(unscaled.eop_value)] <- (1 + asset_returns[d,])*unscaled.bop_value[d,-ncol(unscaled.bop_value)] # De waarde van een activum aan het einde van de dag is gelijk aan de waarde van het activum aan het begin van de dag vermenigvuldigt met 1 + overeenkomstig rendement van de dag 
      # Totale waarde van de portefeuille op het einde van de dag 
        unscaled.eop_value[d,ncol(unscaled.eop_value)] <- sum(unscaled.eop_value[d,-ncol(unscaled.eop_value)]) # Totale portefeuillewaarde is gelijk aan de som van de individuele activawaarden 
      # Berekenen portefeuillegewichten van activa in portefeuille op het einde van de dag 
        unscaled.eop_weight[d,] <- unscaled.eop_value[d,-ncol(unscaled.eop_value)]/rep(unscaled.eop_value[d,ncol(unscaled.eop_value)], (ncol(unscaled.eop_value)-1)) # elke activumwaarde wordt gedeeld door de totale portefeuillewaarde 
      # Portefeuillerendement berekenen
        unscaled.returns[d,] <- (unscaled.eop_value[d,ncol(unscaled.eop_value)]/unscaled.bop_value[d,ncol(unscaled.bop_value)]) - 1 # Portefeuillerendement is de procentuele verandering in totale portefeuillewaarde  
        if (d %in% endpoints(asset_returns, on=rebalance.frequency)){ # Indien dag d een herbalanceringsdag is
          # Waarde van activa worden geherbalanceerd 
            unscaled.bop_value[(d+1),-ncol(unscaled.bop_value)] <- value_0 * weights.balanced # De nieuwe activawaarden zijn gelijk aan de totale portefeuillewaarde op het einde van de vorige dag, vermenigvuldigt met de gewenste gewichten  
          # Totale portefeuillewaarde 
            unscaled.bop_value[(d+1),ncol(unscaled.bop_value)] <- sum(unscaled.bop_value[(d+1),-ncol(unscaled.bop_value)])
          # Gewichten van activa begin volgende dag 
            unscaled.bop_weight[(d+1),] <- weights.balanced
        } else{ # Geen herbalanceringsdag 
          # Waarden van activa aan het begin van de volgende dag zijn gelijk aan de waarden van de activa aan het einde van deze dag 
            unscaled.bop_value[(d+1),] <- unscaled.eop_value[d,]
          # Gewichten van activa aan het begin van de volgende dag zijn gelijk aan de gewichten van de activa aan het einde van deze dag 
            unscaled.bop_weight[(d+1),] <- unscaled.eop_weight[d,]
        }
    }
  # Activa waarden en gewichten bepalen op het einde van de laatste dag 
    unscaled.eop_value[nrow(unscaled.eop_value),-ncol(unscaled.eop_value)] <- (1 + asset_returns[nrow(unscaled.eop_value),])*unscaled.bop_value[nrow(unscaled.eop_value),-ncol(unscaled.eop_value)]
    unscaled.eop_value[nrow(unscaled.eop_value),ncol(unscaled.eop_value)] <- sum(unscaled.eop_value[nrow(unscaled.eop_value),-ncol(unscaled.eop_value)])
    unscaled.eop_weight[nrow(unscaled.eop_weight),] <- unscaled.eop_value[nrow(unscaled.eop_value),-ncol(unscaled.eop_value)]/rep(unscaled.eop_value[nrow(unscaled.eop_value),ncol(unscaled.eop_value)], (ncol(unscaled.eop_value)-1))
  # Portefeuillerendement laatste dag 
    unscaled.returns[nrow(unscaled.returns),] <- (unscaled.eop_value[nrow(unscaled.returns),ncol(unscaled.eop_value)]/ unscaled.bop_value[nrow(unscaled.returns),ncol(unscaled.bop_value)]) - 1
  
  
# Berekenen rendementen volatiliteitstargetingportefeuille 
  # Functie voor het berekenen van de gerealiseerde volatiliteit (voorspelde volatiliteit en doelvolatiliteit)  
    realized_volatility <- function(returns){
      sqrt(mean(returns[-nrow(returns),]^2)) # Laatste rij (dag) wordt niet in rekening genomen om te verzekeren dat het 24u op voorhand gekend is.
    }
  # Berekenen voorspelde volatiliteit (gerealiseerde volatiliteit) 
    #predicted_volatility <- period.apply(unscaled.returns, INDEX = endpoints(unscaled.returns, on = "months"), FUN = realized_volatility)
    #colnames(predicted_volatility) <- "Predicted volatility"
  # Berekenen voorspelde volatiliteit (EWMA) 
    # Initialisatie tabel met gewichten die aan de verschillende observaties gegeven worden  
      EWMA.weights <- matrix(0, nrow =EWMA.window, ncol = 1)
    # Bepalen lamda (indien de halveringstijd is gegeven) 
      #EWMA.lamda <- (0.5)^(1/EWMA.half)
    # Bepalen gewichten 
      for (i in 1:EWMA.window){
       EWMA.weights[EWMA.window+1-i] <- ((1-EWMA.lamda)/(1-(EWMA.lamda^EWMA.window)))*(EWMA.lamda^(i-1))
      }
    # Herbalanceringsmomenten 
      rebalancing.moments <- endpoints(unscaled.returns, on = "months")[-1] # Eerste waarde is 0
    # Initialisatie tabel met voorspelde volatiliteit 
      predicted_volatility <- xts(matrix(0, nrow = length(rebalancing.moments), ncol = 1), order.by =index(unscaled.returns[rebalancing.moments]))
    # Er zijn minstens 250 dagen data nodig (terugkijkperiode) voor het berekenen van de EWMA volatiliteit. De volatiliteit kan berekend worden vanaf het 12e herbalanceringsmoment(dag 252)
      for (i in rebalancing.moments[12:length(rebalancing.moments)]){
       predicted_volatility[index(unscaled.returns[i])] <- sqrt(sum((unscaled.returns[(i-EWMA.window):(i-1)]^2)*EWMA.weights))
      }
  
  # Berekenen doelvolatiliteit
    # Initialisatie tabel die doelvolatiliteit bijhoudt 
      target_volatility <- xts(matrix(0, nrow = nrow(predicted_volatility),ncol = 1), order.by = index(predicted_volatility))
      colnames(target_volatility) <- "Target volatility"
    # Berekenen van doelvolatiliteit door de gerealiseerde volatiliteit te bepalen van de eerste observatie tot het betreffende herbalanceringsmoment 
      for (d in endpoints(unscaled.returns, on = "months")[-1]){ # Lus over herbalanceringsmomenten, eerste endpoint is 0 en wordt dus weggelaten 
        target_volatility[index(unscaled.returns[d])] <- realized_volatility(unscaled.returns[1:d])
      }
  # Berekenen blootstelling in gebalanceerde portefeuille 
    p.rebalance <- pmin(target_volatility/predicted_volatility, exposure_limit) # Indien doelvolatiliteit/ voorspelde volatiliteit hoger is dan de blootstellingslimiet, dan wordt de blootstelling gelijkgesteld aan de blootstellingslimiet  
    colnames(p.rebalance) <- "Exposure"

  # Backtestperiode 
    # Laatste dag van de initialisatieperiode wordt bepaald 
      initialisation.last_day <- index(p.rebalance[initialisation.years*12])
    # Eerste dag van de backtestperiode wordt bepaald 
      backtest.first_day <- index(unscaled.returns[unscaled.returns[initialisation.last_day, which.i =TRUE]+1,])
    # Indexen in backtestperiode worden bepaald 
      backtest_period <- index(unscaled.returns[paste0(backtest.first_day,"/")])
    # Backtestperiode van tijdreeksen splitsen  
      backtest.asset_returns <- asset_returns[backtest_period]
      backtest.rf_returns <- rf_returns[backtest_period]
      backtest.unscaled.returns <- unscaled.returns[backtest_period]
      backtest.p.rebalance <- p.rebalance[backtest_period]
      backtest.unscaled.bop_weight <- unscaled.bop_weight[backtest_period]
      backtest.unscaled.eop_weight <- unscaled.eop_weight[backtest_period]
      backtest.target_volatility <- target_volatility[backtest_period]
      backtest.predicted_volatility <- predicted_volatility[backtest_period]
  
    # Initialisatie resultaattabellen 
      # Tabel die de waarde bijhoudt van de activa in de geschaalde portefeuille aan het begin van elke dag 
        scaled.bop_value <- xts(matrix(0,nrow = nrow(backtest.asset_returns), ncol = (ncol(backtest.asset_returns) + 3)) , order.by = index(backtest.asset_returns))  # Elke rij is een dag, elke kolom is een activa
        colnames(scaled.bop_value) <- c(colnames(asset_returns), "Balanced","RF", "Total")
      # Tabel die de waarde bijhoudt van de activa in de geschaalde portefeuille aan het einde van elke dag 
        scaled.eop_value <- scaled.bop_value
      # Tabel die de gewichten bijhoudt van de activa in de geschaalde portefeuille aan het begin van elke dag 
        scaled.bop_weight <- xts(matrix(0,nrow = nrow(backtest.asset_returns), ncol = (ncol(backtest.asset_returns) + 2)) , order.by = index(backtest.asset_returns))  # Elke rij is een dag, elke kolom is een activa
        colnames(scaled.bop_weight) <- c(colnames(asset_returns), "Balanced","RF")
      # Tabel die de gewichten bijhoudt van de activa in de geschaalde portefeuille aan het einde van elke dag
        scaled.eop_weight <- scaled.bop_weight
      # Tabel die de geschaalde portefeuillerendementen bijhoudt 
        scaled.returns <- xts(matrix(0,nrow = nrow(backtest.asset_returns), ncol = 1) , order.by = index(backtest.asset_returns)) 
        colnames(scaled.returns) <- "Return scaled portfolio"
      
    # Initiele waarden bepalen
      # Initiele portefeuillewaarde wordt gelijkgesteld aan 100
        value_0 <- 100
      # Waarde van individuele activa aan het begin van de eerste dag 
        scaled.bop_value[1,-c(7,8,9)] <- value_0 * weights.balanced * coredata(p.rebalance[initialisation.last_day]) # De initiële waarde van een risicovol activum is gelijk aan de totale portefeuillewaarde vermenigvuldigt met het overeenkomstig gewenst portefeuillegewicht en de gewenste blootstelling 
      # Waarde van gebalanceerde portefeuille aan het begin van de eerste dag is gelijk aan de som van de risicovolle activa waarden 
        scaled.bop_value[1,7] <- sum(scaled.bop_value[1,-c(7,8,9)])
      # Waarde risicovrij activa aan het begin van de eerste dag 
        scaled.bop_value[1,8] <- value_0 * (1-coredata(p.rebalance[initialisation.last_day]))
      # Totale waarde portefeuille aan het begin van de eerste dag 
        scaled.bop_value[1,9] <- sum(scaled.bop_value[1,7:8])
      # Gewichten van individuele activa aan het begin van de eerste dag 
        scaled.bop_weight[1,] <- scaled.bop_value[1,-9]/rep(scaled.bop_value[1,9], 8)
    
    # Backtest uitvoeren 
      for (d in 1:(nrow(backtest.asset_returns)-1)){ # Lus over de dagen in de backtestperiode (behalve de laatste)
        # Berekenen waarden van activa in portefeuille op het einde van de dag 
          scaled.eop_value[d,-c(7:9)] <- (1 + backtest.asset_returns[d,])*scaled.bop_value[d,-c(7:9)] # (1 + overeenkomstig rendement) * waarde aan het begin van de dag 
        # Berekenen waarde gebalanceerde portefeuille op het einde van de dag 
          scaled.eop_value[d,7] <- sum(scaled.eop_value[d,-c(7:9)])
        # Berekenen waarde risicovrij activa op het einde van de dag
          scaled.eop_value[d,8] <- (1 + backtest.rf_returns[d,])*scaled.bop_value[d,8]
        # Totale waarde van de portefeuille op het einde van de dag 
          scaled.eop_value[d,9] <- sum(scaled.eop_value[d,7:8])
        # Gewichten van individuele activa aan het einde van de dag 
          scaled.eop_weight[d,] <- scaled.eop_value[d,-9]/rep(scaled.eop_value[d,9], 8)
        # Portefeuille rendement berekenen
          scaled.returns[d,] <- (scaled.eop_value[d,9]/scaled.bop_value[d,9]) - 1
        
        if (d %in% endpoints(backtest.asset_returns, on=rebalance.frequency)){ # Herbalanceringsdag 
          # Waarden van activa worden geherbalanceerd 
            scaled.bop_value[(d+1),-c(7:9)] <- value_0 * weights.balanced * coredata(p.rebalance[index(backtest.asset_returns[d,])])
          # Totale waarde van de gebalanceerde portefeuille 
            scaled.bop_value[(d+1),7] <- sum(scaled.bop_value[(d+1),-c(7:9)])
          # Waarde risicovrij activa 
            scaled.bop_value[(d+1),8] <- value_0 * (1 - coredata(p.rebalance[index(backtest.asset_returns[d,])]))
          # Totale waarde portefeuille 
            scaled.bop_value[(d+1),9] <- sum(scaled.bop_value[(d+1),c(7:8)]) 
          # Gewichten van activa begin volgende dag 
            scaled.bop_weight[(d+1),] <- scaled.bop_value[(d+1),-9]/rep(scaled.bop_value[(d+1),9], 8)
        } else{ # Geen herbalanceringsdag 
          # Waarden van activa aan het begin van de volgende dag zijn gelijk aan de waarden van de activa aan het einde van deze dag 
            scaled.bop_value[(d+1),] <- scaled.eop_value[d,]
          # Gewichten van activa begin volgende dag 
            scaled.bop_weight[(d+1),] <- scaled.bop_value[(d+1),-9]/rep(scaled.bop_value[(d+1),9], 8)
        }
      }
      # Activa waarden en gewichten bepalen op het einde van de laatste dag 
        scaled.eop_value[nrow(scaled.eop_value),-c(7:9)] <- (1 + backtest.asset_returns[nrow(scaled.eop_value),])*scaled.bop_value[nrow(scaled.eop_value),-c(7:9)]
        scaled.eop_value[nrow(scaled.eop_value),7] <- sum(scaled.eop_value[nrow(scaled.eop_value),-c(7:9)])
        scaled.eop_value[nrow(scaled.eop_value),8] <- (1 + backtest.rf_returns[nrow(scaled.eop_value),])*scaled.bop_value[nrow(scaled.eop_value),8]
        scaled.eop_value[nrow(scaled.eop_value),9] <- sum(scaled.eop_value[nrow(scaled.eop_value),c(7:8)])
        scaled.eop_weight[nrow(scaled.eop_weight),] <- scaled.eop_value[nrow(scaled.eop_value),-9]/rep(scaled.eop_value[nrow(scaled.eop_value),9], 8)
      # Portefeuille rendement laatste dag 
        scaled.returns[nrow(scaled.returns),] <- (scaled.eop_value[nrow(scaled.eop_value),9]/ scaled.bop_value[nrow(scaled.bop_value),9])-1 
      
# Resultaten opslaan voor analyse
  #saveRDS(inputs, paste0(file_map,"inputs.rds"))
  #saveRDS(scaled.returns, paste0(file_map,"scaled_returns.rds"))
  #saveRDS(backtest.unscaled.returns, paste0(file_map,"unscaled_returns.rds"))
  #saveRDS(backtest.rf_returns, paste0(file_map,"rf_returns.rds"))
  #saveRDS(scaled.bop_weight[,-7], paste0(file_map,"scaled_bop_weights.rds"))
  #unscaled.bop_w <- cbind(unscaled.bop_weight, matrix(0, nrow = nrow(unscaled.bop_weight)))
  #colnames(unscaled.bop_w) <- c(colnames(unscaled.bop_weight), "RF")
  #saveRDS(backtest.unscaled.bop_weight, paste0(file_map,"unscaled_bop_weights.rds"))
  #saveRDS(scaled.eop_weight[,-7], paste0(file_map,"scaled_eop_weights.rds"))
  #unscaled.eop_w <- cbind(unscaled.eop_weight, matrix(0, nrow = nrow(unscaled.eop_weight)))
  #colnames(unscaled.eop_w) <- c(colnames(unscaled.eop_weight), "RF")
  #saveRDS(backtest.unscaled.eop_weight, paste0(file_map,"unscaled_eop_weights.rds"))
  #saveRDS(backtest.target_volatility, paste0(file_map,"target_volatility.rds"))
  #saveRDS(backtest.predicted_volatility, paste0(file_map,"predicted_volatility.rds"))
  #saveRDS(backtest.p.rebalance, paste0(file_map,"P_rebalance.rds"))
  #saveRDS(backtest.asset_returns, paste0(file_map,"asset_returns.rds"))
  

  