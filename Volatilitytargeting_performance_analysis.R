# Dit script berekent verschillende prestatiemaatstaven op basis van backtest resultaten. 
# Voor het uitvoeren van dit script is het belangrijk om de plaats waarin de backtestresultaten zijn opgeslagen te wijzigen.
# Het is ook belangrijk dat het R script van Ledoit and Wolf (2008) voor het uitvoeren van HAC inferentie op de Sharpe ratio is opgeslagen in de working directory.
# Dit script kan gedownload worden op https://www.econ.uzh.ch/en/people/faculty/wolf/publications.html#Programming_Code (Robust performance hypothesis testing with the Sharpe ratio.)

# Inladen pakketten
  library(PerformanceAnalytics)
  library(xts)
  library(writexl)
  load("Sharpe.RData") # Afkomstig van https://www.econ.uzh.ch/en/people/faculty/wolf/publications.html#Programming_Code (Robust performance hypothesis testing with the Sharpe ratio.)

# Map waarin backtestresultaten zijn opgeslagen - vergeet naam niet te wijzigen 
  file_map <- "Results/P1/L = inf/"

# Inladen data
  # Inladen rendementen 
    scaled.returns <- readRDS(paste0(file_map,"scaled_returns.rds"))
    unscaled.returns <- readRDS(paste0(file_map,"unscaled_returns.rds"))
    rf.returns <- readRDS(paste0(file_map,"rf_returns.rds"))
  # Inladen gewichten 
    scaled.bop_weight <- readRDS(paste0(file_map,"scaled_bop_weights.rds"))
    scaled.eop_weight <- readRDS(paste0(file_map,"scaled_eop_weights.rds"))
    unscaled.bop_weight <- readRDS(paste0(file_map,"unscaled_bop_weights.rds"))
    unscaled.eop_weight <- readRDS(paste0(file_map,"unscaled_eop_weights.rds"))

# Maandelijkse rendementen berekenen  
  # Functie voor het berekenen van maandelijkse rendementen. Dagelijkse rendementen worden als input gegeven, de output zijn maandelijkse rendementen.  
    monthly.returns <- function(daily.returns){ 
      daily.returns <- daily.returns + 1 
      mon.returns <- period.apply(daily.returns, INDEX = endpoints(daily.returns, on="months"), FUN=prod)
      mon.returns <- mon.returns - 1
    }
  # Berekenen van maandelijkse rendementen 
    scaled.returns.monthly <- monthly.returns(scaled.returns) # Maandelijkse geschaalde rendementen 
    unscaled.returns.monthly <- monthly.returns(unscaled.returns) # Maandelijkse niet-geschaalde rendementen 
    rf.returns.monthly <- monthly.returns(rf.returns) # Maandelijkse risicovrije rendementen 

# Resultaten tabel initialiseren 
  performance <- data.frame(matrix(0, nrow = 2, ncol = 14))
  colnames(performance) <- c("AR", "AV", "ASR", "ES-1%", "ES-5%", "MDD","VOVO", "RV/TV","PT", "SR","P-value", "HAC P-value", "skew", "kurt")
  rownames(performance) <- c("Unscaled", "Scaled")

# Jaarlijks rendement (AR)
  # Berekenen jaarlijks rendement 
    ar.unscaled <- ((prod(1 + unscaled.returns.monthly))^(12/nrow(unscaled.returns.monthly)))-1
    ar.scaled <- ((prod(1 + scaled.returns.monthly))^(12/nrow(scaled.returns.monthly)))-1
  # Resultaten opslaan in resultatentabel 
    performance$AR[1] <- ar.unscaled
    performance$AR[2] <- ar.scaled

# Jaarlijkse volatiliteit (AV)
  # Berekenen jaarlijkse volatiliteit   
    av.unscaled <-sqrt(12)*StdDev(unscaled.returns.monthly) 
    av.scaled <-sqrt(12)*StdDev(scaled.returns.monthly)
  # Resultaten opslaan in resultatentabel 
    performance$AV[1] <- av.unscaled
    performance$AV[2] <- av.scaled

# volatiliteit van de volatiliteit (VOVO) op basis van maandelijkse rendementen
  # Initialisatie van tabellen
    # Tabel die maandelijkse volatiliteiten bijhoudt van de geschaalde rendementen 
      monthly.stdev.scaled <- xts(matrix(0, ncol = 1, nrow = nrow(scaled.returns.monthly)), order.by = index(scaled.returns.monthly))
      colnames(monthly.stdev.scaled) <- "Monthly volatility scaled"
    # Tabel die maandelijkse volatiliteiten bijhoudt van de niet-geschaalde rendementen
      monthly.stdev.unscaled <- xts(matrix(0, ncol = 1, nrow = nrow(unscaled.returns.monthly)), order.by = index(unscaled.returns.monthly))
      colnames(monthly.stdev.unscaled) <- "Monthly volatility unscaled"
  # Berekenen van maandelijkse volatiliteit als standaardafwijking van maandelijkse rendementen in het vorige jaar 
    for (i in c(12:nrow(scaled.returns.monthly))){ # Lus over de maanden, beginnende vanaf maand 12
      monthly.stdev.scaled[i,1]<- StdDev(scaled.returns.monthly[(i-11):i,])
      monthly.stdev.unscaled[i,1]<- StdDev(unscaled.returns.monthly[(i-11):i,])
    }
  # Berekenen van volatiliteit van volatiliteit als de standaardafwijking van maandelijkse volatiliteiten 
    vovo.scaled <- StdDev(monthly.stdev.scaled[12: nrow(scaled.returns.monthly)]) # Voor maand 12 zijn de maandelijkse volatiliteiten gelijk aan 0, we beginnen dus bij maand 12 
    vovo.unscaled <- StdDev(monthly.stdev.unscaled[12: nrow(unscaled.returns.monthly)]) 
  # Resultaten opslaan in resultatentabel  
    performance$VOVO[1] <- vovo.unscaled
    performance$VOVO[2] <- vovo.scaled

# Gerealiseerde volatiliteit ten opzichte van de doelvolatiliteit (RV/TV)
  # Berekenen RV/TV  
    realized.vs.target <- StdDev(scaled.returns.monthly)/StdDev(unscaled.returns.monthly)
  # Resultaten opslaan in resultatentabel 
    performance$`RV/TV`[1] <- 1
    performance$`RV/TV`[2] <- realized.vs.target

# Sharpe ratio (SR)
  # Berekenen Sharpe ratio 
    SR.scaled <-  mean(scaled.returns.monthly - rf.returns.monthly)/StdDev(scaled.returns.monthly - rf.returns.monthly)
    SR.unscaled <-  mean(unscaled.returns.monthly - rf.returns.monthly)/StdDev(unscaled.returns.monthly - rf.returns.monthly)
  # Berekenen Jobson en Korkie (1981) teststatistiek
    # Berekenen correlatie tussen geschaalde en niet-geschaalde maandelijkse rendementen 
      corr.returns <- cor(scaled.returns.monthly, unscaled.returns.monthly)
    # Berekenen teststatistiek 
      Z.SR <- (SR.scaled-SR.unscaled)/(sqrt((1/nrow(scaled.returns.monthly))*(2-(2*corr.returns)+0.5*((SR.scaled^2)+(SR.unscaled^2)-(2*SR.scaled*SR.unscaled*(corr.returns^2)))))) 
    # Berekenen van p-waarde
      P.SR <- 2*pnorm(-abs(Z.SR))
  # Resultaten opslaan in resultatentabel
    # Maandelijkse Sharpe ratio   
      performance$SR[1] <- SR.unscaled
      performance$SR[2] <- SR.scaled
    # Jaarlijkse Sharpe ratio   
      performance$ASR[1] <- SR.unscaled*sqrt(12)
      performance$ASR[2] <- SR.scaled*sqrt(12)
    # P-waarden Jobson and Korkie (1981) test
      performance$`P-value`[1] <- P.SR
      performance$`P-value`[2] <- P.SR
    # P-waarden Ledoit and Wolf (2008) HAC test 
      performance$`HAC P-value`[1] <- hac.inference(cbind(coredata(scaled.returns.monthly-rf.returns.monthly), coredata(unscaled.returns.monthly-rf.returns.monthly)))$p.Values[1] # HAC P-waarde van Ledoit and Wolf(2008)
      performance$`HAC P-value`[2] <- performance$`HAC P-value`[1] 
  
# Expected shortfall (ES)
  # Berekenen van percentielen 
    q.1.scaled <- quantile(scaled.returns.monthly, probs = 0.01)
    q.1.unscaled <- quantile(unscaled.returns.monthly, probs = 0.01)
    q.5.scaled <- quantile(scaled.returns.monthly, probs = 0.05)
    q.5.unscaled <- quantile(unscaled.returns.monthly, probs = 0.05)
  # Berekenen van expected shortfall als gemiddelde van maandelijkse rendementen onder het percentiel
    ES.1.scaled <- mean(scaled.returns.monthly[scaled.returns.monthly <= q.1.scaled])
    ES.1.unscaled <- mean(unscaled.returns.monthly[unscaled.returns.monthly <= q.1.unscaled])
    ES.5.scaled <- mean(scaled.returns.monthly[scaled.returns.monthly <= q.5.scaled])
    ES.5.unscaled <- mean(unscaled.returns.monthly[unscaled.returns.monthly <= q.5.unscaled])
  # Resultaten opslaan in resultatentabel 
    performance$'ES-1%'[1] <- ES.1.unscaled
    performance$'ES-1%'[2] <- ES.1.scaled
    performance$'ES-5%'[1] <- ES.5.unscaled
    performance$'ES-5%'[2] <- ES.5.scaled

# Maximum drawdown (MDD)
  # Berekenen MDD met behulp van maxDrawdown functie van PerformanceAnalytics pakket 
    MDD.scaled <- maxDrawdown(scaled.returns) 
    MDD.unscaled <- maxDrawdown(unscaled.returns)
  # Resultaten opslaan in resultatentabel 
    performance$MDD[1] <- MDD.unscaled
    performance$MDD[2] <- MDD.scaled

# Turnover 
  # Verandering van gewichten bepalen op ieder herbalanceringsmoment
    # Initialisatie van tabel die verandering in gewichten bijhoudt 
      Weight_changes.scaled <- xts(matrix(0, nrow = nrow(scaled.returns), ncol = 6), order.by = index(scaled.returns))
      Weight_changes.unscaled <- xts(matrix(0, nrow = nrow(unscaled.returns), ncol = 6), order.by = index(unscaled.returns))
    # Berekenen veranderingen in gewichten                                  
      for (d in 1:(nrow(scaled.returns)-1)){ # lus over dagen, laatste dag wordt niet bekeken aangezien er geen BOP gewicht is voor de dag die daarop volgt  
        if (d %in% endpoints(scaled.returns, on = "months")){ # d is een herbalanceringsdag 
          Weight_changes.scaled[d,] <- abs(coredata(scaled.bop_weight[d+1,-7]) - coredata(scaled.eop_weight[d,-7])) # De absolute waarde van de veranderingen in gewichten worden bepaald 
          Weight_changes.unscaled[d,] <- abs(coredata(unscaled.bop_weight[d+1,-7]) - coredata(unscaled.eop_weight[d,-7]))
        } else { # d is geen herbalanceringsdag 
          Weight_changes.scaled[d,] <- rep(0,6) # Verandering in gewicht is nul 
          Weight_changes.unscaled[d,] <- rep(0,6)
        }
      }
  # Turnover berekenen 
    TO.unscaled <- Weight_changes.unscaled[rowSums(Weight_changes.unscaled)!=0,] # Rijen selecteren waar de verandering in gewichten verschillend is van 0
    PT.unscaled <- 6*sum(TO.unscaled)/(nrow(unscaled.returns.monthly)-1) # One way turnover berekenen 
    TO.scaled <- Weight_changes.scaled[rowSums(Weight_changes.scaled)!=0,] # Rijen selecteren waar de verandering in gewichten verschillend is van 0
    PT.scaled <- 6*sum(TO.scaled)/(nrow(scaled.returns.monthly)-1) # One way turnover berekenen 
  # Resultaten opslaan in resultatentabel 
    performance$PT[1] <- PT.unscaled
    performance$PT[2] <- PT.scaled

# Skewness 
  performance$skew[1] <- skewness(unscaled.returns.monthly)
  performance$skew[2] <- skewness(scaled.returns.monthly)

# Overtollige kurtosis
  performance$kurt[1] <- kurtosis(unscaled.returns.monthly)
  performance$kurt[2] <- kurtosis(scaled.returns.monthly)

# Resultaten opslaan
  #saveRDS(performance, paste0(file_map, "performance.rds"))
  #write_xlsx(performance, paste0(file_map, "performance.xlsx"))
