# Pakketten inladen 
  library(xts) 
  library(PerformanceAnalytics)
  library(writexl)

# Map waarin backtestresultaten zijn opgeslagen - vergeet naam niet te wijzigen 
  file_map <- "Results/P0_MSCI_NA/L = inf/"

# Data inladen 
  unscaled.returns <- readRDS(paste0(file_map,"unscaled_returns.rds")) # Niet-geschaalde rendementen 

# Maandelijkse rendementen 
  # Functie voor het berekenen van maandelijkse rendementen 
    monthly.returns <- function(daily.returns){
      daily.returns <- daily.returns + 1 
      mon.returns <- period.apply(daily.returns, INDEX = endpoints(daily.returns, on="months"), FUN=prod)
      mon.returns <- (mon.returns - 1)*12 # Wordt vermenigvuldigt met 12 om het rendement te annualiseren 
    }
  # Berekenen van maandelijkse rendementen
    unscaled.returns.monthly <- monthly.returns(unscaled.returns) # (Geannualiseerde) maandelijkse niet-geschaald rendementen 
  
# Gerealiseerde volatiliteit 
  # Functie voor het berekenen van de gerealiseerde volatiliteit  
    realized_volatility <- function(returns){
      sqrt(mean(returns^2))*sqrt(255) # Wordt vermenigvuldigt met sqrt(255) om de volatiliteit te annualiseren  
    }
  # Berekenen van de gerealiseerde volatiliteit van iedere maand 
    unscaled.realized_volatility <- period.apply(unscaled.returns, INDEX = endpoints(unscaled.returns, on = "months"), FUN = realized_volatility) # (Geannualiseerde) maandelijks gerealiseerde volatiliteit van niet-geschaalde rendementen
    colnames(unscaled.realized_volatility) <- "Realized volatility"

# Data op juiste lag plaatsen 
  current_vol <- coredata(unscaled.realized_volatility[-nrow(unscaled.realized_volatility)]) # Gerealiseerde volatiliteit van huidige maand; laatste maand wordt weggelaten aangezien toekomstige waarden na laatste maand niet gekend zijn 
  current_ret <- coredata(unscaled.returns.monthly[-nrow(unscaled.returns.monthly)]) # Rendement van huidige maand; laatste maand wordt weggelaten aangezien toekomstige waarden na laatste maand niet gekend zijn 
  future_vol <- coredata(unscaled.realized_volatility[-1]) # Gerealiseerde volatiliteit van volgende maand; eerste maand wordt weggelaten aangezien waarden voor de eerste maand niet gekend zijn 
  future_ret <- coredata(unscaled.returns.monthly[-1]) # Rendement van volgende maand; eerste maand wordt weggelaten aangezien waarden voor de eerste maand niet gekend zijn
  volret <- cbind(current_vol, future_vol, current_ret, future_ret, Q = 1) # Tabel die alles samenplaatst 
  volret <- xts(volret, order.by = index(unscaled.realized_volatility[-nrow(unscaled.realized_volatility)])) # xts object maken van de tabel
  colnames(volret) <- c("Current volatility", "Future volatility", "Current return","Future return", "Regime")

# Resultatentabel initialiseren
  cor_table <- matrix(0, nrow = 1, ncol = 9)
  
# Analyse over volledige periode (opmerking: met "huidig" wordt het rendement/ volatiliteit over de afgelopen maand bedoeld, met"toekomstig wordt het rendement/ volatiliteit over de volgende maand bedoeld)   
  # Correlatie tussen huidige volatiliteit en huidig rendement
    cur_vol.vs.cur_ret <- cor(current_vol, current_ret) 
    cor_table[1,1]<- cur_vol.vs.cur_ret
  # Correlatie tussen huidige volatiliteit en toekomstig rendement 
    cur_vol.vs.fut_ret <- cor(current_vol, future_ret)
    cor_table[1,2]<- cur_vol.vs.fut_ret
  # Correlatie tussen huidige volatiliteit en toekomstige volatiliteit 
    cur_vol.vs.fut_vol <- cor(current_vol, future_vol)
    cor_table[1,6]<- cur_vol.vs.fut_vol

# Analyse over volatiliteitsregimes
    # Volatiliteitsregimes definieren 
      q1 <- quantile(current_vol, probs = 0.2) # De maanden met gerealiseerde volatiliteit in het laagste kwintiel (laagste 20%) bevinden zich in het laag volatiliteitsregime 
      q2 <- quantile(current_vol, probs = 0.8) # De maanden met gerealiseerde volatiliteit in het hoogste kwintiel (hoogste 20%) bevinden zich in het hoog volatiliteitsregime
    # Maanden in volatiliteitsregimes steken 
      for (m in 1:nrow(volret)){ # Lus over de maanden 
        if (volret[m,1] > q2){
          volret[m,5] <- 3 # Als de gerealiseerde volatiliteit van de vorige (huidige) maand in het hoogste kwintiel ligt, dan wordt de maand in het hoog volatiliteitsregime geplaatst  
        } else if (volret[m,1] > q1){
          volret[m,5] <- 2 # Als de gerealiseerde volatiliteit van de vorige (huidige) maand tussen het laagste kwintiel en hoogste kwintiel ligt, dan wordt de maand in het gemiddelde volatiliteitsregime geplaatst
        } # Anders zit de maand in het laag volatiliteitsregime 
      }
      low_vol <- volret[volret[,5] == 1,]
      mid_vol <- volret[volret[,5] == 2,]
      high_vol <- volret[volret[,5] == 3,]
    # Correlaties per volatiliteitsregime 
      # Hoog volatiliteitsregime
        high_vol.cur_vol.vs.fut_ret <- cor(high_vol[,1], high_vol[,4]) # Correlatie tussen huidige volatiliteit en toekomstig rendement 
        cor_table[1,3] <- high_vol.cur_vol.vs.fut_ret
        high_vol.cur_vol.vs.fut_vol <- cor(high_vol[,1], high_vol[,2]) # Correlatie tussen huidige volatiliteit en toekomstige volatiliteit 
        cor_table[1,7] <- high_vol.cur_vol.vs.fut_vol
      # Gemiddeld volatiliteitsregime
        mid_vol.cur_vol.vs.fut_ret <- cor(mid_vol[,1], mid_vol[,4]) # Correlatie tussen huidige volatiliteit en toekomstig rendement 
        cor_table[1,4] <- mid_vol.cur_vol.vs.fut_ret
        mid_vol.cur_vol.vs.fut_vol <- cor(mid_vol[,1], mid_vol[,2]) # Correlatie tussen huidige volatiliteit en toekomstige volatiliteit
        cor_table[1,8] <- mid_vol.cur_vol.vs.fut_vol
      # Laag volatiliteitsregime 
        low_vol.cur_vol.vs.fut_ret <- cor(low_vol[,1], low_vol[,4]) # Correlatie tussen huidige volatiliteit en toekomstig rendement 
        cor_table[1,5] <- low_vol.cur_vol.vs.fut_ret
        low_vol.cur_vol.vs.fut_vol <- cor(low_vol[,1], low_vol[,2]) # Correlatie tussen huidige volatiliteit en toekomstige volatiliteit
        cor_table[1,9] <- low_vol.cur_vol.vs.fut_vol

# Resultaten opslaan
  saveRDS(cor_table, paste0(file_map, "vol_ret_analysis.rds"))
  write_xlsx(data.frame(cor_table), paste0(file_map, "vol_ret_analysis.xlsx"))
    
  
# Visualisatie met behulp van barcharts (zie literatuurstudie)
  # Quintielen definieren 
    q1 <- quantile(current_vol, probs = 0.2)
    q2 <- quantile(current_vol, probs = 0.4)
    q3 <- quantile(current_vol, probs = 0.6)
    q4 <- quantile(current_vol, probs = 0.8)
  # Maanden in quintielen stoppen 
    for (m in 1:nrow(volret)){
      if (volret[m,1] > q4){ 
        volret[m,5] <- 5 # Hoogste kwintiel
      } else if (volret[m,1] > q3){
        volret[m,5] <- 4 # Vierde kwintiel
      } else if (volret[m,1] > q2){
        volret[m,5] <- 3 # Derde kwintiel
      } else if (volret[m,1] > q1){
        volret[m,5] <- 2 # Tweede kwintiel 
      } # Anders eerste kwintiel  
    }
  # Gemiddeld toekomstig rendement per quintiel
    # Initialisatie tabel die gemiddelde rendementen van de volgende maanden bijhoudt voor ieder kwintiel 
      avg_fut_ret <- matrix(0, ncol = 5, nrow = 1)
      colnames(avg_fut_ret) <- c("Q1", "Q2", "Q3", "Q4", "Q5")
    # Berekenen gemiddelde rendementen
      avg_fut_ret[,1] <- mean(volret[volret[,5] == 1,4]) # Eerste kwintiel 
      avg_fut_ret[,2] <- mean(volret[volret[,5] == 2,4]) # Tweede kwintiel 
      avg_fut_ret[,3] <- mean(volret[volret[,5] == 3,4]) # Derde kwintiel 
      avg_fut_ret[,4] <- mean(volret[volret[,5] == 4,4]) # Vierde kwintiel 
      avg_fut_ret[,5] <- mean(volret[volret[,5] == 5,4]) # Vijfde kwintiel 
  # Gemiddelde toekomstige volatiliteit per quintiel
    # Initialisatie tabel die gemiddelde volatiliteiten van de volgende maanden bijhoudt voor ieder kwintiel 
      avg_fut_vol <- matrix(0, ncol = 5, nrow = 1)
      colnames(avg_fut_vol) <- c("Q1", "Q2", "Q3", "Q4", "Q5")
    # Berekenen gemiddelde volatiliteiten
      avg_fut_vol[,1] <- mean(volret[volret[,5] == 1,2]) # Eerste kwintiel 
      avg_fut_vol[,2] <- mean(volret[volret[,5] == 2,2]) # Tweede kwintiel 
      avg_fut_vol[,3] <- mean(volret[volret[,5] == 3,2]) # Derde kwintiel 
      avg_fut_vol[,4] <- mean(volret[volret[,5] == 4,2]) # Vierde kwintiel 
      avg_fut_vol[,5] <- mean(volret[volret[,5] == 5,2]) # Vijfde kwintiel 
  # Barcharts maken 
    par(mfrow=c(1,2))
    barplot(avg_fut_ret,xlab = "Kwintiel gerealiseerde volatiliteit", ylab = "Geannualiseerd rendement volgende maand", ylim = c(0,0.2))
    barplot(avg_fut_ret, add = TRUE)
    grid(col = 1)
    barplot(avg_fut_vol, xlab = "Kwintiel gerealiseerde volatiliteit", ylab = "Geannualiseerde volatiliteit volgende maand", ylim = c(0, 0.3))
    grid(col = 1)
    par(mfrow=c(1,1))




  