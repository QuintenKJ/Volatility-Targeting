# Dit script creëert grafieken voor de cumulatieve rendementen, blootstellingen en doelvolatiliteit. 
# Dit script toont ook het verloop van de Sharpe ratio op basis van een rollend venster met een lengte van tien jaar.
# Voor het uitvoeren van dit script is het belangrijk om de locaties van de backtest resultaten te wijzigen.
# Dit script moet uitgevoerd worden door de code te selecteren en vervolgens ctrl + enter drukken. 

# Inladen pakketten
  library(PerformanceAnalytics)
  library(xts)
  library(writexl)

# Mappen waarin backtestresultaten zijn opgeslagen - vergeet namen niet te wijzigen  
  file_map_conventional <- "Conventional volatility targeting/Results/P1/L = inf/" # Locatie backtestresultaten conventionele volatiliteitstargetingstrategie
  file_map_conditional <- "Conditional volatility targeting/Results/P1/L = inf/" # Locatie backtestresultaten conditionele volatiliteitstargetingstrategie

# Titel grafieken 
  main_title <- "P1"

# Data inladen
  # Niet geschaalde rendementen 
    unscaled.returns <- readRDS(paste0(file_map_conventional,"unscaled_returns.rds"))
    colnames(unscaled.returns) <- "Niet geschaald"
  # Geschaalde rendementen conventionele volatiliteitstargetingstrategie 
    conventional.scaled.returns <- readRDS(paste0(file_map_conventional,"scaled_returns.rds"))
    colnames(conventional.scaled.returns) <- "Conventionele strategie"
  # Geschaalde rendementen conditionele volatiliteitstargetingstrategie
    conditional.scaled.returns <- readRDS(paste0(file_map_conditional,"scaled_returns.rds"))
    colnames(conditional.scaled.returns) <- "Conditionele strategie"
  # Blootstelling conventionele volatiliteitstargetingstrategie 
    conventional.p.rebalance <- readRDS(paste0(file_map_conventional,"p_rebalance.rds"))
    colnames(conventional.p.rebalance) <- "Conventionele strategie"
  # Blootstelling conditionele volatiliteitstargetingstrategie 
    conditional.p.rebalance <- readRDS(paste0(file_map_conditional,"p_rebalance.rds"))
    colnames(conditional.p.rebalance) <- "Conditionele strategie"
  # Doelvolatiliteit
    target_volatility <- readRDS(paste0(file_map_conventional,"target_volatility.rds"))
    predicted_volatility <- readRDS(paste0(file_map_conventional,"predicted_volatility.rds"))
  # Volatiliteitslimieten 
    q1 <- readRDS(paste0(file_map_conditional,"q1.rds"))
    q2 <- readRDS(paste0(file_map_conditional,"q2.rds"))

# Maandelijkse rendementen 
  # Functie voor het berekenen van maandelijkse rendementen 
    monthly.returns <- function(daily.returns){
      daily.returns <- daily.returns + 1 
      mon.returns <- period.apply(daily.returns, INDEX = endpoints(daily.returns, on="months"), FUN=prod)
      mon.returns <- (mon.returns - 1)
    }
# Berekenen van maandelijkse rendementen 
  unscaled.returns.monthly <- monthly.returns(unscaled.returns) # Maandelijks niet-geschaald rendement 
  conventional.scaled.returns.monthly <- monthly.returns(conventional.scaled.returns) # Maandelijks geschaald rendement, conventionele strategie
  conditional.scaled.returns.monthly <- monthly.returns(conditional.scaled.returns) # Maandelijks geschaald rendement, conditionele strategie 


# Data samenplaatsen 
  ret_compare <- cbind(conventional.scaled.returns.monthly, conditional.scaled.returns.monthly, unscaled.returns.monthly) # Vergelijking tussen rendement van conventionele strategie en conditionele strategie
  cum_ret_compare <- cumprod(ret_compare+1) # Vergelijking tussen cumulatief rendement van conventionele strategie en conditionele strategie 
  p_rebalance_compare <- cbind(conventional.p.rebalance, conditional.p.rebalance) # Vergelijking tussen blootstelling conventionele strategie en conditionele strategie
  predicted_volatility_compare <- cbind(predicted_volatility, q1, q2)*sqrt(250) # Wordt vermenigvuldigt met sqrt(250) om te annualiseren 

# Plots maken
  dev.off()
  par(mfrow = c(4,1))
  chart.TimeSeries(cum_ret_compare, main = paste0("Cumulatief rendement ", main_title), colorset = c(7,4,1), legend.loc = "topleft")
  chart.TimeSeries(p_rebalance_compare, main = paste0("Blootstelling ", main_title), colorset = c(7,4), legend.loc = "top")
  chart.TimeSeries(predicted_volatility_compare, main = paste0("Voorspelde volatiliteit ", main_title), colorset = c(1,4,4))
  chart.TimeSeries(target_volatility*sqrt(250), main = paste0("Doelvolatiliteit ", main_title), colorset = c(1))
  par(mfrow = c(1,1))

# Rollende Sharpe ratio met window van 10 jaar 
  # Berekenen rollende Sharpe ratio
    unscaled_roll_SR <- rollapply(unscaled.returns.monthly, width = 120, FUN = SharpeRatio.annualized) # Rollende Sharpe ratio niet geschaalde strategie 
    conventional_roll_SR <- rollapply(conventional.scaled.returns.monthly, width = 120, FUN = SharpeRatio.annualized) # Rollende Sharpe ratio conventionele strategie
    conditional_roll_SR <- rollapply(conditional.scaled.returns.monthly, width = 120, FUN = SharpeRatio.annualized) # Rollende Sharpe ratio conditionele strategie 
    roll_SR <- cbind(conventional_roll_SR, conditional_roll_SR, unscaled_roll_SR)
  # Plot
    chart.TimeSeries(roll_SR["2004/"], main = paste0("Sharpe ratio 10 jaar rollend venster ", main_title), legend.loc = "topright", colorset = c(7,4,1))
    
    
    
    