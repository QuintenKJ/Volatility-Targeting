# Volatility-Targeting
Deze repository bevat de R bestanden die gebruikt werden in de masterproef: Een onderzoek naar het succes van conditionele volatiliteitstargetingstrategieën voor gebalanceerde portefeuilles. Alle bestanden kunnen gedownload worden als volgt: druk op "Code" (rechtsboven) > selecteer "Download ZIP".
## Bestanden:
 - Volatilitytargeting_1.R : dit script vormt Datastream datasets om naar rendementen die gebruikt worden in de backtests en geeft beschrijvende statistieken.
 - Conventional_volatilitytargeting_1.R : dit script voert de backtest uit voor de conventionele volatiliteitstargetingstrategie (snelle versie). 
 - Conventional_volatilitytargeting_2.R : dit script voert de backtest uit voor de conventionele volatiliteitstargetingstrategie (trage versie).
 - Conditional_volatilitytargeting_1.R : dit script voert de backtest uit voor de conditionele volatiliteitstargetingstrategie (snelle versie).
 - Conditional_volatilitytargeting_2.R : dit script voert de backtest uit voor de conditionele volatiliteitstargetingstrategie (trage versie).
 - Volatilitytargeting_performance_analysis.R : dit script berekent de prestatiemaatstaven. 
 - Volatility_return_analysis.R : dit script berekent de correlaties tussen rendementen en volatiliteiten en biedt een visualisatie met behulp van een staafgrafiek.
 - Volatilitytargeting_visualisation.R : dit script creëert grafieken voor de cumulatieve rendementen, blootstellingen en doelvolatiliteit. Dit script toont ook het verloop van de Sharpe ratio op basis van een rollend venster met een lengte van tien jaar.
 - Volatilitytargeting_crisis.R : dit script berekent de cumulatieve rendementen gedurende de kredietcrisis en covidcrisis.

Opmerking: voor zowel de conventionele als de conditionele strategieën zijn er twee scripts beschikbaar. Het eerste script maakt gebruik van de Return.portfolio() functie van het PerformanceAnalytics pakket. Deze versie is sneller maar geeft de lezer geen duidelijk beeld van hoe de portefeuille in de praktijk werkt. Het tweede script maakt geen gebruik van de Return.portfolio() functie en toont hierdoor beter aan hoe de portefeuille achter de schermen werkt, deze versie is echter wel aanzienlijk trager. Beide versies leveren hetzelfde resultaat, indien de lezer dus niet geïnteresseerd is in de onderliggende werking van de portefeuille, dan is het aangeraden om de eerste versie te gebruiken.
## Gebruik
Eerst moeten de Datastream datasets omgezet worden in rendementen met behulp van Volatilitytargeting_1.R. Vervolgens kunnen de backtests uitgevoerd worden met Conventional_volatilitytargeting_1.R en Conditional_volatilitytargeting_1.R. Tot slot kunnen prestatiemaatstaven berekend worden en visualisaties gemaakt worden met behulp van Volatilitytargeting_performance_analysis.R en Volatilitytargeting_visualisation.R. Bij het gebruik van de scripts is het belangrijk om de plaats waar de bestanden staan opgeslagen aan te passen en de gewenste wijzigingen in de parameters van de strategie door te voeren.   

