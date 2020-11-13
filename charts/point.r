
# Bar chart of full first year month mean value by type
firstYear <- rbind(firstC_2017, secondC_2017)

firstMeanGasolina <- firstYear                                           %>%
                     select(Estado...Sigla, Valor.de.Venda, Produto)     %>%
                     filter(Produto == 'GASOLINA')                       %>%
                     group_by(state=Estado...Sigla)                      %>%
                     summarise(meanByState = mean(Valor.de.Venda))       

firstMeanDieselS10 <- firstYear                                          %>%
                     select(Estado...Sigla, Valor.de.Venda, Produto)     %>%
                     filter(Produto == 'DIESEL S10')                     %>%
                     group_by(state=Estado...Sigla)                      %>%
                     summarise(meanByState = mean(Valor.de.Venda))       

firstMeanDiesel    <- firstYear                                          %>%
                     select(Estado...Sigla, Valor.de.Venda, Produto)     %>%
                     filter(Produto == 'DIESEL')                         %>%
                     group_by(state=Estado...Sigla)                      %>%
                     summarise(meanByState = mean(Valor.de.Venda))       

firstMeanEtanol <- firstYear                                             %>%
                     select(Estado...Sigla, Valor.de.Venda, Produto)     %>%
                     filter(Produto == 'ETANOL')                         %>%
                     group_by(state=Estado...Sigla)                      %>%
                     summarise(meanByState = mean(Valor.de.Venda))    

ggplot()                                                                                                                   + 
   geom_point(data=firstMeanGasolina,  aes(x = state, y= meanByState,  color = "Gasolina",   size=meanByState), alpha=0.9) +
   geom_point(data=firstMeanDieselS10, aes(x = state, y= meanByState,  color = "Diesel S10", size=meanByState), alpha=0.9) +
   geom_point(data=firstMeanDiesel,    aes(x = state, y= meanByState,  color = "Diesel",     size=meanByState), alpha=0.9) +
   geom_point(data=firstMeanEtanol,    aes(x = state, y= meanByState,  color = "Etanol",     size=meanByState), alpha=0.9) +
   labs(x="Estado", y="Valor em R$")                                                                                       +
   theme_ipsum()                                                                                                           +
   theme(plot.title = element_text(hjust = 0.5))                                                                           +
   ggtitle("Tendência central da evolução do preço do combustível de 2017/1 á 2017/2 por estado")   


# ggplot()                                                                                                                           + 
#    geom_bar(stat = "identity", data=firstMeanGasolina,  aes(x = state, y= meanByState,   fill = "Gasolina"),  size=1, alpha=0.9)   +
#    geom_bar(stat = "identity", data=firstMeanDieselS10, aes(x = state, y= meanByState,   fill = "Diesel S10"),  size=1, alpha=0.9) +
#    geom_bar(stat = "identity",  data=firstMeanDiesel,    aes(x = state, y= meanByState,  fill = "Diesel"),  size=1, alpha=0.9)     +
#    geom_bar(stat = "identity",  data=firstMeanEtanol,    aes(x = state, y= meanByState,  fill = "Etanol"),  size=1, alpha=0.9)     +
#    labs(x="Estado", y="Valor em R$")                                                                                               +
#    theme_ipsum()                                                                                                                   +
#    theme(plot.title = element_text(hjust = 0.5))                                                                                   +
#    ggtitle("Tendência central da evolução do preço do combustível de 2017/1 á 2017/2 por estado")   
