
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



secondYear <- rbind(firstC_2018, secondC_2018)
secondYear$Estado...Sigla <- sub(" ", "", secondYear$Estado...Sigla)

firstMeanGasolina <- secondYear                                          %>%
                     select(Estado...Sigla, Valor.de.Venda, Produto)     %>%
                     filter(Produto == 'GASOLINA')                       %>%
                     group_by(state=Estado...Sigla)                      %>%
                     summarise(meanByState = mean(Valor.de.Venda))       

firstMeanDieselS10 <- secondYear                                         %>%
                     select(Estado...Sigla, Valor.de.Venda, Produto)     %>%
                     filter(Produto == 'DIESEL S10')                     %>%
                     group_by(state=Estado...Sigla)                      %>%
                     summarise(meanByState = mean(Valor.de.Venda))       

firstMeanDiesel    <- secondYear                                         %>%
                     select(Estado...Sigla, Valor.de.Venda, Produto)     %>%
                     filter(Produto == 'DIESEL')                         %>%
                     group_by(state=Estado...Sigla)                      %>%
                     summarise(meanByState = mean(Valor.de.Venda))       

firstMeanEtanol <- secondYear                                            %>%
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
   ggtitle("Tendência central da evolução do preço do combustível de 2018/1 á 2018/2 por estado")   

View(secondYear)