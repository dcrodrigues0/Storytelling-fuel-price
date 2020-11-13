
# Line chart of full first year month mean value by type
firstYear <- rbind(firstC_2017, secondC_2017)

firstMeanGasolina <- firstYear                                           %>%
                     select(Data.da.Coleta, Valor.de.Venda, Produto)     %>%
                     filter(Produto == 'GASOLINA')                       %>%
                     group_by(month=floor_date(Data.da.Coleta, "month")) %>%
                     summarise(meanByMonth = mean(Valor.de.Venda))       

firstMeanDieselS10 <- firstYear                                          %>%
                     select(Data.da.Coleta, Valor.de.Venda, Produto)     %>%
                     filter(Produto == 'DIESEL S10')                     %>%
                     group_by(month=floor_date(Data.da.Coleta, "month")) %>%
                     summarise(meanByMonth = mean(Valor.de.Venda))       

firstMeanDiesel    <- firstYear                                          %>%
                     select(Data.da.Coleta, Valor.de.Venda, Produto)     %>%
                     filter(Produto == 'DIESEL')                         %>%
                     group_by(month=floor_date(Data.da.Coleta, "month")) %>%
                     summarise(meanByMonth = mean(Valor.de.Venda))       

firstMeanEtanol <- firstYear                                             %>%
                     select(Data.da.Coleta, Valor.de.Venda, Produto)     %>%
                     filter(Produto == 'ETANOL')                         %>%
                     group_by(month=floor_date(Data.da.Coleta, "month")) %>%
                     summarise(meanByMonth = mean(Valor.de.Venda))    


ggplot()                                                                                                           + 
   geom_line(data=firstMeanGasolina,  aes(x = month, y= meanByMonth,  color = "Gasolina"),    size=1.3, alpha=0.9) +
   geom_line(data=firstMeanDieselS10, aes(x = month, y= meanByMonth,  color = "Diesel S10"),  size=1.3, alpha=0.9) +
   geom_line(data=firstMeanDiesel,    aes(x = month, y= meanByMonth,  color = "Diesel"),      size=1.3, alpha=0.9) +
   geom_line(data=firstMeanEtanol,    aes(x = month, y= meanByMonth,  color = "Etanol"),      size=1.3, alpha=0.9) +
   labs(x="Semestre", y="Valor em R$")                                                                             +
   theme_ipsum()                                                                                                   +
   theme(plot.title = element_text(hjust = 0.5))                                                                   +
   ggtitle("Tendência central da evolução do preço do combustível de 2017/1 á 2017/2")   

