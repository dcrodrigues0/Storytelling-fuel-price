
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

firstMeanGnv    <- firstYear                                             %>% 
                     select(Data.da.Coleta, Valor.de.Venda, Produto)     %>%
                     filter(Produto == 'GNV')                            %>%
                     group_by(month=floor_date(Data.da.Coleta, 'month')) %>%
                     summarise(meanByMonth = mean(Valor.de.Venda))


ggplot()                                                                                                           + 
   geom_line(data=firstMeanGasolina,  aes(x = month, y= meanByMonth,  color = "Gasolina"),    size=1.3, alpha=0.9) +
   geom_line(data=firstMeanDieselS10, aes(x = month, y= meanByMonth,  color = "Diesel S10"),  size=1.3, alpha=0.9) +
   geom_line(data=firstMeanDiesel,    aes(x = month, y= meanByMonth,  color = "Diesel"),      size=1.3, alpha=0.9) +
   geom_line(data=firstMeanEtanol,    aes(x = month, y= meanByMonth,  color = "Etanol"),      size=1.3, alpha=0.9) +
   geom_line(data=firstMeanGnv,       aes(x = month, y = meanByMonth, color = 'GNV'),         size=1.3, alpha=0.9) +
   labs(x="Semestre", y="Valor em R$")                                                                             +
   theme_ipsum()                                                                                                   +
   theme(plot.title = element_text(hjust = 0.5))                                                                   +
   ggtitle("Tendência central da evolução do preço do combustível de 2017/1 à 2017/2")



# Line chart of full second year month mean value by type
secondYear <- rbind(firstC_2018, secondC_2018)

firstMeanGasolina <- secondYear                                           %>%
   select(Data.da.Coleta, Valor.de.Venda, Produto)                        %>%
   filter(Produto == 'GASOLINA')                                          %>%
   group_by(month=floor_date(Data.da.Coleta, "month"))                    %>%
   summarise(meanByMonth = mean(Valor.de.Venda))       

firstMeanDieselS10 <- secondYear                                          %>%
   select(Data.da.Coleta, Valor.de.Venda, Produto)                        %>%
   filter(Produto == 'DIESEL S10')                                        %>%
   group_by(month=floor_date(Data.da.Coleta, "month"))                    %>%
   summarise(meanByMonth = mean(Valor.de.Venda))       

firstMeanDiesel    <- secondYear                                          %>%
   select(Data.da.Coleta, Valor.de.Venda, Produto)                        %>%
   filter(Produto == 'DIESEL')                                            %>%
   group_by(month=floor_date(Data.da.Coleta, "month"))                    %>%
   summarise(meanByMonth = mean(Valor.de.Venda))       

firstMeanEtanol <- secondYear                                             %>%
   select(Data.da.Coleta, Valor.de.Venda, Produto)                        %>%
   filter(Produto == 'ETANOL')                                            %>%
   group_by(month=floor_date(Data.da.Coleta, "month"))                    %>%
   summarise(meanByMonth = mean(Valor.de.Venda))    

firstMeanGnv    <- secondYear                                             %>% 
   select(Data.da.Coleta, Valor.de.Venda, Produto)                        %>%
   filter(Produto == 'GNV')                                               %>%
   group_by(month=floor_date(Data.da.Coleta, 'month'))                    %>%
   summarise(meanByMonth = mean(Valor.de.Venda))

ggplot()                                                                                                           + 
   geom_line(data=firstMeanGasolina,  aes(x = month, y= meanByMonth,  color = "Gasolina"),    size=1.3, alpha=0.9) +
   geom_line(data=firstMeanDieselS10, aes(x = month, y= meanByMonth,  color = "Diesel S10"),  size=1.3, alpha=0.9) +
   geom_line(data=firstMeanDiesel,    aes(x = month, y= meanByMonth,  color = "Diesel"),      size=1.3, alpha=0.9) +
   geom_line(data=firstMeanEtanol,    aes(x = month, y= meanByMonth,  color = "Etanol"),      size=1.3, alpha=0.9) +
   geom_line(data=firstMeanGnv,       aes(x = month, y = meanByMonth, color = 'GNV'),         size=1.3, alpha=0.9) +
   labs(x="Semestre", y="Valor em R$")                                                                             +
   theme_ipsum()                                                                                                   +
   theme(plot.title = element_text(hjust = 0.5))                                                                   +
   ggtitle("Tendência central da evolução do preço do combustível de 2018/1 à 2018/2")

mayMonth <- firstC_2018

firstMeanGasolina <- mayMonth                                                                                          %>%
   select(Data.da.Coleta, Valor.de.Venda, Produto)                                                                     %>%
   filter(Produto == 'GASOLINA' & Data.da.Coleta >= as.Date('2018-05-01') & Data.da.Coleta <= as.Date('2018-05-31'))   %>%
   group_by(day=floor_date(Data.da.Coleta, 'day'))                                                                     %>%
   summarise(meanByDay = mean(Valor.de.Venda))

firstMeanDieselS10 <- mayMonth                                                                                         %>%
   select(Data.da.Coleta, Valor.de.Venda, Produto)                                                                     %>%
   filter(Produto == 'DIESEL S10' & Data.da.Coleta >= as.Date('2018-05-01') & Data.da.Coleta <= as.Date('2018-05-31')) %>%
   group_by(day=floor_date(Data.da.Coleta, 'day'))                                                                     %>%
   summarise(meanByDay = mean(Valor.de.Venda))

firstMeanDiesel <- mayMonth                                                                                            %>%
   select(Data.da.Coleta, Valor.de.Venda, Produto)                                                                     %>%
   filter(Produto == 'DIESEL' & Data.da.Coleta >= as.Date('2018-05-01') & Data.da.Coleta <= as.Date('2018-05-31'))     %>%
   group_by(day=floor_date(Data.da.Coleta, 'day'))                                                                     %>%
   summarise(meanByDay = mean(Valor.de.Venda))

firstMeanEtanol <- mayMonth                                                                                            %>%
   select(Data.da.Coleta, Valor.de.Venda, Produto)                                                                     %>%
   filter(Produto == 'ETANOL' & Data.da.Coleta >= as.Date('2018-05-01') & Data.da.Coleta <= as.Date('2018-05-31'))     %>%
   group_by(day=floor_date(Data.da.Coleta, 'day'))                                                                     %>%
   summarise(meanByDay = mean(Valor.de.Venda))

firstMeanGnv <- mayMonth                                                                                               %>%
   select(Data.da.Coleta, Valor.de.Venda, Produto)                                                                     %>%
   filter(Produto == 'GNV' & Data.da.Coleta >= as.Date('2018-05-01') & Data.da.Coleta <= as.Date('2018-05-31'))        %>%
   group_by(day=floor_date(Data.da.Coleta, 'day'))                                                                     %>%
   summarise(meanByDay = mean(Valor.de.Venda))

ggplot()                                                                                                       + 
   geom_line(data=firstMeanGasolina,  aes(x = day, y= meanByDay,  color = "Gasolina"),    size=1.3, alpha=0.9) +
   geom_line(data=firstMeanDieselS10, aes(x = day, y= meanByDay,  color = "Diesel S10"),  size=1.3, alpha=0.9) +
   geom_line(data=firstMeanDiesel,    aes(x = day, y= meanByDay,  color = "Diesel"),      size=1.3, alpha=0.9) +
   geom_line(data=firstMeanEtanol,    aes(x = day, y= meanByDay,  color = "Etanol"),      size=1.3, alpha=0.9) +
   geom_line(data=firstMeanGnv,       aes(x = day, y= meanByDay,  color = 'GNV'),         size=1.3, alpha=0.9) +
   labs(x="Semestre", y="Valor em R$")                                                                         +
   theme_ipsum()                                                                                               +
   theme(plot.title = element_text(hjust = 0.5))                                                               +
   ggtitle("Tendência central da evolução do preço do combustível no mês de Maio de 2018")
   
dataSet <- data.frame(
   meanByDay   = c(firstMeanGnv$meanByDay, firstMeanEtanol$meanByDay, firstMeanDiesel$meanByDay, firstMeanDieselS10$meanByDay, firstMeanGasolina$meanByDay),
   day = c(firstMeanGnv$day, firstMeanEtanol$day, firstMeanDiesel$day, firstMeanDieselS10$day, firstMeanGasolina$day))

typeFuel = c()
for(gnv in firstMeanGnv){ 
   append(typeFuel, 'GNV')
}
print(typeFuel)

