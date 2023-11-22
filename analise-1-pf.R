
#Análise 1: Faturamento anual por categoria

# organizando o dataframe
fapcVendas <- select(vendas,"Category","Price","Data Venda","Product ID")
fapcSemDuplicacao <- fapcVendas%>%
  distinct(`Product ID`, .keep_all = TRUE)
fapcTabelaSemNA <- na.omit(fapcSemDuplicacao)
fapcTabelaSemNA$Data_Venda <- as.Date(fapcTabelaSemNA$`Data Venda`, format = "%m/%d/%Y")
fapcTabelaBase <-  fapcTabelaSemNA %>%
   select(Category,Price, Data_Venda)
fapcTabelaSeparacao <- fapcTabelaBase %>%
  mutate(year = lubridate::year(Data_Venda), 
         month = lubridate::month(Data_Venda), 
         day = lubridate::day(Data_Venda))
fapcNomeMeses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
fapcTabelaSeparacao$Mes <- fapcNomeMeses[fapcTabelaSeparacao$month]
fapcTabelaMes <- fapcTabelaSeparacao %>%
  select(Category,Price,Mes)
fapcOrdemMeses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", 
                 "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
fapcTabelaMes$Mes <- factor(fapcTabelaMes$Mes, levels = fapcOrdemMeses)
fapcTotalVendas <- fapcTabelaMes %>%
  group_by(Mes, Category) %>%
  summarise(fapcTotalVendas = sum(Price))
fapcTotalVendas <- rename(fapcTotalVendas, Categoria = Category)
fapcTabelaVendas <- fapcTotalVendas %>%
  mutate(Categoria = case_when(
    Categoria == "Women's Fashion" ~ "Moda Feminina",
    Categoria == "Men's Fashion" ~ "Moda Masculina",
    Categoria == "Kids' Fashion" ~ "Moda Infantil",
    TRUE ~ Categoria
  ))   



# Calculo para a tabela com o preço total
fapcVendasPreco <- select(vendas,"Category","Price","Data Venda","Product ID")
fapcSemDuplicacaoPreco <- fapcVendasPreco %>%
  distinct(`Product ID`, .keep_all = TRUE)
fapcTabelaPreco <- select(fapcSemDuplicacaoPreco, Category, Price,`Data Venda`)
fapcTabelaPreco
fapcTabelaSemNAPreco <- na.omit(fapcTabelaPreco)
fapcTabelaSemNAPreco 
fapcPrecoTotal<- fapcTabelaSemNAPreco  %>% group_by(Category) %>%
  summarise(faturamento_total = sum(Price), .groups ='drop')
fapcPrecoTotal

 # Padronização do gráfico
 theme_estat <- function(...) {
   theme <- ggplot2::theme_bw() +
     ggplot2::theme(
       axis.title.y = ggplot2::element_text(colour = "black", size = 12),
       axis.title.x = ggplot2::element_text(colour = "black", size = 12),
       axis.text = ggplot2::element_text(colour = "black", size = 9.5),
       panel.border = ggplot2::element_blank(),
       axis.line = ggplot2::element_line(colour = "black"),
       legend.position = "top",
       ...
     )
   
   return(
     list(
       theme,
       scale_fill_manual(values = cores_estat),
       scale_colour_manual(values = cores_estat)
     )
   )
 }
 
 cores_estat <- c('#A11D21','#003366','#CC9900','#663333','#FF6600','#CC9966','#999966','#006606','#008091','#041835','#666666')
 
 #Fazendo gráfico de linhas
 ggplot(fapcTabelaVendas, aes(x = Mes, y = fapcTotalVendas, group = Categoria, colour = Categoria)) +
   geom_line(size = 1) +
   geom_point(size = 2) +
   scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
   labs(x = "Mês", y = "Faturamento") +
   ylim(0, 4000) + 
   theme_estat() 
 ggsave("grafico_linhas_An1.pdf", width = 158, height = 93, units = "mm")
 

 
