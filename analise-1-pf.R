
#Análise 1: Faturamento anual por categoria

# tabela com o faturamento mensal por categoria
vendas_1 <- select(vendas,"Category","Price","Motivo devolução","Data Venda","Product ID")
tabela_faturamento <- vendas_1[is.na(vendas_1$`Motivo devolução`),]
dados_sem_duplicacao <- tabela_faturamento%>%
  distinct(`Product ID`, .keep_all = TRUE)
tabela_faturamento_reduzida <- select(dados_sem_duplicacao, Category, Price,`Data Venda`)
tabela_sem_NA <- na.omit(tabela_faturamento_reduzida)
tabela_sem_NA$Data_Venda <- as.Date(tabela_sem_NA$`Data Venda`, format = "%m/%d/%Y")
tabela_base <-  tabela_sem_NA %>%
   select(Category,Price, Data_Venda)
tabela_separacao <- tabela_base %>%
  mutate(year = lubridate::year(Data_Venda), 
         month = lubridate::month(Data_Venda), 
         day = lubridate::day(Data_Venda))
month_names <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
tabela_separacao$Mes <- month_names[tabela_separacao$month]
tabela_com_mes <- tabela_separacao %>%
  select(Category,Price,Mes)
ordem_meses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", 
                 "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
tabela_com_mes$Mes <- factor(tabela_com_mes$Mes, levels = ordem_meses)
total_vendas <- tabela_com_mes %>%
  group_by(Mes, Category) %>%
  summarise(Total_Vendas = sum(Price))
total_vendas_1 <- rename(total_vendas, Categoria = Category)
Tabela_vendas <- total_vendas_1 %>%
  mutate(Categoria = case_when(
    Categoria == "Women's Fashion" ~ "Moda Feminina",
    Categoria == "Men's Fashion" ~ "Moda Masculina",
    Categoria == "Kids' Fashion" ~ "Moda Infantil",
    TRUE ~ Categoria
  ))   



# Calculo para a tabela com o preço total
vendas_2 <- select(vendas,"Category","Price","Motivo devolução","Data Venda","Product ID")
tabela_p_preço <- vendas_2[is.na(vendas_1$`Motivo devolução`),]
dados_nao_duplicados <- tabela_p_preço %>%
  distinct(`Product ID`, .keep_all = TRUE)
tabela_preço_resumida <- select(dados_nao_duplicados, Category, Price,`Data Venda`)
tabela_preço_resumida
tabela_sem_NA_pt <- na.omit(tabela_preço_resumida)
tabela_sem_NA_pt 
preço_total <- tabela_sem_NA_pt  %>% group_by(Category) %>%
  summarise(faturamento_total = sum(Price), .groups ='drop')
preço_total


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
 
 cores_estat <- c('#A11D21','#663333','#FF6600','#CC9900','#CC9966','#999966','#006606','#008091','#003366','#041835','#666666')
 
 ##Fazendo gráfico de linhas
 ggplot(Tabela_vendas, aes(x = Mes, y= Total_Vendas, group = Categoria, colour = Categoria)) +
   geom_line(size= 1) +
   geom_point(size=2) +
   scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
   labs(x= "Mês", y = "Faturamento") + 
   theme_estat() 
 ggsave("gráfico_linhas_An1.pdf", width = 158, height = 93, units = "mm")


 