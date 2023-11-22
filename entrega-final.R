# Padronização para os gráficos

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

#Fazendo gráfico de linhas

ggplot(fapcTabelaVendas, aes(x = Mes, y = fapcTotalVendas, group = Categoria, colour = Categoria)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "Mês", y = "Faturamento") +
  ylim(0, 4000) + 
  theme_estat() 
ggsave("grafico_linhas_An1.pdf", width = 158, height = 93, units = "mm")


#Análise 2: Variação do preço por marca

#organizando dataframe

pmSelecionandoMarcas<- select(vendas,"Brand","Price","Product ID")

pmTabelaMarcas <- na.omit(pmSelecionandoMarcas)

pmDadosSemDuplicatas <- pmTabelaMarcas%>%
  distinct(`Product ID`, .keep_all = TRUE)

# boxplot

ggplot(pmDadosSemDuplicatas, aes(x = Brand, y = Price)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Marca", y = "Preço") +
  theme_estat()
ggsave("grafico_bi_boxplot_An2.pdf", width = 158, height = 93, units = "mm")

#quadro de medidas

pmDadosParaMedidas <- pmDadosSemDuplicatas %>%
  group_by(Brand) 

pmQuadroMedidas<- pmDadosParaMedidas %>%
  summarize(
    Média = formatC(mean(Price), format = "f", digits = 2),
    Desvio_Padrão = formatC(sd(Price), format = "f", digits = 2),
    Mínimo = formatC(min(Price), format = "f", digits = 2),
    Quartil_1 = formatC(quantile(Price, 0.25), format = "f", digits = 2),
    Mediana = formatC(median(Price), format = "f", digits = 2),
    Quartil_3 = formatC(quantile(Price, 0.75), format = "f", digits = 2),
    Máximo = formatC(max(Price), format = "f", digits = 2)
  )
pmQuadroMedidas

# analise de comparacao das médias(kruskal-wallis)

pmTesteKruskal <- kruskal.test(Price ~ Brand, data = pmDadosSemDuplicatas)
pmTesteKruskal 


# Análise 3: Relação entre categorias (apenas feminino e masculino) e cor

#organizando o dataframe

catcoVendas <- select(vendas,"Category","Color","Product ID")

catcoDadosNaoDuplicados <- catcoVendas%>%
  distinct(`Product ID`, .keep_all = TRUE)

catcoDadosNaoDuplicados <- select(catcoDadosNaoDuplicados, "Category", "Color")

catcoTabelaSemNA <- na.omit(catcoDadosNaoDuplicados)

catcoTabelaHMCor <- filter(catcoTabelaSemNA, Category != "Kids' Fashion")

catcoTabelaHMCor <- catcoTabelaHMCor %>%
  group_by(Color, Category) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  group_by(Color) %>%
  mutate(total = sum(freq))

catcoTabelaHMCor <- catcoTabelaHMCor%>%
  rename(Categoria = Category) %>%
  mutate(Categoria = recode(Categoria, 
                            "Men's Fashion" = "Moda Masculina",
                            "Women's Fashion" = "Moda Feminina"))

catcoTabelaHMCor <- catcoTabelaHMCor %>%
  mutate(Color = recode(Color, 
                        "White"= "Branco",
                        "Yellow" = "Amarelo",
                        "Blue"= "Azul",
                        "Green" = "Verde",
                        "Red" = "Vermelho",
                        "Black" = "Preto"))

catcoTabelaHMCor<- catcoTabelaHMCor %>%
  mutate(porcentagem = sprintf("%.2f%%", (freq / total) * 100)) %>%
  mutate(porcentagem = str_replace(porcentagem,"\\.", ","))

legendas <- str_squish(str_c(catcoTabelaHMCor$freq, " (", catcoTabelaHMCor$porcentagem, ")"))

# gráfico de colunas cor por categoria

ggplot(catcoTabelaHMCor) +
  aes(x = fct_reorder(Color, freq, .desc = T), y = freq,
      fill = Categoria, label = legendas) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Cor", y = "Frequência") +
  theme_estat()
ggsave("grafico-coluna-An3.pdf", width = 248, height = 114, units = "mm")


# Teste qui-quadrado

catcoVendas <- select(vendas,"Category","Color","Product ID")

catcoDadosNaoDuplicados <- catcoVendas%>%
  distinct(`Product ID`, .keep_all = TRUE)

catcoDadosNaoDuplicados <- select(catcoDadosNaoDuplicados, "Category", "Color")

catcoTabelaSemNA <- na.omit(catcoDadosNaoDuplicados)

catcoTabelaHMCor <- filter(catcoTabelaSemNA, Category != "Kids' Fashion")


catcoTabelaContingencia<- table(catcoTabelaHMCor$Category, catcoTabelaHMCor$Color)
catcoTabelaContingencia

catcoTesteChisq <- chisq.test(catcoTabelaContingencia)
catcoTesteChisq 


# Análise 4: Relação entre preço e avaliação

# organizando dataframe

paVendas <- select(vendas,"Price","Rating","Product ID")

paDadosNaoDuplicados <- paVendas%>%
  distinct(`Product ID`, .keep_all = TRUE)

paDadosNaoDuplicados <- select(paDadosNaoDuplicados, "Price", "Rating")

paTabelaSemNA <- na.omit(paDadosNaoDuplicados)

# gráfico de dispersão

ggplot(paTabelaSemNA) +
  aes(x = Price, y = Rating) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Preço",
    y = "Avaliação"
  ) +
  theme_estat()
ggsave("dispersao-bi.pdf", width = 158, height = 93, units = "mm")


# qqplot

ggplot(paTabelaSemNA) + 
  aes(sample = Rating) +
  stat_qq(colour = "#A11D21") +
  stat_qq_line(size = 0.8) + 
  labs(
    x = "Quantis da Normal",
    y = "Avaliação"
  ) +
  theme_estat()
ggsave("qq_plot1.pdf", width = 158, height = 93, units = "mm")

ggplot(paTabelaSemNA) + 
  aes(sample = Price) +
  stat_qq(colour = "#A11D21") +
  stat_qq_line(size = 0.8) + 
  labs(
    x = "Quantis da Normal",
    y = "Preço"
  ) +
  theme_estat()
ggsave("qq_plot2.pdf", width = 158, height = 93, units = "mm")

# Shapiro-Wilk

paTesteShapiroPreco <- shapiro.test(paTabelaSemNA$Price)
paTesteShapiroPreco

paTesteShapiroRating <- shapiro.test(paTabelaSemNA$Rating)
paTesteShapiroRating

# correlacao de pearson

paCorrelacaoPearson <- with(paTabelaSemNA, cor.test(Price, Rating, alternative = "two.sided", method = "pearson"))
paCorrelacaoPearson

#Análise 5:Frequência de cada tipo de devolução por marca


#organizando dataframe

devolução_atualizado <- merge(devolução_atualizado, vendas %>% select(`Unique ID`, Brand) %>% distinct() ,by = "Unique ID", all.x = TRUE)

dvmaTabelaMarca <-devolução_atualizado %>%
  select("Brand","Motivo devolução") 

dvmaTabelaSemNA <- na.omit(dvmaTabelaMarca)

dvmaTabelaFrequencia <- dvmaTabelaSemNA %>%
  group_by(Brand, `Motivo devolução`) %>%
  summarize(Frequência = n()) %>%
  ungroup() %>%
  group_by(Brand) %>%
  mutate(total = sum(Frequência))

dvmaTabelaFrequencia<- dvmaTabelaFrequencia %>%
  mutate(porcentagem = sprintf("%.2f%%", (Frequência / total) * 100)) %>%
  mutate(porcentagem = str_replace(porcentagem,"\\.", ","))

legendas <- str_squish(str_c(dvmaTabelaFrequencia$Frequência, " (", dvmaTabelaFrequencia$porcentagem, ")"))


# grafico frequencia devolucao por marca

ggplot(dvmaTabelaFrequencia) +
  aes(x = fct_reorder(Brand, total, .desc = T), y = Frequência,
      fill = `Motivo devolução`, label = legendas) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Marca", y = "Frequência") +
  theme_estat()
ggsave("grafico-coluna-An5.pdf", width = 307, height = 114, units = "mm")

# teste qui-quadrado

devolução_atualizado <- merge(devolução_atualizado, vendas %>% select(`Unique ID`, Brand) %>% distinct() ,by = "Unique ID", all.x = TRUE)

dvmaTabelaMarca <-devolução_atualizado %>%
  select("Brand","Motivo devolução") 

dvmaTabelaSemNA <- na.omit(dvmaTabelaMarca)


dvmaTabelaContingencia <- table(dvmaTabelaSemNA$Brand, dvmaTabelaSemNA$`Motivo devolução`)
dvmaTabelaContingencia

dvmaTesteChisq <- chisq.test(dvmaTabelaContingencia)
dvmaTesteChisq 


# Análise 6: Avaliação (rating) média por marca

# organizando o dataframe

avmaVendas <- select(vendas,"Rating","Brand","Product ID")

avmaDadosSemNA <- avmaVendas[complete.cases(avmaVendas$Brand, avmaVendas$Rating), ]

avmaDadosSemDuplicatas <- avmaDadosSemNA[!duplicated(avmaDadosSemNA$`Product ID`) | is.na(avmaDadosSemNA$`Product ID`), ]

avmaVendasRatingBrand <- select(avmaDadosSemDuplicatas, "Rating","Brand")

avmaVendasAvMedia <- aggregate(avmaVendasRatingBrand$Rating, by=list(avmaVendasRatingBrand$Brand), FUN=mean)

colnames(avmaVendasAvMedia) <- c("Marca", "Avaliacao_Media")

avmaVendasAvMedia$Porcentagem <- gsub("\\.", ",", round(avmaVendasAvMedia$Avaliacao_Media, 2))

# gráfico de colunas avaliacao media por marca 

ggplot(avmaVendasAvMedia) +
  aes(x = fct_reorder(Marca, Avaliacao_Media, .desc=T), y = Avaliacao_Media, label = Porcentagem) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    aes(y = Avaliacao_Media, label = Porcentagem),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3
  ) + 
  labs(x = "Marca", y = "Avaliação") +
  theme_estat()
ggsave("grafico-coluna-An6.pdf", width = 158, height =93 , units = "mm")










