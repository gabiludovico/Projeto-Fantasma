#Análise 2: Variação do preço por marca


selecionando_marcas <- select(vendas,"Brand","Price","Product ID")
tabela_marcas <- na.omit(selecionando_marcas)
dados_sem_duplicatas <- tabela_marcas%>%
  distinct(`Product ID`, .keep_all = TRUE)
tabela_marcas_preco <- dados_sem_duplicatas %>%
  group_by(Brand) %>%
  summarise(Preço = sum(Price)) %>%
  arrange(desc(Preço))
tabela_marcas_preco
dados_somados <-tabela_marcas_preco  %>%
  mutate(percentagem = sprintf("%.2f%%", round((Preço / sum(Preço)) * 100, 2)))
dados_somados

#padronização
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


# grafico de barras
ggplot(dados_somados) +
aes(x =reorder(Brand,-Preço), y = Preço) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(aes(label = paste(Preço, " (", gsub("\\.", ",", percentagem), ")", sep = "")), 
            vjust = -0.5, size = 3)  +
  labs(x = "Marca", y = "Preço") +
  theme_estat()
ggsave("grafico_colunas_An2.pdf", width = 158, height = 93, units = "mm")

# tabela com a quantidade de roupas por marca e o preço médio
tabela_preço_marca <- dados_sem_duplicatas  %>%
  group_by(Brand) %>%
  summarise(Quantidade =n(),PrecoMedio = mean(Price))
tabela_preço_marca  
