#Análise 2: Variação do preço por marca


selecionando_marcas <- select(vendas,"Brand","Price","Product ID")
tabela_marcas <- na.omit(selecionando_marcas)
dados_sem_duplicatas <- tabela_marcas%>%
  distinct(`Product ID`, .keep_all = TRUE)


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

cores_estat <- c('#A11D21','#003366','#CC9900','#663333','#FF6600','#CC9966','#999966','#006606','#008091','#041835','#666666')


# boxplot
ggplot(dados_sem_duplicatas, aes(x = Brand, y = Price)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Marca", y = "Preço") +
  theme_estat()
ggsave("grafico_bi_boxplot_An2.pdf", width = 158, height = 93, units = "mm")

#quadro de medidas

dados_p_medidas <- dados_sem_duplicatas %>%
  group_by(Brand) 

quadro_medidas <- dados_p_medidas %>%
  summarize(
    Média = formatC(mean(Price), format = "f", digits = 2),
    Desvio_Padrão = formatC(sd(Price), format = "f", digits = 2),
    Mínimo = formatC(min(Price), format = "f", digits = 2),
    Quartil_1 = formatC(quantile(Price, 0.25), format = "f", digits = 2),
    Mediana = formatC(median(Price), format = "f", digits = 2),
    Quartil_3 = formatC(quantile(Price, 0.75), format = "f", digits = 2),
    Máximo = formatC(max(Price), format = "f", digits = 2)
  )
quadro_medidas

# analise de comparacao das médias(kruskal-wallis)

kruskal_test_result <- kruskal.test(Price ~ Brand, data = dados_sem_duplicatas)
kruskal_test_result 
  
