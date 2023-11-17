# Análise 6: Avaliação (rating) média por marca


rating_marca <- select(vendas,"Rating","Brand","Product ID")
dados_sem_na <- rating_marca[complete.cases(rating_marca$Brand, rating_marca$Rating), ]
dados_sem_duplicatas <- dados_sem_na[!duplicated(dados_sem_na$`Product ID`) | is.na(dados_sem_na$`Product ID`), ]
rating_marca_2 <- select(dados_sem_duplicatas, "Rating","Brand")

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
ggplot(rating_marca_2, aes(x = Brand, y = Rating)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Marca", y = "Avaliação") +
  theme_estat()
ggsave("grafico_bi_boxplot_An6.pdf", width = 158, height = 93, units = "mm")


#quadro de medidas

dados_p_medidas <- rating_marca_2 %>%
  group_by(Brand) 

quadro_medidas <- dados_p_medidas %>%
  summarize(
    Média = formatC(mean(Rating), format = "f", digits = 2),
    Desvio_Padrão = formatC(sd(Rating), format = "f", digits = 2),
    Mínimo = formatC(min(Rating), format = "f", digits = 2),
    Quartil_1 = formatC(quantile(Rating, 0.25), format = "f", digits = 2),
    Mediana = formatC(median(Rating), format = "f", digits = 2),
    Quartil_3 = formatC(quantile(Rating, 0.75), format = "f", digits = 2),
    Máximo = formatC(max(Rating), format = "f", digits = 2)
  )
quadro_medidas

# analise de comparacao das médias(kruskal-wallis)

kruskal_test_result <- kruskal.test(Rating ~ Brand, data = rating_marca_2)
kruskal_test_result 

