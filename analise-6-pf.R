# Análise 6: Avaliação (rating) média por marca

#organizando o dataframe
avmaVendas <- select(vendas,"Rating","Brand","Product ID")

avmaDadosSemNA <- avmaVendas[complete.cases(avmaVendas$Brand, avmaVendas$Rating), ]

avmaDadosSemDuplicatas <- avmaDadosSemNA[!duplicated(avmaDadosSemNA$`Product ID`) | is.na(avmaDadosSemNA$`Product ID`), ]

avmaVendasRatingBrand <- select(avmaDadosSemDuplicatas, "Rating","Brand")

avmaVendasAvMedia <- aggregate(avmaVendasRatingBrand$Rating, by=list(avmaVendasRatingBrand$Brand), FUN=mean)

colnames(avmaVendasAvMedia) <- c("Marca", "Avaliacao_Media")

avmaVendasAvMedia$Porcentagem <- paste0(gsub("\\.", ",", round(avmaVendasAvMedia$Avaliacao_Media, 2)), 
 " (", gsub("\\.", ",", sprintf("%.2f", (avmaVendasAvMedia$Avaliacao_Media / sum(avmaVendasAvMedia$Avaliacao_Media)) * 100)), "%)")


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
  ggsave("grafico-coluna-An5.pdf", width = 158, height =93 , units = "mm")




