# Análise 4: Relação entre preço e avaliação

# organizando dataframe
paVendas <- select(vendas,"Price","Rating","Product ID")
paDadosNaoDuplicados <- paVendas%>%
  distinct(`Product ID`, .keep_all = TRUE)
paDadosNaoDuplicados <- select(paDadosNaoDuplicados, "Price", "Rating")
paTabelaSemNA <- na.omit(paDadosNaoDuplicados)

# padronização
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

