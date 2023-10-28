# Análise 4: Relação entre preço e avaliação

# organizando dataframe
vendas_4 <- select(vendas,"Price","Rating","Product ID")
dados_nao_duplicados <- vendas_4%>%
  distinct(`Product ID`, .keep_all = TRUE)
dados_nao_duplicados <- select(dados_nao_duplicados, "Price", "Rating")
tabela_sem_NA <- na.omit(dados_nao_duplicados)

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

ggplot(tabela_sem_NA) +
  aes(x = Price, y = Rating) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Preço",
    y = "Avaliação"
  ) +
  theme_estat()
ggsave("dispersao-bi.pdf", width = 158, height = 93, units = "mm")


# qqplot
ggplot(tabela_sem_NA) + 
  aes(sample = Rating) +
  stat_qq(colour = "#A11D21") +
  stat_qq_line(size = 0.8) + 
  labs(
    x = "Quantis da Normal",
    y = "Avaliação"
  ) +
  theme_estat()
ggsave("qq_plot1.pdf", width = 158, height = 93, units = "mm")

ggplot(tabela_sem_NA) + 
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
shapiro_test1 <- shapiro.test(tabela_sem_NA$Price)
shapiro_test1

shapiro_test2 <- shapiro.test(tabela_sem_NA$Rating)
shapiro_test2


# correlacao de pearson

correlacao_pearson <- with(tabela_sem_NA, cor.test(Price, Rating, alternative = "two.sided", method = "pearson"))