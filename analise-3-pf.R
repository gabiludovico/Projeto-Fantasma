# Análise 3: Relação entre categorias (apenas feminino e masculino) e cor

#organizando o dataframe
vendas_3 <- select(vendas,"Category","Color","Product ID")
dados_nao_duplicados <- vendas_3%>%
  distinct(`Product ID`, .keep_all = TRUE)
dados_nao_duplicados <- select(dados_nao_duplicados, "Category", "Color")
tabela_sem_NA <- na.omit(dados_nao_duplicados)
tabela_h_m_cor <- filter(tabela_sem_NA, Category != "Kids' Fashion")
tabela_h_m_cor <- tabela_h_m_cor %>%
  group_by(Color, Category) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  group_by(Color) %>%
  mutate(total = sum(freq))
tabela_h_m_cor <- tabela_h_m_cor%>%
  rename(Categoria = Category) %>%
  mutate(Categoria = recode(Categoria, 
                            "Men's Fashion" = "Moda Masculina",
                            "Women's Fashion" = "Moda Feminina"))
tabela_h_m_cor <- tabela_h_m_cor %>%
  mutate(Color = recode(Color, 
                        "White"= "Branco",
                        "Yellow" = "Amarelo",
                        "Blue"= "Azul",
                        "Green" = "Verde",
                        "Red" = "Vermelho",
                        "Black" = "Preto"))
tabela_h_m_cor<- tabela_h_m_cor %>%
  mutate(porcentagem = sprintf("%.2f%%", (freq / total) * 100)) %>%
  mutate(porcentagem = str_replace(porcentagem,"\\.", ","))

legendas <- str_squish(str_c(tabela_h_m_cor$freq, " (", tabela_h_m_cor$porcentagem, ")"))
                      

# padronizacao pra gráfico 

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


# gráfico de colunas cor por categoria
ggplot(tabela_h_m_cor) +
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
vendas_3 <- select(vendas,"Category","Color","Product ID")
dados_nao_duplicados <- vendas_3%>%
  distinct(`Product ID`, .keep_all = TRUE)
dados_nao_duplicados <- select(dados_nao_duplicados, "Category", "Color")
tabela_sem_NA <- na.omit(dados_nao_duplicados)
tabela_h_m_cor <- filter(tabela_sem_NA, Category != "Kids' Fashion")


tabela_contingencia <- table(tabela_h_m_cor$Category, tabela_h_m_cor$Color)
tabela_contingencia
resultado_teste_chisq <- chisq.test(tabela_contingencia)
resultado_teste_chisq 

