
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


