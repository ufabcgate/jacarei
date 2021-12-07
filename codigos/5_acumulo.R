
df_acumulo <- df %>%
  mutate(Acumulo = "")

df_acumulo

aux <- c("D01", "D02", "D03", "D04", "D05", "D06", "D07", "I01", "I02", 
         "I03", "I04", "I05", "I06", "I07", "I08", "I09")

for (i in aux) {
  
  df_acumulo$Acumulo <- if_else(df_acumulo[[i]] == "Inadequado", paste0(df_acumulo$Acumulo, " ", i), df_acumulo$Acumulo)
  
  #df_acumulo <- df_acumulo %>%
  #  mutate(Acumulo = if_else(i == "Inadequado", paste0(Acumulo, " + ", i), Acumulo))
  
}

df_acumulo2 <- df_acumulo %>%
  st_drop_geometry() %>%
  group_by(`Dentro ou fora`, Acumulo) %>%
  tally() %>%
  arrange(desc(n)) %>%
  filter(Acumulo != "") %>%
  mutate(Acumulo = substr(Acumulo, 2, nchar(Acumulo)))
df_acumulo2

names(df_acumulo2) <- c("Situação", "Inadequações", "Quantidade de famílias")

write.csv2(df_acumulo2, "resultados/acumulo_de_inadequacoes.csv", row.names = FALSE)
