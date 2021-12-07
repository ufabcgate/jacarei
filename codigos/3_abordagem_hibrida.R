
# abre as bibliotecas
library(readxl)
library(tidyverse)
library(sf)
library(stringr)

# importa os dados
territorial <- read_sf("dados/ap/Assentamentos_Precarios.shp") %>% st_transform(crs = 4326)
domiciliar <- read_sf("resultados/dados2.gpkg") %>% st_transform(crs = 4326) %>%
  rename("Código familiar" = "Código.familiar",
         "Famílias novas" = "Famílias.novas",
         "Urbana/rural" = "Urbana.rural",
         "Déficit ou Inadequação" = "Déficit.ou.Inadequação")

# tabela territorial
tabela_territorial <- territorial %>%
  st_drop_geometry() %>%
  mutate(Deficit = ceiling(Domicilios * Remocao / 100),
         Inadequacao = Domicilios - Deficit) %>%
  summarize(Deficit = sum(Deficit),
            Inadequacao = sum(Inadequacao))

# domiciliar dentro
domiciliar_dentro <- st_intersection(domiciliar, territorial) %>%
  mutate("Dentro ou fora" = "Dentro") %>%
  rename("Código familiar" = "Código.familiar",
         "Famílias novas" = "Famílias.novas",
         "Urbana/rural" = "Urbana.rural",
         "Déficit ou Inadequação" = "Déficit.ou.Inadequação")

# domiciliar fora
domiciliar_fora <- domiciliar %>%
  filter(!`Código familiar` %in% domiciliar_dentro$`Código familiar`) %>%
  mutate("Dentro ou fora" = "Fora") %>%
  mutate(Cascata = case_when(`Déficit ou Inadequação` == "Déficit" ~ "1 - Provisão de Moradias",
                             (I01 == "Inadequado" | I02 == "Inadequado" | I03 == "Inadequado" | I04 == "Inadequado") ~ "2 - Melhorias Habitacionais",
                             (I01 != "Inadequado" | I02 != "Inadequado" | I03 != "Inadequado" | I04 != "Inadequado") & (I09 == "Inadequado")  ~ "3 - Políticas de Apoio à Locação",
                             (I05 == "Inadequado" | I06 == "Inadequado" | I07 == "Inadequado" | I08 == "Inadequado") ~ "4 - Melhorias Urbanas"))

# agrupa domiciliar por ap
grupos <- domiciliar_dentro %>%
  st_drop_geometry() %>%
  group_by(NUCLEO, `Déficit ou Inadequação`) %>%
  tally() %>%
  pivot_wider(id_cols = NUCLEO, names_from = `Déficit ou Inadequação`, values_from = n)

# consolida grupos por ap (domiciliar e territorial)
territorial_grupos <- territorial %>%
  left_join(grupos) %>%
  replace(is.na(.), 0) %>%
  mutate(Domicilios_territorial = Domicilios,
         Domicilios_domiciliar = Adequado + Déficit + Inadequação,
         Deficit_territorial = ceiling(Domicilios * Remocao / 100),
         Deficit_domiciliar = Déficit,
         D0 = pmax(Deficit_domiciliar, Deficit_territorial),
         I0 = pmax(Domicilios_territorial, Domicilios_domiciliar) - D0)

# tabela hibrida
tabela_hibrida <- tibble(Componente = c("Déficit Híbrido", "Inadequação Híbrido"),
                         Dentro = c(sum(territorial_grupos$D0), sum(territorial_grupos$I0)),
                         Fora = c(as.numeric(table(domiciliar_fora$`Déficit ou Inadequação`)[2]), as.numeric(table(domiciliar_fora$`Déficit ou Inadequação`)[3]))) %>%
  mutate(Total = Dentro + Fora)

# exporta dados
write.csv2(tabela_territorial, "resultados/tabela_abordagem_territorial.csv", row.names = FALSE)
write.csv2(tabela_hibrida, "resultados/tabela_abordagem_hibrida.csv", row.names = FALSE)

### TABELAS FINAIS

df <- bind_rows(domiciliar_dentro, domiciliar_fora)

tabelao <- tibble("Código" = c("Déficit",  
                               "D00", "D01", "D02", "D03", "D04", "D05", "D06", "D07", 
                               "Inadequação",
                               "I00", "I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09"),
                  "Componente" = c("Total Déficit", 
                                   "Domicílios em assentamentos precários com previsão de remoção", "Domicílio Improvisado", "Material de Parede Inadequado", "Densidade Excessiva em Apartamento ou Lote Condominial", "Coabitação", "Aluguel com Inadequações", "Moradia em Cômodo", "Famílias em Situação de Rua",
                                   "Total Inadequação", 
                                   "Domicílios em assentamentos precários sem previsão de remoção", "Densidade Excessiva", "Ausência de Banheiro", "Material do Piso", "Água Canalizada (Urbano)", "Esgotamento Sanitário", "Abastecimento de Água por Rede Pública (Urbano)", "Energia Elétrica", "Coleta de Lixo (Urbano)", "Ônus Excessivo com Aluguel"))

tabela <- tibble("Código" = c("Déficit", "Inadequação", 
                              "D01", "D02", "D03", "D04", "D05", "D06", "D07", 
                              "I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09"),
                 "Componente" = c("Total Déficit", "Total Inadequação", 
                                  "Domicílio Improvisado", "Material de Parede Inadequado", "Densidade Excessiva em Apartamento ou Lote Condominial", "Coabitação", "Aluguel com Inadequações", "Moradia em Cômodo", "Famílias em Situação de Rua",
                                  "Densidade Excessiva", "Ausência de Banheiro", "Material do Piso", "Água Canalizada (Urbano)", "Esgotamento Sanitário", "Abastecimento de Água por Rede Pública (Urbano)", "Energia Elétrica", "Coleta de Lixo (Urbano)", "Ônus Excessivo com Aluguel"))


#DENTRO
tabelao_dentro_1 <- domiciliar_dentro %>%
  st_drop_geometry() %>%
  pivot_longer(cols = -c(ID, `Código familiar`, `Famílias novas`, `Urbana/rural`, `Déficit ou Inadequação`, NUCLEO, Domicilios, Remocao, `Dentro ou fora`), 
               names_to = "Código", 
               values_to = "Condição") %>%
  pivot_wider(names_from = "Condição",
              values_from = "Condição") %>%
  group_by(`Código`) %>%
  summarize(Inadequado = sum(!is.na(Inadequado))) %>%
  filter(Código %in% c("D01", "D02", "D03", "D04", "D05", "D06", "D07")) %>%
  add_row(Código = "D00",
          Inadequado = territorial_grupos$Deficit_territorial) %>%
  add_row(Código = "Déficit",
          Inadequado = sum(territorial_grupos$D0)) %>%
  mutate("%" = Inadequado/sum(territorial_grupos$D0))

tabelao_dentro_2 <- domiciliar_dentro %>%
  st_drop_geometry() %>%
  filter(`Déficit ou Inadequação` != "Déficit") %>%
  pivot_longer(cols = -c(ID, `Código familiar`, `Famílias novas`, `Urbana/rural`, `Déficit ou Inadequação`, NUCLEO, Domicilios, Remocao, `Dentro ou fora`), 
               names_to = "Código", 
               values_to = "Condição") %>%
  pivot_wider(names_from = "Condição",
              values_from = "Condição") %>%
  group_by(`Código`) %>%
  summarize(Inadequado = sum(!is.na(Inadequado))) %>%
  filter(Código %in% c("I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09")) %>%
  add_row(Código = "I00",
          Inadequado = territorial_grupos$Domicilios_territorial) %>%
  add_row(Código = "Inadequação",
          Inadequado = sum(territorial_grupos$I0)) %>%
  mutate("%" = Inadequado/sum(territorial_grupos$I0))

tabelao_dentro_3 <- rbind(tabelao_dentro_1, tabelao_dentro_2) %>%
  left_join(tabelao) %>%
  mutate("%" = round(`%`, 3)) %>%
  select("Código", "Componente", "Inadequado", "%", everything())
tabelao_dentro_3

#FORA
tabela1 <- domiciliar_fora %>%
  st_drop_geometry() %>%
  #filter(D07 == "Inadequado") %>%
  pivot_longer(cols = -c(ID, `Código familiar`, `Famílias novas`, `Urbana/rural`, `Déficit ou Inadequação`), 
               names_to = "Código", 
               values_to = "Condição") %>%
  pivot_wider(names_from = "Condição",
              values_from = "Condição") %>%
  group_by(`Código`) %>%
  summarize(Inadequado = sum(!is.na(Inadequado))) %>%
  filter(Código %in% c("D01", "D02", "D03", "D04", "D05", "D06", "D07", "Déficit")) %>%
  mutate("%" = Inadequado/2404) ## !!! sempre conferir esse valor


tabela1

tabela2 <- domiciliar_fora %>%
  st_drop_geometry() %>%
  filter(`Déficit ou Inadequação` != "Déficit") %>%
  pivot_longer(cols = -c(ID, `Código familiar`, `Famílias novas`, `Código familiar`, `Urbana/rural`, `Déficit ou Inadequação`), 
               names_to = "Código", 
               values_to = "Condição") %>%
  pivot_wider(names_from = "Condição",
              values_from = "Condição") %>%
  group_by(`Código`) %>%
  summarize(Inadequado = sum(!is.na(Inadequado))) %>%
  filter(Código %in% c("I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09", "Inadequação")) %>%
  mutate("%" = Inadequado/4629) ## !!! sempre conferir esse valor

tabela2

tabela3 <- rbind(tabela1, tabela2) %>%
  left_join(tabela) %>%
  mutate("%" = round(`%`, 3)) %>%
  select("Código", "Componente", "Inadequado", "%", everything())
tabela3

# exporta os resultados
write.csv2(tabelao_dentro_3, "resultados/deficit_dentro.csv", row.names = FALSE)
write.csv2(tabela3, "resultados/deficit_fora.csv", row.names = FALSE)


#DEFICIT POR NUCLEO
deficit <- function(nuc){
  tg <- territorial_grupos %>% filter(NUCLEO == nuc)
  dd <- domiciliar_dentro %>% filter(NUCLEO == nuc)
  
  if (nrow(dd) > 0 & "Inadequação" %in% names(table(dd$`Déficit ou Inadequação`))) {
    tb1 <- dd %>%
      st_drop_geometry() %>%
      pivot_longer(cols = -c(ID, `Código familiar`, `Famílias novas`, `Urbana/rural`, `Déficit ou Inadequação`, NUCLEO, Domicilios, Remocao, `Dentro ou fora`), 
                   names_to = "Código", 
                   values_to = "Condição") %>%
      pivot_wider(names_from = "Condição",
                  values_from = "Condição") %>%
      group_by(`Código`) %>%
      summarize(Inadequado = sum(!is.na(Inadequado)))
  } else {
    tb1 <- tibble(Código = c("D01", "D02", "D03", "D04", "D05", "D06", "D07"),
                  Inadequado = c(0, 0, 0, 0, 0, 0, 0))
  }
  
   tb1 <- tb1 %>%
    filter(Código %in% c("D01", "D02", "D03", "D04", "D05", "D06", "D07")) %>%
    add_row(Código = "D00",
            Inadequado = tg$Deficit_territorial) %>%
    add_row(Código = "Déficit",
            Inadequado = sum(tg$D0))
  
  if (nrow(dd) > 0 & "Inadequação" %in% names(table(dd$`Déficit ou Inadequação`))) {
    tb2 <- dd %>%
      st_drop_geometry() %>%
      filter(`Déficit ou Inadequação` != "Déficit") %>%
      pivot_longer(cols = -c(ID, `Código familiar`, `Famílias novas`, `Urbana/rural`, `Déficit ou Inadequação`, NUCLEO, Domicilios, Remocao, `Dentro ou fora`), 
                   names_to = "Código", 
                   values_to = "Condição") %>%
      pivot_wider(names_from = "Condição",
                  values_from = "Condição") %>%
      group_by(`Código`) %>%
      summarize(Inadequado = sum(!is.na(Inadequado)))
  } else {
    tb2 <- tibble(Código = c("I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09"),
                  Inadequado = c(0, 0, 0, 0, 0, 0, 0, 0, 0))
  }
   
  tb2 <- tb2 %>%
    filter(Código %in% c("I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09")) %>%
    add_row(Código = "I00",
            Inadequado = tg$Domicilios_territorial - filter(tb1, Código == "Déficit")$Inadequado) %>%
    add_row(Código = "Inadequação",
            Inadequado = sum(tg$I0))
  
  tb3 <- rbind(tb1, tb2) %>%
    left_join(tabelao) %>%
    select("Código", "Componente", "Inadequado", everything()) %>%
    replace(is.na(.), 0)
  tb3
  
  write.csv(tb3, paste0("resultados/deficit_nucleos/", nuc, ".csv"), row.names = FALSE)
}

lapply(territorial$NUCLEO, deficit)

deficit("Veraneio Ijal")
