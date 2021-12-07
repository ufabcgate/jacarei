
# abre as bibliotecas
library(readxl)
library(tidyverse)
library(stringr)
library(janitor)

# cria a tabela com codigo e componente do deficit e inadequação
tabela <- tibble("Código" = c("Déficit", "Inadequação", 
                              "D01", "D02", "D03", "D04", "D05", "D06", "D07", 
                              "I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09"),
                 "Componente" = c("Total Déficit", "Total Inadequação", 
                                  "Domicílio Improvisado", "Material de Parede Inadequado", "Densidade Excessiva em Apartamento ou Lote Condominial", "Coabitação", "Aluguel com Inadequações", "Moradia em Cômodo", "Famílias em Situação de Rua",
                                  "Densidade Excessiva", "Ausência de Banheiro", "Material do Piso", "Água Canalizada (Urbano)", "Esgotamento Sanitário", "Abastecimento de Água por Rede Pública (Urbano)", "Energia Elétrica", "Coleta de Lixo (Urbano)", "Ônus Excessivo com Aluguel"))

# define as condições do condomínio simples
condominio_simples <- c("AP", "ANDAR", "PAVIMENTO", "BL", "TORRE", "PREDIO",
                        "FUNDO", "FRENTE", "CIMA", "BAIXO", "SUPERIOR", "INFERIOR",
                        "TERREA", "DIREITA", "ESQUERDA", "MEIO", "EDICULA", "PRINCIPAL",
                        "PORAO", "BAR", "GARAGEM", "TRAILER", "BARRACO", "CDHU")

# importa os dados
cadunico <- read_excel("dados/cadunico/CECAD - agosto 2020 - SEPLAN_Original.xlsx")

# agrupa por familias
cadunico_familias <- cadunico[,c(1:42)] %>%
  distinct() %>%
  mutate(ID = as.character(`Código familiar`),
         `Famílias novas` = "ID0")

# identifica familias em situação de rua
cadunico_D07 <- cadunico_familias %>%
  filter(`Código familiar` %in% filter(cadunico, `Situacao de Rua` == 1)$`Código familiar`) %>%
  group_by(`Código familiar`) %>%
  summarize(D07 = "Inadequado") %>%
  mutate(`Famílias novas` = "ID7",
         ID = paste0(`Código familiar`, " ", `Famílias novas`))

# identifica familias em coabitacao
cadunico_coab1 <- cadunico_familias %>% 
  filter(`Quantidade de famílias no domicílio` > 1) %>%
  select(`Código familiar`) %>%
  mutate(D04 = "Inadequado",
         `Famílias novas` = "ID1",
         ID = paste0(`Código familiar`, " ", `Famílias novas`))

cadunico_coab2 <- cadunico_familias %>% 
  filter(`Quantidade de famílias no domicílio` > 2) %>%
  select(`Código familiar`) %>%
  mutate(D04 = "Inadequado",
         `Famílias novas` = "ID2",
         ID = paste0(`Código familiar`, " ", `Famílias novas`))

cadunico_coab3 <- cadunico_familias %>% 
  filter(`Quantidade de famílias no domicílio` > 3) %>%
  select(`Código familiar`) %>%
  mutate(D04 = "Inadequado",
         `Famílias novas` = "ID3",
         ID = paste0(`Código familiar`, " ", `Famílias novas`))

# cria base final de familias
cadunico_familias <- bind_rows(cadunico_familias, cadunico_D07, cadunico_coab1, cadunico_coab2, cadunico_coab3) %>%
  mutate(ID = row_number())

# computa os componentes
cadunico_componentes <- cadunico_familias %>%
  mutate(D01 = case_when(`Espécie de domicílio` == "Particular improvisado" ~ "Inadequado",
                         `Espécie de domicílio` %in% c("Particular permanente", "Coletivo") ~ "Adequado"),
         D02 = case_when(`Material paredes` %in% c("Taipa não revestida", "Madeira aproveitada", "Palha", "Outro Material") ~ "Inadequado",
                         `Material paredes` %in% c("Alvenaria/tijolo sem revestimento", "Alvenaria/tijolo com revestimento", "Madeira aparelhada", "Taipa revestida") ~ "Adequado"),
         D03 = case_when((`Quantidade de pessoas no domicílio`/`Quantidade de dormitórios` >= 3) & (str_detect(`complemento adicional`, paste(condominio_simples, collapse = "|")) == TRUE)  ~ "Inadequado",
                         `Quantidade de pessoas no domicílio`/`Quantidade de dormitórios` >= 3 & str_detect(`complemento adicional`, paste(condominio_simples, collapse = "|")) == FALSE ~ "Adequado"),
         #D04 = case_when((`Quantidade de famílias no domicílio` > 1) ~ "Inadequado",
         #                 `Quantidade de famílias no domicílio` <= 1 ~ "Adequado"),
         D05 = case_when((`Valor de despesas com aluguel` > 0) & (`Quantidade de pessoas no domicílio`/`Quantidade de dormitórios` >= 3 | `Existe banheiro?` == "Não" | `Material do piso` %in% c("Terra", "Madeira aproveitada") | (`Possui água canalizada?` == "Não" & `Urbana/rural` == "Urbanas")) ~ "Inadequado",
                         `Valor de despesas com aluguel` > 0 & `Quantidade de pessoas no domicílio`/`Quantidade de dormitórios` < 3 & `Existe banheiro?` == "Sim" & `Material do piso` %in% c("Cerâmica, lajota ou pedra", "Cimento", "Madeira aparelhada", "Carpete", "Outro material") & (`Possui água canalizada?` == "Sim" & `Urbana/rural` == "Urbanas") ~ "Adequado"),
         D06 = case_when(`Quantidade de cômodos` <= 1 ~ "Inadequado",
                         `Quantidade de cômodos` > 1 ~ "Adequado"),
         #D07 = if_else(`Código familiar` %in% cadunico_rua$`Código familiar`, "Inadequado", "Adequado"),
         I01 = case_when(`Quantidade de pessoas no domicílio`/`Quantidade de dormitórios` < 3 ~ "Adequado",
                         `Quantidade de pessoas no domicílio`/`Quantidade de dormitórios` >= 3 ~ "Inadequado"),
         I02 = case_when(`Existe banheiro?` == "Sim" ~ "Adequado",
                         `Existe banheiro?` == "Não" ~ "Inadequado"),
         I03 = case_when(`Material do piso` %in% c("Cerâmica, lajota ou pedra", "Cimento", "Madeira aparelhada", "Carpete", "Outro material") ~ "Adequado",
                         `Material do piso` %in% c("Terra", "Madeira aproveitada") ~ "Inadequado"),
         I04 = case_when(`Possui água canalizada?` == "Sim" & `Urbana/rural` == "Urbanas" ~ "Adequado",
                         `Possui água canalizada?` == "Não" & `Urbana/rural` == "Urbanas" ~ "Inadequado"),
         I05 = case_when(`Forma de escoamento sanitário` %in% c("Rede coletora de esgoto", "Fossa séptica") ~ "Adequado",
                         `Forma de escoamento sanitário` %in% c("Fossa rudimentar", "Vala a céu aberto", "Direto para um rio, lago ou mar", "Outra forma") ~ "Inadequado"),
         I06 = case_when(`Forma de abastecimento` == "Rede geral de distribuição" & `Urbana/rural` == "Urbanas" ~ "Adequado",
                         `Forma de abastecimento` %in% c("Poço ou nascente", "Cisterna", "Outra forma") & `Urbana/rural` == "Urbanas" ~ "Inadequado"),
         I07 = case_when(`Tipo de iluminação` %in% c("Elétrica com medidor próprio", "Elétrica com medidor comunitário") ~ "Adequado",
                         `Tipo de iluminação` %in% c("Elétrica sem medidor", "Óleo, querosene ou gás", "Vela", "Outra forma") ~ "Inadequado"),
         I08 = case_when(`Forma de coleta do lixo` %in% c("É coletado diretamente", "É coletado indiretamente") & `Urbana/rural` == "Urbanas" ~ "Adequado",
                         `Forma de coleta do lixo` %in% c("É queimado ou enterrado na propriedade", "É jogado em terreno baldio ou logradouro", "É jogado em rio ou mar", "Tem outro destino") & `Urbana/rural` == "Urbanas" ~ "Inadequado"),
         I09 = case_when(`Valor de despesas com aluguel`/`Renda total familiar` < 0.3 ~ "Adequado",
                         `Valor de despesas com aluguel`/`Renda total familiar` >= 0.3 ~ "Inadequado")) %>%
  select(ID, `Código familiar`, `Famílias novas`, `Urbana/rural`, D01, D02, D03, D04, D05, D06, D07, I01, I02, I03, I04, I05, I06, I07, I08, I09) %>%
  mutate_at(vars(-c(ID, `Código familiar`, `Famílias novas`)), ~replace(., is.na(.), "Sem Dados ou Não Aplicável")) %>%
  mutate(`Déficit` = if_else(D01 == "Inadequado" | D02 == "Inadequado" | D03 == "Inadequado" | D04 == "Inadequado" | D05 == "Inadequado" | D06 == "Inadequado" | D07 == "Inadequado", "Inadequado", "Adequado"),
         `Inadequação` = if_else(I01 == "Inadequado" | I02 == "Inadequado" | I03 == "Inadequado" | I04 == "Inadequado" | I05 == "Inadequado" | I06 == "Inadequado" | I07 == "Inadequado" | I08 == "Inadequado" | I09 == "Inadequado", "Inadequado", "Adequado"),
         `Déficit ou Inadequação` = case_when(`Déficit` == "Inadequado" ~ "Déficit",
                                              `Déficit` == "Adequado" & `Inadequação` == "Inadequado" ~ "Inadequação",
                                              `Déficit` == "Adequado" & `Inadequação` == "Adequado" ~ "Adequado"))

# tabela sintese
tabela1 <- cadunico_componentes %>%
  #filter(D07 == "Inadequado") %>%
  pivot_longer(cols = -c(ID, `Código familiar`, `Famílias novas`, `Urbana/rural`, `Déficit ou Inadequação`), 
               names_to = "Código", 
               values_to = "Condição") %>%
  pivot_wider(names_from = "Condição",
              values_from = "Condição") %>%
  group_by(`Código`) %>%
  summarize(Adequado = sum(!is.na(Adequado)),
            Inadequado = sum(!is.na(Inadequado)),
            `Sem Dados ou Não Aplicável` = sum(!is.na(`Sem Dados ou Não Aplicável`))) %>%
  filter(Código %in% c("D01", "D02", "D03", "D04", "D05", "D06", "D07", "Déficit")) %>%
  mutate("%" = Inadequado/2814) ## !!! sempre conferir esse valor

tabela1

tabela2 <- cadunico_componentes %>%
  filter(`Déficit ou Inadequação` != "Déficit") %>%
  pivot_longer(cols = -c(ID, `Código familiar`, `Famílias novas`, `Código familiar`, `Urbana/rural`, `Déficit ou Inadequação`), 
               names_to = "Código", 
               values_to = "Condição") %>%
  pivot_wider(names_from = "Condição",
              values_from = "Condição") %>%
  group_by(`Código`) %>%
  summarize(Adequado = sum(!is.na(Adequado)),
            Inadequado = sum(!is.na(Inadequado)),
            `Sem Dados ou Não Aplicável` = sum(!is.na(`Sem Dados ou Não Aplicável`))) %>%
  filter(Código %in% c("I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09", "Inadequação")) %>%
  mutate(`Sem Dados ou Não Aplicável` = `Sem Dados ou Não Aplicável` + 2814, "%" = Inadequado/5421) ## !!! sempre conferir esse valor

tabela2

tabela3 <- rbind(tabela1, tabela2) %>%
  left_join(tabela) %>%
  mutate("%" = round(`%`, 3)) %>%
  select("Código", "Componente", "Inadequado", "%", everything())
tabela3

write.csv2(cadunico_componentes, "resultados/dados1.csv", row.names = FALSE)

write.csv2(tabela3, "resultados/tabela_abordagem_domiciliar.csv", row.names = FALSE)
