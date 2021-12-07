
dats3 <- structure(list(NUCLEO = "Jd Nova Esperanca", D01 = 0L, D02 = 0L, 
                        D03 = 4L, D04 = 0L, D05 = 2L, D06 = 1L, D07 = 0L, D00 = 0L, 
                        Déficit = 6L, I01 = 1L, I02 = 0L, I03 = 0L, I04 = 0L, I05 = 0L, 
                        I06 = 0L, I07 = 0L, I08 = 0L, I09 = 3L, I00 = 44L, Inadequação = 44L, 
                        Total = 50L), row.names = c(NA, -1L), class = c("tbl_df", 
                                                                        "tbl", "data.frame"))

for (i in territorial$NUCLEO) {
  dats <- read.csv2(paste0("resultados/deficit_nucleos/", i, ".csv"))
  
  dats2 <- dats %>%
    select(-Componente) %>%
    pivot_wider(id_cols = `Código`,
                names_from = `Código`,
                values_from = `Inadequado`) %>%
    mutate(NUCLEO = i) %>%
    mutate(Total = `Déficit` + `Inadequação`) %>%
    select(NUCLEO, everything())
  
  dats3 <- dats3 %>% bind_rows(dats2) %>% distinct()
  
}

deficit_nucleos <- territorial %>% left_join(dats3)

write_sf(deficit_nucleos, "resultados/deficit_nucleos.gpkg")
