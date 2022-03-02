pacman::p_load(here, dplyr, plyr, readr, ggplot2)

dir_path <- here::here("data/raw_data/becas")
filenames <- list.files(dir_path, pattern=".csv", full.names=TRUE)

datalist = list()

for (i in seq_along(filenames)) {
  
  assign(paste("Bek", i, sep = ""), read_csv2(filenames[i], col_names = TRUE))
  dfname = paste("Bek", i, sep="")
  df = get(dfname)
  names(df) = toupper(names(df))
  datalist[[i]] <- df
}

for (i in seq_along(datalist)){
  colnames(datalist[[i]]) <- substr(names(datalist[[i]]), 1, 14)
}

totalbeca <- plyr::ldply(datalist, rbind)

rm(list=ls(pattern="Bek"))

totalbeca2 <- totalbeca %>%
  filter(ANIO_BENEFICIO != 2020) %>%
  mutate(MRUNANIO = paste(MRUN, "-", ANIO_BENEFICIO)) %>%
  arrange(ANIO_BENEFICIO) %>% 
  dplyr::group_by(MRUN) %>%
  dplyr::summarise(
    across(ANIO_BENEFICIO:BENEFICIO_BECA,           # apply to all columns
           ~paste0(na.omit(.x), collapse = ", ")))

totalbeca %>% 
  group_by(ANIO_BENEFICIO, BENEFICIO_BECA) %>%
  dplyr::summarise(count=n()) %>%
  ggplot2::ggplot(aes(x = as.factor(ANIO_BENEFICIO) )) + 
  ggplot2::stat_count(aes(fill = BENEFICIO_BECA ), binwidth=1500, 
                          colour="grey20", lwd=0.2)
beca_plot <- totalbeca %>%
  dplyr::count(ANIO_BENEFICIO, BENEFICIO_BECA) 

beca_plot %>%
  filter(n > 10000) %>%
  ggplot(aes(ANIO_BENEFICIO, n, color = BENEFICIO_BECA )) +
  geom_line() +
  xlab("Año Beneficio")+
  ylab("")+
  scale_color_brewer(palette = "Set1",
                     name = "Becas") +
  scale_y_continuous(labels = scales::comma, limits = c(8000,403500))
#ggsave("beca.pdf")