#Load libraries.
library(tidyverse)
library(tm)
library(tidytext)
library(pdftools)
library(here)

#Load fonts.
windowsFonts("Bahnschrift" = windowsFont("Bahnschrift"))

#Scrape data from PDFs.
vox <- pdf_text("data/vox.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "VOX")
pp <- pdf_text("data/pp.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "PP")
comuns <- pdf_text("data/programa-comuns-14-febrer.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "ECP")
cup <- pdf_text("data/programa-electoral-cup-eleccions-catalunya-2021-2.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "CUP")
erc <- pdf_text("data/programa-electoral-erc-eleccions-2021.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "ERC")
junts <- pdf_text("data/programa-electoral-junts-020221.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "Junts")
pdecat <- pdf_text("data/programa-electoral-pdecat-14.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "PDC")
psc <- pdf_text("data/programa-electoral-psc-eleccions-catalunya-2021-14-f.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "PSC")
cs <- pdf_text("data/cs.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "Cs")

#Merge data.
full_parties <- mget(ls()) %>% bind_rows() 

#Remove stopwords from corpus.
parties_wo_stopwords<- full_parties %>% anti_join(as_tibble(stopwords("spanish")), by = c("word" = "value")) %>% anti_join(as_tibble(stopwords("catalan")), by = c("word" = "value"))

#Create new dataframe with word counts.

count_words <- parties_wo_stopwords %>% count(party, word, sort = TRUE)

total_words <- count_words %>% group_by(party) %>% summarise(total = sum(n))

words_parties <- left_join(count_words, total_words)

#Remove custom stopwords.

stopwords <- c("als", "dels", "ció", "ment", "als", "ons", "tat", "posarem", "establirem", "garantirem", "aprovarem", "tat", "fer", "crearem", "potenciarem", "p", "ió", "r", "ció", "ec", "st", "st","tat", "aprobaremos", "impulsaremos", "nuevos", "mejoraremos","garantizaremos","así", "p1", "p2", "p3", "p4", "p5", "p6", "p7","p8","p9", "p10", "p11", "p12", "p13", "o1", "o2", "o3", "p14", "p15", "p16", "p17", "p18", "reforçarem",  "garantir", "assegurarem", "fomentarem", "elaborarem", "impulsarem", "canviem", "in", "impulsarem", "promourem", "c", "í", "so", "rt", "in","cions","ac","años","p21", "p20", "p19","promoure", "pa", "ro","po", "cata", "mp", "lu", "io", "di", "v", "lunya", "ra", "ci", "è", "ic", "x", "talunya", "tats","ge","tre","consolidarem","puguin", "proteger", "detener", "reforzar", "facilitarem", "incrementarem", "acabarem","donarem","proposem","2021","b","destacada","desplegarem","pdecat","acabar","eliminarem","reduirem","millorarem","evitarem","f","prohibirem","dotarem","eliminar")
  
#Compute tf_idf.

parties_tf_idf <- words_parties %>%
                filter(n > 2 & !word %in% stopwords) %>% 
                bind_tf_idf(word, party, n) %>% 
                mutate(party = factor(party, levels = c("PSC", "Junts", "ERC", "Cs", "ECP", "VOX", "CUP", "PP", "PDC")))

order_color <- c("#fb2a34", "#57bcc7", "#ffbf05", "#fc6e24", "#7c1c8b", "#77bf27", "#fefe34", "#41b9d4", "#19438d")

#Plot.

parties_tf_idf %>% 
  group_by(party)%>%
  slice_max(tf_idf, n = 9, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder_within(str_to_title(word), tf_idf, party), fill = party)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~party, ncol = 3, scales = "free") +
  labs(title = "¿Cuáles son los temas más diferenciales de los programas de cada partido en las elecciones catalanas?", subtitle = "Palabras más frecuentes en el programa de un partido en comparación con su frecuencia en el del resto (tf-idf)", y = NULL, x = NULL, caption = "Fuente: Programas electorales de los partidos políticos | @jantleon") +
  theme_minimal(base_family = "Bahnschrift") +
  theme(strip.text.x = element_text(face = "bold"),
        axis.text.y = element_text(size = 7),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = order_color)

#Save plot.
ggsave("tfidf_partidos.png", width = 10, height = 6)

