setwd("C:/Users/ra243464/Documents/repos/ME315/labs")

# Q1: Carregue o pacote tidyverse
library(tidyverse)

# Q2: Apresente os bancos de dados table1, table2, table3, table4a e table4b
table1 # Está no formato tidy
table2 # Não está no formato tidy, pois duas variáveis estão representadas em uma mesma coluna
table3 # Não está no formato tidy, a coluna rate tem informação de duas variáveis
table4a # Não está no formato tidy, pois uma variável está dispersa em mais de uma coluna
table4b # Não está no formato tidy, pois uma variável está dispersa em mais de uma coluna

# Q3: Determine a taxa de ocorrência de tuberculose para cada 10.000 pessoas com comantos dplyr
dplyr::mutate(table1, "rate" = (cases/population)*10000)

pivot_wider(table2, names_from = type, values_from = count) %>%
  dplyr::mutate("rate" = (cases/population)*10000)

separate(table3, into=c("cases", "population"), col = rate, sep="/", convert=T) %>%
  dplyr::mutate("rate" = (cases/population)*10000)

pivot_longer(table4a, c(`1999`, `2000`), names_to = "ano", values_to = "cases") %>%
  cbind(
    "population" = pivot_longer(table4b, c(`1999`, `2000`), names_to = "ano", values_to = "population")$population
  ) %>%
  dplyr::mutate("rate" = ((cases/population)*10000))


# Q4: Apresente, utilizando comandos do pacote dplyr, o número de casos de tuberculose por ano.
dplyr::group_by(table1, year) %>% summarise("cases" = sum(cases))

# Q5: Apresente, utilizando comandos do pacote dplyr, o número de casos de tuberculose em cada país.
dplyr::group_by(table1, country) %>% summarise("cases" = sum(cases))

# Q6: Utilizando comandos do pacote dplyr, apresente uma tabela que descreva a mudança no número de casos, em cada país, ao longo dos anos de 1999 e 2000.
dplyr::group_by(table1, country) %>% summarise("diff" = diff(cases))
dplyr::group_by(table1, country) %>% summarise("diff" = (cases[2]-cases[1])/cases[1])

# Q7: Apresente um gráfico de linhas, preparado via ggplot2, apresentando a mudança na taxa de casos (por 10.000 habitantes) estratificado por país.
dplyr::mutate(table1, "rate" = (cases/population)*10000) %>%
  ggplot(aes(x=year, y=rate, color=country)) +
  geom_line()
