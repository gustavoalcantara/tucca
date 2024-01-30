#

library(tidyverse)

x <- read.csv('meduloblastoma_20240123.csv')

x|>
  dplyr::glimpse()

x|>
  dplyr::distinct(data_admis)




x|>
  dplyr::distinct(data)

x |>
  dplyr::mutate(inst_origem = 
                  dplyr::case_when(
                    inst_origem == 1 ~ "Hospital Santa Marcelina",
                    inst_origem == 2 ~ "GRAAC",
                    inst_origem == 3 ~ "GACC",
                    inst_origem == 4 ~ "GPACI",
                    inst_origem == 5 ~ "Hospital Aristides Maltez",
                    inst_origem == 6 ~ "Hospital da Criança de Brasília",
                    inst_origem == 7 ~ "Hospital da Liga",
                    inst_origem == 8 ~ "Hospital de Clínicas da UFPR",
                    inst_origem == 9 ~ "Hospital Estadual de Bauru",
                    inst_origem == 10 ~ "Hospital Infantil Joana de Gusmão",
                    inst_origem == 11 ~ "Hospital Mário Covas",
                    inst_origem == 12 ~ "Hospital Moinhos de Ventos",
                    inst_origem == 13 ~ "Hospital Nacional de Niños Benjamin Bloom",
                    inst_origem == 14 ~ "Hospital Napoleão Laureano",
                    inst_origem == 15 ~ "Hospital Pereira Rossell",
                    inst_origem == 16 ~ "Hospital Regional do Câncer de Presidente Prudente",
                    inst_origem == 17 ~ "Hospital Santo Antônio",
                    inst_origem == 18 ~ "Hospital São Marcos",
                    inst_origem == 19 ~ "Hospital Unimed de Araçatuba",
                    inst_origem == 20 ~ "IMOAB",
                    inst_origem == 21 ~ "Instituto de Medicina Integral Prof. Fernando Figueira",
                    inst_origem == 22 ~ "Hospital UDI",
                    inst_origem == 23 ~ "Santa Casa de Misericórdia de Presidente Prudente",
                    inst_origem == 24 ~ "Santa Casa de Belo Horizonte",
                    inst_origem == 25 ~ "Serviço de Saúde Metropolitano Oriente",
                    inst_origem == 26 ~ "UOCP",
                    inst_origem == 27 ~ "Hospital Del Niño Dr Francisco de Icaza Bustamante",
                    inst_origem == 28 ~ "Hospital Carlos Van Buren",
                    inst_origem == 29 ~ "Fundação Faculdade Regional de Medicina São José Rio Preto",
                    inst_origem == 30 ~ "Hospital das Clinicas Botucatu",
                    inst_origem == 31 ~ "Hospital São Vicente de Paulo",
                    inst_origem == 32 ~ "Hospital de Clínicas da Universidade Federal de Uberlândia",
                    inst_origem == 33 ~ "Hospital Pequeno Príncipe")) |>
  mutate(data_admissao = ymd(datachegbloco),
         ano = year(data_admissao),
         mes = month(data_admissao),
         mes = case_when(
           mes == "1" ~ "Janeiro",
           mes == "2" ~ "Fevereiro",
           mes == "3" ~ "Março",
           mes == "4" ~ "Abril",
           mes == "5" ~ "Maio",
           mes == "6" ~ "Junho",
           mes == "7" ~ "Julho",
           mes == "8" ~ "Agosto",
           mes == "9" ~ "Setembro",
           mes == "10" ~ "Outubro",
           mes == "11" ~ "Novembro",
           mes == "12" ~ "Dezembro",
           TRUE ~ "Desconhecido"  # Trate qualquer outro valor de mês como desconhecido
         ))|>
  dplyr::group_by(inst_origem, nome_pac, ano, mes)|>
  dplyr::summarise()|>
  dplyr::arrange(desc(ano))|>
  print(n=1000)  

c|>
  print(n=1000)
write.csv(c, 'meduloblastoma_redcap.csv',
            fileEncoding = 'UTF-16',
            row.names = F)
  
    
x|>
  dplyr::distinct(dataaprimsint)

z|>
  print(n=1000)

y <- readxl::read_excel('meduloblastoma_geral.xlsx',
                        sheet = 'Planilha2')

y <- read.table(file = 'meduloblastoma_txt.txt',
                sep = '')



y <- meduloblastoma_txt
#até 2020
y |> 
  dplyr::mutate(data_admissao = dmy(DATA.ENTRADA.TUCCA),
                ano = year(data_admissao),
                mes = month(data_admissao),
                mes = case_when(
                  mes == 1 ~ "Janeiro",
                  mes == 2 ~ "Fevereiro",
                  mes == 3 ~ "Março",
                  mes == 4 ~ "Abril",
                  mes == 5 ~ "Maio",
                  mes == 6 ~ "Junho",
                  mes == 7 ~ "Julho",
                  mes == 8 ~ "Agosto",
                  mes == 9 ~ "Setembro",
                  mes == 10 ~ "Outubro",
                  mes == 11 ~ "Novembro",
                  mes == 12 ~ "Dezembro",
                  TRUE ~ "Desconhecido"  # Trate qualquer outro valor de mês como desconhecido
                )) |> 
  dplyr::filter(ano <= 2020)|>
  dplyr::group_by(SERVIÇO.DE.ORIGEM, ano, PACIENTE) |> 
  dplyr::summarise(quantidade_entrada = dplyr::n())|>
  dplyr::arrange(desc(ano)) -> a
  print(n=1000)


write.csv(a, 'meduloblastoma_ate2020_com_paciente.csv',
            fileEncoding = 'UTF-16',
            row.names = F)  
  

#2021 em diante
y |> 
  dplyr::mutate(data_admissao = dmy(DATA.ENTRADA.TUCCA),
                ano = year(data_admissao),
                mes = month(data_admissao),
                mes = case_when(
                  mes == 1 ~ "Janeiro",
                  mes == 2 ~ "Fevereiro",
                  mes == 3 ~ "Março",
                  mes == 4 ~ "Abril",
                  mes == 5 ~ "Maio",
                  mes == 6 ~ "Junho",
                  mes == 7 ~ "Julho",
                  mes == 8 ~ "Agosto",
                  mes == 9 ~ "Setembro",
                  mes == 10 ~ "Outubro",
                  mes == 11 ~ "Novembro",
                  mes == 12 ~ "Dezembro",
                  TRUE ~ "Desconhecido"  # Trate qualquer outro valor de mês como desconhecido
                )) |> 
  dplyr::filter(ano >= 2021)|>
  dplyr::group_by(SERVIÇO.DE.ORIGEM, ano, mes, PACIENTE) |> 
  dplyr::summarise()|>
  dplyr::arrange(desc(ano)) -> b
  
  
write.csv(b, 'meduloblastoma_ate2023.csv',
          fileEncoding = 'UTF-16',
          row.names = F) 

y|>
  dplyr::
  dplyr::glimpse()
  
  
  
  
