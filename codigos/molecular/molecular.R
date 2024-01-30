######
#Código para limpeza e estatistica básica de todo o atendimento laboratorial da 
#TUCCA (2018 a 2023). 
#Limpe seu ambiente
rm( list = ls( )) 
graphics.off()
gc()
gc(reset = TRUE)
#Reading the archives
#2018 a 2021

library(readxl)
registro_geral_2018 <- read_excel("PLANILHA DE REGISTRO GERAL ENTRADA_2018.xlsx", 
                                                           sheet = "Plan1", col_types = c("text", "text", "date", "date", "date", "text", 
                                                                                                    "text", "text", "text", "text", "text", 
                                                                                                    "text", "text", "text", "text", "text", 
                                                                                                    "text", "text", "text", "text", "text", 
                                                                                                    "text", "text", "text", "text", "text", 
                                                                                                    "text", "text", "text", "text", "text", 
                                                                                                    "text", "text", "text", "text", "text"))



#2022
registro_geral_2022 <- read_excel("PLANILHA DE REGISTRO GERAL ENTRADA_2022.xlsx", 
                                       col_types = c("text", "text", "date", 
                                                               "date", "date", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "date", "date", 
                                                               "numeric", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "numeric", "text", "text", "text", 
                                                               "text", "text", "text", "date", "text"))

#2023 
registro_geral_2023 <- read_excel("PLANILHA DE REGISTRO GERAL ENTRADA_2023.xlsx", 
                                       col_types = c("text", "text", "date", 
                                                              "date", "date", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "text"))


#Meduloblastoma_geral
meduloblastoma_geral <- read.delim("C:/Users/Gustavo Casteletti/OneDrive - Associação para Crianças e Adolescentes com Câncer TUCCA/Área de Trabalho/gustavo_alcantara/gustavo/tucca/meduloblastoma/meduloblastoma/meduloblastoma_geral.txt", comment.char="#")


#ETL
#seleciona as variáveis que vou trabalhar
registro_geral_2018|>
  dplyr::glimpse()


registro_geral_2018|>
  dplyr::select(`ID (M-MEDULO; TS-TUMOR SOLIDO/GLIOMAS)`,
                PACIENTE,
                `Data de Nascimento`,
                `DATA ENTRADA TUCCA`,
                `Data solicitação (registrada no formulário)`,
                `Médico Solicitante`,
                `Serviço de Origem`,
                `resultado_biologia_molecular DA BIOMOL`,
                Observações,
                ) -> geral_2018a2021

registro_geral_2022|>
  dplyr::select(`ID (M-MEDULO; TS-TUMOR SOLIDO/GLIOMAS)`,
                PACIENTE,
                `Data de Nascimento`,
                `DATA ENTRADA TUCCA`,
                `Data solicitação (registrada no formulário)`,
                `Médico Solicitante`,
                `Serviço de Origem`,
                `resultado_biologia_molecular DA BIOMOL`,
                Observações) -> geral_2022

registro_geral_2023|>
  dplyr::select(`ID (M-MEDULO; TS-TUMOR SOLIDO/GLIOMAS)`,
                PACIENTE,
                `Data de Nascimento`,
                `DATA ENTRADA TUCCA`,
                `Data solicitação (registrada no formulário)`,
                `Médico Solicitante`,
                `Serviço de Origem`,
                `resultado_biologia_molecular DA BIOMOL`,
                Observações) -> geral_2023

#Coloca data como Character
class(registro_geral_2023$`resultado_biologia_molecular DA BIOMOL`)
registro_geral_2018$Observações <- as.character(registro_geral_2018$Observações)
registro_geral_2022$Observações <- as.character(registro_geral_2022$Observações)
registro_geral_2023$Observações <- as.character(registro_geral_2023$Observações)
registro_geral_2018$`resultado_biologia_molecular DA BIOMOL` <- as.character(registro_geral_2018$`resultado_biologia_molecular DA BIOMOL`)
registro_geral_2022$`resultado_biologia_molecular DA BIOMOL` <- as.character(registro_geral_2022$`resultado_biologia_molecular DA BIOMOL`)
registro_geral_2023$`resultado_biologia_molecular DA BIOMOL` <- as.character(registro_geral_2023$`resultado_biologia_molecular DA BIOMOL`)
geral_2018a2021 <- mutate(geral_2018a2021, `resultado_biologia_molecular DA BIOMOL` = as.character(`resultado_biologia_molecular DA BIOMOL`))
geral_2022 <- mutate(geral_2022, `resultado_biologia_molecular DA BIOMOL` = as.character(`resultado_biologia_molecular DA BIOMOL`))
geral_2023 <- mutate(geral_2023, `resultado_biologia_molecular DA BIOMOL` = as.character(`resultado_biologia_molecular DA BIOMOL`))



#Junta as bases
x <- dplyr::bind_rows(
  dplyr::mutate(geral_2018a2021, Ano = 2018),
  dplyr::mutate(geral_2022, Ano = 2022),
  dplyr::mutate(geral_2023, Ano = 2023)
)

#Renomeia as variáveis e cria variável de idade e faixa etária
x|>
  dplyr::rename(id = `ID (M-MEDULO; TS-TUMOR SOLIDO/GLIOMAS)`,
                paciente = PACIENTE,
                data_nascimento = `Data de Nascimento`,
                data_entrada_tucca = `DATA ENTRADA TUCCA`,
                data_solicitacao = `Data solicitação (registrada no formulário)`,
                medico_solicitante = `Médico Solicitante`,
                servico_origem = `Serviço de Origem`,
                observacoes = Observações,
                resultado_biologia_molecular_biologia_molecular = `resultado_biologia_molecular DA BIOMOL`) |>
  dplyr::mutate(idade_hoje = interval(as.Date(data_nascimento), Sys.Date()) %/% years(1))|>
  dplyr::mutate(idade_entrada_servico = interval(as.Date(data_nascimento), as.Date(data_entrada_tucca)) %/% years(1))|>
  dplyr::mutate(faixa_etaria = dplyr::case_when(
    idade_entrada_servico >= 0 & idade_entrada_servico <= 4 ~ "0 a 4",
    idade_entrada_servico >= 5 & idade_entrada_servico <= 9 ~ "5 a 9",
    idade_entrada_servico >= 10 & idade_entrada_servico <= 14 ~ "10 a 14",
    idade_entrada_servico >= 15 & idade_entrada_servico <= 19 ~ "15 a 19",
    idade_entrada_servico >= 20 ~ "20+"))|>
  dplyr::mutate(faixa_etaria = factor(faixa_etaria, levels = c("0 a 4", "5 a 9", "10 a 14", "15 a 19", '20+'))) -> x

#Analytics
#Quantas vezes cada nome de paciente aparece? No distinct deu um total de 283 observações. 
x|>
  dplyr::group_by(paciente)|>
  dplyr::summarise(quantidade = dplyr::n())|>
  dplyr::filter(quantidade > 1)|>
  dplyr::summarise(soma = sum(quantidade))
#Tá 'tranquilo' porque tem relaçao com a qtde. exame solicitado

#Qual a faixa etária dos pacientes por ano
x|>
  dplyr::mutate(ano = lubridate::year(data_entrada_tucca)) -> x

x|>
  dplyr::group_by(faixa_etaria, ano)|>
  dplyr::summarise(quantidade = dplyr::n())|>
  dplyr::arrange(desc(ano))|>
  print(n=1000)


#########
#Atendimentos por ano
x|>
  dplyr::group_by(ano)|>
  dplyr::summarise(qtde = dplyr::n())|>
  dplyr::arrange(desc(ano))


#Atendimentos de Meduloblastoma por ano
x|>
  dplyr:: filter(grepl("^M\\d+", id, ignore.case = TRUE))|>
  dplyr::group_by(ano)|>
  dplyr::summarise(qtde = dplyr::n())|>
  dplyr::arrange(desc(ano))


#Atendimentos do Laboratório por Instituição/Médico
x|>
  dplyr::group_by(medico_solicitante)|>
  dplyr::summarise(qtde = dplyr::n())|>
  dplyr::arrange(desc(qtde))|>
  print(n=1000)

x|>
  dplyr::mutate(resultado_biologia_molecular = 
                  dplyr::case_when(
                    resultado_biologia_molecular == 'GRUPO  4' ~ 'Grupo 4',
                    resultado_biologia_molecular == 'GRUPO 4' ~ 'Grupo 4',
                    resultado_biologia_molecular == 'Grupo 04' ~ 'Grupo 4',
                    resultado_biologia_molecular == 'MEDULOBLASTOMA DO GRUPO 4' ~ 'Grupo 4',
                    resultado_biologia_molecular == 'MEDULOBLASTOMA DO GRUPO 3.' ~ 'Grupo 3',
                    resultado_biologia_molecular == 'Grupo3' ~ 'Grupo 3',
                    resultado_biologia_molecular == 'Grupo3' ~ 'Grupo 3',
                    resultado_biologia_molecular == 'INCONCLUSIVO' ~ 'Indeterminado',
                    resultado_biologia_molecular == 'INDETERMINADO' ~ 'Indeterminado',
                    resultado_biologia_molecular == 'Indeterminado' ~ 'Indeterminado',
                    TRUE ~ resultado_biologia_molecular
                  ))|>
  dplyr::group_by(resultado_biologia_molecular, ano)|>
  dplyr::summarise(qtde = dplyr::n())|>
  dplyr::arrange(desc(ano))|>
  print(n=1000)

x|>
  dplyr::mutate(id = stringr::str_extract(id, "\\bM\\d+\\b"))|>
  dplyr::inner_join(meduloblastoma_geral, by = c('id' = 'ID..M.MEDULO.NÚMERO.DE.ENTRADA.'))|>
  dplyr::mutate(resultado_biologia_molecular = RESULTADO.DA.BIOMOL)|>
  dplyr::mutate(resultado_biologia_molecular = 
                  dplyr::case_when(
                    resultado_biologia_molecular == 'Não WTN /não SHH' ~ 'Grupo 3/4',
                    resultado_biologia_molecular == 'Não WNT/SHH' ~ 'Grupo 3/4',
                    resultado_biologia_molecular == 'Grupo 4 ' ~ 'Grupo 4',
                    resultado_biologia_molecular == 'INCONCLUSIVO' ~ 'Indeterminado',
                    resultado_biologia_molecular == 'Inconclusivo' ~ 'Indeterminado',
                    resultado_biologia_molecular == '' ~ 'Indeterminado',
                    resultado_biologia_molecular == 'Material biológico não analisado por qPCR devido à qualidade ser insuficiente para esse tipo de análise ' ~ 'Não Analisado',
                    resultado_biologia_molecular == 'Material biológico não analisado por qPCR devido à qualidade ou por não ser meduloblastoma' ~ 'Não Analisado',
                    TRUE ~ resultado_biologia_molecular
                  ))|>
  mutate(ordem = factor(resultado_biologia_molecular, levels = ordem_desejada))|>
  dplyr::group_by(resultado_biologia_molecular)|>
  dplyr::summarise(qtde = 
                     dplyr::n())|>
  dplyr::arrange(desc(qtde))|>
  dplyr::filter(resultado_biologia_molecular != 'Não Analisado')|>
  ggplot(aes(x = reorder(resultado_biologia_molecular, -qtde), y = qtde)) +
  geom_bar(stat = "identity", fill = 'steelblue') +
  geom_text(aes(label = qtde), vjust = -0.5, size = 4, fontface = 'bold') +
  labs(title = "Contagem de Resultados da Biologia Molecular",
       x = "Resultado Biologia Molecular",
       y = "Quantidade") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = 'bold', size = 16),
    axis.text.x = element_text(face = 'bold', size = 12),
    axis.text.y = element_text(face = 'bold', size = 12),
    axis.title.x = element_text(face = 'bold', size = 14),
    axis.title.y = element_text(face = 'bold', size = 14)
  ) +
  scale_x_discrete(limits = c("WNT", "SHH", "Grupo 3", "Grupo 4", "Grupo 3/4", "Indeterminado"))


windows()


ordem_desejada <- c("WNT", "SHH", "Grupo 3", "Grupo 4", "Grupo 3/4", "Indeterminado", "Não Analisado")

x$ordem <- factor(x$resultado_biologia_molecular, levels = ordem_desejada)



windows()
  