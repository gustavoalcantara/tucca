#esse é um código para trabalhar com os dados de atendimento geral de 2001 a 2023
#bora
getwd()
rm(list=ls())
graphics.off()
gc(reset = TRUE)
windows()

x <- readxl::read_excel('banco_geral.xlsx',
                        sheet = 'Geral_Crianças')

#Como estão os nomes?
x|>
  dplyr::count(Nome)|>
  dplyr::filter(n>1)

x|>
  dplyr::filter(`Ano de Entrada` >= 2001) |>
  dplyr::group_by(`Ano de Entrada`) |>
  dplyr::summarise(quantidade = 
                     dplyr::n_distinct((Nome))) |>
  ggplot(aes(x = `Ano de Entrada`,
             y = quantidade)) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(min(x$`Ano de Entrada`), 
                                 max(x$`Ano de Entrada`), 
                                 1))
x|>
  dplyr::glimpse()


x|>
  dplyr::

x|>
  dplyr::distinct(`RGH Novo (após perda de dados do sistema em final de marco / inicio de abril - 2019`)|>
  print(n=1000)
    
x|>
  dplyr::distinct(`Motivo Óbito`) |>
  print(n=1000)


install.packages('REDCapR')
#####
uri <- "https://redcap.tuccaensinoepesquisa.org.br/redcap/api/pid=33"
token <- 'C8C0652AFC8CA446C0716CA2F2D7CB56'

df <- REDCapR::redcap_read(redcap_uri = uri,
                     token = token)$data
uri     <- "https://bbmc.ouhsc.edu/redcap/api/"
token   <- "9A81268476645C4E5F03428B8AC3AA7B"


#Teste Red Cap

x|>
  dplyr::distinct(ano)


#ETL Clássica das variáveis disponíveis no DF. 
#Veja que as variáveis que não utilizei o case_when são variáveis que preciso de um apoio melhor. 
#Vamos deixalas todas bonitinhas e fechar esse df com 4216 registros!

names(x)
x|>
  dplyr::rename(
    id = ID,
    ano = `seq ano`,
    ano_entrada = `Ano de Entrada`,
    mes_entrada = `Mês de Entrada`,
    data_primeira_consulta = `Data da Primeira consulta / TUCCA`,
    rgh = RGH,
    nome = Nome,
    sexo = SEXO,
    data_nascimento = `Data Nascimento`,
    cor_etnia = `COR/Etnia`,
    endereco = Endereço,
    regiao = Região,
    estado = Estado,
    cidade = `Cidade / Municipio`,
    prefeitura_subprefeitura = `Prefeitura / Subprefeitura`,
    hospital_encaminhou = `Hospital que encaminhou`,
    oncologico = `Oncologico x Não Oncologico`,
    classificacao_tumor = `Maligno x Benigno`,
    classificacao_diagnostico = `Classificação - Diag`,
    diagnostico = Diagnóstico,
    complemento_diagnostico = `Complemento - Diag`,
    localizacao_anatomica_tumor_primario = `Localização Anatômica do tumor primario`,
    localizacao_metastase = `Localização da metastase, se aplicavel`,
    estadiamento = Estadiamento,
    risco = Risco,
    data_primeiros_sinais_sintomas = `Data dos primeiros sinais e sintomas`,
    quais_sinais_sintomas = `Quais sinais e sintomas`,
    data_diagnostico = `Data do diagnostico`,
    data_inicio_tratamento = `Data do inicio do tratamento`,
    qual_inicio_tratamento = `Qual inicio de TTO considerado`,
    quais_esquemas_quimioterapicos = `Quais Esquemas Quimioterapicos (Em ordem de realização)`,
    virgem_tratamento = `Virgem de Tratamento`,
    paciente_teve_recidiva = `Paciente teve recidiva`,
    data_primeira_recidiva = `Data da 1º Recidiva`,
    status_atual_paciente = `Status atual do paciente`,
    status_tratamento = `Status do Tratamento`,
    data_ultima_informacao = `Data da ultima informação`,
    motivo_obito = `Motivo Óbito`,
    hospital_obito = `Hospital onde ocorreu o Óbito`,
    local_obito = `Local do Óbito - Classificação`,
    data_fora_terapia = `Se Fora de Terapia - Data`,
    motivo_alta = `Motivo da Alta`,
    motivo_encaminhamento = `Motivo do encaminhamento`,
    cartao_sus = `Cartão SUS`,
    rgh = RGH,
    cpf = CPF,
    nome_mae = `Nome da Mãe`,
    nome_pai = `Nome do Pai`,
    telefone = Telefone,
    historico_familiar_cancer = `Historico Familiar de Cancer Sim/Não`,
    parentesco = Parentesco,
    observacao = OBS,
    como_paciente_chegou = `Indique como o paciente chegou`,
    convenio_paciente = `Convênio do paciente`,
    rgh_novo = `RGH Novo (após perda de dados do sistema em final de marco / inicio de abril - 2019`,
    uf = `SP X Outros Estados`,
    municipio = `SP Capital X Outros Municipios`,
    regiao_sao_paulo = `Região - Municipio de São Paulo`,
    neoplasia_maligna = `Neoplasia Maligna x Geral`,
    mes_entrada_2 = Mês_Entrada_Cod,
    idade = `Idada na Chegada "anos"`)|>
  dplyr::filter(ano_entrada >= 2001 & ano_entrada <= 2021)|>
  dplyr::mutate(mes_split = stringr::str_split(mes_entrada,
                                               '\\.', simplify = TRUE),
                mes_numero = as.integer(mes_split[,1]),
                mes_abreviacao = stringr::str_trim(mes_split[,2]))|>
  dplyr::mutate(cor_etnia = 
                  dplyr::case_when(
  cor_etnia = cor_etnia == 'NI'~'Não Informado',
                   cor_etnia == 'Não Informardo'~'Não Informado',
                   cor_etnia == 'Pardo'~'Parda',
                   cor_etnia == 'Negro'~'Negra',
                   cor_etnia == 'parda'~ 'Parda',
                   cor_etnia == 'Negro' ~ 'Negra', 
                   cor_etnia == 'Amarelo' ~ 'Amarela',
                   TRUE ~ cor_etnia)) |> #Deletar variável de regiao
  dplyr::mutate(estado =
                  dplyr::case_when(
                    estado = estado == 'Santa Catarina' ~ 'SC - Santa Catarina',
                    TRUE ~ estado))|>
  dplyr::mutate(prefeitura_subprefeitura = 
                  dplyr::case_when(
                    prefeitura_subprefeitura = 
                    prefeitura_subprefeitura == 'Outra Cidade / Municipio de - SP' ~ 'Não se Aplica',
                    prefeitura_subprefeitura == 'Outro Pais' ~ 'Não se Aplica', 
                    prefeitura_subprefeitura == 'Outro pais: Bolivia' ~ 'Não se Aplica',
                    prefeitura_subprefeitura == 'Outro Pais: Paraguai' ~ 'Não se Aplica',
                    prefeitura_subprefeitura == 'Outro pais: Paraguai' ~ 'Não se Aplica',
                    prefeitura_subprefeitura == 'Outro Estado / Outra Cidade' ~ 'Não se Aplica',
                    TRUE ~ prefeitura_subprefeitura
                  ))|>
  dplyr::mutate(oncologico = 
                  dplyr:: case_when(oncologico = 
                                      oncologico == 'retinoblastoma'~ 'Retinoblastoma',
                                    TRUE ~ oncologico))|>
  dplyr::mutate(paciente_teve_recidiva = 
                  dplyr::case_when(paciente_teve_recidiva =
                                     paciente_teve_recidiva == 'não' ~ 'Não',
                                   paciente_teve_recidiva == 'Sim: SNC' ~ 'Sim - SNC',
                                   paciente_teve_recidiva == 'Não se aplica' ~ 'Não se Aplica',
                                   TRUE ~ paciente_teve_recidiva)) |>
  dplyr::mutate(status_atual_paciente = 
                  dplyr::case_when(status_atual_paciente = 
                                     status_atual_paciente == 'óbito'~'Óbito',
                                   status_atual_paciente == 'Perda de segmento'~ 'Perda de Segmento',
                                   TRUE ~ status_atual_paciente))|>
  dplyr::mutate(status_tratamento = 
                  dplyr::case_when(status_tratamento = 
                                     status_tratamento == 'ALTA'~ 'Alta',
                                   status_tratamento == 'óbito' ~ 'Óbito',
                                   status_tratamento == 'perda de segmento' ~ 'Perda de Segmento',
                                   status_tratamento == 'Perda de segmento' ~ 'Perda de Segmento',
                                   status_tratamento == 'Em tratamento' ~ 'Em Tratamento',
                                   TRUE ~ status_tratamento))|>
  #dplyr::filter(paciente_teve_recidiva == 'Abdomen inchado')
  
  #dplyr::glimpse()
  dplyr::distinct(historico_familiar_cancer) |> #verificar variável de Risco, Deletar variavel de 'qual_inicio_tratamento',
                                             #
  print(n=1000)
  dplyr::filter(regiao == 'Centro-Oeste')

  dplyr::distinct(regiao) 
  dplyr::filter(regiao == 'Centro-Oeste')
  dplyr::distinct(regiao)
                  dplyr::case_when()
  print(n=1000)
    
  


