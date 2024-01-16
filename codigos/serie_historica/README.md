# Descrição e Documentação das Variáveis da serie_historica.csv da TUCCA
## Introdução
Este repositório contém informações da série histórica de 2001 a 2020 da TUCCA. O conjunto de dados possui 4212 observações e cerca de 67 variáveis. A contagem exata pode variar devido à exclusão de algumas variáveis durante o processo de padronização e carregamento no RedCap.

O processo de carregamento será realizado em colaboração com a equipe médica e administrativa da TUCCA. Como o arquivo não possui padronização de preenchimento e estava em formato .xlsx, são necessários códigos de limpeza (ETL) e a criação do instrumento de coleta de dados na plataforma RedCap. O código correspondente pode ser acessado [aqui](https://github.com/gustavoalcantara/tucca/blob/main/codigos/serie_historica/serie_historica.R).

Descrição das Variáveis
id: Identificador único para cada registro, variando de 1 a 4212, aparentemente gerado a partir da tabela do Excel.\
ano: Ano do registro, pode ser considerada uma variável menos relevante e passível de exclusão.\
ano_entrada: Ano de entrada na TUCCA.\
mes_entrada: Mês de entrada na TUCCA.\
data_primeira_consulta: Data da primeira consulta na TUCCA.\
rgh: Registro Geral Hospitalar. É um identificador único e padronizado. É a partir dele que ocorrerá a integração dos demais bancos de dados. Esse identificador veio do sistema MV que o hospital cede ao TUCCA.\
nome: Nome do paciente.\
sexo: Gênero do paciente.\
data_nascimento: Data de nascimento do paciente.\
cor_etnia: Cor ou etnia do paciente.\
endereco: Endereço do paciente.\
regiao: Região geográfica do país que o paciente reside.\
estado: Estado de residência do paciente.\
cidade: Cidade de residência do paciente.\
prefeitura_subprefeitura: Prefeitura ou subprefeitura relacionada ao endereço do paciente. Essa variável encontra-se atrelada aos residentes da cidade de São Paulo.\
hospital_encaminhou: Hospital que encaminhou o paciente.\
oncologico: Indicação se o paciente é ou não oncológico e se está sob invesitagação.\
classificacao_tumor: Classificação do tipo de tumor.\
classificacao_diagnostico: Classificação do diagnóstico.\
diagnostico: Diagnóstico do paciente.\
complemento_diagnostico: Informações adicionais sobre o diagnóstico.\
localizacao_anatomica_tumor_primario: Localização anatômica do tumor primário do paciente.\
localizacao_metastase: Localização das metástases do paciente.\
estadiamento: Estadiamento do câncer do paciente.\
risco: Avaliação de risco do paciente.\
data_primeiros_sinais_sintomas: Data dos primeiros sinais ou sintomas.\
quais_sinais_sintomas: Descrição dos sinais e sintomas apresentados.\
data_diagnostico: Data do diagnóstico.\
data_inicio_tratamento: Data de início do tratamento.
qual_inicio_tratamento: Qual foi o início do tratamento. Variável que pode ser excluída por apresentar somente NA.\
quais_esquemas_quimioterapicos: Quais esquemas de quimioterapia foram utilizados.\
virgem_tratamento: Indicação se o paciente é virgem de tratamento.\
paciente_teve_recidiva: Indicação se o paciente teve recidiva.\
data_primeira_recidiva: Data da primeira recidiva.\
status_atual_paciente: Status atual do paciente.\
status_tratamento: Status do tratamento.\
data_ultima_informacao: Data da última informação.\
motivo_obito: Motivo do óbito.\
hospital_obito: Hospital onde ocorreu o óbito.\
local_obito: Local onde ocorreu o óbito.\
data_fora_terapia: Data em que o paciente saiu da terapia.\
motivo_alta: Motivo da alta.\
motivo_encaminhamento: Motivo do encaminhamento.\
cartao_sus: Número do Cartão Nacional de Saúde (SUS).\
RG: Número do Registro Geral.\
cpf: Número do Cadastro de Pessoas Físicas.\
nome_mae: Nome da mãe do paciente.\
nome_pai: Nome do pai do paciente.\
telefone: Número de telefone do paciente.\
historico_familiar_cancer: Histórico familiar de câncer.\
parentesco: Grau de parentesco no histórico familiar.\
observacao: Observações adicionais.\
como_paciente_chegou: Como o paciente chegou ao hospital.\
convenio_paciente: Convênio médico do paciente.\
rgh_novo: Novo Registro Geral Hospitalar. \
uf: Unidade Federativa. \
municipio: Município.\
regiao_sao_paulo: Região específica de São Paulo.\
neoplasia_maligna: Indicação de neoplasia maligna.\
mes_entrada_2: Segunda variável para o mês de entrada.\
idade: Idade do paciente.\
mes_split: Mês de entrada, variável separada.\
mes_numero: Número correspondente ao mês de entrada.\
mes_abreviacao: Abreviação do mês de entrada.\
