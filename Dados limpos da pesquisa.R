#carregar o tidyverse

library ("tidyverse")
library("stringr")

#inserir a base de dados

dados <- read.csv("C://Users//vitordouglas//Desktop//USP//TCC POS//dados//dados_imoveis.csv")

dados_teste <- dados

dados_teste <- dados_teste %>%
  fill(Valor, .direction = "down") %>%
  fill(Localizacao,.direction = "down" )


# Usar a função separate para dividir as colunas nas características e valores
dados_teste <- dados_teste %>%
  separate(Caracteristicas, into = c("Caracteristica_Area", "Valor_Area"), sep = "(?<=\\D)(?=\\d)", extra = "merge", fill = "right")


# Pivotar os dados para transformar características de área em colunas
dados_transformados <- dados_teste %>%
  pivot_wider(names_from = Caracteristica_Area, values_from = Valor_Area, values_fn = list) %>%
  select(Valor, Localizacao, `Área útil`, Quartos, Banheiros, `Vagas na garagem`, Caracteristicas_Imovel)

###############################################################################################################
#Agora os dados estão organizados em 7 variavéis, mas ainda é necessário que as informações estejam numa única linha, para isto:

# Criando uma Função auxiliar para remover NAs e strings "NULL" antes de concatenar as linhas dos mesmos AP's

remove_na_and_null <- function(x) {
  x <- x[!is.na(x)]            # Remove NAs
  x <- x[x != "NULL"]          # Remove strings "NULL"
  paste(x, collapse = ", ")    # Concatena os valores restantes
}

# Agrupando por Localizacao e combinar os valores

dados_concatenados_2 <- dados_transformados %>%
  group_by(Valor) %>%
  summarize(
    Localizacao = remove_na_and_null(Localizacao),
    `Área útil` = remove_na_and_null(`Área útil`),
    Quartos = remove_na_and_null(Quartos),
    Banheiros = remove_na_and_null(Banheiros),
    `Vagas na garagem` = remove_na_and_null(`Vagas na garagem`),
    Caracteristicas_Imovel = remove_na_and_null(Caracteristicas_Imovel)
  ) %>%
  ungroup()


# Separar as características em uma lista
dados_concatenados_2$Caracteristicas_Imovel <- strsplit(as.character(dados_concatenados_2$Caracteristicas_Imovel), ", ")

# Obter uma lista única de todas as características
todas_caracteristicas <- unique(unlist(dados_concatenados_2$Caracteristicas_Imovel))

# Criar uma matriz binária para cada característica
dados_binarios <- sapply(todas_caracteristicas, function(caracteristica) {
  sapply(dados_concatenados_2$Caracteristicas_Imovel, function(caracteristicas_imovel) {
    ifelse(caracteristica %in% caracteristicas_imovel, 1, 0)
  })
})

# Juntar os dados binários ao dataset original, removendo a coluna original de características
dados_concatenados_3 <- cbind(dados_concatenados_2[, -7], dados_binarios)

dados_concatenados_3 <- dados_concatenados_3 %>%
  select(Localizacao, Valor, everything())

######################################################################################################

# Com a lista de caracteristas em binário pronta, o próximo passo é tratar os elementos das colunas 3 a 6,
# além de transformar os números de string para inteiros


# Função para verificar se a string é um vetor e retornar o primeiro elemento
replace_vector_string_with_first <- function(x) {
  if (is.character(x) && grepl("^c\\(", x)) {
    # Remover "c(" e ")" da string e dividir em elementos
    elements <- strsplit(sub("^c\\(|\\)$", "", x), ", ")[[1]]
    return(elements[1])
  } else {
    return(x)
  }
}

# Função para remover aspas dos elementos
remove_quotes <- function(x) {
  gsub("\"", "", x)
}

# Função para manter apenas os caracteres antes da primeira vírgula
keep_before_comma <- function(x) {
  sapply(strsplit(x, ", "), `[`, 1)
}

# Função para remover 'm²' e converter para inteiro
remove_m2_and_convert <- function(x) {
  as.integer(gsub("m²", "", x))
}

# Função para converter string para inteiro
convert_to_integer <- function(x) {
  as.integer(x)
}

# Aplicar as funções
dados_filtrados <- dados_concatenados_3 %>%
  mutate(across(3:6, ~ sapply(., replace_vector_string_with_first))) %>%
  mutate(across(3:6, ~ remove_quotes(.))) %>%
  mutate(across(3:6, ~ keep_before_comma(.))) %>%
  mutate(`Área útil` = remove_m2_and_convert(`Área útil`)) %>%
  mutate(across(4:24, ~ convert_to_integer(.)))

############################################################################################################
#Iremos agora retirar das duas primeiras colunas os valores de venda, condominio e IPTU

# Carregar as bibliotecas necessárias
library(dplyr)
library(stringr)

# Função para extrair valores monetários
extract_value <- function(string, pattern) {
  value <- str_extract(string, pattern)
  value <- gsub("[^0-9,]", "", value)  # Remove caracteres não numéricos
  value <- as.numeric(gsub(",", ".", value))  # Converte para número
  return(value)
}

# Padrões para encontrar valores de venda, condomínio e IPTU
pattern_venda <- "R\\$ [0-9\\.]+(,[0-9]{2})?"
pattern_condominio <- "CondomínioR\\$ [0-9\\.]+(,[0-9]{2})?"
pattern_iptu <- "IPTUR\\$ [0-9\\.]+(,[0-9]{2})?"

# Aplicar a extração às colunas
dados_filtrados <- dados_filtrados %>%
  mutate(
    Valor_Venda = extract_value(Valor, pattern_venda),
    Valor_Condominio = extract_value(Valor, pattern_condominio),
    Valor_IPTU = extract_value(Valor, pattern_iptu)
  ) %>%
  select(-Valor)  # Remover a coluna antiga

# Exibir o resultado
print(dados_filtrados)

#######################################################################################
# Agora vamos ajustar os endereços

# Carregar as bibliotecas necessárias
library(dplyr)
library(stringr)

# Função para corrigir a separação entre rua e endereço
correct_separation <- function(string) {
  gsub("([a-z])([A-Z])", "\\1, \\2", string)
}

# Função para extrair o primeiro endereço completo até o CEP
extract_first_address <- function(string) {
  # Corrigir a separação entre rua e endereço
  corrected_string <- correct_separation(string)
  # Dividir a string em múltiplos endereços
  addresses <- strsplit(corrected_string, ",\\s*")[[1]]
  # Pegar o primeiro conjunto de endereço até o CEP (assumindo que CEP tem 8 dígitos)
  first_address <- paste(addresses[1:5], collapse = ", ")
  return(first_address)
}

# Aplicar a extração à coluna Localizacao
dados_filtrados <- dados_filtrados %>%
  mutate(Localizacao = sapply(Localizacao, extract_first_address))

#####
# Função para extrair o bairro baseado na localização
extrair_bairro <- function(localizacao) {
  ifelse(grepl("Maraponga", localizacao, ignore.case = TRUE), "Maraponga",
         ifelse(grepl("Mondubim", localizacao, ignore.case = TRUE), "Mondubim",
                ifelse(grepl("Jardim Cearense", localizacao, ignore.case = TRUE), "Jardim Cearense",
                       "Outro")))
}

# Aplicar a função para criar a variável de bairro
dados_filtrados <- dados_filtrados %>%
  mutate(Bairro = sapply(Localizacao, extrair_bairro))%>%
  select(Localizacao, Bairro, Valor_Venda, Valor_Condominio, Valor_IPTU, everything())

######################################
#Limpeza dos variáveis

# Função para remover acentos, substituir espaços por underlines mantendo palavras separadas e converter para letras minúsculas
limpar_nomes <- function(nome) {
  # Remover acentos usando expressões regulares
  nome_limpo <- iconv(nome, "UTF-8", "ASCII//TRANSLIT")  # Remove acentos sem remover a letra
  # Substituir espaços por underlines mantendo palavras separadas
  nome_limpo <- gsub("\\b\\s+\\b", "_", nome_limpo, perl = TRUE)
  nome_limpo <- tolower(nome_limpo)                      # Converter para letras minúsculas
  return(nome_limpo)
}

# Renomear as colunas
dados_filtrados <- dados_filtrados %>%
  rename_all(limpar_nomes)

#########################################################################################################
#Por fim, devemos criar um CSV para exportar dados_filtrados

# Caminho onde o arquivo será salvo
caminho <- "C://Users//vitordouglas//Desktop//USP//TCC POS//dados//"

# Exportar dataframe para um arquivo CSV no caminho especificado
write.csv(dados_filtrados, file = paste0(caminho, "dados_limpos.csv"), row.names = FALSE)
