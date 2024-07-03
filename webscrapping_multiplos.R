library("RSelenium")
library("rvest")
library("tidyverse")

# Lista de páginas para raspar
link_inicial <- "https://olx.com.br/vi/"
caminho_complemento <- "C:/Users/vitordouglas/Documents/anuncios.txt"
lista_complemento <- scan(caminho_complemento, what = "character")  # Certifique-se de especificar "character" para ler como texto
links_completos <- paste0(link_inicial, lista_complemento)

# Configurar o navegador e o cliente remoto RSelenium
remote_driver <- rsDriver(
  browser = "chrome",
  chromever = "latest",  # Usar "latest" para usar a versão mais recente do Chrome
  verbose = FALSE,      # Definir verbose como FALSE para evitar mensagens de progresso
  port = 4545L
)
remDr <- remote_driver[["client"]]

# Listas para armazenar características
todos_valores <- c()
todas_localizacoes <- c()
todas_caracteristicas <- c()
todas_caracteristicas_imovel <- c()

# Iterar sobre todos os links
for (link_teste in links_completos) {
  # Navegar para a página atual
  remDr$navigate(link_teste)
  # Aguardar um tempo antes de raspar os dados (para evitar ser detectado como um bot)
  Sys.sleep(sample(1:10, 1))
  # Ler o HTML da página atual
  codigo <- read_html(remDr$getPageSource()[[1]])
  # Raspagem das características dos imóveis
  caracteristicas <- codigo %>%
    html_nodes(".olx-flex") %>% 
    html_text()
  caracteristicas_imovel <- codigo %>%
    html_nodes(".bDLdHD") %>% 
    html_text()
  localizacao <- codigo %>%
    html_nodes("#location .olx-ai-center") %>% 
    html_text()
  valor <- codigo %>%
    html_nodes("#price-box-container") %>% 
    html_text()
  
  # Encontrar o comprimento máximo das características
  max_length <- max(length(caracteristicas), length(caracteristicas_imovel), length(localizacao), length(valor))
  
  # Preencher vetores com NA para que todos tenham o mesmo comprimento
  caracteristicas <- c(caracteristicas, rep(NA, max_length - length(caracteristicas)))
  caracteristicas_imovel <- c(caracteristicas_imovel, rep(NA, max_length - length(caracteristicas_imovel)))
  localizacao <- c(localizacao, rep(NA, max_length - length(localizacao)))
  valor <- c(valor, rep(NA, max_length - length(valor)))
  
  # Armazenar as características em vetores
  todos_valores <- c(todos_valores, valor)
  todas_localizacoes <- c(todas_localizacoes, localizacao)
  todas_caracteristicas <- c(todas_caracteristicas, caracteristicas)
  todas_caracteristicas_imovel <- c(todas_caracteristicas_imovel, caracteristicas_imovel)
}

# Fechar o navegador
remDr$close()
remote_driver$server$stop()

# Criar o dataframe
df <- data.frame(
  Valor = todos_valores,
  Localizacao = todas_localizacoes,
  Caracteristicas = todas_caracteristicas,
  Caracteristicas_Imovel = todas_caracteristicas_imovel
)

# Exibir o dataframe
print(df)

# Salvar o dataframe em um arquivo CSV
write.csv(df, file = "dados_imoveis.csv", row.names = FALSE)
