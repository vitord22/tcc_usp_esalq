#%% Nosso objetivo é calcular o valor do apartamento estudado, a partir do banco de dados coletados
# Comeaçamos instalando os pacotes necessarios

#!pip install pandas
#!pip install numpy
#!pip install -U seaborn
#!pip install matplotlib
#!pip install plotly
#!pip install scipy
#!pip install statsmodels
#!pip install scikit-learn
#!pip install pingouin
#!pip install statstests

#%%
#importacaoo dos pacotes, usaremos os mesmos que utilizados nas aulas de regressao multipla, do professor Favero, e alguns outros mais
#que nos foram necessarios

import pandas as pd # manipulaÃ§Ã£o de dados em formato de dataframe
import numpy as np # operaÃ§Ãµes matemÃ¡ticas
import seaborn as sns # visualizaÃ§Ã£o grÃ¡fica
import matplotlib.pyplot as plt # visualizaÃ§Ã£o grÃ¡fica
import plotly.graph_objects as go # grÃ¡ficos 3D
from scipy.stats import pearsonr # correlaÃ§Ãµes de Pearson
import statsmodels.api as sm # estimaÃ§Ã£o de modelos
from statsmodels.iolib.summary2 import summary_col # comparaÃ§Ã£o entre modelos
from sklearn.preprocessing import LabelEncoder # transformaÃ§Ã£o de dados
import pingouin as pg # outro modo para obtenÃ§Ã£o de matrizes de correlaÃ§Ãµes
from statstests.process import stepwise # procedimento Stepwise
from statstests.tests import shapiro_francia # teste de Shapiro-Francia
from scipy.stats import boxcox # transformaÃ§Ã£o de Box-Cox
from scipy.stats import norm # para plotagem da curva normal
from scipy import stats # utilizado na definiÃ§Ã£o da funÃ§Ã£o 'breusch_pagan_test'
import scipy.stats as stats # Para o Gladder
from scipy.stats import shapiro, anderson, normaltest
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.metrics import r2_score, mean_squared_error

#%% 
#Precisamos trazer a base de dados em CSV que tratamos no RStudio

df_apartamentos = pd.read_csv('C:/Users/vitordouglas/Desktop/dados_limpos.csv', delimiter=',')
df_original = df_apartamentos.copy() #sÃ³ uma garantia extra, caso algo dÃª errado
df_apartamentos = df_apartamentos.dropna() #se fez necessÃ¡rio remover os dados que continham NA 

# CaracterÃ­sticas das variÃ¡veis do dataset
df_apartamentos.info()

# EstatÃ­sticas univariadas: contagem, mÃ©dia, min, max, quartis, etc
df_apartamentos.describe()

#%%Tabela de frequencia dos bairros

df_apartamentos['bairro'].value_counts().sort_index()

#%%# Procedimento n-1 dummies para os bairros que se encontram os apartamentos
    
# DummizaÃ§Ã£o da variÃ¡vel 'bairro'

df_apartamentos_dummies = pd.get_dummies(df_apartamentos, columns=['bairro'],
                                      dtype=int,
                                      drop_first=True)

df_apartamentos_dummies

# O bairro que serve de categoria de referencia Ã© o jardim cearense, o primeiro em ordem alfabetica
# Isso me garante usar os bairros, que de inicio eram variaveis qualitativas, sem ferir a analise

#%% Matriz de correlação
correlation_matrix = df_apartamentos_dummies.iloc[:, 1:31].corr()

# Criar o gráfico
plt.figure(figsize=(20, 15))
heatmap = sns.heatmap(
    correlation_matrix,
    annot=True,
    fmt=".2f",
    cmap=plt.cm.viridis_r,
    vmin=-1,
    vmax=1,
    annot_kws={'size': 8}
)
# Ajustar rótulos dos eixos
heatmap.set_xticklabels(heatmap.get_xticklabels(), fontsize=9, rotation=45, ha='right')
heatmap.set_yticklabels(heatmap.get_yticklabels(), fontsize=9, rotation=0)
# Estilizar barra de cores
cbar = heatmap.collections[0].colorbar
cbar.ax.tick_params(labelsize=10)
# Título
plt.title("Matriz de Correlação entre Variáveis", fontsize=14)
plt.tight_layout()
# Mostrar
plt.show()

#%% Remover as 3 variaveis que estão sem dados 

df_apartamentos_dummies = df_apartamentos_dummies.drop(
    columns=['portao_eletronico', 'area_murada', 'quarto_de_servico']
)

#%% Matriz de correlação ajustada

correlation_matrix2 = df_apartamentos_dummies.iloc[:, 1:28].corr()

# Criar o gráfico
plt.figure(figsize=(20, 15))
heatmap = sns.heatmap(
    correlation_matrix2,
    annot=True,
    fmt=".2f",
    cmap=plt.cm.viridis_r,
    vmin=-1,
    vmax=1,
    annot_kws={'size': 8}
)
# Ajustar rótulos dos eixos
heatmap.set_xticklabels(heatmap.get_xticklabels(), fontsize=9, rotation=45, ha='right')
heatmap.set_yticklabels(heatmap.get_yticklabels(), fontsize=9, rotation=0)
# Estilizar barra de cores
cbar = heatmap.collections[0].colorbar
cbar.ax.tick_params(labelsize=10)
# Título
plt.title("Matriz de Correlação entre Variáveis 2", fontsize=14)
plt.tight_layout()
# Mostrar
plt.show()

#%% Vejamos novamente como ficou nosso dataframe

# CaracterÃ­sticas das variÃ¡veis do dataset
df_apartamentos_dummies.info()
# EstatÃ­sticas univariadas: contagem, mÃ©dia, min, max, quartis, etc
df_apartamentos_dummies.describe()

#%% Estimação do modelo de regressão múltipla com n-1 dummies

# Definição da fórmula utilizada no modelo
lista_colunas = list(df_apartamentos_dummies.drop(columns=['localizacao',
                                                         'valor_venda']).columns)
formula_dummies_modelo = ' + '.join(lista_colunas)
formula_dummies_modelo = "valor_venda ~ " + formula_dummies_modelo

# Estimação
modelo_valor_venda = sm.OLS.from_formula(formula_dummies_modelo,
                                        df_apartamentos_dummies).fit()

# Parâmetros do modelo
modelo_valor_venda.summary()

#Foi observado que temos pelo menos 1 beta significativo no modelo, porém, muitas variaveis não passam no teste T
#Provavel que seja problema de multicolinearidade

#%%# Carregamento da função 'stepwise' do pacote 'statstests.process'
# Autores do pacote: Luiz Paulo Fávero e Helder Prado Santos
# https://stats-tests.github.io/statstests/. Gratidão Professores! 

from statstests.process import stepwise

# Estimação do modelo por meio do procedimento Stepwise
modelo_step_apartamento = stepwise(modelo_valor_venda, pvalue_limit=0.05)

modelo_step_apartamento.summary()

#%% Diagnóstico de multicolinearidade (Variance Inflation Factor
#e Tolerance)

from statsmodels.stats.outliers_influence import variance_inflation_factor

# Escolha das variaveis do Step Wise
var_selec1 = [
    'banheiros', 'vagas_na_garagem', 'churrasqueira',
    'piscina', 'elevador', 'permitido_animais'
]

# Calculando os valores de VIF
X1 = sm.add_constant(df_apartamentos_dummies[var_selec1])
VIF = pd.DataFrame()
VIF["Variável"] = X1.columns[1:]
VIF["VIF"] = [variance_inflation_factor(X1.values, i+1)
              for i in range(X1.shape[1]-1)]

# Calculando as Tolerâncias
VIF["Tolerância"] = 1 / VIF["VIF"]
VIF

#%% Teste de verificação da aderência dos resíduos à normalidade

# Teste de Shapiro-Francia (n >= 30)
# Carregamento da função 'shapiro_francia' do pacote 'statstests.tests'
# Autores do pacote: Luiz Paulo Fávero e Helder Prado Santos
# https://stats-tests.github.io/statstests/. Gratidão novamente, professores!

from statstests.tests import shapiro_francia

# Teste de Shapiro-Francia: interpretação
teste_sf = shapiro_francia(modelo_step_apartamento.resid) #criação do objeto 'teste_sf'
teste_sf = teste_sf.items() #retorna o grupo de pares de valores-chave no dicionário
method, statistics_W, statistics_z, p = teste_sf #definição dos elementos da lista (tupla)
print('Statistics W=%.5f, p-value=%.6f' % (statistics_W[1], p[1]))
alpha = 0.05 #nível de significância
if p[1] > alpha:
	print('Não se rejeita H0 - Distribuição aderente à normalidade')
else:
	print('Rejeita-se H0 - Distribuição não aderente à normalidade')
    
# O TESTE REJEITOU H0

#%% Histograma dos resíduos do 'modelo_step_apartamento' com curva normal
#teórica para comparação das distribuições
# Kernel density estimation (KDE) - forma não-paramétrica para estimação da
#função densidade de probabilidade de determinada variável

from scipy.stats import norm

# Cálculo da curva normal
residuos = modelo_step_apartamento.resid
(mu, sigma) = norm.fit(residuos)

# Valores do eixo X
x = np.linspace(residuos.min(), residuos.max(), 100)
p = norm.pdf(x, mu, sigma)

# Gráfico
plt.figure(figsize=(12, 7))
sns.histplot(residuos, bins=30, kde=False, stat="density", color='salmon', alpha=0.6, label='Resíduos')
plt.plot(x, p, 'k--', linewidth=2.5, label='Distribuição Normal Teórica')

# Estilo e elementos gráficos
plt.title('Distribuição dos Resíduos do modelo_step_apartamento', fontsize=18)
plt.xlabel('Resíduos do Modelo Stepwise Linear', fontsize=14)
plt.ylabel('Densidade', fontsize=14)
plt.xticks(fontsize=12)
plt.yticks(fontsize=12)
plt.grid(True, linestyle='--', alpha=0.4)
plt.legend(fontsize=12)
plt.tight_layout()
plt.show()

#%%Função para o teste de Breusch-Pagan para a elaboração de diagnóstico
#de heterocedasticidade

# Criação da função 'breusch_pagan_test'

def breusch_pagan_test(modelo):

    df = pd.DataFrame({'yhat':modelo.fittedvalues,
                       'resid':modelo.resid})
   
    df['up'] = (np.square(df.resid))/np.sum(((np.square(df.resid))/df.shape[0]))
   
    modelo_aux = sm.OLS.from_formula('up ~ yhat', df).fit()
   
    anova_table = sm.stats.anova_lm(modelo_aux, typ=2)
   
    anova_table['sum_sq'] = anova_table['sum_sq']/2
    
    chisq = anova_table['sum_sq'].iloc[0]
   
    p_value = stats.chi2.pdf(chisq, 1)*2
    
    print(f"chisq: {chisq}")
    
    print(f"p-value: {p_value}")
    
    return chisq, p_value


#%% Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
#no 'modelo_step_apartamento'

breusch_pagan_test(modelo_step_apartamento)

# Interpretação
teste_bp = breusch_pagan_test(modelo_step_apartamento) #criação do objeto 'teste_bp'
chisq, p = teste_bp #definição dos elementos contidos no objeto 'teste_bp'
alpha = 0.05 #nível de significância
if p > alpha:
    print('Não se rejeita H0 - Ausência de Heterocedasticidade')
else:
	print('Rejeita-se H0 - Existência de Heterocedasticidade')
    
#Rejeita-se H0 - Existência de Heterocedasticidade

#%% Agora é rezar para o modelo Box-Cox normalizar e resolver a heterocedasticidade kkk

#Criando um novo DataFrame para não dar conflito com o anterior
df_box_cox = df_apartamentos_dummies.copy()

# Para o cálculo do lambda de Box-Cox
from scipy.stats import boxcox

# 'yast' é uma variável que traz os valores transformados (Y*)
# 'lmbda' é o lambda de Box-Cox
yast, lmbda = boxcox(df_box_cox['valor_venda'])

print("Lambda: ",lmbda)

#%% Inserindo o lambda de Box-Cox no dataset para a estimação de um
#novo modelo

df_box_cox['bc_valor_venda'] = yast
df_box_cox

#%% Estimando um novo modelo com todas as variáveis e a variável
#dependente transformada
    
lista_colunas_bc = list(df_box_cox.drop(columns=['localizacao',
                                                         'valor_venda', 'bc_valor_venda',]).columns)
formula_bc_modelo = ' + '.join(lista_colunas_bc)
formula_bc_modelo = "bc_valor_venda ~ " + formula_bc_modelo

# Estimação
modelo_bc_apartamento = sm.OLS.from_formula(formula_bc_modelo,
                                        df_box_cox).fit()

# Parâmetros do modelo
modelo_bc_apartamento.summary()

#%% Estimação do modelo por meio do procedimento Stepwise no modelo Box-Cox

modelo_step_bc_apartamento = stepwise(modelo_bc_apartamento, pvalue_limit=0.05)

modelo_step_bc_apartamento.summary()

#%% Diagnóstico de multicolinearidade (Variance Inflation Factor
#e Tolerance)

# Escolha das variaveis do Step Wise
var_selec2 = [
    'quartos', 'banheiros', 'vagas_na_garagem', 'academia' , 'churrasqueira',
    'piscina', 'elevador', 'permitido_animais', 'salao_de_festas'
]

# Calculando os valores de VIF
X1 = sm.add_constant(df_box_cox[var_selec2])
VIF = pd.DataFrame()
VIF["Variável"] = X1.columns[1:]
VIF["VIF"] = [variance_inflation_factor(X1.values, i+1)
              for i in range(X1.shape[1]-1)]

# Calculando as Tolerâncias
VIF["Tolerância"] = 1 / VIF["VIF"]
VIF
#%% Teste de verificação da aderência à normalidade dos resíduos do novo
#'modelo_step_bc_apartamento'

# Teste de Shapiro-Francia: interpretação
teste_sf = shapiro_francia(modelo_step_bc_apartamento.resid) #criação do objeto 'teste_sf'
teste_sf = teste_sf.items() #retorna o grupo de pares de valores-chave no dicionário
method, statistics_W, statistics_z, p = teste_sf #definição dos elementos da lista (tupla)
print('Statistics W=%.5f, p-value=%.6f' % (statistics_W[1], p[1]))
alpha = 0.05 #nível de significância
if p[1] > alpha:
	print('Não se rejeita H0 - Distribuição aderente à normalidade')
else:
	print('Rejeita-se H0 - Distribuição não aderente à normalidade')

# Rejeita-se H0 - Distribuição não aderente à normalidade
#%%
# Calcula os parâmetros da normal com base nos resíduos
residuos = modelo_step_bc_apartamento.resid
mu, sigma = norm.fit(residuos)

# Valores para a curva normal teórica
x = np.linspace(residuos.min(), residuos.max(), 200)
p = norm.pdf(x, mu, sigma)

# Gráfico
plt.figure(figsize=(12, 7))

# Histograma dos resíduos
sns.histplot(residuos, bins=30, kde=False, stat="density",
             color='limegreen', alpha=0.5, label='Resíduos')

# Curva da normal teórica ajustada
plt.plot(x, p, 'k--', linewidth=2, label='Distribuição Normal Teórica')

# Título e eixos
plt.title('Distribuição dos Resíduos do modelo_step_bc_apartamento', fontsize=18)
plt.xlabel('Resíduos do Modelo Stepwise com Box-Cox', fontsize=14)
plt.ylabel('Densidade', fontsize=14)
plt.xticks(fontsize=12)
plt.yticks(fontsize=12)

# Legenda e grade
plt.legend(fontsize=12)
plt.grid(True, linestyle='--', alpha=0.4)
plt.tight_layout()
plt.show()

#%% Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
#no 'modelo_step_apartamento'

breusch_pagan_test(modelo_step_bc_apartamento)

# Interpretação
teste_bp = breusch_pagan_test(modelo_step_bc_apartamento) #criação do objeto 'teste_bp'
chisq, p = teste_bp #definição dos elementos contidos no objeto 'teste_bp'
alpha = 0.05 #nível de significância
if p > alpha:
    print('Não se rejeita H0 - Ausência de Heterocedasticidade')
else:
	print('Rejeita-se H0 - Existência de Heterocedasticidade')
    
#Rejeita-se H0 - Existência de Heterocedasticidade

#%% O box cox não funcionou, o motivo foi que o RSELENIUM capturou apartamentos que estavam
# para repasse, ou seja, seus valores no anuncio não representavam seu valor real, sendo necessário o descarte
# dos apartamentos abaixo de 150mil reais para melhor estimação do modelo; alem dos outliers

df_final = df_apartamentos_dummies[df_apartamentos_dummies['valor_venda'] >= 150000]

df_final.info()
df_final.describe()

#%% Remover outliers dos resíduos usando IQR
residuos = modelo_step_bc_apartamento.resid
Q1, Q3 = np.percentile(residuos, [25, 75])
IQR = Q3 - Q1
limite_inferior = Q1 - 1.5 * IQR
limite_superior = Q3 + 1.5 * IQR
df_final = df_final[(residuos >= limite_inferior) & (residuos <= limite_superior)]

df_final.info()
df_final.describe()

#%% Agora é rezar novamente modelo Box-Cox normalizar e resolver a heterocedasticidade kkk

# Para o cálculo do lambda de Box-Cox
from scipy.stats import boxcox

# 'yast' é uma variável que traz os valores transformados (Y*)
# 'lmbda' é o lambda de Box-Cox
yast, lmbda = boxcox(df_final['valor_venda'])

print("Lambda: ",lmbda)

#%% Inserindo o lambda de Box-Cox no dataset para a estimação de um
#novo modelo

df_final['bc_valor_venda'] = yast
df_final

#%% Estimando um novo modelo com todas as variáveis e a variável
#dependente transformada
    
lista_colunas_bc2 = list(df_final.drop(columns=['localizacao',
                                                         'valor_venda', 'bc_valor_venda']).columns)
formula_bc_modelo2 = ' + '.join(lista_colunas_bc2)
formula_bc_modelo2 = "bc_valor_venda ~ " + formula_bc_modelo2

# Estimação
modelo_bc_apartamento2 = sm.OLS.from_formula(formula_bc_modelo2,
                                        df_final).fit()

# Parâmetros do modelo
modelo_bc_apartamento2.summary()

#%% Estimação do modelo por meio do procedimento Stepwise no modelo Box-Cox

modelo_step_bc_apartamento2 = stepwise(modelo_bc_apartamento2, pvalue_limit=0.05)

modelo_step_bc_apartamento2.summary()

#%% Diagnóstico de multicolinearidade (Variance Inflation Factor
#e Tolerance)

# Escolha das variaveis do Step Wise
var_selec3 = [
    'quartos', 'banheiros', 'vagas_na_garagem' , 'churrasqueira',
    'piscina', 'elevador', 'permitido_animais', 'bairro_Maraponga'
]
# Calculando os valores de VIF
X1 = sm.add_constant(df_final[var_selec3])
VIF = pd.DataFrame()
VIF["Variável"] = X1.columns[1:]
VIF["VIF"] = [variance_inflation_factor(X1.values, i+1)
              for i in range(X1.shape[1]-1)]

# Calculando as Tolerâncias
VIF["Tolerância"] = 1 / VIF["VIF"]
VIF

#%% Teste de verificação da aderência à normalidade dos resíduos do novo
#'modelo_step_bc_apartamento'

# Teste de Shapiro-Francia: interpretação
teste_sf = shapiro_francia(modelo_step_bc_apartamento2.resid) #criação do objeto 'teste_sf'
teste_sf = teste_sf.items() #retorna o grupo de pares de valores-chave no dicionário
method, statistics_W, statistics_z, p = teste_sf #definição dos elementos da lista (tupla)
print('Statistics W=%.5f, p-value=%.6f' % (statistics_W[1], p[1]))
alpha = 0.05 #nível de significância
if p[1] > alpha:
	print('Não se rejeita H0 - Distribuição aderente à normalidade')
else:
	print('Rejeita-se H0 - Distribuição não aderente à normalidade')

#Não se rejeita H0 - Distribuição aderente à normalidade

#%%
# Calcula os parâmetros da normal com base nos resíduos
residuos = modelo_step_bc_apartamento2.resid
mu, sigma = norm.fit(residuos)

# Valores para a curva normal teórica
x = np.linspace(residuos.min(), residuos.max(), 200)
p = norm.pdf(x, mu, sigma)

# Gráfico
plt.figure(figsize=(12, 7))

# Histograma dos resíduos
sns.histplot(residuos, bins=30, kde=False, stat="density",
             color='limegreen', alpha=0.5, label='Resíduos')

# Curva da normal teórica ajustada
plt.plot(x, p, 'k--', linewidth=2, label='Distribuição Normal Teórica')

# Título e eixos
plt.title('Distribuição dos Resíduos do Modelo com Box-Cox', fontsize=18)
plt.xlabel('Resíduos do Modelo Stepwise com Box-Cox', fontsize=14)
plt.ylabel('Densidade', fontsize=14)
plt.xticks(fontsize=12)
plt.yticks(fontsize=12)

# Legenda e grade
plt.legend(fontsize=12)
plt.grid(True, linestyle='--', alpha=0.4)
plt.tight_layout()
plt.show()

#%% Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
#no 'modelo_step_apartamento'

breusch_pagan_test(modelo_step_bc_apartamento2)

# Interpretação
teste_bp = breusch_pagan_test(modelo_step_bc_apartamento2) #criação do objeto 'teste_bp'
chisq, p = teste_bp #definição dos elementos contidos no objeto 'teste_bp'
alpha = 0.05 #nível de significância
if p > alpha:
    print('Não se rejeita H0 - Ausência de Heterocedasticidade')
else:
	print('Rejeita-se H0 - Existência de Heterocedasticidade')
    
#Não se rejeita H0 - Ausência de Heterocedasticidade   

#########################################################################################################################
#%%####################################################################################################################

#%% Vamos agora fazer uma Random Forest para comparar os valores

# Primeiramente, vamos separar as variaveis do valor do imovel

# Criar um novo dataframe apenas para trabalhar com o log

df_log = df_final.copy()
df_log['log_valor_venda'] = np.log(df_log['valor_venda'])

X = df_log.drop(columns=[
    'valor_venda',
    'log_valor_venda',
    'bc_valor_venda',
    'inv_valor_venda',
    'fitted_step',
    'residuos_step',
    'localizacao'
], errors='ignore')

y = df_log['log_valor_venda']

# Dividir treino/teste
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# %% 3. Configurar o GridSearchCV
param_grid = {
    'n_estimators': [100, 300, 500],
    'max_depth': [5, 10, 20],
    'min_samples_split': [2, 5, 10],
    'min_samples_leaf': [1, 2, 4],
    'max_features': ['sqrt', 'log2']
}

# Criar o modelo Random Forest
rf = RandomForestRegressor(random_state=42)

# Criar o GridSearch
grid_search = GridSearchCV(
    estimator=rf,
    param_grid=param_grid,
    cv=5,  # cross-validation de 5 folds
    scoring='neg_root_mean_squared_error',  # para otimizar RMSE
    n_jobs=-1,  # usa todos os núcleos do seu computador
    verbose=2
)

# %% 4. Rodar o GridSearchCV
grid_search.fit(X_train, y_train)

# Melhor modelo encontrado
melhor_modelo = grid_search.best_estimator_
print("Melhores parâmetros:", grid_search.best_params_)

# %% 5. Avaliar o melhor modelo
y_pred_log = melhor_modelo.predict(X_test)

# Reverter o log
y_test_real = np.exp(y_test)
y_pred_real = np.exp(y_pred_log)

# Avaliação
r2_melhor = r2_score(y_test_real, y_pred_real)
rmse_melhor = mean_squared_error(y_test_real, y_pred_real, squared=False)

print(f"R² (real): {r2_melhor:.3f}")
print(f"RMSE (real): R$ {rmse_melhor:,.2f}")

###################################################################################################################

#%% Random Forest com GridSearch e Previsão Final

df_log = df_final.copy()
df_log['log_valor_venda'] = np.log(df_log['valor_venda'])

# Criar df_log e variável log_valor_venda corretamente
df_log = df_final.copy()
df_log['log_valor_venda'] = np.log(df_log['valor_venda'])

# Separar X e y usando df_log corretamente
X = df_log.drop(columns=['valor_venda', 'log_valor_venda', 'bc_valor_venda', 'localizacao'], errors='ignore')
y = df_log['log_valor_venda']

# Dividir treino e teste
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Configuração do GridSearchCV
param_grid = {
    'n_estimators': [100, 300, 500],
    'max_depth': [5, 10, 20],
    'min_samples_split': [2, 5, 10],
    'min_samples_leaf': [1, 2, 4],
    'max_features': ['sqrt', 'log2']
}

rf = RandomForestRegressor(random_state=42)
grid_search = GridSearchCV(estimator=rf, param_grid=param_grid, cv=5, scoring='neg_root_mean_squared_error', n_jobs=-1, verbose=2)
grid_search.fit(X_train, y_train)

melhor_modelo_rf = grid_search.best_estimator_

# Avaliação do melhor modelo
y_pred_log = melhor_modelo_rf.predict(X_test)
y_pred_real = np.exp(y_pred_log)
y_test_real = np.exp(y_test)

r2_rf = r2_score(y_test_real, y_pred_real)
rmse_rf = mean_squared_error(y_test_real, y_pred_real, squared=False)

print(f"R² (Random Forest): {r2_rf:.3f}")
print(f"RMSE (Random Forest): R$ {rmse_rf:,.2f}")

###############################################################################################################################
###############################################################################################################################
#%% Previsão do valor do apartamento

# Características do apartamento
caracteristicas_apartamento = {
    'valor_condominio': 511,
    'valor_iptu': 1004,
    'area_util': 66,
    'quartos': 3,
    'banheiros': 2,
    'vagas_na_garagem': 1,
    'academia': 1,
    'area_de_servico':1,
    'churrasqueira': 1,
    'piscina': 1,
    'condominio_fechado': 1,
    'elevador': 1,
    'permitido_animais': 1,
    'salao_de_festas': 1,
    'portaria': 1,
    'seguranca_24h': 0,
    'mobiliado': 0,
    'armarios_na_cozinha': 0,
    'varanda': 1,
    'ar_condicionado': 0,
    'armarios_no_quarto': 0,
    'bairro_Maraponga': 1,
    'bairro_Mondubim': 0,
    'bairro_Outro': 0
}

X_novo = pd.DataFrame([caracteristicas_apartamento], columns=X.columns)

# Previsão
pred_log_novo = melhor_modelo_rf.predict(X_novo)[0]
valor_estimado_rf = np.exp(pred_log_novo)

print(f"\nValor estimado do apartamento (Random Forest): R$ {valor_estimado_rf:,.2f}")
print(f"R² (Random Forest): {r2_rf:.3f}")
print(f"RMSE (Random Forest): R$ {rmse_rf:,.2f}")

#%% Estimando o valor do meu apartamento com o modelo de regressao multipla (modelo_step_bc_apartamento2)

# Definir as características do apartamento (ajuste conforme suas informações reais)

from scipy.special import inv_boxcox 

# Criar DataFrame com as características do apartamento
meu_apartamento_df = pd.DataFrame([caracteristicas_apartamento])

# Fazer previsão com intervalo de confiança usando o modelo final
pred_bc = modelo_step_bc_apartamento2.get_prediction(meu_apartamento_df)
intervalo_confianca = pred_bc.conf_int(alpha=0.05)  # 95% de confiança

# Valor previsto (Box-Cox invertido)
valor_predito_final = inv_boxcox(pred_bc.predicted_mean[0], lmbda)

# Inverter Box-Cox no intervalo de confiança
lim_inf_original = inv_boxcox(intervalo_confianca[0, 0], lmbda)
lim_sup_original = inv_boxcox(intervalo_confianca[0, 1], lmbda)

# Imprimir resultado final com intervalo de confiança
print(f"Valor estimado do apartamento: R$ {valor_predito_final:,.2f}")
print(f"Intervalo de confiança de 95%: R$ {lim_inf_original:,.2f} a R$ {lim_sup_original:,.2f}")

#%% Cálculo Final do Valor Estimado - Média Ponderada com base no R² dos modelos

# Valores estimados pelos modelos
valor_regressao = valor_predito_final   # Valor previsto pelo modelo de Regressão Linear (com Box-Cox invertido)
valor_random_forest = valor_estimado_rf # Valor previsto pelo modelo Random Forest

# R² ajustado da Regressão Linear e R² da Random Forest
r2_regressao = 0.627
r2_random_forest = 0.697

# Cálculo dos pesos proporcionais
peso_regressao = r2_regressao / (r2_regressao + r2_random_forest)
peso_random_forest = r2_random_forest / (r2_regressao + r2_random_forest)

# Exibindo os pesos
print(f"Peso da Regressão Linear: {peso_regressao:.4f}")
print(f"Peso da Random Forest: {peso_random_forest:.4f}")

# Cálculo da média ponderada
valor_final_estimado = (valor_regressao * peso_regressao) + (valor_random_forest * peso_random_forest)

# Exibindo o valor final estimado
print(f"\nValor Final Estimado do Apartamento (Média Ponderada): R$ {valor_final_estimado:,.2f}")
