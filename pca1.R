
library(dplyr)
library(factoextra)

dados <- readxl::read_excel('/home/cayan/Downloads/bd_Fatorial_PE.xlsx')

glimpse(dados)
dados <- dados[ , -c(1,2,3,4,5) ]


pca <- prcomp(dados, scale. = T)

fviz_eig(pca)

indicador = pca$rotation[ , 1]





### 2 

library(ISLR)
data("Hitters")


# Minimos Quadrados Ordinarios (MQO)

mod1 = lm(Salary ~ ., data = Hitters)
summary(mod1)

# Modelos pelas Componentes

glimpse(Hitters)
dados_hit <- Hitters %>% na.omit() %>% select(-c(14,15,16,19,20))

pca_hit <- prcomp(dados_hit)
fviz_eig(pca_hit)

dados_hit_mod <- data.frame(Salario = Hitters %>% na.omit() %>% .$Salary ,
                            PCA1    = pca_hit$x[,1])

mod_pca <- lm(Salario ~ PCA1 , data = dados_hit_mod)
summary(mod_pca)
