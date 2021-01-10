if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
if(!require(DataCombine)){
  install.packages("DataCombine")
  library(DataCombine)
}

if(!require(Rmisc)){
  install.packages("Rmisc")
  library(Rmisc)
}
if(!require(MLmetrics)){
  install.packages("MLmetrics")
  library(MLmetrics)
}

if(!require(agricolae)){
  install.packages("agricolae")
  library(agricolae)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

setwd("C:/Users/franj/Desktop/Repositorios MÁSTER UOC/Est. Avanzada/A4_ANOVA/Data")

# 1. Lectura del fichero y preparación de los datos
fifa_filename<-"../Data/Fifa.csv"
fifa <- read.csv(file=fifa_filename, header=TRUE, sep=",", 
         na.strings=c(""," "), stringsAsFactors=TRUE, encoding = 'UTF-8')
head(fifa)


str(fifa)


fifa$Name<-as.character(fifa$Name)
fifa$National_Kit<-as.integer(fifa$National_Kit)
fifa$Club_Kit<-as.integer(fifa$Club_Kit)
fifa$Contract_Expiry<-as.integer(fifa$Contract_Expiry)

## 1.1 Preparación de los datos
head(fifa$Weight)
fifa$Weight <- gsub("kg","",fifa$Weight)
fifa$Weight<-as.numeric(fifa$Weight)
head(fifa$Weight)

head(fifa$Height)
fifa$Height <- gsub("cm","",fifa$Height)
fifa$Height<-as.numeric(fifa$Height)
head(fifa$Height)
head(fifa$Rating)
## 1.2 Clasificación de jugadores
min(fifa$Rating)
max(fifa$Rating)

get_clasificacion <- function(x){
  if (x >= 90 & x <= 99)
    return("Excelente")
  else if (x >= 80 & x<=89)
    return ("Muy bueno")
  else if (x >= 70 & x<=79)
    return("Bueno")
  else if (x >= 50 & x<=69)
    return("Regular")
  else if (x >= 40 & x <= 49)
    return("Malo")
  else if (x >= 0 & x <= 39)
    return ("Muy malo")
}

fifa$clasificacion <- lapply(fifa$Rating, get_clasificacion)
fifa$clasificacion <- unlist(fifa$clasificacion)
fifa$clasificacion <- as.factor(fifa$clasificacion)

head(fifa$Rating)
head(fifa$clasificacion)
tail(fifa$Rating)
tail(fifa$clasificacion)

# 2. Estadística descriptiva y visualización
## 2.1 Análisis descriptivo

summary(fifa)
length(levels(fifa$Club))
length(levels(fifa$Nationality))

## 2.2 Valores ausentes
colSums(is.na(fifa))

fifa$National_Position<-as.character(fifa$National_Position)
fifa[is.na(fifa$National_Position),]$National_Position <- "-"
fifa$National_Position<-factor(fifa$National_Position)
head(fifa$National_Position)


fifa[is.na(fifa$National_Kit),]$National_Kit <- "-1"
fifa$National_Kit <- as.integer(fifa$National_Kit)
min(fifa$National_Kit)


colSums(is.na(fifa))

fifaNet = DropNA(fifa)

colSums(is.na(fifaNet))

## 2.3 Visualización

### 2.3.1
get_portero <- function(x){
  if(x == 'GK')
    return ("Yes")
  else
    return ("No")
}
fifaNet$portero <- lapply(fifaNet$Club_Position,get_portero)
fifaNet$portero <- unlist(fifaNet$portero)
fifaNet$portero <- as.factor(fifaNet$portero)
levels(fifaNet$portero)

length(fifaNet[fifaNet$Club_Position == 'GK',])
length(fifaNet[fifaNet$portero == 'Yes',])

table_portero <- table(fifaNet$portero)
pct_portero <- round(table_portero/sum(table_portero)*100)
lbls_portero <- paste(names(table_portero), "\n", pct_portero, sep="")
lbls_portero <- paste(lbls_portero, '%', sep="")
pie(table_portero, labels = lbls_portero, main="Pie Chart of Portero\n", col=
      c("red4","darkblue"))
### 2.3.2 
class(fifaNet$Weight)

boxplot(Weight~portero,data=fifaNet,xlab="Portero", 
        ylab="Peso")

boxplot(Weight~Preffered_Foot,data=fifaNet,xlab="Preffered foot", 
        ylab="Peso")

boxplot(Weight~clasificacion,data=fifaNet,xlab="Clasificacion", 
        ylab="Peso")

boxplot(Weight~Age,data=fifaNet,xlab="Age", 
        ylab="Peso")

### 2.3.3

table_contract <- table(fifaNet$Contract_Expiry)
table_contract
pct_contract <- round(table_contract/sum(table_contract)*100)
barplot(pct_contract, ylab ="Proporción de jugadores %", 
        xlab="Distribución de años de expiración de contratos",
        cex.main = 0.8, cex.lab = 0.8, col="#33FFB5")

## 2.4 Comprobación de normalidad

hist(fifaNet$Weight, breaks=sqrt(dim(fifaNet)[1]), 
     xlab="Peso en kg", col="#33C4FF")

qqnorm(fifaNet$Weight)
qqline(fifaNet$Weight)

# 3 Estadística inferencial
head(fifaNet)
## a)
getConfidentInterval<- function(var){
  s = sd(var)
  n = length(var)
  me = abs(qt((1-0.95)/2,n-1 )) * (s/sqrt(n))
  x = mean(var)
  confidenceInterval = c(x-me,x+me)
  return (confidenceInterval)
}
getConfidentInterval(fifaNet$Weight)

CI(fifaNet$Weight, ci=0.95)

## b)
getConfidentInterval(fifaNet[fifaNet$portero=='Yes',]$Weight)
getConfidentInterval(fifaNet[fifaNet$portero=='No',]$Weight)

## 3.2 Contraste de hipótesis para la diferencia de medias

mean_hip_test <- function(x1, x2, CL=0.95, equalvar=TRUE, type="bilateral",
                          value=0){
  
  mean1<-mean(x1)
  n1<-length(x1)
  sd1<-sd(x1)
  mean2<-mean(x2)
  n2<-length(x2)
  sd2<-sd(x2)
  
  if(equalvar){
    comun_std <- sqrt( ((n1-1)*sd1^2 + (n2-1)*sd2^2 )/(n1+n2-2) )
    Sb <- comun_std*sqrt(1/n1 + 1/n2)
    df <- n1+n2-2
  }
  else{
    Sb <- sqrt( sd1^2/n1 + sd2^2/n2 )
    denom <- ( (sd1^2/n1)^2/(n1-1) + (sd2^2/n2)^2/(n2-1))
    df <- ( (sd1^2/n1 + sd2^2/n2)^2 ) / denom
  }
  
  alfa <- (1-CL)
  t<- (mean1-mean2-value) / Sb
  
  if (type=="bilateral"){
    tcritical <- qt( alfa/2, df, lower.tail=FALSE ) #two sided
    pvalue<-pt( abs(t), df, lower.tail=FALSE )*2 #two sided
  }
  else if (type=="less"){
    tcritical <- qt( alfa, df, lower.tail=TRUE )
    pvalue<-pt( t, df, lower.tail=TRUE )
  }
  else{ #(type=="greater")
    tcritical <- qt( alfa, df, lower.tail=FALSE )
    pvalue<-pt( t, df, lower.tail=FALSE )
  }
  
  solution<-data.frame(t,tcritical,pvalue,df)
  return(solution)
}

mean(fifaNet[fifaNet$portero=='Yes',]$Height)
mean(fifaNet[fifaNet$portero=='No',]$Height)

### 3.2.1 Escribid la hipótesis nula y la alternativa
## H0 = Uhp - Uhj < 5
## H1 = Uhp - Uhj >= 5

### 3.2.2 Justificación del test a aplicar

nrow(fifaNet)

var.test(fifaNet$Height[fifaNet$portero=='Yes'], fifaNet$Height[fifaNet$portero=='No'])
## p-value < 0.05, descartamos la igualdad de varianzas

### 3.2.3 Cálculos
mean_hip_test(x1=fifaNet$Height[fifaNet$portero=='Yes'], 
              x2=fifaNet$Height[fifaNet$portero=='No'],
              equalvar=FALSE, type="greater",
              value=5)

##descartamos la H0 aprobamos la H1


# 4. Modelo de regresión lineal

features_model <- c("Age","portero","Weight","Preffered_Foot", "Vision", 
                    "Ball_Control", "Rating")
fifaNet_model <- fifaNet %>% select(features_model)
head(fifaNet_model)
str(fifaNet_model)

fifaNet_model$portero <- relevel(fifaNet_model$portero,ref="Yes")
fifaNet_model$Preffered_Foot <- relevel(fifaNet_model$Preffered_Foot,ref="Left")

fifaNet_vectors <- fifaNet_model %>%
  mutate_if(is.factor, as.numeric)
head(fifaNet_vectors)


lm1 = lm(Rating~.,data=fifaNet_vectors)
lm1

summary(lm1)

## 4.2 Predicción
predict(lm1, newdata = data.frame(Age=24,portero=2,Weight=70,Preffered_Foot=1,
                                  Vision=60,Ball_Control=80), type='response')

# 5. Regresión Logística
## 5.1 Modelo predictivo

get_internacional <- function(x) {
  if (x == -1)
    return (0)
  else 
    return (1)
}


fifaNet$internacional <- lapply(fifaNet$National_Kit,get_internacional)
fifaNet$internacional <- unlist(fifaNet$internacional)
fifaNet$internacional <- as.numeric(fifaNet$internacional)
head(fifaNet$internacional)
tail(fifaNet$internacional)

features_glm <- c("portero","Rating","Age","Work_Rate","internacional")
fifaNet_glm <- fifaNet %>% select(features_glm)
head(fifaNet_glm)

fifaNet_glm_vectors <- fifaNet_glm %>%
  mutate_if(is.factor, as.numeric)
head(fifaNet_glm_vectors)

glm1 <- glm(internacional~.,data=fifaNet_glm_vectors,
            family=binomial (link = logit))
summary(glm1)

## 5.2 Matriz de confusión
y_pred <- ifelse(glm1$fitted.values < 0.5, 0, 1)
glm1.confusion_matrix <- ConfusionMatrix(y_true=fifaNet_glm_vectors$internacional,
                                         y_pred=y_pred)
glm1.confusion_matrix

## 5.4 Interpretación de la variable Work_Rate
features_glm2 <- c("portero","Rating","Age","Work_Rate","internacional")
fifaNet_glm2 <- fifaNet %>% select(features_glm2)

fifaNet_glm2$Work_Rate <- relevel(fifaNet_glm2$Work_Rate,ref="Medium / Medium")

fifaNet_glm2_vectors <- fifaNet_glm2 %>%
  mutate_if(is.factor, as.numeric)

head(fifaNet_glm2_vectors)

glm2 <- glm(internacional~.,data=fifaNet_glm2_vectors,
            family=binomial (link = logit))
summary(glm2)

y_pred_glm2 <- ifelse(glm2$fitted.values < 0.5, 0, 1)
glm2.confusion_matrix <- ConfusionMatrix(y_true=fifaNet_glm2_vectors$internacional,
                                         y_pred=y_pred_glm2)
glm2.confusion_matrix

## 5.5 Importancia de ser portero

exp(confint(glm2))

## 5.6 Predicción
head(fifaNet_glm2)
levels(fifaNet_glm2$Work_Rate)
head(fifaNet_glm2[fifaNet_glm2$Work_Rate=="High / High",])
fifaNet_glm2[16,]

fifaNet_glm2_vectors[16,]


predict(glm2,newdata = data.frame(portero=2,Age=25,Rating=95, Work_Rate=2), 
        type='response')

# Análisis de la varianza (ANOVA) de un factor

## 6.1 Visualización gráfica

get_age_int<-function(x){
  if (x <= 20)
    return ("Junior")
  else if (x >= 21 & x <=27)
    return("Middle")
  else if (x>=28)
    return("Senior")
}

fifaNet$Age_Int <- lapply(fifaNet$Age,get_age_int)
fifaNet$Age_Int <- unlist(fifaNet$Age_Int)
fifaNet$Age_Int <- as.factor(fifaNet$Age_Int)

boxplot(Rating~Age_Int,data=fifaNet,xlab="Edad", 
        ylab="Puntuación")

##6.2
#H0: alpha1 = alpha2 = alpha3 = 0
#H1 alphai != alphaj para algún i!=j

##6.3
lm_anova <- lm(Rating~Age_Int,data=fifaNet)
taov<-anova(lm_anova)
taov
##Aceptamos H1 y concluimos que el factor es significativo

##6.4
#LSD.test(lm_anova,"Age_Int",group=F,p.adj="bonferroni",console=T)
tapply(fifaNet$Rating, fifaNet$Age_Int, mean)
##Estos son los alpha i de cada una de las categorías del factor


##6.6 Adecuación del modelo

plot(lm_anova)
#primero nos muestra el gráfico que nos permite comprobar la homocedasticidad
# segundo nos muestra el gráfico que nos permite comprobar la normalidad
# tercero vuelve a mostrarnos un gráfico para comprobar la homocedasticidad

###6.6.1
##Los residuos se distribuyen de manera normal
##La homocedasticidad se cumple, es decir, la varianza parece ser la misma 
##o muy parecida en los 3 grupos

#7 ANOVA multifactorial

##7.1 Análisis visual de los efectos principales y posibles interacciones
fifaNet_mean_group <- fifaNet %>%
  dplyr::group_by(Age_Int,portero) %>%
  dplyr::summarise(mean_rating = mean(Rating))

fifaNet_mean_group
fifaNet_mean_group.df <- data.frame(fifaNet_mean_group)

ggplot(fifaNet_mean_group,aes(x=Age_Int,y=mean_rating,fill=factor(portero)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Portero",
                      breaks=c(1, 2),
                      labels=levels(fifaNet_mean_group))+
  xlab("Age")+ylab("Mean")

##7.2 Cálculo del modelo

anova_two_factors <- aov(Rating ~ Age_Int*portero, data = fifaNet)
anova(anova_two_factors)
model.tables(anova_two_factors, type = "effects")


##7.4
plot(anova_two_factors, which=1)
plot(anova_two_factors, which=2)

