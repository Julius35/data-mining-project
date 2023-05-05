rm(list=ls())
library(corrplot)
library(caret)
library(heplots)
library(class)
library(ROCR)
library(MASS)
library(magrittr)
library(tidyr)
library(dplyr)


# ANALISI PRELIMINARE
data <- read.csv('winequality-white.csv',header = T,sep = ';',dec = '.')
class(data)
str(data)
summary(data)
#non ci sono valori negativi, essendo delle variabili fisiche.
head(data)
n <- nrow(data)
p <- ncol(data)

sum(is.na(data))
# non sono presenti NA

boxplot(data[, -12])
table(data[, 12])

data[data$quality < 7, 12] <- 0
data[data$quality >= 7, 12] <- 1
data$quality <- factor(data$quality)

table(data$quality)
table(data$quality) / nrow(data)
#0.78 non eccellente, 0.22 eccellente


#TRAIN TEST SPLIT
set.seed(123)
data_idx <- sample(nrow(data), nrow(data)*0.8) 

# Train
data_train <- data[data_idx, ]
data_train_orig <- data_train
# Test
data_test <- data[-data_idx, ]

#Validation e una versione pi� piccola del training set
data_idx_val <- sample(nrow(data_train), 
                       nrow(data_train)*0.75) 
data_train_small <- data_train[data_idx_val, ]
data_train_small_orig <- data_train_small

data_val = data_train[-data_idx_val, ]


## ANALISI ESPLORATIVA SULLO SMALL TRAINING (abbiamo molte osservazioni)

#Controllo presenza di valori anomali
summary(data_train_small)
## non ci sono valori negativi, essendo delle variabili fisiche.
# Notiamo min citric acid = 0, poi analizziamo hist

par(mfrow = c(1,2))

for(i in 1:11){
  hist(data_train_small[,i], main=colnames(data_train_small)[i], xlab = "")
  boxplot(data_train_small[,i])
}

par(mfrow=c(1,1))
for (i in 1:11){
  hist(data_train_small[,i], prob = TRUE, col = "white",xlab='',main=colnames(data_train_small)[i])
  par(new = TRUE)
  boxplot(data_train_small[,i], horizontal = TRUE, axes = FALSE,
          col = rgb(1, 0, 0, alpha = 0.5))
}

#2, 4, 5, 6, 10 sono variabili che saranno probabilmente da trasformare
par(mfrow =c(1,2))
for(i in c(2,4,5,6,10)){
  hist(log(data_train_small[,i]), main=colnames(data_train_small)[i], xlab = "")
  boxplot(log(data_train_small[,i]))
}
par(mfrow=c(1,1))
# residual sugar non funziona bene
# volatile acidity, chlorides, free sulfure dioxide e sulphates funziona bene
#Decidiamo di modificare le variabili con log quelle scelte e log+10 l'altro
data_train_small[, c(2,5,6,10)] <- log(data_train_small[, c(2,5,6,10)])
data_train_small[, 4] <- log(10+data_train_small[, 4])

par(mfrow=c(2,1))
for (i in c(2,4,5,6,10)){
  hist(data_train_small_orig[,i], prob = TRUE, col = "white",xlab='',main=c(colnames(data_train_small_orig)[i],' original'))
  par(new = TRUE)
  boxplot(data_train_small_orig[,i], horizontal = TRUE, axes = FALSE,
          col = rgb(1, 0, 0, alpha = 0.5))
  hist(data_train_small[,i], prob = TRUE, col = "white",xlab='',main=colnames(data_train_small)[i])
  par(new = TRUE)
  boxplot(data_train_small[,i], horizontal = TRUE, axes = FALSE,
          col = rgb(1, 0, 0, alpha = 0.5))
}

# ricerca e trattazione outliers
par(mfrow=c(1,1))
for (i in 1:11){
  hist(data_train_small[,i], prob = TRUE, col = "white",xlab='',main=colnames(data_train_small)[i])
  par(new = TRUE)
  boxplot(data_train_small[,i], horizontal = TRUE, axes = FALSE,
          col = rgb(1, 0, 0, alpha = 0.5))
}

# density, total.sulfur.dioxide, reidual sugar hanno oulier potenzialmente problematici

which.max(data_train_small$density)
which.max(data_train_small$total.sulfur.dioxide)
which.max(data_train_small$residual.sugar)
data_train_small <- data_train_small[-c(1861,2652),]

for (i in 1:11){
  hist(data_train_small[,i], prob = TRUE, col = "white",xlab='',main=colnames(data_train)[i])
  par(new = TRUE)
  boxplot(data_train_small[,i], horizontal = TRUE, axes = FALSE,
          col = rgb(1, 0, 0, alpha = 0.5))
}

# analisi correlazione
cor(data_train_small[, -12])
par(mfrow=c(1,1))
corrplot(cor(data_train_small[, -12]), diag = F, type = "upper")

# Costruzione di una heatmap basata sui valori di correlazione (corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(data_train_small[, -12]), method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE, cl.cex = 0.8, order = "FPC"
)
# uniche variabili molto correlate:
# residual.sugar e density 0.81
# density alcohol -0.8

plot(data_train_small$residual.sugar, data_train_small$density)
plot(data_train_small$density, data_train_small$alcohol)
# si potrebbe pensare anche su basi teoriche di rimuovere la variabile density, ma non lo facciamo

# decidiamo di standardizzare per media e varianza sullo small training, siccome abbiamo trattato gli otlier più influenti
pp <- preProcess(data_train_small[,-12],method = c('center','scale'))
x_data_train_small_std <- predict(pp,data_train_small[,-12])
data_train_small_std <- cbind(x_data_train_small_std,data_train_small[,12])

summary(data_train_small_std)
par(cex.axis=0.7)
boxplot(data_train_small_std[,-12])
par(cex.axis=1)

# verifichiamo la distribuzione delle variabili condizionatamente al target
par(mfrow=c(1,1))
for(i in 1:11){
  plot(density(data_train_small_std[data_train_small_std[,12] == 0,i]),xlim = c(min(c(min(data_train_small_std[data_train_small_std[,12] == 0,i]),min(data_train_small_std[data_train_small_std[,12] == 1,i]))),c(max(max(data_train_small_std[data_train_small_std[,12] == 0,i]),max(data_train_small_std[data_train_small_std[,12] == 1,i])))),ylim=c(0,max(c(max(density(data_train_small_std[data_train_small_std[,12] == 0,i])$y),max(density(data_train_small_std[data_train_small_std[,12] == 1,i])$y)))),main=colnames(data_train_small_std)[i])
  lines(density(data_train_small_std[data_train_small_std[,12] == 1,i]),col='red')
}


# OPERAZIONI PREPARATIVE VALIDATION
data_val[, c(2,5,6,10)] <- log(data_val[, c(2,5,6,10)])
data_val[, 4] <- log(10+data_val[, 4])
x_data_val_std <- predict(pp,data_val[,-12])
data_val_std <- cbind(x_data_val_std,data_val[,12])
summary(data_val_std)


# INIZIA IL PROCESSO DI MACHINE LEARNING
# funzione errore di classificazione
calc_class_err <- function(actual,predicted){
  mean(actual!=predicted)
}


# MODELLI SU SMALL TRAINING E VALIDATION


# KNN
# Ottimizzazione parametro K
par(mfrow=c(1,1))
set.seed(123)

k_to_test <- seq(from=3,to=100,by=2)
err_k1 <- rep(0, length(k_to_test))
for (i in 1:length(k_to_test)) {
  pred_k1 <- knn(train = data_train_small_std[,-12], test = data_val_std[,-12], cl = data_train_small_std[,12],k = k_to_test[i])
  err_k1[i] <- calc_class_err(data_val_std[,12], pred_k1) 
}
plot(y = err_k1, x = k_to_test,type = "b", col = "dodgerblue", cex = 1, pch = 20,xlab = "Parametro K", ylab = "Errore di classificazione", main = "Errore vs K")
abline(h = min(err_k1), col = "darkorange", lty = 3)
min(err_k1)*100
k_to_test[which(err_k1==min(err_k1))] # 23

# Knn con K=23 migliore su small training
set.seed(123)
pred1_data_train_small_std <- knn(train = data_train_small_std[,-12], test = data_train_small_std[,-12], cl = data_train_small_std[,12], k =23)
test_error_rate_tstra1 <- calc_class_err(actual = data_train_small_std[,12], predicted = pred1_data_train_small_std)
test_error_rate_tstra1*100
table(pred1_data_train_small_std, data_train_small_std[,12],dnn = c('predicted','actual'))
(1-test_error_rate_tstra1)*100 # 84.16% accuracy in training

# Knn con K=23 migliore su validation
set.seed(123)
pred1_data_val_std <- knn(train = data_train_small_std[,-12], test = data_val_std[,-12], cl = data_train_small_std[,12], k =23,prob=T)
test_error_rate_tval1 <- calc_class_err(actual = data_val_std[,12], predicted = pred1_data_val_std)
test_error_rate_tval1*100
table(pred1_data_val_std, data_val_std[,12],dnn = c('predicted','actual'))
(1-test_error_rate_tval1)*100 # 81.94% accuracy in validation


# REGRESSIONE LOGISTICA
set.seed(123)
colnames(data_train_small_std)[12] <- 'quality'
model_logit <- glm(quality~., data = data_train_small_std, family = binomial)
summary(model_logit)
step.model <- stepAIC(model_logit, direction = "both", trace = T)
summary(step.model)
# non ci sono variabili da rimuovere in base all'AIC
# proviamo a rimuovere manualmente le variabili meno significative, ma il modello ottenuto è peggiore

anova(model_logit, test="Chisq")
influencePlot(model_logit)
punti_influenti <- c(773,772,813,3498,2771,4481)
model_logit2 <- glm(quality ~., data = data_train_small_std[-punti_influenti,], family = binomial)
summary(model_logit2)
step.model2 <- stepAIC(model_logit2, direction = "both", trace = T)
summary(step.model2)
# anche in questo caso non togliamo covariate

# reg_log su small training
pred_logit_tstra_st1 <- predict(model_logit2,newdata = data_train_small_std,type = 'response')
pred_logit_class_tstra_st1 <- ifelse (pred_logit_tstra_st1 > 0.5, 1, 0)
table(pred_logit_class_tstra_st1,data_train_small_std[,12],dnn = c('predicted','actual'))
test_error_rate_logit_tstra_st1 <- calc_class_err(actual = data_train_small_std[,12], predicted = pred_logit_class_tstra_st1)
test_error_rate_logit_tstra_st1*100
(1-test_error_rate_logit_tstra_st1)*100 # 80.86% accuracy in small training

# reg_log su tvalidation
pred_logit_tval_st1 <- predict(model_logit2,newdata = data_val_std,type = 'response')
pred_logit_class_tval_st1 <- ifelse (pred_logit_tval_st1 > 0.5, 1, 0)
table(pred_logit_class_tval_st1,data_val_std[,12],dnn = c('predicted','actual'))
test_error_rate_logit_tval_st1 <- calc_class_err(actual = data_val_std[,12], predicted = pred_logit_class_tval_st1)
test_error_rate_logit_tval_st1*100
(1-test_error_rate_logit_tval_st1)*100 # 80.71% accuracy in validation


# VERIFICA RELAZIONE SINGOLE COVARIATE LOGIT MODEL
probabilities <- predict(model_logit2,newdata = data_train_small_std, type = "response")
predictors <- colnames(data)[1:11]

supp <- data_train_small_std[, 1:11]
supp <- supp %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(supp[, -9], aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
# solo qualche variabile ha relazione lineare con logit del target


# VERIFICA ASSUNZIONI ANALISI DISCRIMINANTE
# Omoschedasticità tra classi
par(mfrow=c(1,3))
for (i in 1:11){
  boxplot(data_train_small_std[,i]~data_train_small_std[,12],main=colnames(data_train_small_std)[i])
}
par(mfrow=c(1,1))
featurePlot(x = data_train_small_std[,1:11],
            y = data_train_small_std[,12], 
            plot = "box")
# variabili condizionatamente a classe sembrano avere spesso stessa varianza
# Covarianza tra classi
covEllipses(x = data_train_small_std[,1:11],group = data_train_small_std[,12],pooled = F,fill=T,variables=c(1:11),var.cex=0.8)
# le variabili non sempre hanno stessa matrice di varianza e covarianza
# Distribuzione normale covariate
par(mfrow = c(2, 2))
for(i in c(1:11)) {
  qqnorm(data_train_small_std[data_train_small_std[,12] == 0, i], main = c(colnames(data_train_small_std)[i],'classe 0')); qqline(data_train_small_std[data_train_small_std[,12] == 0, i], col = 2)
}
par(mfrow = c(2, 2))
for(i in c(1:11)) {
  qqnorm(data_train_small_std[data_train_small_std[,12] == 1, i], main = c(colnames(data_train_small_std)[i],'classe 1')); qqline(data_train_small_std[data_train_small_std[,12] == 1, i], col = 2)
}
# la distribuzione delle variabili condizionata a entrambe le classi non sembra normale spesso

pvalue_shapiro <- matrix(0, nrow = (dim(data_train_small_std)[2]-1), ncol = 2)
rownames(pvalue_shapiro) = colnames(data_train_small_std)[-12]
colnames(pvalue_shapiro) = c("Excellent", "Poor")

# Test Shapiro e costruzione di una matrice riassuntiva con i p-value condizionati alla classe
for (i in colnames(data_train_small_std)[-12]){
  pvalue_shapiro[i, "Excellent"] <- shapiro.test(data_train_small_std[data_train_small_std[,12] == 1, i])$p.value
  pvalue_shapiro[i, "Poor"] <- shapiro.test(data_train_small_std[data_train_small_std[,12] == 0, i])$p.value
}
round(pvalue_shapiro, 2)
# secondo il test di shapiro nessuna variabile condizionata presenta distribuzione normale


# LDA
set.seed(123)
lda_model <- lda(quality ~ ., data = data_train_small_std)
lda_model
plot(lda_model) #sembra identificare abbastanza bene 
#lda su training
pred_lda_tstra <- predict(lda_model,data_train_small_std[,-12])
pred_lda_class_tstra <- pred_lda_tstra$class
table(pred_lda_class_tstra,data_train_small_std[,12],dnn = c('predicted','actual'))
test_error_rate_lda_tstra <- calc_class_err(actual = data_train_small_std[,12], predicted = pred_lda_class_tstra)
test_error_rate_lda_tstra*100
(1-test_error_rate_lda_tstra)*100 # 80.52% di accuracy in training
#lda su validation
pred_lda_tval <- predict(lda_model,data_val_std[,-12])
pred_lda_class_tval <- pred_lda_tval$class
table(pred_lda_class_tval,data_val_std[,12],dnn = c('predicted','actual'))
test_error_rate_lda_tval <- calc_class_err(actual = data_val_std[,12], predicted = pred_lda_class_tval)
test_error_rate_lda_tval*100
(1-test_error_rate_lda_tval)*100 # 80.71% di accuracy


# QDA
set.seed(123)
qda_model <- qda(quality ~ ., data = data_train_small_std)
qda_model
# qda su training
pred_qda_tstra <- predict(qda_model,data_train_small_std[,-12])
pred_qda_class_tstra <- pred_qda_tstra$class
table(pred_qda_class_tstra,data_train_small_std[,12],dnn = c('predicted','actual'))
test_error_rate_qda_tstra <- calc_class_err(actual = data_train_small_std[,12], predicted = pred_qda_class_tstra)
test_error_rate_qda_tstra*100
(1-test_error_rate_qda_tstra)*100 # 79.53% di accuracy in training
#qda su validation
pred_qda_tval <- predict(qda_model,data_val_std[,-12])
pred_qda_class_tval <- pred_qda_tval$class
table(pred_qda_class_tval,data_val_std[,12],dnn = c('predicted','actual'))
test_error_rate_qda_tval <- calc_class_err(actual = data_val_std[,12], predicted = pred_qda_class_tval)
test_error_rate_qda_tval*100
(1-test_error_rate_qda_tval)*100 # 78.27% di accuracy


# RIASSUNTO
# RIASSUNTO METODI SU TRAINING E VALIDATION
confusionMatrix(data = pred1_data_val_std,reference = data_val_std[,12],positive = '1')
confusionMatrix(data = as.factor(pred_logit_class_tval_st1),reference = data_val_std[,12],positive = '1')
confusionMatrix(data = pred_lda_class_tval,reference = data_val_std[,12],positive = '1')
confusionMatrix(data = pred_qda_class_tval,reference = data_val_std[,12],positive = '1')
# qda anche se peggiore in accuracy totale è l'unico metodo che ha sensitivity sufficiente


# CURVE ROC
prob_knn <- attributes(pred1_data_val_std)$prob
prob_knn <- 2*ifelse(pred1_data_val_std == 0, 1-prob_knn, prob_knn) - 1
pred_rocknn <- prediction(prob_knn, data_val_std[, 12])
perf_knn<- performance(pred_rocknn,"tpr","fpr")
auc_knn <- performance(pred_rocknn, measure = "auc")@y.values
auc_knn #82.17%

pred_roclogit <- prediction(pred_logit_tval_st1, data_val_std[, 12])
perf_logit <- performance(pred_roclogit,"tpr","fpr")
auc_logit <- performance(pred_roclogit, measure = "auc")@y.values
auc_logit #79.23%

pred_roclda <- prediction(pred_lda_tval$posterior[,2], data_val_std[, 12])
perf_lda<- performance(pred_roclda,"tpr","fpr")
auc_lda <- performance(pred_roclda, measure = "auc")@y.values
auc_lda #78.63%

pred_rocqda <- prediction(pred_qda_tval$posterior[,2], data_val_std[, 12])
perf_qda<- performance(pred_rocqda,"tpr","fpr")
auc_qda <- performance(pred_rocqda, measure = "auc")@y.values
auc_qda #81.26%

par(mfrow = c(1, 4))
plot(perf_logit, main = "Regressione Logistica")
plot(perf_lda, main = "Analisi Discriminante Lineare")
plot(perf_qda, main = "Analisi Discriminante Quadratica")
plot(perf_knn, main = "23-NN")
par(mfrow=c(1,1))

# Siccome le classi sono sbilanciate per la scelta del modello da applicare su training e test
# decidiamo di affidarci all'AUC e quindi ai modelli 23NN e QDA.
# Notiamo anche che questi due sono i modelli che costruiscono linee di confine non lineari.
# Vantaggio qda è anche che modello ha sensitivity sufficiente.

confusionMatrix(data = pred1_data_val_std,reference = data_val_std[,12],positive = '1')
auc_knn
confusionMatrix(data = pred_qda_class_tval,reference = data_val_std[,12],positive = '1')
auc_qda


# MODELLI SU TRAINING E TEST


# OPERAZIONI SU TRAINING E TEST
#Trasformo log e log+10
data_train[, c(2,5,6,10)] <- log(data_train[, c(2,5,6,10)])
data_train[, 4] <- log(10+data_train[, 4])

data_test[, c(2,5,6,10)] <- log(data_test[, c(2,5,6,10)])
data_test[, 4] <- log(10+data_test[, 4])

#Guardo hist e outliers
par(mfrow=c(1,1))
for (i in 1:11){
  hist(data_train[,i], prob = TRUE, col = "white",xlab='',main=colnames(data_train)[i])
  par(new = TRUE)
  boxplot(data_train[,i], horizontal = TRUE, axes = FALSE,
          col = rgb(1, 0, 0, alpha = 0.5))
}
# outliers in:
#citric acid
#residual sugar
#total sulfur dioxide
#density

which.max(data_train$density) #3365
which.max(data_train$total.sulfur.dioxide) #3369
which.max(data_train$residual.sugar) #3365
which.max(data_train$citric.acid) #2812

data_train <- data_train[-c(2812,3365,3369),]

#hist aggiornati
par(mfrow=c(1,1))
for (i in 1:11){
  hist(data_train[,i], prob = TRUE, col = "white",xlab='',main=colnames(data_train)[i])
  par(new = TRUE)
  boxplot(data_train[,i], horizontal = TRUE, axes = FALSE,
          col = rgb(1, 0, 0, alpha = 0.5))
}

#Standardizzo
pp2 <- preProcess(data_train[,-12],method = c('center','scale'))
x_data_train_std <- predict(pp2,data_train[,-12])
data_train_std <- cbind(x_data_train_std,data_train[,12])
summary(data_train_std)

x_data_test_std <- predict(pp2,data_test[,-12])
data_test_std <- cbind(x_data_test_std,data_test[,12])
summary(data_test_std)


# 23NN
# Knn con K=23 migliore su training
set.seed(123)
pred1_data_train_std <- knn(train = data_train_std[,-12], test = data_train_std[,-12], cl = data_train_std[,12], k =23,prob=T)
test_error_rate_train <- calc_class_err(actual = data_train_std[,12], predicted = pred1_data_train_std)
test_error_rate_train*100
table(pred1_data_train_std, data_train_std[,12],dnn = c('predicted','actual'))
(1-test_error_rate_train)*100 # 83.60% accuracy in training

colnames(data_train_std)[12] <- "quality"
colnames(data_test_std)[12] <- "quality"

# Knn con K=23 migliore su test
set.seed(123)
pred1_data_test_std <- knn(train = data_train_std[,-12], test = data_test_std[, -12], cl = data_train_std[,12], k =23,prob=T)
test_error_rate_test <- calc_class_err(actual = data_test_std[,12], predicted = pred1_data_test_std)
test_error_rate_test*100
table(pred1_data_test_std, data_test_std[,12],dnn = c('predicted','actual'))
(1-test_error_rate_test)*100 # 82.35% accuracy in test
confusionMatrix(data = pred1_data_test_std,reference = data_test_std[,12],positive = '1')

#auc knn
prob_knn_t <- attributes(pred1_data_test_std)$prob
prob_knn_t <- 2*ifelse(pred1_data_test_std == 0, 1-prob_knn_t, prob_knn_t) - 1
pred_rocknn_t <- prediction(prob_knn_t, data_test_std[, 12])
perf_knn_t <- performance(pred_rocknn_t,"tpr","fpr")
auc_knn_t <- performance(pred_rocknn_t, measure = "auc")@y.values
auc_knn_t #83.92%



# QDA
set.seed(123)
qda_model_t <- qda(quality ~ ., data = data_train_std)
qda_model_t

# qda su training
pred_qda_ttra <- predict(qda_model_t,data_train_std[,-12])
pred_qda_class_ttra <- pred_qda_ttra$class
table(pred_qda_class_ttra,data_train_std[,12],dnn = c('predicted','actual'))
test_error_rate_qda_ttra <- calc_class_err(actual = data_train_std[,12], predicted = pred_qda_class_ttra)
test_error_rate_qda_ttra*100
(1-test_error_rate_qda_ttra)*100 # 79.62% di accuracy in training

#qda su test
pred_qda_test <- predict(qda_model_t,data_test_std[,-12])
pred_qda_class_test <- pred_qda_test$class
table(pred_qda_class_test,data_test_std[,12],dnn = c('predicted','actual'))
test_error_rate_qda_test <- calc_class_err(actual = data_test_std[,12], predicted = pred_qda_class_test)
test_error_rate_qda_test*100
(1-test_error_rate_qda_test)*100 # 79.39% di accuracy
confusionMatrix(data = pred_qda_class_test,reference = data_test_std[,12],positive = '1')


#auc qda
pred_rocqda_t <- prediction(pred_qda_test$posterior[,2], data_test_std[, 12])
perf_qda_t <- performance(pred_rocqda_t,"tpr","fpr")
auc_qda_t <- performance(pred_rocqda_t, measure = "auc")@y.values
auc_qda_t #83.09%

confusionMatrix(data = pred1_data_test_std,reference = data_test_std[,12],positive = '1')
auc_knn_t #83.92%
confusionMatrix(data = pred_qda_class_test,reference = data_test_std[,12],positive = '1')
auc_qda_t #83.09%
par(mfrow=c(1,2))
plot(perf_qda, main = "Analisi Discriminante Quadratica")
plot(perf_knn, main = "23-NN")
par(mfrow=c(1,1))
# KNN è migliore sia dal punto di vista dell'AUC che dell'accuracy
# QDA ha però sensitivity decisamente più alta 


# DOWN-SAMPLING

# COSTRUZIONE SMALL TRAINING E OPERAZIONI DI TRASFORMAZIONE
table(data_train_small[,12])
set.seed(123)
idx_0_el <- sample(which(data_train_small[,12]==0),size = 638)
idx_1_el <- which(data_train_small[,12]==1)
data_train_small_bl <- data_train_small[c(idx_0_el,idx_1_el),]
table(data_train_small_bl[,12])
summary(data_train_small_bl)
boxplot(data_train_small_bl[,-12])
par(mfrow=c(1,1))
for (i in 1:11){
  hist(data_train_small_bl[,i], prob = TRUE, col = "white",xlab='',main=colnames(data_train_small)[i])
  par(new = TRUE)
  boxplot(data_train_small_bl[,i], horizontal = TRUE, axes = FALSE,
          col = rgb(1, 0, 0, alpha = 0.5))
}
pp3 <- preProcess(data_train_small_bl[,-12],method = c('center','scale'))
x_data_train_small_bl_std <- predict(pp3,data_train_small_bl[,-12])
data_train_small_bl_std <- cbind(x_data_train_small_bl_std,data_train_small_bl[,12])
summary(data_train_small_bl_std)

x_data_val_bl_std <- predict(pp3,data_val[,-12])
data_val_bl_std <- cbind(x_data_val_bl_std,data_val[,12])
summary(data_val_bl_std)


# KNN SU SET BILANCIATO
set.seed(123)
k_to_test <- seq(from=3,to=100,by=2)
err_k1 <- rep(0, length(k_to_test))
for (i in 1:length(k_to_test)) {
  pred_k1 <- knn(train = data_train_small_bl_std[,-12], test = data_val_bl_std[,-12], cl = data_train_small_bl_std[,12],k = k_to_test[i])
  err_k1[i] <- calc_class_err(data_val_bl_std[,12], pred_k1) 
}
plot(y = err_k1, x = k_to_test,type = "b", col = "dodgerblue", cex = 1, pch = 20,xlab = "Parametro K", ylab = "Errore di classificazione", main = "Errore vs K")
abline(h = min(err_k1), col = "darkorange", lty = 3)
min(err_k1)*100
k_to_test[which(err_k1==min(err_k1))] # 3

# Knn con K=3 migliore su small training balanced
set.seed(123)
pred1_data_train_small_std_bl <- knn(train = data_train_small_bl_std[,-12], test = data_train_small_bl_std[,-12], cl = data_train_small_bl_std[,12], k =3)
test_error_rate_tstra1_bl <- calc_class_err(actual = data_train_small_bl_std[,12], predicted = pred1_data_train_small_std_bl)
test_error_rate_tstra1_bl*100
table(pred1_data_train_small_std_bl, data_train_small_bl_std[,12],dnn = c('predicted','actual'))
(1-test_error_rate_tstra1_bl)*100 # 85.27% accuracy in training

# Knn con K=3 migliore su validation balanced
set.seed(123)
pred1_data_val_std_bl <- knn(train = data_train_small_bl_std[,-12], test = data_val_bl_std[,-12], cl = data_train_small_bl_std[,12], k =3,prob=T)
test_error_rate_tval1_bl <- calc_class_err(actual = data_val_bl_std[,12], predicted = pred1_data_val_std_bl)
test_error_rate_tval1_bl*100
table(pred1_data_val_std_bl, data_val_bl_std[,12],dnn = c('predicted','actual'))
(1-test_error_rate_tval1_bl)*100 # 70.31% accuracy in validation


# QDA SU SET BILANCIATO
set.seed(123)
colnames(data_train_small_bl_std)[12] <- 'quality'
qda_model_bl <- qda(quality ~ ., data = data_train_small_bl_std)
qda_model_bl

# qda su small training balanced
pred_qda_tstra_bl <- predict(qda_model_bl,data_train_small_bl_std[,-12])
pred_qda_class_tstra_bl <- pred_qda_tstra_bl$class
table(pred_qda_class_tstra_bl,data_train_small_bl_std[,12],dnn = c('predicted','actual'))
test_error_rate_qda_tstra_bl <- calc_class_err(actual = data_train_small_bl_std[,12], predicted = pred_qda_class_tstra_bl)
test_error_rate_qda_tstra_bl*100
(1-test_error_rate_qda_tstra_bl)*100 # 74.84% di accuracy in training

#qda su validation balanced
pred_qda_tval_bl <- predict(qda_model_bl,data_val_bl_std[,-12])
pred_qda_class_tval_bl <- pred_qda_tval_bl$class
table(pred_qda_class_tval_bl,data_val_bl_std[,12],dnn = c('predicted','actual'))
test_error_rate_qda_tval_bl <- calc_class_err(actual = data_val_bl_std[,12], predicted = pred_qda_class_tval_bl)
test_error_rate_qda_tval_bl*100
(1-test_error_rate_qda_tval_bl)*100 # 66.43% di accuracy

confusionMatrix(data = pred1_data_val_std_bl,reference = data_val_bl_std[,12],positive = '1')
confusionMatrix(data = pred_qda_class_tval_bl,reference = data_val_bl_std[,12],positive = '1')

prob_knn_bl <- attributes(pred1_data_val_std_bl)$prob
prob_knn_bl <- 2*ifelse(pred1_data_val_std_bl == 0, 1-prob_knn_bl, prob_knn_bl) - 1
pred_rocknn_bl <- prediction(prob_knn_bl, data_val_bl_std[, 12])
perf_knn_bl <- performance(pred_rocknn_bl,"tpr","fpr")
auc_knn_bl <- performance(pred_rocknn_bl, measure = "auc")@y.values
auc_knn_bl #78.66%

pred_rocqda_bl <- prediction(pred_qda_tval_bl$posterior[,2], data_val_bl_std[, 12])
perf_qda_bl<- performance(pred_rocqda_bl,"tpr","fpr")
auc_qda_bl <- performance(pred_rocqda_bl, measure = "auc")@y.values
auc_qda_bl #81.27%

par(mfrow = c(1, 2))
plot(perf_qda_bl, main = "Analisi Discriminante Quadratica su balanced")
plot(perf_knn_bl, main = "3-NN su balanced")
par(mfrow=c(1,1))


# MODELLI SU TRAINING BILANCIATO E TEST

# COSTRUZIONE TRAINING E OPERAZIONI DI TRASFORMAZIONE
table(data_train_small_bl[,12])
table(data_val[,12])
table(data_train[,12])
set.seed(123)
idx_1_el2 <- which(data_train[,12]==1)
data_train_bl_1 <- data_train[idx_1_el2,]
idx_0_el1 <- which(data_train_small_bl[,12]==0)
data_train_bl_0 <- data_train_small_bl[idx_0_el1,]
idx_0_val <- which(data_val[,12]==0)
data_val_0 <- data_val[idx_0_val,]
idx_0_val.1 <- sample(nrow(data_val_0),size = 848-638)
data_train_bl_0.1 <- data_val_0[idx_0_val.1,]
data_train_bl <- rbind(data_train_bl_1,data_train_bl_0,data_train_bl_0.1)
table(data_train_bl[,12])
summary(data_train_bl)
boxplot(data_train_bl[,-12])
par(mfrow=c(1,1))
for (i in 1:11){
  hist(data_train_bl[,i], prob = TRUE, col = "white",xlab='',main=colnames(data_train_small)[i])
  par(new = TRUE)
  boxplot(data_train_bl[,i], horizontal = TRUE, axes = FALSE,
          col = rgb(1, 0, 0, alpha = 0.5))
}
pp4 <- preProcess(data_train_bl[,-12],method = c('center','scale'))
x_data_train_bl_std <- predict(pp4,data_train_bl[,-12])
data_train_bl_std <- cbind(x_data_train_bl_std,data_train_bl[,12])
summary(data_train_bl_std)

x_data_test_bl_std <- predict(pp4,data_test[,-12])
data_test_bl_std <- cbind(x_data_test_bl_std,data_test[,12])
summary(data_test_bl_std)


# KNN SU TRAINING SET BILANCIATO
# Knn con K=3 migliore su small training balanced
set.seed(123)
pred1_data_train_std_bl <- knn(train = data_train_bl_std[,-12], test = data_train_bl_std[,-12], cl = data_train_bl_std[,12], k =3)
test_error_rate_ttra1_bl <- calc_class_err(actual = data_train_bl_std[,12], predicted = pred1_data_train_std_bl)
test_error_rate_ttra1_bl*100
table(pred1_data_train_std_bl, data_train_bl_std[,12],dnn = c('predicted','actual'))
(1-test_error_rate_ttra1_bl)*100 # 86.50% accuracy in training

# Knn con K=3 migliore su test balanced
set.seed(123)
pred1_data_test_std_bl <- knn(train = data_train_bl_std[,-12], test = data_test_bl_std[,-12], cl = data_train_bl_std[,12], k =3,prob=T)
test_error_rate_ttest1_bl <- calc_class_err(actual = data_test_bl_std[,12], predicted = pred1_data_test_std_bl)
test_error_rate_ttest1_bl*100
table(pred1_data_test_std_bl, data_test_bl_std[,12],dnn = c('predicted','actual'))
(1-test_error_rate_ttest1_bl)*100 # 72.65% accuracy in validation

# QDA SU SET BILANCIATO
set.seed(123)
colnames(data_train_bl_std)[12] <- 'quality'
qda_model_bl_f <- qda(quality ~ ., data = data_train_bl_std)
qda_model_bl_f

# qda su training balanced
pred_qda_ttra_bl <- predict(qda_model_bl_f,data_train_bl_std[,-12])
pred_qda_class_ttra_bl <- pred_qda_ttra_bl$class
table(pred_qda_class_ttra_bl,data_train_bl_std[,12],dnn = c('predicted','actual'))
test_error_rate_qda_ttra_bl <- calc_class_err(actual = data_train_bl_std[,12], predicted = pred_qda_class_ttra_bl)
test_error_rate_qda_ttra_bl*100
(1-test_error_rate_qda_ttra_bl)*100 # 74.53% di accuracy in training

#qda su test balanced
pred_qda_ttest_bl <- predict(qda_model_bl_f,data_test_bl_std[,-12])
pred_qda_class_ttest_bl <- pred_qda_ttest_bl$class
table(pred_qda_class_ttest_bl,data_test_bl_std[,12],dnn = c('predicted','actual'))
test_error_rate_qda_ttest_bl <- calc_class_err(actual = data_test_bl_std[,12], predicted = pred_qda_class_ttest_bl)
test_error_rate_qda_ttest_bl*100
(1-test_error_rate_qda_ttest_bl)*100 # 69.49% di accuracy

confusionMatrix(data = pred1_data_test_std_bl,reference = data_test_bl_std[,12],positive = '1')
confusionMatrix(data = pred_qda_class_ttest_bl,reference = data_test_bl_std[,12],positive = '1')

prob_knn_bl_f <- attributes(pred1_data_test_std_bl)$prob
prob_knn_bl_f <- 2*ifelse(pred1_data_test_std_bl == 0, 1-prob_knn_bl_f, prob_knn_bl_f) - 1
pred_rocknn_bl_f <- prediction(prob_knn_bl_f, data_test_bl_std[, 12])
perf_knn_bl_f <- performance(pred_rocknn_bl_f,"tpr","fpr")
auc_knn_bl_f <- performance(pred_rocknn_bl_f, measure = "auc")@y.values
auc_knn_bl_f #82.93%

pred_rocqda_bl_f <- prediction(pred_qda_ttest_bl$posterior[,2], data_test_bl_std[, 12])
perf_qda_bl_f <- performance(pred_rocqda_bl_f,"tpr","fpr")
auc_qda_bl_f <- performance(pred_rocqda_bl_f, measure = "auc")@y.values
auc_qda_bl_f #82.63%

par(mfrow = c(1, 2))
plot(perf_qda_bl_f, main = "Analisi Discriminante Quadratica su balanced")
plot(perf_knn_bl_f, main = "11-NN su balanced")
par(mfrow=c(1,1))


# CONCLUSIONI
# generalmente i modelli hanno un'accuracy e un AUC maggiore utilizzando un set di allenamento
# non bilanciato. Ciò però che rende comunque utile i modelli allenati su set bilanciati
# è il valore della balanced accuracy (media sensitivity e specificity) che è abbastanza 
# elevata.

# MODELLI NON BILANCIATI
confusionMatrix(data = pred1_data_test_std,reference = data_test_std[,12],positive = '1')
auc_knn_t #83.92%
confusionMatrix(data = pred_qda_class_test,reference = data_test_std[,12],positive = '1')
auc_qda_t #83.09%
# MODELLI BILANCIATI
confusionMatrix(data = pred1_data_test_std_bl,reference = data_test_bl_std[,12],positive = '1')
auc_knn_bl_f #82.93%
confusionMatrix(data = pred_qda_class_ttest_bl,reference = data_test_bl_std[,12],positive = '1')
auc_qda_bl_f #82.63%


# Siccome vogliamo prevedere sufficientemente bene entrambe le classi i modelli che si possono scegliere sono:
# QDA su set non bilanciato
# KNN su set bilanciato




## CODICE PER TABELLE RIASSUNTIVE
#Salvo i balanced accuracy quelli bilanciati
conf_knn <- confusionMatrix(data = pred1_data_test_std,reference = data_test_std[,12],positive = '1')
ba_knn_test <- conf_knn$byClass["Balanced Accuracy"]

conf_qda <- confusionMatrix(data = pred_qda_class_test,reference = data_test_std[,12],positive = '1')
ba_qda_test <- conf_qda$byClass["Balanced Accuracy"]

#Balanced accuracy per quelli sbilanciati
conf_knn_bl <- confusionMatrix(data = pred1_data_test_std_bl,reference = data_test_bl_std[,12],positive = '1')
ba_knn_test_bl <- conf_knn_bl$byClass["Balanced Accuracy"]

conf_qda_bl <- confusionMatrix(data = pred_qda_class_ttest_bl,reference = data_test_bl_std[,12],positive = '1')
ba_qda_test_bl <- conf_qda_bl$byClass["Balanced Accuracy"]

#Tabelle riassuntone per confrontare bilanciato e non bilanciato
#colonne: accuracy, auc, balanced accuracy
#riga knn
test_score_knn_nonbl <- matrix(c((1-test_error_rate_test)*100,
                                 as.numeric(auc_knn_t) * 100, 
                                 ba_knn_test*100), nrow = 1, ncol = 3)

#riga qda
test_score_qda_nonbl <- matrix(c((1-test_error_rate_qda_test)*100,
                                 as.numeric(auc_qda_t) * 100, 
                                 ba_qda_test*100), nrow = 1, ncol = 3)

#riga knn balanced
test_score_knn_bl <- matrix(c((1-test_error_rate_ttest1_bl)*100,
                              as.numeric(auc_knn_bl_f) * 100, 
                              ba_knn_test_bl*100), nrow = 1, ncol = 3)

#riga qda balanced
test_score_qda_bl <- matrix(c((1-test_error_rate_qda_ttest_bl)*100,
                              as.numeric(auc_qda_bl_f) * 100, 
                              ba_qda_test_bl*100), nrow = 1, ncol = 3)

riassunto <- rbind(test_score_knn_nonbl,
                   test_score_knn_bl,
                   test_score_qda_nonbl,
                   test_score_qda_bl)
riassunto <- data.frame(riassunto)
colnames(riassunto) <- c("Accuracy", "AUC", "Balanced Accuracy")
rownames(riassunto) <- c("KNN", "KNN Balanced", "QDA", "QDA Balanced")
riassunto

# RIASSUNTONE per validation iniziale
#Salvo i balanced accuracy validation
#knn validation
conf_knn_val <- confusionMatrix(data = pred1_data_val_std,reference = data_val_std[,12],positive = '1')
ba_knn_val <- conf_knn_val$byClass["Balanced Accuracy"]
#log validation
conf_log_val <- confusionMatrix(data = as.factor(pred_logit_class_tval_st1),reference = data_val_std[,12],positive = '1')
ba_log_val <- conf_log_val$byClass["Balanced Accuracy"]
#lda validation
conf_lda_val <- confusionMatrix(data = pred_lda_class_tval,reference = data_val_std[,12],positive = '1')
ba_lda_val <- conf_lda_val$byClass["Balanced Accuracy"]
#qda validation
conf_qda_val <- confusionMatrix(data = pred_qda_class_tval,reference = data_val_std[,12],positive = '1')
ba_qda_val <- conf_qda_val$byClass["Balanced Accuracy"]

#riga knn
val_score_knn <- matrix(c((1-test_error_rate_tval1)*100,
                          as.numeric(auc_knn) * 100, 
                          ba_knn_val*100), nrow = 1, ncol = 3)

#riga log
val_score_log <- matrix(c((1-test_error_rate_logit_tval_st1)*100,
                          as.numeric(auc_logit) * 100, 
                          ba_log_val*100), nrow = 1, ncol = 3)

#riga lda
val_score_lda <- matrix(c((1-test_error_rate_lda_tval)*100,
                          as.numeric(auc_lda) * 100, 
                          ba_lda_val*100), nrow = 1, ncol = 3)

#riga qda 
val_score_qda <- matrix(c((1-test_error_rate_qda_tval)*100,
                          as.numeric(auc_qda) * 100, 
                          ba_qda_val*100), nrow = 1, ncol = 3)

#riassuntone validation
riassunto_val <- rbind(val_score_knn,
                       val_score_log,
                       val_score_lda,
                       val_score_qda)
riassunto_val <- data.frame(riassunto_val)
colnames(riassunto_val) <- c("Accuracy", "AUC", "Balanced Accuracy")
rownames(riassunto_val) <- c("KNN", "Reg Logistica", "LDA", "QDA")


