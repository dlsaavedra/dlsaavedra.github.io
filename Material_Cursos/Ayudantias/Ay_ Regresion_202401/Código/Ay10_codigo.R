
library(lmtest)
data = read.table("Ayudantia10/pudahuel.txt", header = T, sep = ',')
data = data[seq(1, 547,7),]
data = data[, c(-1, -4, -8, -9, -11)]
data$log_pm25 = log(data$pm25)
data = data[, -2]
plot(data[-1,])
colnames(data) = c("Fecha", "Temperatura", "Viento", "Humedad" ,"Ozono", "log_pm25")
# temp = Temperatura
# wind = Viento
# rh = Humedad Relativa
# ozono = nivel de ozono
save(data, file = "Ayudantia10/Ay10_Datos_pm25")




load("Ayudantia10/Ay10_Datos_pm25")
head(data)
ts.plot(data$log_pm25)
plot(data[,-1])
data = data[,-1]
cor(data)


model = lm(log_pm25 ~ ., data = data)
summary(model)


plot(data$log_pm25, model$residuals)

plot(model$fitted.values, model$residuals)
res= model$residuals


shapiro.test(res)
ad.test(res)

dwtest(model)
bptest(model)

# Método Backward ----
colnames(data)
## Paso 1 ----
model = lm(log_pm25~ Temperatura + Viento + Humedad + Ozono , data = data)
model1 = lm(log_pm25~  Viento + Humedad + Ozono , data = data)
model2 = lm(log_pm25~ Temperatura +  Humedad + Ozono , data = data)
model3 = lm(log_pm25~ Temperatura + Viento +  Ozono , data = data)
model4 = lm(log_pm25~ Temperatura + Viento + Humedad , data = data)
summary(model)
summary(model1)
summary(model2)
summary(model3)
summary(model4)

A = anova(model, model1); A  # Test F inclusión
B = anova(model, model2); B  # Test F inclusión
C = anova(model, model3); C  # Test F inclusión
D = anova(model, model4); D  # Test F inclusión
which.max(c(A$`Pr(>F)`[2], B$`Pr(>F)`[2], C$`Pr(>F)`[2], D$`Pr(>F)`[2]))
dropterm(model, test = "F")

## Paso 2----
model = lm(log_pm25~  Temperatura + Viento  + Ozono , data = data)
model1 = lm(log_pm25~ Viento + Ozono , data = data)
model2 = lm(log_pm25~  Temperatura + Viento , data = data)
model3 = lm(log_pm25~  Temperatura + Viento, data = data)
summary(model)
summary(model1)
summary(model2)
summary(model3)
A = anova(model, model1);A  # Test F exclusión
B = anova(model, model2);B# Test F exclusión
C = anova(model, model3); C  # Test F inclusión
#anova(model1)
which.max(c(A$`Pr(>F)`[2], B$`Pr(>F)`[2],C$`Pr(>F)`[2]))

dropterm(model, test = "F")

## Paso 3 ----
model = lm(log_pm25~  Viento + Ozono , data = data)

model1 = lm(log_pm25~  Ozono , data = data)
model2 = lm(log_pm25~  Viento  , data = data)
A = anova(model, model1);A  # Test F exclusión
B = anova(model, model2);B# Test F exclusión
which.max(c(A$`Pr(>F)`[2], B$`Pr(>F)`[2]))

dropterm(model, test = "F")

## Nos quedamos con el paso 3 ----

## Analisis de Residuos ----
model_original = lm(log_pm25~  . , data = data)
summary(model_original)
model = lm(log_pm25~  Viento + Ozono , data = data)
summary(model)

plot(data$log_pm25, model$residuals)

plot(model$fitted.values, model$residuals)
res= model$residuals

ad.test(res)
shapiro.test(res)
dwtest(model)
bptest(model)



# Método Forward -----
colnames(data)
## Paso 1 -----
model = lm(log_pm25 ~  1 , data = data)
model1 = lm(log_pm25 ~  Temperatura , data = data)
model2 = lm(log_pm25 ~  Viento , data = data)
model3 = lm(log_pm25 ~  Humedad , data = data)
model4 = lm(log_pm25 ~  Ozono , data = data)
summary(model)
summary(model1)
summary(model2)
summary(model3)
summary(model4)


A = anova(model, model1); A  # Test F inclusión
B = anova(model, model2); B  # Test F inclusión
C = anova(model, model3); C  # Test F inclusión
D = anova(model, model4); D  # Test F inclusión
which.min(c(A$`Pr(>F)`[2], B$`Pr(>F)`[2], C$`Pr(>F)`[2], D$`Pr(>F)`[2]))
addterm(model,~. + Temperatura + Viento + Humedad + Ozono, test = "F")
## Paso 2 ----

model = lm(log_pm25 ~  Viento , data = data)
model1 = lm(log_pm25 ~  Viento + Temperatura, data = data)
model2 = lm(log_pm25 ~  Viento + Humedad, data = data)
model3 = lm(log_pm25 ~  Viento + Ozono, data = data)
summary(model)
summary(model1)
summary(model2)
summary(model3)
anova(model, model1)  # Test F exclusión
anova(model, model2)  # Test F exclusión
anova(model, model3)  # Test F exclusión
addterm(model,~. + Temperatura + Humedad + Ozono , test = "F")

## Paso 3 ----
model = lm(log_pm25 ~  Viento + Ozono, data = data)
model1 = lm(log_pm25 ~  Viento + Ozono+ Temperatura, data = data)
model2 = lm(log_pm25 ~   Viento + Ozono + Humedad, data = data)
summary(model)
summary(model1)
summary(model2)
anova(model, model1)  # Test F exclusión
anova(model, model2)  # Test F exclusión
addterm(model,~. + Temperatura + Humedad, test = "F")

## Nos quedamos con el paso 3 ----

## Analisis de Residuos ----
model = lm(log_pm25 ~  Viento + Ozono, data = data)

plot(model$residuals)
plot(data$log_pm25, model$residuals)


res= model$residuals
qqnorm(res)
qqline(res)

ad.test(res)
shapiro.test(res)
dwtest(model)
bptest(model)



# Metodo 2backward 1 forward ----
model = lm(log_pm25~ Temperatura + Viento + Humedad + Ozono, data = data)
dropterm(model, test = "F")
## Sacamos 2----
model = lm(log_pm25~  Viento + Ozono , data = data)
addterm(model,~. + Temperatura + Humedad ,test = "F")
## Agregamos 1----
model = lm(log_pm25~  Viento + Ozono, data = data)
dropterm(model, test = "F")

model = lm(log_pm25~  Viento + Ozono, data = data)
