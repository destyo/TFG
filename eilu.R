########################
# TFG - Antonio Tello  #  
########################

#----------------  Librerias e importación dataset ----------------

library(sandwich)
library(lmtest)
library(tidyverse)
library(arsenal)
library(MatchIt)
library(optmatch)
library(DescTools)
library(cem)
library(polycor)
library(survminer)
library(erer)



fs <- read.delim("EILU_GRADUADOS/CSV/EILU_GRAD_2019.csv")
fs
fs[,names(fs)] <- lapply(fs[,names(fs)] , factor)
fs$IDENT <- as.numeric(as.character(fs$IDENT))
str(fs)


# --------------- Creación varaibles --------------------- 

fs$ESP <- as.factor(as.integer(fs$NACIO != 3))
fs$EMPLEO <- as.factor(as.integer(fs$TRBPRN1 == 1))
fs$ACT <- as.factor(as.integer(fs$TRBPRN1 != 3))
fs$PRIV <- as.factor((as.integer(fs$T_UNIV )> 2)*1)
fs$INGL <- as.factor(as.numeric(fs$IDI_MT1 == "6" |
                       fs$IDI_MT2 == "6"|
                       fs$IDIOMA1== "6" & (fs$TIPOACREIDI1 == "4" | fs$TIPOACREIDI1 == "5" | fs$TIPOACREIDI1 == "6")|
                       fs$IDIOMA2== "6" & (fs$TIPOACREIDI2 == "4" | fs$TIPOACREIDI1 == "5" | fs$TIPOACREIDI1 == "6")|
                       fs$IDIOMA3== "6" & (fs$TIPOACREIDI3 == "4" | fs$TIPOACREIDI1 == "5" | fs$TIPOACREIDI1 == "6")|
                       fs$IDIOMA4== "6" & (fs$TIPOACREIDI4 == "4" | fs$TIPOACREIDI1 == "5" | fs$TIPOACREIDI1 == "6")))
fs$INGL[is.na(fs$INGL)] <- 0


summary(fs$INGL) #Ingles B2 o superior acreditado
summary(fs$ESP) #Nacionalidad española 
summary(fs$EMPLEO) #Tiene Empleo 
summary(fs$ACT) #Activo
summary(fs$PRIV) #Fue a universidad privada




#Variables Personales
fs$SEXO <- factor(fs$SEXO, labels = c("Hombre", "Mujer"))
fs$PRIV <- factor(fs$PRIV, labels = c("Pública", "Privada"))
fs$EDAD <- factor(fs$EDAD, labels = c("Menos de 30", "De 30 a 34", "35 y más"))
fs$NACIO <- factor(fs$NACIO, labels = c("Esp", "Esp y otra", "Otra"))
fs$PAIS_NACI <- factor(fs$PAIS_NACI, labels = c("ESP", "UE", "No UE"))
fs$DISCA <-  as.factor(as.numeric(fs$DISCA== 1))
fs$RAMA <- factor(fs$RAMA, labels = c("Artes y humanidades", "Ciencias", "CCSS", "ingeniería","C. Salud"))
fs$AMBITO <- factor(fs$AMBITO, labels = c("Educación", "Artes", "Humanidades", "Lenguas",
                                        "CCSS y del comportamiento", "Peridosimo", "Negocios y admin", 
                                        "Derecho", "C. de la vida", "Medio Ambiente", "C. FyQ",
                                        "Matemáticas", "Informatica", "Ingeniería", "Industria",
                                        "Arquitectura", "Agricultura", "Silvicultura", "Veterinaria",
                                        "Salud (otros)", "Servicios Sociales", "Servicos(otros)", "Enseñanza infantil", 
                                        "Enseñanza Primaria", "Audisovisuales", "Economía", "Psicología",
                                        "Direccion y admin", "Medicina", "Efremería", "C. Actividad Física",
                                        "Turismo"))


#madre y padre eliminación ns/nc
summary(fs$ESTUDIOS_MADRE)
summary(fs$ESTUDIOS_PADRE)
which(fs$ESTUDIOS_MADRE == 9)
which(fs$ESTUDIOS_PADRE == 9)

fs <- fs[!fs$ESTUDIOS_MADRE == 9,]
fs <- fs[!fs$ESTUDIOS_PADRE == 9,]
which(fs$ESTUDIOS_MADRE == 9)
which(fs$ESTUDIOS_PADRE == 9)

summary(fs$PAIS_NAC_MADRE)
summary(fs$PAIS_NAC_PADRE)

fs <- fs[!fs$PAIS_NAC_MADRE == 9,]
fs <- fs[!fs$PAIS_NAC_PADRE == 9,]
which(fs$PAIS_NAC_MADRE == 9)
which(fs$PAIS_NAC_PADRE == 9)

#Variables familiares 
fs$ESTUDIOS_MADRE <- factor(fs$ESTUDIOS_MADRE, labels = c("Sin Estudios", "Primaria Incompleta", "Educ. Primaria", "Educ. Secundaria",
                                                          "Bachillerato", "Grado Medio", "Grado Superior", "Universidad" ))
fs$ESTUDIOS_PADRE <- factor(fs$ESTUDIOS_PADRE, labels = c("Sin Estudios", "Primaria Incompleta", "Educ. Primaria", "Educ. Secundaria",
                                                          "Bachillerato", "Grado Medio", "Grado Superior", "Universidad" ))
fs$PAIS_NAC_MADRE <- factor(fs$PAIS_NAC_MADRE, labels = c("ESP", "UE", "No UE" ))
fs$PAIS_NAC_PADRE <- factor(fs$PAIS_NAC_PADRE, labels = c("ESP", "UE", "No UE" ))


#Becas y formación adicional 

summary(fs$EST_B1) #Alguna Beca
fs$EST_B1 <-  as.factor(as.numeric(fs$EST_B1== 1))
names(fs)[names(fs) == "EST_B1"] <- "BECA"
summary(BECA)

summary(fs$EST_B2_1) #Beca General 
fs$EST_B2_1 <-  as.factor(as.numeric(fs$EST_B2_1== 1))
fs$EST_B2_1[is.na(fs$EST_B2_1)] <- 0
names(fs)[names(fs) == "EST_B2_1"] <- "BECAg"


summary(fs$EST_B2_2) #Beca Excelencia
fs$EST_B2_2 <-  as.factor(as.numeric(fs$EST_B2_2== 1))
fs$EST_B2_2[is.na(fs$EST_B2_2)] <- 0
names(fs)[names(fs) == "EST_B2_2"] <- "BECAe"



summary(fs$EST_B2_3) #Beca colaboración universidad
fs$EST_B2_3 <-  as.factor(as.numeric(fs$EST_B2_3== 1))
fs$EST_B2_3[is.na(fs$EST_B2_3)] <- 0
names(fs)[names(fs) == "EST_B2_3"] <- "BECAcu"

summary(fs$EST_B2_4) #Beca Practicas Externas
fs$EST_B2_4 <-  as.factor(as.numeric(fs$EST_B2_4== 1))
fs$EST_B2_4[is.na(fs$EST_B2_4)] <- 0
names(fs)[names(fs) == "EST_B2_4"] <- "BECApe"

summary(fs$EST_B2_5) #Beca estudios fuera (no erasmus)
fs$EST_B2_5 <-  as.factor(as.numeric(fs$EST_B2_5== 1))
fs$EST_B2_5[is.na(fs$EST_B2_5)] <- 0
names(fs)[names(fs) == "EST_B2_5"] <- "BECAef"

summary(fs$EST_M1) #parte de sus estudios fuera (Erasmus + otros)
fs$EST_M1 <-  as.factor(as.numeric(fs$EST_M1== 1))
names(fs)[names(fs) == "EST_M1"] <- "ERAMUS"


summary(fs$EST_MES)#Parte de sus estudios en otra universidad española
fs$EST_MES <-  as.factor(as.numeric(fs$EST_MES== 1))
names(fs)[names(fs) == "EST_MES"] <- "SICUE"

summary(fs$EST_B8) #Motivación para estudiar 
fs[fs$EST_B8 == 9,] = 3
fs$EST_B8 <- relevel(fs$EST_B8, "3")
fs$EST_B8 <- factor(fs$EST_B8, labels = c("Otros", "Laboral", "Satisfacción Personal"))
names(fs)[names(fs) == "EST_B8"] <- "MOTIV"

summary(fs$IDIOMAS) #Nº de idiomas que habla sin contar maternos
fs <- fs[!fs$IDIOMAS == 9,]


summary(fs$TIC) #Conocimientos Informatica 
fs <- fs[!fs$TIC == 9,]
fs$TIC <- factor(fs$TIC, labels = c("Basico", "Intermedio","Avanzado"))

summary(fs$EST_B38_5) #Curso de idiomas 
fs$EST_B38_5 <-  as.factor(as.numeric(fs$EST_B38_5 == 1))
names(fs)[names(fs) == "EST_B38_5"] <- "CURSOidi"

summary(fs$EST_B38_6) #Curso de informatica
fs$EST_B38_6 <-  as.factor(as.numeric(fs$EST_B38_6 == 1))
names(fs)[names(fs) == "EST_B38_6"] <- "CURSOinf"

summary(fs$EST_B38_7) #Otros estudios no reglados 
fs$EST_B38_7 <-  as.factor(as.numeric(fs$EST_B38_7 == 1))
names(fs)[names(fs) == "EST_B38_7"] <- "CURSOotro"

summary(fs$HL_E1) #Ha Realizado practicas (durante o después de los estudios?)
fs$HL_E1 <- factor(fs$HL_E1, labels = c("Si, curriculares", "Si, extracurriculares", "Si, ambas", "No" ))
fs$HL_E1 <- relevel(fs$HL_E1, "No")
names(fs)[names(fs) == "HL_E1"] <- "PRACTICAS"

fs_backup <- fs 
#Eliminación NA´s en apartado otros estudios 
fs <- fs[!is.na(fs$EST_B11_1),]
tail(fs)
rownames(fs) <- NULL #Reseteando el indice 
fs_backup <- fs

summary(fs$EST_B11_1) #Otros estudios GRADO (¿inluye dobles grados?)
summary(fs$EST_B14_1) #(Es posible que level "2" sean dobles grados )
#Eliminar todos menos los que sean posibles dobles grados
fs <- fs[fs$EST_B11_1== 2,]
fs_backup2 <- fs

summary(fs$EST_B11_2) #Otros estuidios Master ( No queremos estudiantes de master)
fs <- fs[fs$EST_B11_2== 2,]
# Primero ver si hay diferencias significaticas en la realización de master entre egrasadso de publica y privada
summary(fs$EST_B11_3) #Otros estuidios Doctorado 
fs <- fs[fs$EST_B11_3== 2,]
#Igual
summary(fs$EST2_NC) #Estudios en curso 

#Otras Variables post-treatment
summary(fs$HO_F1) #Estado civil
summary(fs$TRBPRN2) #Situación de inactividad 
summary(fs$ACT) #Activo
summary(fs$TR_SUELDO) #Sueldo Mensual Neto 


#---------------- Tabla Descriptiva  -------------------

preVars = PRIV ~ SEXO + EDAD + NACIO + RAMA + DISCA + PAIS_NAC_MADRE +
  PAIS_NAC_PADRE + ESTUDIOS_PADRE + ESTUDIOS_MADRE + BECAg + BECAe + 
  BECAef + ERAMUS + MOTIV + TIC + INGL + 
  CURSOidi + CURSOinf + CURSOotro + PRACTICAS + TRBPRN1

table_one <- tableby(preVars, data = fs) 
summary(table_one, title = "Estadísticos descriptivos")


postVars = PRIV ~  EMPLEO + TRBPRN1 + TRBPRN2 + TR_SUELDO + HATR_E11 + 
  HO_F1 
table_two <- tableby(postVars, data = fs)  
summary(table_two, title = " Distribution of post-treatment variables")

#---------------- Matriz de correlaciones  -------------------

covfs <- data.frame(fs$EMPLEO, fs$PRIV , fs$SEXO , fs$EDAD , fs$NACIO , fs$RAMA , fs$DISCA , fs$PAIS_NAC_MADRE ,
                      fs$PAIS_NAC_PADRE , fs$ESTUDIOS_PADRE , fs$ESTUDIOS_MADRE , fs$BECAg , fs$BECAe , 
                      fs$BECAe , fs$BECAef , fs$ERAMUS , fs$MOTIV, fs$TIC , fs$INGL , 
                      fs$CURSOidi , fs$CURSOinf , fs$CURSOotro , fs$PRACTICAS)
corrM <- hetcor(covfs)
corrM
det(corrM)
#----------------  MODELO: Logit sin controles  -------------------

m1 <- glm(EMPLEO ~ PRIV,
          family = binomial (link = logit), data= fs, x=TRUE)
summary(m1)
coeftest(m1, vcov. = vcovHC, type = "HC1")
PseudoR2(m1, which = "all")
plogis(predict(m1))

#Efecto Marginal
maBina(m1, x.mean = FALSE, rev.dum = TRUE, digits = 5)


#----------------  MODELO: Logit con controles -------------------
#Por RAMA: 
FormulaR <- EMPLEO ~ PRIV + SEXO + EDAD + NACIO + RAMA + DISCA  + ESTUDIOS_MADRE + ESTUDIOS_PADRE + BECAg + BECAe + 
  ERAMUS +  MOTIV+ TIC + INGL +CURSOidi + CURSOinf + CURSOotro + PRACTICAS    
#Eliminadas por Multicolinealidad PAIS_NAC_MADRE + PAIS_NAC_PADRE + BECAef + ESTUDIOS_PADRE
m2 <- glm(FormulaR, family = binomial (link = logit), data= fs, x=TRUE)
summary(m2)
coeftest(m2, vcov. = vcovHC, type = "HC1")
PseudoR2(m2, which = "all")
plogis(predict(m2))

#Efecto Marginal
maBina(m2, x.mean = FALSE, rev.dum = TRUE, digits = 5)




#Por AMBITO: 
FormulaA <- EMPLEO ~ PRIV + SEXO + EDAD + NACIO + AMBITO + DISCA + ESTUDIOS_PADRE + 
  ESTUDIOS_MADRE +  BECAg + BECAe + ERAMUS + MOTIV + TIC + INGL + 
  CURSOidi + CURSOinf + CURSOotro + PRACTICAS
#Eliminadas por Multicolinealidad PAIS_NAC_MADRE + PAIS_NAC_PADRE + BECAef 
m3 <- glm(FormulaA, family = binomial (link = logit), data= fs, x=TRUE)
summary(m3)
coeftest(m3, vcov. = vcovHC, type = "HC1")
PseudoR2(m3, which = "all")
plogis(predict(m3))

#Efecto Marginal
maBina(m3, x.mean = FALSE, rev.dum = TRUE, digits = 3) 


#----------------  Función de supervivencia Kaplan Meier -------------------

library(survival)
summary(fsu$HATR_E11)
fsu <- fs[!fs$HATR_E11 == 9,]
fsu <- fsu[!is.na(fsu$HATR_E11),]
fsu$HATR_E11 <- as.numeric(fsu$HATR_E11)
km.model <- survfit(Surv(fsu$HATR_E11) ~ fsu$PRIV + fsu$SEXO + fsu$EDAD + fsu$NACIO + fsu$RAMA + fsu$DISCA + fsu$PAIS_NAC_MADRE +
                      fsu$PAIS_NAC_PADRE + fsu$ESTUDIOS_PADRE + fsu$ESTUDIOS_MADRE + fsu$BECAg + fsu$BECAe + 
                      + fsu$BECAe + fsu$BECAef + fsu$ERAMUS +  fsu$MOTIV+ fsu$TIC + fsu$INGL + 
                      fsu$CURSOidi + fsu$CURSOinf + fsu$CURSOotro + fsu$PRACTICAS, type= "kaplan-meier")
summary(km.model)



?ggsurvplot
h <- ggsurvplot(km.model, data = fsu,
           linetype = "strata", xlim = c(1,7), 
            font.tickslab = c(10, "bold", "red"))
h + xlab("Tiempo desempleo") + ylab("Probabilidad") +
  ggpar(h, xticks.by= 7, yticks.by=10)
  





#----------------  Comprobando balance   -------------------

preVars2 <- c("SEXO" , "EDAD" , "NACIO" , "RAMA" , "DISCA" , "PAIS_NAC_MADRE" ,
              "PAIS_NAC_PADRE" , "ESTUDIOS_PADRE" , "ESTUDIOS_MADRE" , "BECAg" , "BECAe" , 
              "BECAcu" , "BECAe" , "BECAef" , "ERAMUS" , "SICUE" , "MOTIV", "TIC" , "INGL" , 
              "CURSOidi" , "CURSOinf" , "CURSOotro" , "PRACTICAS" , "IDIOMAS")

imbalance(group=fs$PRIV, data=fs[preVars2])
L1 <-  imbalance(group=fs$PRIV, data=fs[preVars2])
L1


#---------------- MODELO: Propensity Score Matching (PSM) -------------------
#Por RAMA:

FormulaRM <- PRIV ~ SEXO + EDAD + NACIO + RAMA + DISCA + 
  ESTUDIOS_PADRE + ESTUDIOS_MADRE +  BECAg + BECAe + 
  ERAMUS + MOTIV+ TIC + INGL + 
  CURSOidi + CURSOinf + CURSOotro + PRACTICAS 
m.outNR <- matchit(FormulaRM,
                   data = fs, method = "nearest",
                   ratio = 1, replace=T, caliper=0.25)
summary(m.outNR)
plot(m.outNR, type = "jitter") #Plot 1 
plot(m.outNR, type = "hist") #Plot 2
NearestR <- match.data(m.outNR) #MatchedData 


L2 <-  imbalance(group=NearestR$PRIV, data=NearestR[preVars2])
L2

m4 <- glm(FormulaR, family = binomial (link = logit), data= NearestR, x=TRUE)
summary(m4)
coeftest(m4, vcov. = vcovHC, type = "HC1")
PseudoR2(m4, which = "all")
plogis(predict(m4))

#Efecto Marginal
maBina(m4, x.mean = FALSE, rev.dum = TRUE, digits = 3)

#Por AMBITO: 
FormulaRA <- PRIV ~ SEXO + EDAD + NACIO + AMBITO + DISCA  + 
  ESTUDIOS_PADRE + ESTUDIOS_MADRE +  BECAg + BECAe + 
  BECAef + ERAMUS +  MOTIV+ TIC + INGL + 
  CURSOidi + CURSOinf + CURSOotro + PRACTICAS
m.outNA <- matchit(FormulaRA,
                   data = fs, method = "nearest",
                   ratio = 1, replace=F, caliper=0.25)
summary(m.outNA)
plot(m.outNA, type = "jitter") #Plot 1 
plot(m.outNA, type = "hist") #Plot 2
NearestA <- match.data(m.outNA) #MatchedData 


L3 <-  imbalance(group=NearestA$PRIV, data=NearestA[preVars2])
L3

m5 <- glm(FormulaA, family = binomial (link = logit), data= NearestA, x=TRUE)
summary(m5)
coeftest(m5, vcov. = vcovHC, type = "HC1")
PseudoR2(m5, which = "all")
plogis(predict(m5))

#Efecto Marginal
maBina(m5, x.mean = FALSE, rev.dum = TRUE, digits = 3)

#---------------- MODELO: PSM con Optimal Matching -------------------

#Por Rama 
options("optmatch_max_problem_size" = Inf)
m.outOR <- matchit(FormulaRM, data = fs, method = "full")
summary(m.outOR)
plot(m.outOR, type = "jitter")
plot(m.outOR, type = "hist")
OptimalR <- match.data(m.outOR) 

L4 <-  imbalance(group=OptimalR$PRIV, data=OptimalR[preVars2])
L4

m6 <- glm(FormulaR, family = binomial (link = logit), data= OptimalR, x=TRUE)
summary(m6)
coeftest(m6, vcov. = vcovHC, type = "HC1")
PseudoR2(m6, which = "all")
plogis(predict(m6))

#Efecto Marginal
maBina(m6, x.mean = FALSE, rev.dum = TRUE, digits = 3)

#Por Ambito
options("optmatch_max_problem_size" = Inf)
m.outOA <- matchit(FormulaRA, data = fs, method = "full")
summary(m.outOA)
plot(m.outOA, type = "jitter")
plot(m.outOA, type = "hist")
OptimalA <- match.data(m.outOA) 

L5 <-  imbalance(group=OptimalA$PRIV, data=OptimalA[preVars2])
L5

m7 <- glm(FormulaA, family = binomial (link = logit), data= OptimalA, x=TRUE)
summary(m7)
coeftest(m7, vcov. = vcovHC, type = "HC1")
PseudoR2(m7, which = "all")
plogis(predict(m7))

#Efecto Marginal
maBina(m7, x.mean = FALSE, rev.dum = TRUE, digits = 3)

#---------------- MODELO: Exact Matching  -------------------
#Por Rama 

m.outER <- matchit(FormulaRM, data = fs, method = "exact")
summary(m.outER)
ExactR <- match.data(m.outER) 

L6 <-  imbalance(group=ExactR$PRIV, data=ExactR[preVars2])
L6
str(ExactR)
m8 <- glm(EMPLEO ~ PRIV , family = binomial (link = logit), data= ExactR, x=TRUE)
summary(m8)
coeftest(m8, vcov. = vcovHC, type = "HC1")
PseudoR2(m8, which = "all")
plogis(predict(m8))

#Efecto Marginal
maBina(m8, x.mean = FALSE, rev.dum = TRUE, digits = 3)

#Por Ambito

m.outEA <- matchit(FormulaRA, data = fs, method = "exact")
summary(m.outEA)

ExactA <- match.data(m.outEA) 

L7 <-  imbalance(group=ExactA$PRIV, data=ExactA[preVars2])
L7

m9 <- glm(EMPLEO ~ PRIV + SEXO + EDAD + AMBITO + 
            ESTUDIOS_PADRE + ESTUDIOS_MADRE + TIC + INGL + 
            + PRACTICAS, family = binomial (link = logit), data= OptimalA, x=TRUE)
summary(m9)
coeftest(m9, vcov. = vcovHC, type = "HC1")
PseudoR2(m9, which = "all")
plogis(predict(m9))

#Efecto Marginal
maBina(m9, x.mean = FALSE, rev.dum = TRUE, digits = 3)

#----------------  MODELO: Logit sin controles BACKUP (Con estudiantes de master) -------------------

m1 <- glm(EMPLEO ~ PRIV,
          family = binomial (link = logit), data= fs_backup2, x=TRUE)
summary(m1)
coeftest(m1, vcov. = vcovHC, type = "HC1")
PseudoR2(m1, which = "all")
plogis(predict(m1))

#Efecto Marginal
maBina(m1, x.mean = FALSE, rev.dum = TRUE, digits = 5)


#----------------  MODELO: Logit con controles BACKUP (Con estudiantes de master) -------------------
#Por RAMA: 
FormulaR <- EMPLEO ~ PRIV + SEXO + EDAD + NACIO + RAMA + DISCA + PAIS_NAC_MADRE +
  PAIS_NAC_PADRE + ESTUDIOS_PADRE + ESTUDIOS_MADRE + BECAg + BECAe + 
  + BECAe + BECAef + ERAMUS +  MOTIV+ TIC + INGL + 
  CURSOidi + CURSOinf + CURSOotro + PRACTICAS + EST_B11_2 + EST_B11_3
m2 <- glm(FormulaR, family = binomial (link = logit), data= fs_backup2, x=TRUE)
summary(m2)
coeftest(m2, vcov. = vcovHC, type = "HC1")
PseudoR2(m2, which = "all")
plogis(predict(m2))

#Efecto Marginal
maBina(m2, x.mean = FALSE, rev.dum = TRUE, digits = 5)




#Por AMBITO: 
FormulaA <- EMPLEO ~ PRIV + SEXO + EDAD + NACIO + AMBITO + DISCA + PAIS_NAC_MADRE +
  PAIS_NAC_PADRE + ESTUDIOS_PADRE + ESTUDIOS_MADRE +  BECAg + BECAe + 
  + BECAe + BECAef + ERAMUS + MOTIV + TIC + INGL + 
  CURSOidi + CURSOinf + CURSOotro + PRACTICAS + EST_B11_2 + EST_B11_3

m3 <- glm(FormulaA, family = binomial (link = logit), data= fs_backup2, x=TRUE)
summary(m3)
coeftest(m3, vcov. = vcovHC, type = "HC1")
PseudoR2(m3, which = "all")
plogis(predict(m3))

#Efecto Marginal
maBina(m3, x.mean = FALSE, rev.dum = TRUE, digits = 3)


