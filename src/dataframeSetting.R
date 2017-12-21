## Fichier: dataframeSetting.r
## Auteur : Eric HAMMEL
## Description : Importation du jeu de données et 
##               modifications nécessaires pour les diverses manipulations
## Date : 10 décembre 2018



#####################################################################################
#### NE PLUS EXECUTER CE FICHIER, IL SERVAIT UNIQUEMENT A LA CREATION DU FICHIER ####
############### mydata.txt, mydata.txt CONTIENT LES DONNEES PREPAREES ###############
#####################################################################################


## CE FICHIER A UNIQUEMENT POUR BUT DE MONTRER LES TRAITEMENTS QUI ONT ETE REALISES ##



#### Importation des données brutes ####



Test = createDataset()

Test_2 = Test



#### Modification de nos donnees, variable a prédire en 'bad'/'good' ####



Test_2 = data.frame(Test_2, connection_type = 'bad')

Test_2$connection_type = as.character(Test_2$connection_type)

# Test_2$attack_type[Test_2$attack_type != 'normal'] = 'bad'

Test_2$connection_type[Test_2$attack_type == 'normal'] = 'good'

Test_2$connection_type = as.factor(Test_2$connection_type)

### Preparation de nos donnees.

Test_3 = Test_2[sample(nrow(Test_2)),]

Test_3 = Test_3[, -c(7,8,9,11,14,15,18,20,21,(ncol(Test_3)-1))] # Sup vars inutiles

## Selection d un echantillon

n = 30001

Base_test = Test_3[(n+1):floor(n*0.3),]

Test_3 = Test_3[1:n,]

Test_3[,8] = as.factor(Test_3[,8])
Test_3[,13] = as.factor(Test_3[,13])



#### Selection des vars/tests de dependance. ####


#### Test Var quali/quali ####


# ks.test(as.numeric(Test_3$logged_in),as.numeric(Test_3$connection_type))
# chisq.test(as.numeric(Test_3$logged_in),as.numeric(Test_3$connection_type))
# 
# TB_CT = table(as.numeric(Test_3$logged_in),as.numeric(Test_3$connection_type))
# 
# assocstats(TB_CT)  ## Cramer V = 0.819, grosse association.

vec = 1:(ncol(Test_3)-1)
vec = vec[sapply(1:(ncol(Test_3)-1), function(x) is.factor(Test_3[1,x]))]

# cl <- makeCluster(2)
# registerDoParallel(cl)
# system.time(CRAMER.value(Test_3, vec))
# system.time(CRAMER.value.vec(Test_3, vec))
# system.time(CRAMER.value.vec.para(cl, Test_3, vec))
# stopCluster(cl)

CRAMER.value.vec(Test_3, vec)

## Au vu des resultats, 3 et 8 sont tres dep avec la var de sortie.
## 2 un peu


#### Test var quanti/quali ####


#### test diff deux echs ####

MEAN.classe = aggregate(x = Test_3[,-c(vec, ncol(Test_3))],
                        by = list(Test_3$connection_type), FUN = mean)

MEDIAN.classe = aggregate(x = Test_3[,-c(vec, ncol(Test_3))],
                          by = list(Test_3$connection_type), FUN = median)

SD.classe = aggregate(x = Test_3[,-c(vec, ncol(Test_3))],
                      by = list(Test_3$connection_type), FUN = sd)


#### test sans hyp sur la distribution. ####

# kruskal.test(Test_3$connection_type ~ Test_3$dst_bytes)
# kruskal.test(Test_3$dst_bytes ~ Test_3$connection_type)

vec2 = 1:(ncol(Test_3)-1)
vec2 = vec2[-vec]

Indep.quanti.quali(Test_3,vec2)

summary(glm(connection_type ~ . , data = Test_3[,-vec], family = binomial(link = "logit")))

## Au vu des resultats, dst_bytes, count, svr_count et dst_host_same_src_port_rate
## sont tres dep avec la var de sortie.

# dst_host_count, dst_host_svr_diff_host_rate, dst_host_diff_svr_rate, dst_host_srv_serror_rate.


#### Jeu de donnees apres tri ####

DATA.svm = Test_3[, -c(3,8, 2, 6,14,15,27, 23,26,28,30, 19:22)]

Base_test.svm = Base_test[, -c(3,8, 2, 6,14,15,27, 23,26,28,30, 19:22)]

u = DATA.svm[sample(1:nrow(DATA.svm)),-c(2,5,8)]

u = u[1:5000,]

# write.table(u, "mydata2.txt", sep=",")
