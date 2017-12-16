Test = createDataset()

Test_2 = Test

# str(Test_2)

Test_2 = data.frame(Test_2, connection_type = 'bad')

Test_2$connection_type = as.character(Test_2$connection_type)

# Test_2$attack_type[Test_2$attack_type != 'normal'] = 'bad'

Test_2$connection_type[Test_2$attack_type == 'normal'] = 'good'

Test_2$connection_type = as.factor(Test_2$connection_type)

barplot(sort(table(Test_2$connection_type), decreasing = TRUE))

prop.table(table(Test_2$connection_type))

########################### Tests de modèle.

library("e1071")

Test_3 = Test_2[sample(nrow(Test_2)),]

n = 30001

Test_3 = Test_3[, -c(7,8,9,11,14,15,18,20,21,(ncol(Test_3)-1))]

Base_test = Test_3[(n+1):floor(n*0.3),]

Test_3 = Test_3[1:n,]

Mod_SVM = svm(connection_type~., data = Test_3, kernel="linear")

Pred = predict(Mod_SVM, Base_test[,-ncol(Base_test)])

library(SDMTools)

Pred = as.character(Pred)
Pred[Pred == 'good'] = 1
Pred[Pred == 'bad'] = 0

OBS = as.character(Base_test[,ncol(Base_test)])
OBS[OBS == 'good'] = 1
OBS[OBS == 'bad'] = 0

confusion.matrix(OBS, Pred, threshold = 0.5)

# ks.test(as.numeric(Test_3$logged_in),as.numeric(Test_3$connection_type))
chisq.test(as.numeric(Test_3$logged_in),as.numeric(Test_3$connection_type))

library(vcd)

TB_CT = table(as.numeric(Test_3$logged_in),as.numeric(Test_3$connection_type))

assocstats(TB_CT)  ## Cramer V = 0.819, grosse association.
