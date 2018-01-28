# Dane dotycza…ce spamu
d = read.csv("data.csv", header = FALSE, sep = ";")
names = read.csv("names.csv", header = FALSE, sep = ";")
names(d) = sapply((1:nrow(names)),function(i) toString(names[i,1]))
d$y = as.factor(d$y)
# próba 1000 z 4601 emaili
sample = d[sample(nrow(d), 1000),]

#install.packages('caret')
#install.packages('kernlab')
#install.packages('doParallel')

library('caret')
library('kernlab')
library('doParallel')

trainIndex = createDataPartition(sample$y, p = .8, list = FALSE, times = 1)
# próba uczaca
dataTrain = sample[ trainIndex,]
# próba testowa
dataTest  = sample[-trainIndex,]

# do przyspieszenia obliczen
registerDoParallel(cores=5)

#SVM model:

### znajdowanie optymalnej wartosci dostrajanego parametru
sigDist = sigest(y ~ ., data = dataTrain, frac = 1)
### tworzenie ramki danych z 2 parametrami: .sigma i .C
svmTuneGrid = data.frame(.sigma = sigDist[[1]], .C = 2^(-2:7))

### model x
x <- train(y ~ .,
           data = dataTrain,
           method = "svmRadial",
           preProc = c("center", "scale"),
           tuneGrid = svmTuneGrid,
           trControl = trainControl(method = "repeatedcv", repeats = 5, 
                                    classProbs =  FALSE))

#Ocena / predykcja modelu
pred <- predict(x,dataTest[,1:57])

(wynik <- confusionMatrix(pred,dataTest$y) )


