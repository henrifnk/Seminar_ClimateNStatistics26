mod = lm(Sepal.Length ~ ., data = iris)
saveRDS(mod, "work/00-introduction/results/mod_iris.Rda")

