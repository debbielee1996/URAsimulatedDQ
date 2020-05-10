list.of.packages <- c('tibble', 'dplyr', 'readr', 'missForest', 'randomForest', 'mice', 
                      'doParallel', 'VIM', 'e1071', 'BaylorEdPsych', 'mvnmle',
                     'Amelia', 'psych', 'MissMech', 'Hmisc', 'missMDA', 'bpca',
                     'cluster', 'factoextra', 'Rtsne', 'fpc', 'clValid')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tibble)
library(dplyr)
library(readr)
library(missForest)
library(randomForest)
library(mice)
library(doParallel)
library(VIM)
library(e1071)
library(BaylorEdPsych)
library(mvnmle)
library(Amelia)
library(psych)
library(MissMech)
library(Hmisc)
library(missMDA)
library(bpca)
library(cluster)
library(factoextra)
library(Rtsne)
library(fpc)
library(clValid)

# data with simulated missing values
hdb <- data.frame(read_csv('C:/Users/d-ebb/Desktop/hdb_missing.csv', col_names=TRUE))

# 'true' values
hdb_complete <- data.frame(read_csv('C:/Users/d-ebb/Desktop/hdb_complete.csv'))

hdb_subset <- hdb %>% subset(select = -c(block, street_name))

head(hdb_subset)

# change categorical variables into factor
hdb_subset$town = as.factor(hdb_subset$town)
hdb_subset$flat_type = as.factor(hdb_subset$flat_type)
hdb_subset$flat_model = as.factor(hdb_subset$flat_model)

print(lapply(hdb_subset, class))

head(hdb_subset)

missmap(hdb_subset)

# on dataset BEFORE missingness simulation
hist(hdb_complete$floor_area_sqm)
hist(hdb_complete$resale_price)
hist(hdb_complete$remaining_lease)
hist(hdb_complete$storey_no)
hist(hdb_complete$lease_commence_date)

# on dataset AFTER missingness simulation
hist(hdb_subset$floor_area_sqm)
hist(hdb_subset$resale_price)
hist(hdb_subset$remaining_lease)
hist(hdb_subset$storey_no)
hist(hdb_subset$lease_commence_date)

hdb_subset_shadowmatrix <- as.data.frame(abs(is.na(hdb_subset)))

head(hdb_subset_shadowmatrix)

sum(hdb_subset_shadowmatrix)
sum(is.na(hdb_subset))

#Extracting variables that have some missing values. Conclusion that no variables have particularly strong r/s
hdb_subset_missingvar <- hdb_subset_shadowmatrix[which(sapply(hdb_subset_shadowmatrix, sd) > 0)]
cor(hdb_subset_missingvar)

# for df with <50 variables
LittleMCAR(hdb_subset)

# fail to reject null hypo that data is MCAR

hdb_subset_encoded <- hdb_subset

hdb_subset_encoded$town_encoded <- as.numeric(hdb_subset_encoded$town)
hdb_subset_encoded$flat_type_encoded <- as.numeric(hdb_subset_encoded$flat_type)
hdb_subset_encoded$flat_model_encoded <- as.numeric(hdb_subset_encoded$flat_model)


hdb_subset_encoded_final <- hdb_subset_encoded %>% subset(select = -c(town, flat_type, flat_model))

head(hdb_subset_encoded_final)

# will go out of memory if dataset is too huge
out <- TestMCARNormality(data = hdb_subset_encoded_final)

out

missingness_matrix <- as.data.frame(abs(is.na(hdb_subset)))
original_shadowmatrix <- data.frame(hdb_subset, missingness_matrix)

pairs.panels(original_shadowmatrix)

hdb_meanimputeset <- hdb_subset

start_time <- Sys.time()

hdb_meanimputeset$floor_area_sqm[is.na(hdb_meanimputeset$floor_area_sqm)] = mean(hdb_meanimputeset$floor_area_sqm, na.rm=TRUE)
hdb_meanimputeset$resale_price[is.na(hdb_meanimputeset$resale_price)] = mean(hdb_meanimputeset$resale_price, na.rm=TRUE)
hdb_meanimputeset$remaining_lease[is.na(hdb_meanimputeset$remaining_lease)] = mean(hdb_meanimputeset$remaining_lease, na.rm=TRUE)
hdb_meanimputeset$storey_no[is.na(hdb_meanimputeset$storey_no)] = mean(hdb_meanimputeset$storey_no, na.rm=TRUE)
hdb_meanimputeset$lease_commence_date[is.na(hdb_meanimputeset$lease_commence_date)] = mean(hdb_meanimputeset$lease_commence_date, na.rm=TRUE)

end_time <- Sys.time()
end_time - start_time

Mode <- function (x, na.rm) {
    xtab <- table(x)
    xmode <- names(which(xtab == max(xtab)))
    if (length(xmode) > 1) xmode <- ">1 mode"
    return(xmode)
}

start_time <- Sys.time()

hdb_meanimputeset$town[is.na(hdb_meanimputeset$town)] = Mode(hdb_meanimputeset$town, na.rm=TRUE)
hdb_meanimputeset$flat_type[is.na(hdb_meanimputeset$flat_type)] = Mode(hdb_meanimputeset$flat_type, na.rm=TRUE)
hdb_meanimputeset$flat_model[is.na(hdb_meanimputeset$flat_model)] = Mode(hdb_meanimputeset$flat_model, na.rm=TRUE)

end_time <- Sys.time()
end_time - start_time

# horrible results
mixError(hdb_meanimputeset, hdb_subset, subset(hdb_complete, select = -c(block, street_name)))

start_time <- Sys.time()

hdb_impute <- missForest(hdb_subset)

end_time <- Sys.time()
end_time - start_time

sum(is.na(hdb_impute))

print(hdb_impute)

mixError(hdb_impute$ximp, hdb_subset, subset(hdb_complete, select = -c(block, street_name)))

sum(is.na(hdb_subset$town))

#registerDoParallel(cores = 4)

#hdb_impute_parallel <- missForest(hdb_subset, parallelize = "variables")

#print(hdb_impute_parallel)

start_time <- Sys.time()

hdb_knnimpute <- kNN(hdb_subset, k=5, imp_var=FALSE)

end_time <- Sys.time()
end_time - start_time

head(hdb_knnimpute)

# higher amount of error compared to random forest
mixError(hdb_knnimpute, hdb_subset, subset(hdb_complete, select = -c(block, street_name)))

n <- 5 # number of output datasets

start_time <- Sys.time()

hdb_miceimpute <- mice(hdb_subset, m=n)

end_time <- Sys.time()
end_time - start_time

# get average of NRMSE and PFC
mice_stats <- data.frame(mixError(complete(hdb_miceimpute, 1), hdb_subset, subset(hdb_complete, select = -c(block, street_name))))

for (i in 2:n) {
    mice_stats <- cbind(mice_stats, data.frame(mixError(complete(hdb_miceimpute, i), hdb_subset, subset(hdb_complete, select = -c(block, street_name)))))
}

mice_stats_avg <- rowMeans(mice_stats[,])

head(mice_stats)
mice_stats_avg

# look at marginal distributions of observed data vs imputed data. see if imputations are reasonable
densityplot(hdb_miceimpute)

# e.g. apply linear model. analyze relations of some variables to a response variable
fit <- with(data = hdb_miceimpute, exp = lm(resale_price ~ town + flat_type + flat_model + floor_area_sqm + remaining_lease + storey_no + lease_commence_date))
summary(pool(fit), conf.int=TRUE)

summary(pool.r.squared(fit))

start_time <- Sys.time()

hmisc_impute <- aregImpute(~ town + flat_type + floor_area_sqm + flat_model + resale_price + 
                           remaining_lease + storey_no + lease_commence_date, hdb_subset, n.impute=5, nk=5, tlinear=FALSE)

end_time <- Sys.time()
end_time - start_time

hmisc_impute_df <- as.data.frame(impute.transcan(hmisc_impute, imputation=3, data=hdb_subset, list.out=TRUE, pr=FALSE, check=FALSE))  

# change impute variables into numeric
hmisc_impute_df$floor_area_sqm = as.numeric(hmisc_impute_df$floor_area_sqm)
hmisc_impute_df$resale_price = as.numeric(hmisc_impute_df$resale_price)
hmisc_impute_df$remaining_lease = as.numeric(hmisc_impute_df$remaining_lease)
hmisc_impute_df$storey_no = as.numeric(hmisc_impute_df$storey_no)
hmisc_impute_df$lease_commence_date = as.numeric(hmisc_impute_df$lease_commence_date)

mixError(hmisc_impute_df, hdb_subset, subset(hdb_complete, select = -c(block, street_name)))

# get average of NRMSE and PFC
hmisc_impute_df <- as.data.frame(impute.transcan(hmisc_impute, imputation=1, data=hdb_subset, list.out=TRUE, pr=FALSE, check=FALSE))  

# change impute variables into numeric
hmisc_impute_df$floor_area_sqm = as.numeric(hmisc_impute_df$floor_area_sqm)
hmisc_impute_df$resale_price = as.numeric(hmisc_impute_df$resale_price)
hmisc_impute_df$remaining_lease = as.numeric(hmisc_impute_df$remaining_lease)
hmisc_impute_df$storey_no = as.numeric(hmisc_impute_df$storey_no)
hmisc_impute_df$lease_commence_date = as.numeric(hmisc_impute_df$lease_commence_date)

hmisc_stats <- data.frame(mixError(hmisc_impute_df, hdb_subset, subset(hdb_complete, select = -c(block, street_name))))

for (i in 2:n) {
    # get average of NRMSE and PFC
    hmisc_impute_df <- as.data.frame(impute.transcan(hmisc_impute, imputation=i, data=hdb_subset, list.out=TRUE, pr=FALSE, check=FALSE))  

    # change impute variables into numeric
    hmisc_impute_df$floor_area_sqm = as.numeric(hmisc_impute_df$floor_area_sqm)
    hmisc_impute_df$resale_price = as.numeric(hmisc_impute_df$resale_price)
    hmisc_impute_df$remaining_lease = as.numeric(hmisc_impute_df$remaining_lease)
    hmisc_impute_df$storey_no = as.numeric(hmisc_impute_df$storey_no)
    hmisc_impute_df$lease_commence_date = as.numeric(hmisc_impute_df$lease_commence_date)

    hmisc_stats <- cbind(hmisc_stats, data.frame(mixError(hmisc_impute_df, hdb_subset, subset(hdb_complete, select = -c(block, street_name)))))
}


hmisc_stats_avg <- rowMeans(hmisc_stats[,])

head(hmisc_stats)
hmisc_stats_avg

start_time <- Sys.time()

famd_impute <- imputeFAMD(hdb_subset)

end_time <- Sys.time()
end_time - start_time

head(famd_impute$completeObs)

mixError(famd_impute$completeObs, hdb_subset, subset(hdb_complete, select = -c(block, street_name)))

# comparison of one example
head(famd_impute$completeObs[25,])
head(hdb_subset[25,])
head(hdb_complete[25,])

# change categorical variables into factor
hdb_bpca <- hdb_subset

# duplicate original categorical cols
hdb_bpca$town_original = hdb_bpca$town
hdb_bpca$flat_type_original = hdb_bpca$flat_type
hdb_bpca$flat_model_original = hdb_bpca$flat_model


# encode categorical cols
hdb_bpca$town = as.numeric(as.factor(hdb_bpca$town))
hdb_bpca$flat_type = as.numeric(as.factor(hdb_bpca$flat_type))
hdb_bpca$flat_model = as.numeric(as.factor(hdb_bpca$flat_model))

head(hdb_bpca)

start_time <- Sys.time()

bpca_impute <- MIPCA(subset(hdb_bpca, select = -c(town_original, flat_type_original, flat_model_original)), nboot=100, method.mi="Bayes")

end_time <- Sys.time()
end_time - start_time

res.over<-Overimpute(bpca_impute)

print(res.over)

bpca_impute <- data.frame(bpca_impute$res.imputePCA)

# round categorical variables
bpca_impute$town <- round(bpca_impute$town)
bpca_impute$flat_type <- round(bpca_impute$flat_type)
bpca_impute$flat_model <- round(bpca_impute$flat_model)

head(bpca_impute)
sum(complete.cases(bpca_impute))

# create new df for original impute values vs encoded number
hdb_encoded_flat_type <- data.frame()
hdb_encoded_flat_type <- rbindlist(list(hdb_encoded_flat_type, distinct(hdb_bpca, flat_type_original, flat_type)))
hdb_encoded_flat_type <- hdb_encoded_flat_type %>% na.exclude

head(hdb_encoded_flat_type)

# create new df for original impute values vs encoded number
hdb_encoded_flat_model <- data.frame()
hdb_encoded_flat_model <- rbindlist(list(hdb_encoded_flat_model, distinct(hdb_bpca, flat_model_original, flat_model)))
hdb_encoded_flat_model <- hdb_encoded_flat_model %>% na.exclude

head(hdb_encoded_flat_model)

# create new df for original impute values vs encoded number
hdb_encoded_town <- data.frame()
hdb_encoded_town <- rbindlist(list(hdb_encoded_town, distinct(hdb_bpca, town_original, town)))
hdb_encoded_town <- hdb_encoded_town %>% na.exclude

head(hdb_encoded_town)

bpca_impute_test <- bpca_impute

bpca_impute_test$town <- hdb_encoded_town$town_original[match(bpca_impute_test$town, hdb_encoded_town$town)]
bpca_impute_test$flat_type <- hdb_encoded_flat_type$flat_type_original[match(bpca_impute_test$flat_type, hdb_encoded_flat_type$flat_type)]
bpca_impute_test$flat_model <- hdb_encoded_flat_model$flat_model_original[match(bpca_impute_test$flat_model, hdb_encoded_flat_model$flat_model)]

head(bpca_impute_test)

unique(bpca_impute_test$town)

head(bpca_impute_test)

mixError(bpca_impute_test, hdb_subset, subset(hdb_complete, select = -c(block, street_name)))

n <- 5

start_time <- Sys.time()

hdb_cartimpute <- mice(hdb_subset, meth='cart', m=n)

end_time <- Sys.time()
end_time - start_time

mixError(complete(hdb_cartimpute, 2), hdb_subset, subset(hdb_complete, select = -c(block, street_name)))

# get average of NRMSE and PFC
cart_stats <- data.frame(mixError(complete(hdb_cartimpute, 1), hdb_subset, subset(hdb_complete, select = -c(block, street_name))))

for (i in 2:n) {
    cart_stats <- cbind(cart_stats, data.frame(mixError(complete(hdb_cartimpute, i), hdb_subset, subset(hdb_complete, select = -c(block, street_name)))))
}

cart_stats_avg <- rowMeans(cart_stats[,])

head(cart_stats)
cart_stats_avg

# e.g. apply linear model. analyze relations of some variables to a response variable
fit <- with(data = hdb_cartimpute, exp = lm(resale_price ~ town + flat_type + flat_model + floor_area_sqm + remaining_lease + storey_no + lease_commence_date))
summary(pool(fit), conf.int=TRUE)

n <- 5
start_time <- Sys.time()

hdb_rfmiceimpute <- mice(hdb_subset, meth='rf', m=n)

end_time <- Sys.time()
end_time - start_time

# get average of NRMSE and PFC
rfmice_stats <- data.frame(mixError(complete(hdb_rfmiceimpute, 1), hdb_subset, subset(hdb_complete, select = -c(block, street_name))))

for (i in 2:n) {
    rfmice_stats <- cbind(rfmice_stats, data.frame(mixError(complete(hdb_rfmiceimpute, i), hdb_subset, subset(hdb_complete, select = -c(block, street_name)))))
}

rfmice_stats_avg <- rowMeans(rfmice_stats[,])

head(rfmice_stats)
rfmice_stats_avg

results <- data.frame()
rowNum <- 2227

stats_results <- data.frame()

stats_results <- rbind(stats_results, mixError(hdb_meanimputeset, hdb_subset, subset(hdb_complete, select = -c(block, street_name))))
stats_results <- rbind(stats_results, mixError(famd_impute$completeObs, hdb_subset, subset(hdb_complete, select = -c(block, street_name))))
stats_results <- rbind(stats_results, mixError(bpca_impute_test, hdb_subset, subset(hdb_complete, select = -c(block, street_name))))
stats_results <- rbind(stats_results, mixError(hdb_impute$ximp, hdb_subset, subset(hdb_complete, select = -c(block, street_name))))
stats_results <- rbind(stats_results, mixError(hdb_knnimpute, hdb_subset, subset(hdb_complete, select = -c(block, street_name))))

stats_results

# single imputation results
results<- rbind(results, hdb_meanimputeset[rowNum,])
results<- rbind(results, famd_impute$completeObs[rowNum,])
results<- rbind(results, bpca_impute_test[rowNum,])
results<- rbind(results, hdb_impute$ximp[rowNum,])
results<- rbind(results, hdb_knnimpute[rowNum,])

results<- cbind(list(imputation_technique=c('mean/mode', 'famd', 'bpca', 'randomforest', 'knn')), results)


# multiple imputation results
for (i in 1:5) {
    results<- rbind(results, cbind(complete(hdb_miceimpute, i)[rowNum,], list(imputation_technique=c('mice'))))
    stats_results <- rbind(stats_results, mixError(complete(hdb_miceimpute, i), hdb_subset, subset(hdb_complete, select = -c(block, street_name))))

}

for (i in 1:5) {
    results<- rbind(results, cbind(complete(hdb_cartimpute, i)[rowNum,], list(imputation_technique=c('cart'))))
    stats_results <- rbind(stats_results, mixError(complete(hdb_cartimpute, i), hdb_subset, subset(hdb_complete, select = -c(block, street_name))))
}

for (i in 1:5) {
    results<- rbind(results, cbind(complete(hdb_rfmiceimpute, i)[rowNum,], list(imputation_technique=c('rfmice'))))
    stats_results <- rbind(stats_results, mixError(complete(hdb_rfmiceimpute, i), hdb_subset, subset(hdb_complete, select = -c(block, street_name))))

}

for (i in 1:5) {
    hmisc_impute_df <- as.data.frame(impute.transcan(hmisc_impute, imputation=i, data=hdb_subset, list.out=TRUE, pr=FALSE, check=FALSE)) 
    # change impute variables into numeric
    hmisc_impute_df$floor_area_sqm = as.numeric(hmisc_impute_df$floor_area_sqm)
    hmisc_impute_df$resale_price = as.numeric(hmisc_impute_df$resale_price)
    hmisc_impute_df$remaining_lease = as.numeric(hmisc_impute_df$remaining_lease)
    hmisc_impute_df$storey_no = as.numeric(hmisc_impute_df$storey_no)
    hmisc_impute_df$lease_commence_date = as.numeric(hmisc_impute_df$lease_commence_date)
    
    results<- rbind(results, cbind(hmisc_impute_df[rowNum,], list(imputation_technique=c('hmisc'))))
    stats_results <- rbind(stats_results, mixError(hmisc_impute_df, hdb_subset, subset(hdb_complete, select = -c(block, street_name))))
}

results <- cbind(results, stats_results)
colnames(results)[10] <- "NRMSE"
colnames(results)[11] <- "PFC"

hdb_actual_result <- subset(hdb_complete[rowNum,],  select=-c(block, street_name))
hdb_actual_result <- cbind(imputation_technique='ACTUAL', hdb_actual_result, NRMSE=0, PFC=0)

results <- rbind(results, hdb_actual_result)

write_csv(results, "C:/Users/d-ebb/Desktop/stats.csv")

results

hdb_missforestimpute <- hdb_impute$ximp

start_time <- Sys.time()

gower_dist <- daisy(hdb_missforestimpute,
                    metric = "gower",
                    type = list(logratio = 3))

end_time <- Sys.time()
end_time - start_time

class(gower_dist)

summary(gower_dist)

start_time <- Sys.time()

gower_mat <- as.matrix(gower_dist)

end_time <- Sys.time()
end_time - start_time

# Output most similar pair
start_time <- Sys.time()

hdb_missforestimpute[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

end_time <- Sys.time()
end_time - start_time

# extract closest observations in each column i
# bigger picture is to select the closest observation for selected units
start_time <- Sys.time()

for (i in 1:5)
{
  #Extract all rows of the ith column and find the maxiumum value in the same column
    print(hdb_missforestimpute[
  which(gower_mat == min(gower_mat[gower_mat[,i]>0,i]),
        arr.ind = TRUE)[1, ], ])
}

end_time <- Sys.time()
end_time - start_time

n <- 20
sil_width <- c(NA)

start_time <- Sys.time()

for(i in 2:n){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:n, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:n, sil_width)

end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()

pam_fit <- pam(gower_dist, diss = TRUE, k = 12)

end_time <- Sys.time()
end_time - start_time

# Dunn Index
start_time <- Sys.time()

dunn_value <- dunn(gower_mat, pam_fit$clustering)
dunn_value

end_time <- Sys.time()
end_time - start_time

pam_results <- hdb_missforestimpute %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

# Silhouette Coefficient
start_time <- Sys.time()

si <- silhouette(pam_fit)
summary(si)

end_time <- Sys.time()
end_time - start_time

plot(si)

# hclust <- eclust(subset(hdb_missforestimpute, select = -c(town, flat_type, flat_model)), "hclust", k=12, method="complete")
# not enough memory for this method of hclust

start_time <- Sys.time()

hier_cluster <- hclust(gower_dist, method  = "complete")

end_time <- Sys.time()
end_time - start_time

nc <- 2 ## number of clusters      
start_time <- Sys.time()

hcluster <- cutree(hier_cluster,nc)

end_time <- Sys.time()
end_time - start_time

# Dunn Index
start_time <- Sys.time()

dunn_value <- dunn(gower_mat, hcluster)
dunn_value

end_time <- Sys.time()
end_time - start_time

# Silhouette Coefficient
start_time <- Sys.time()

si <- silhouette(hcluster, gower_mat)
summary(si)

end_time <- Sys.time()
end_time - start_time














