## ----setup,  include=FALSE-----------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- message=FALSE------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(plyr)
#install.packages('cowplot')
library(cowplot)
library(tibble)
library(flextable)
#install.packages('nortest')
library(nortest)
#install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)
library(corrplot)
library(RColorBrewer)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
AGP <- read.csv("~/Desktop/master_project/Master-Project-Natasa-Mortvanski/01_tidy_data/AGP_all.csv.gz", header = TRUE, sep = ",")

all_healthy <- read.csv("~/Desktop/master_project/Master-Project-Natasa-Mortvanski/01_tidy_data/AGP_healthy.csv.gz", header = TRUE, sep = ",")

nrow(AGP)
nrow(all_healthy)


## ---- include=FALSE------------------------------------------------------------------------------------------------------------------------------------------
# easy function for cross tabulating and plotting two variables

#install.packages("CGPfunctions")
library(CGPfunctions)

PlotXTabs(all_healthy, race, country_residence)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
metric <- c("shannon_entropy", "chao1", "menhinick", "margalef", "fisher_alpha", "simpson", "gini_index", "strong", "pielou_evenness", "faith_pd" )


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
colors <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
histo <- vector('list', length(metric))

for (i in 1:length(metric)){
  histo[[i]] <- all_healthy %>% ggplot(aes(x = .data[[metric[i]]])) +
    geom_histogram(aes(y=..density..), colour="black", fill="white", bins=30)+
    geom_density(alpha=.2, fill=colors[i]) +
    xlab(label = metric[i]) +
    ylab(label = "density")
}

grid.arrange(histo[[1]], histo[[2]],histo[[3]], histo[[4]],histo[[5]], histo[[6]],histo[[7]], histo[[8]],histo[[9]], histo[[10]], ncol=3, top = textGrob("Distributions of 1) Shannon's index  2) Chao1 3) Menhinick's index \n4) Margalef's index 5) Fisher's index 6) Simpson 7) Gini index 8) Strong's index 9) Pielou's evenness \nand 10) Faith's PD in healthy dataset", gp=gpar(fontsize=10,font=2)))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
pval_ks <- list()
stat_ks <- list()

for (i in 1:length(metric)){
  pval_ks[i] <- ks.test(all_healthy[[metric[i]]], 'pnorm')[2]
  stat_ks[i] <- ks.test(all_healthy[[metric[i]]], 'pnorm')[1]
}

stat_ks <- lapply(stat_ks, unname) %>% unlist(stat_ks)
pval_ks <- unlist(pval_ks)

data.frame(metric=metric, statistic=stat_ks, p.value=pval_ks) %>% flextable() %>%
    add_header_lines(values = "Results of the Kolmogorov-Smirnov Test of distribution normality")


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
pval_ad <- list()
stat_ad <- list()

for (i in 1:length(metric)){
  pval_ad[i] <- ad.test(all_healthy[[metric[i]]])[2]
  stat_ad[i] <- ad.test(all_healthy[[metric[i]]])[1]
}

stat_ad <- lapply(stat_ad, unname) %>% unlist(stat_ad)
pval_ad <- unlist(pval_ad)

data.frame(metric=metric, statistic=stat_ad, p.value=pval_ad) %>% flextable() %>%
    add_header_lines(values = "Results of the Anderson-Darling Test of distribution normality")


## ---- include=FALSE------------------------------------------------------------------------------------------------------------------------------------------
#str(all_healthy)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
library(corrplot)

metrics <- all_healthy[,2:11]
#metrics <- all_healthy[,2:9]
cor_matrix <- cor(metrics)

corrplot(cor_matrix, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
chart.Correlation(all_healthy[, 2:11], histogram=TRUE, pch=19)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# Identify numeric columns
num_cols <- unlist(lapply(all_healthy, is.numeric))

# Subset numeric columns of data
data_num <- all_healthy[ , num_cols]
data_num$birth_year <- NULL

# Save results as data frame
correlation_num <- as.data.frame(cor(data_num[-c(11:14)], data_num[-c(1:10)]))
correlation_num %>% tibble::rownames_to_column() %>% flextable()

# Plot correlation
cor_matrix <- cor(data_num[-c(1:10)], data_num[-c(11:14)])

corrplot(cor_matrix, tl.col = "black", tl.srt = 45)


## ---- message=FALSE------------------------------------------------------------------------------------------------------------------------------------------
# Extract categorical variables from all_healthy data frame

cat_cols <- unlist(lapply(all_healthy, is.character))

# Subset numeric columns of data
data_cat <- all_healthy[ , cat_cols]
data_cat$sample_id <- NULL


## ---- message=FALSE------------------------------------------------------------------------------------------------------------------------------------------
# Calculate regression coefficient for all categorical variables in relation to all alpha diversity metrics

correlation_cat <- data.frame(variable=character(0))

for (i in 1:ncol(data_cat)){
  correlation_cat[i,1] <- colnames(data_cat)[i]
}

for (j in 2:11){
  temp <- data.frame(col = numeric(0))
  for (i in 1:ncol(data_cat)){
    names(temp)[1] <- colnames(all_healthy)[j]
    model <- lm(all_healthy[,j] ~ data_cat[,i])
    sumry <- summary(model)
    r <- sqrt(sumry$r.squared)
    temp[i,1] <- r
  }
  correlation_cat <- cbind(correlation_cat, temp)
}

modelXY <- lm(all_healthy$shannon_entropy ~ data_cat$age_cat)


## model summary
sumryXY <- summary(modelXY)
sumryXY

## r-sq of model
rsqXY <- sumryXY$r.squared
rsqXY

#print(rsqXY)


## ---- message=FALSE------------------------------------------------------------------------------------------------------------------------------------------
# Extract first 10 variables with the biggest influence on alpha metrics

most_correlated <- data.frame(variable=character(20),regression_coefficient=numeric(20))

for (j in 2:11){
  temp <- data.frame(variable=character(20), regression_coefficient=numeric(20))
  names(temp)[2] <- paste("regression_coefficient", colnames(correlation_cat)[j], sep="_")
  temp[,1] <- head(correlation_cat[order(-correlation_cat[,j]),]$variable, 20)
  temp[,2] <- head(correlation_cat[order(-correlation_cat[,j]),][,j], 20)
  most_correlated <- cbind(most_correlated, temp)
}

most_correlated[,1:2] <- NULL


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
### Plot "correlations" of categorical variables

# Transform correlation_cat data frame to matrix
cor_matrix_cat <- correlation_cat %>% column_to_rownames(var = "variable")
cor_matrix_cat <- data.matrix(cor_matrix_cat)

# Scale regression coefficients from 0 to 1 and order matrix by the decreasing value of Shannon's entropy
dat <- as.matrix(apply(cor_matrix_cat, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))
dat <- dat[order(dat[,1], decreasing=TRUE),]

# Plot the "correlation" matrix
corrplot(dat[1:20,], tl.col = "black", tl.srt = 45)


## ---- include=FALSE------------------------------------------------------------------------------------------------------------------------------------------
#mu_shannon <- ddply(all_healthy, "sex", summarise, grp_mean_shannon = mean(shannon_entropy))

#shannon_density <- ggplot(all_healthy, aes(x=shannon_entropy, color=race)) +
#  geom_density()+
#  geom_vline(data=mu_shannon, aes(xintercept=grp_mean_shannon, color=race),
#             linetype="dashed")+
# labs(x ="Shannon's index")

#plot_shannon_box <- ggplot(all_healthy, aes(x=shannon_entropy, color=race)) +
#  geom_boxplot() +
# labs(x ="Shannon's index")

#plot_grid(shannon_density, plot_shannon_box, nrow = 2)


## ---- include=FALSE------------------------------------------------------------------------------------------------------------------------------------------
# library(RColorBrewer)
#
# compare <- c("sex", "age_cat",  "race", "bowel_movement_quality","milk_cheese_frequency", "bmi_cat")
#
# metric <- c("shannon_entropy", "chao1", "menhinick", "margalef", "inverse_simpson", "fisher_alpha", "gini_index", "strong" , "faith_pd")
#
# density <- vector('list', length(compare)*length(metric))
# box <- vector('list', length(compare)*length(metric))
#
#
# for (j in 1:length(compare)){
#   for (i in 1:length(metric)){
#     mean_line <- all_healthy %>% dplyr::group_by(.data[[compare[j]]]) %>% dplyr::summarise(grp_mean=mean(.data[[metric[i]]]))
#
#     density[[j]][[i]] <- all_healthy %>% ggplot(aes(x = .data[[metric[i]]], color = .data[[compare[j]]])) +
#       geom_density()+
#       geom_vline(data = mean_line, aes(xintercept = grp_mean, color = .data[[compare[j]]]), linetype = "dashed")+
#       #scale_x_continuous(trans = 'log10') +
#       labs(x = metric[i])
#
#     box[[j]][[i]] <- all_healthy %>% ggplot(aes(x = .data[[metric[i]]], color = .data[[compare[j]]])) +
#       geom_boxplot() +
#       #scale_x_continuous(trans = 'log10') +
#       labs(x = metric[i])
#   }
# }
#
# #plots for Shannon entropy
# for (j in 1:length(compare)){
#   #grid.arrange(density[[j]][[1]], box[[j]][[1]], ncol=2, top = textGrob("Distribution of ...", gp=gpar(fontsize=10,font=2)))
#   plot(density[[j]][[2]])
#   plot(box[[j]][[2]])
# }
#
#
# # With facet wrap
#
# mean_line <- all_healthy %>% dplyr::group_by(bowel_movement_quality) %>% dplyr::summarise(grp_mean=mean(.data[[metric[i]]]))
#
# density_test <- all_healthy %>% ggplot(aes(x = shannon_entropy, color = bowel_movement_quality)) +
#   geom_density()+
#   geom_vline(data = mean_line, aes(xintercept = grp_mean, color = bowel_movement_quality), linetype = "dashed")+
#   #scale_x_continuous(trans = 'log10') +
#   labs(x = "shannon_entropy") +
#   facet_wrap(vars(sex), nrow = all_healthy %>% group_by(bowel_movement_quality) %>% summarise() %>% nrow)
#
# box_test<- all_healthy %>% ggplot(aes(x = shannon_entropy, color = bowel_movement_quality)) +
#   geom_boxplot() +
#   #scale_x_continuous(trans = 'log10') +
#   labs(x = "shannon_entropy") +
#   facet_wrap(vars(sex), nrow = all_healthy %>% group_by(bowel_movement_quality) %>% summarise() %>% nrow)
#
# box_test
# density_test
#
# length(table(all_healthy$bowel_movement_quality))
#
# all_healthy %>% group_by(bowel_movement_quality) %>% summarise() %>% nrow


## ----  include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# compare <- c("sex", "age_cat", "race")
# metric <- c("shannon_entropy", "chao1", "menhinick", "margalef", "inverse_simpson", "fisher_alpha", "gini_index", "strong", "faith_pd" )
# test <- list()
# tables <- list()
#
# for (j in 1:length(compare)){
#   for (i in 1:length(metric)){
#     test[[i]] <- pairwise.wilcox.test(all_healthy[[metric[i]]], all_healthy[[compare[j]]], p.adjust.method="none") %>%
#     broom::tidy() %>% add_column(parameter = metric[i], .before='group1')
#   }
#
#   tables[[j]] <- do.call(what = rbind, args = test)
#
#   tables[[j]] <- tables[[j]] %>%
#     add_column(p.adjusted = p.adjust(tables[[j]]$p.value, "fdr"), .after='p.value') %>%
#     flextable() %>%
#     bold(~ p.value < 0.05, 4) %>%
#     bold(~ p.adjusted < 0.05, 5) %>%
#     add_header_lines(values = "Results of the Wilcox test for distributions of different groups")
# }
#
# tables


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
library(stringr)

cols <- c("sample_id", "shannon_entropy", "chao1", "menhinick", "margalef", "fisher_alpha", "gini_index", "pielou_evenness" , "simpson", "strong", "faith_pd","country_of_birth", "country_residence" )  ##"inverse_simpson"
healthy_countries <- all_healthy[, names(all_healthy) %in% cols]

europe <- c("Albania", "Austria", "Belarus", "Belgium", "Croatia", "Czech Republic", "Denmark", "France", "Germany", "Greece", "Gibraltar", "Hungary", "Ireland", "Italy", "Moldova, Republic of", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russian Federation", "Serbia", "Slovakia", "Spain", "Sweden", "Switzerland", "United Kingdom")
soutrh_america <- c("Argentina", "Chile", "Brazil", "Bolivia", "Colombia", "Peru", "Venezuela")
north_america <- c("Canada", "Mexico", "United States", "United States Minor Outlying Islands", "Jamaica")
asia <- c("Azerbaijan", "China", "Hong Kong", "India", "Iraq", "Japan", "Kazakhstan", "Korea, Republic of", "Lebanon", "Malaysia", "Pakistan", "Philippines", "Singapore", "Thailand", "Turkey", "United Arab Emirates", "Viet Nam", "Bangladesh")
africa <- c("Ethiopia", "Kenya", "Nigeria", "South Africa", "Tanzania, United Republic of", "Zambia")
australia_oceania <- c("Australia", "New Zealand", "Fiji")

healthy_countries$country_of_birth <- sapply(healthy_countries$country_of_birth, function(x) replace(x, x %in% europe, "Europe"))
healthy_countries$country_of_birth <- sapply(healthy_countries$country_of_birth, function(x) replace(x, x %in% soutrh_america, "South America"))
healthy_countries$country_of_birth <- sapply(healthy_countries$country_of_birth, function(x) replace(x, x %in% north_america, "North America"))
healthy_countries$country_of_birth <- sapply(healthy_countries$country_of_birth, function(x) replace(x, x %in% asia, "Asia"))
healthy_countries$country_of_birth <- sapply(healthy_countries$country_of_birth, function(x) replace(x, x %in% africa, "Africa"))
healthy_countries$country_of_birth <- sapply(healthy_countries$country_of_birth, function(x) replace(x, x %in% australia_oceania, "Australia and Oceania"))

healthy_countries$country_residence <- sapply(healthy_countries$country_residence, function(x) replace(x, x %in% europe, "Europe"))
healthy_countries$country_residence <- sapply(healthy_countries$country_residence, function(x) replace(x, x %in% soutrh_america, "South America"))
healthy_countries$country_residence <- sapply(healthy_countries$country_residence, function(x) replace(x, x %in% north_america, "North America"))
healthy_countries$country_residence <- sapply(healthy_countries$country_residence, function(x) replace(x, x %in% asia, "Asia"))
healthy_countries$country_residence <- sapply(healthy_countries$country_residence, function(x) replace(x, x %in% africa, "Africa"))
healthy_countries$country_residence <- sapply(healthy_countries$country_residence, function(x) replace(x, x %in% australia_oceania, "Australia and Oceania"))

table(healthy_countries$country_of_birth)
table(healthy_countries$country_residence)

#delete underrepresented values

healthy_countries_2 <- healthy_countries[!(healthy_countries$country_of_birth=="Africa" | healthy_countries$country_of_birth=="Australia and Oceania" | healthy_countries$country_of_birth=="South America" | healthy_countries$country_residence=="Africa" | healthy_countries$country_residence=="Australia and Oceania" | healthy_countries$country_residence=="Asia" | healthy_countries$country_residence=="South America" | healthy_countries$country_residence=="Unspecified"),]

table(healthy_countries_2$country_of_birth)
table(healthy_countries_2$country_residence)


## ---- echo = FALSE-------------------------------------------------------------------------------------------------------------------------------------------
compare <- c("country_of_birth", "country_residence")

density <- vector('list', length(compare)*length(metric))
box <- vector('list', length(compare)*length(metric))


for (j in 1:length(compare)){
  for (i in 1:length(metric)){
    mean_line <- healthy_countries_2 %>% dplyr::group_by(.data[[compare[j]]]) %>% dplyr::summarise(grp_mean=mean(.data[[metric[i]]]))
    #print(mean_line)

    density[[j]][[i]] <- healthy_countries_2 %>% ggplot(aes(x = .data[[metric[i]]], color = .data[[compare[j]]])) +
      geom_density()+
      geom_vline(data = mean_line, aes(xintercept = grp_mean, color = .data[[compare[j]]]), linetype = "dashed")+
      #scale_x_continuous(trans = 'log10') +
      labs(x = metric[i])

    box[[j]][[i]] <- healthy_countries_2 %>% ggplot(aes(x = .data[[metric[i]]], color = .data[[compare[j]]])) +
      geom_boxplot() +
      #scale_x_continuous(trans = 'log10') +
      labs(x = metric[i])
  }
}

#plots for country_of_birth
for (j in 1:length(metric)){
  plot(density[[1]][[j]])
  plot(box[[1]][[j]])
}

#plots for country_residence
for (j in 1:length(metric)){
  plot(density[[2]][[j]])
  plot(box[[2]][[j]])
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
test <- list()
tables <- list()

for (j in 1:length(compare)){
  for (i in 1:length(metric)){
    test[[i]] <- pairwise.wilcox.test(healthy_countries_2[[metric[i]]], healthy_countries_2[[compare[j]]], p.adjust.method="none") %>%
    broom::tidy() %>% add_column(parameter = metric[i], .before='group1')
  }

  tables[[j]] <- do.call(what = rbind, args = test)

  tables[[j]] <- tables[[j]] %>%
    add_column(p.adjusted = p.adjust(tables[[j]]$p.value, "fdr"), .after='p.value') %>%
    arrange(p.value)  %>%
    flextable() %>%
    bold(~ p.value < 0.05, 4) %>%
    bold(~ p.adjusted < 0.05, 5) %>%
    add_header_lines(values = "Results of the Wilcox test for distributions of different groups")
}

tables


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
sessionInfo()

