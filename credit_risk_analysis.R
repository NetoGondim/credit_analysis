# Load dataset #

creditdata <- read.csv("C:/Users/Neto/Documents/Análise de crédito - árvore de decisão/credito.csv", 
                       header = TRUE)

creditdata %>% head

#################################
# 1.Classify customers by credit performance: Delinquency (D) vs. Compliance (C)

creditdata_class <- creditdata

creditdata_class$default <- ifelse(creditdata_class$default == 1, "D", "C")
creditdata_class$default <- as.factor(creditdata_class$default)

creditdata_class %>% str
summary(creditdata_class)
##########################################
# 2. Quantiles for continuous variables

creditdata_class$limite_credito <- as.numeric(gsub(",", ".", gsub("\\.", "", creditdata_class$limite_credito)))
creditdata_class$valor_transacoes_12m <- as.numeric(gsub(",", ".", gsub("\\.", "", creditdata_class$valor_transacoes_12m)))


creditdata_class$idade <- quantcut(creditdata_class$idade, q=10)
creditdata_class$meses_de_relacionamento <- quantcut(creditdata_class$meses_de_relacionamento, q=10)
creditdata_class$limite_credito <- quantcut(creditdata_class$limite_credito, q=10, dig.lab = 7)
creditdata_class$valor_transacoes_12m <- quantcut(creditdata_class$valor_transacoes_12m, q=10, dig.lab = 7)
creditdata_class$qtd_transacoes_12m <- quantcut(creditdata_class$qtd_transacoes_12m, q=10)



##########################################
# 3. Descriptive Analysis
# Evaluate the distribution according to each variable
# Summarize for each category of y for x category and build a graphic

descriptive <- function(var){
  creditdata$default <- 1 - creditdata$default
  summary <- Rmisc::summarySE(creditdata, measurevar="default", groupvars=c(var))
  ggplot(summary) + 
    geom_bar(aes(x=summary[,var], weight=N/10127, fill=as.factor(summary[,var]))) + 
    geom_errorbar(aes(x=summary[,var], y=default, ymin=default-se, ymax=default+se, colour='1'), width=.1) +
    geom_point(aes(x=summary[,var], y=default, colour='1', group='1')) +
    geom_line(aes(x=summary[,var], y=default, colour='1', group='1')) +
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    scale_fill_viridis_d(direction = -1, begin=.85, end=.95) +
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey")) + 
    theme(legend.position = "none") +
    xlab(var) + ylab("Compliance rate") + 
    scale_y_continuous(sec.axis = sec_axis(~.*10127, name = "Frequency"), labels = scales::percent)
}

descriptive("idade")
descriptive("sexo")
descriptive("dependentes")
descriptive("escolaridade")
descriptive("salario_anual")
descriptive("meses_de_relacionamento")
descriptive("qtd_produtos")
descriptive("iteracoes_12m")
descriptive("meses_inativo_12m")


#############################################
# Decision tree (classification)
class_tree <- rpart(default ~ idade + sexo + dependentes + escolaridade + 
                      estado_civil + salario_anual + tipo_cartao +
                      meses_de_relacionamento + qtd_produtos + iteracoes_12m +
                      meses_inativo_12m + limite_credito + valor_transacoes_12m +
                      qtd_transacoes_12m,
                data=creditdata_class,
                parms = list(split = 'gini'), 
                method='class' 
)

color_palette = scales::viridis_pal(begin=.75, end=1)(20)

rpart.plot::rpart.plot(class_tree,
                       box.palette = color_palette,
                       cex = 0.5) 

##############################
# Decision tree evaluation #

# Prediction

# Compliance rate probability 

class <- predict(class_tree, creditdata_class, type = "class")
conf_matrix <- table(class, creditdata_class$default)
print(conf_matrix)

# Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy: ", accuracy)
prob = predict(class_tree, creditdata_class)
