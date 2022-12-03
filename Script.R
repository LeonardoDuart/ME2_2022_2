pacman::p_load(cowplot,dplyr,vroom,ggplot2,moments,treemapify)

setwd(choose.dir())
base <- vroom('amostra_190016183.csv')

######## questao 3 ########

#Regiao
table(base$REGIAO)

ggplot(base,aes(x=REGIAO))+
  geom_bar(fill = "#00CCCC")+
  xlab("Região") +
  ylab("Frequência absoluta") + 
  theme_bw()

#Area
table(base$AREA)

base %>% na.omit(AREA) %>%
ggplot(.,aes(x=AREA))+
  geom_bar(fill = "#00CCCC")+
  xlab("Área") +
  ylab("Frequência absoluta")+ 
  theme_bw()

#Sexo
table(base$SEXO)

base %>% na.omit(SEXO) %>%
ggplot(.,aes(x=SEXO))+
  geom_bar(fill = "#00CCCC")+
  xlab("Sexo") +
  ylab("Frequência absoluta")+ 
  theme_bw()

#Trabalho
table(base$TRABALHO)

base %>% na.omit(SEXO) %>%
ggplot(.,aes(x=TRABALHO))+
  geom_bar(fill = "#00CCCC")+
  xlab("Trabalho") +
  ylab("Frequência absoluta")+ 
  theme_bw()

#RACA_COR
table(base$RACA_COR)

base %>% na.omit(RACA_COR) %>%
  mutate(value = 1) %>%
  group_by(RACA_COR) %>%
  summarise(n = n()) %>%
  ggplot(.,aes(area=n,fill=RACA_COR))+
  geom_treemap()+
  theme_bw()+ labs(fill = "Raça/cor")

######## questao 4 ########

#Distribuição de frequências, com intervalos de classe
base$NOTA_LP_INTERVAL <- cut(base$NOTA_LP, seq(from=125,to=425,by=25))
base$NOTA_MT_INTERVAL <- cut(base$NOTA_MT, seq(from=125,to=425,by=25))

table(base$NOTA_LP_INTERVAL)
table(base$NOTA_MT_INTERVAL)

#Histograma
a <- ggplot(base, aes(x=NOTA_LP)) + geom_histogram(fill = "#00CCCC")+
  xlab("Proficiência em Português") +
  ylab("Frequência absoluta")+ 
  expand_limits(x=c(120,415), y=c(0, 170)) +
  theme_bw()

b <- ggplot(base, aes(x=NOTA_MT)) + geom_histogram(fill = "#00CCCC")+
  xlab("Proficiência em Matemática") +
  ylab("")+ 
  expand_limits(x=c(120,415), y=c(0, 170)) +
  theme_bw()

plot_grid(a,b,ncol = 2,nrow = 1)

#Medidas de posição, variabilidade, assimetria e curtose.

quantile(base$NOTA_LP, seq(from=0.1,to=0.9,by=0.1)) #Quartis_LP
quantile(base$NOTA_MT, seq(from=0.1,to=0.9,by=0.1)) #Quartis_MT

coef_var_LP <- sd(base$NOTA_LP)/mean(base$NOTA_LP)*100
coef_var_MT <- sd(base$NOTA_MT)/mean(base$NOTA_MT)*100

assimetria_LP <- skewness(base$NOTA_LP)
assimetria_MT <- skewness(base$NOTA_MT)

curtose_LP <- kurtosis(base$NOTA_LP)
curtose_MT <- kurtosis(base$NOTA_MT)

#Box-plot
ggplot(base, aes(x=NOTA_LP)) + 
  geom_boxplot(fill = "#00CCCC")+ 
  coord_flip()+
  xlab("Nota Português") +
  ylab("Frequência absoluta")+
  theme_bw()+
  expand_limits(x=c(120,415)) +
  theme(axis.text.x=element_blank())

ggplot(base, aes(x=NOTA_MT)) + 
  geom_boxplot(fill = "#00CCCC")+ 
  coord_flip()+
  xlab("Nota Matemática") +
  ylab("Frequência absoluta") +
  theme_bw()+
  expand_limits(x=c(120,415)) +
  theme(axis.text.x=element_blank())


