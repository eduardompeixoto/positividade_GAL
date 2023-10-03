esus<-function(){



require(googlesheets4)
require(dplyr)

googlesheets4::gs4_deauth()

lacen <- read_sheet("120iSBaMBpMIf6NI3d2azx-zXokBHa_9qd-yX4CGw-Cs")

lacen<-distinct(lacen)


fio <- read_sheet("1WzHQ-7GIwYeFuForFQ7a2GS3nhNaHh4T86SIwzhGGr0")

fio<-distinct(fio)

##deixando os dois bancos (lacen e fiocruz) com o campo de resultado conm o mesmo nome: Resultado, para unir as bases
### e filtrando somente os campos exame liberado e resultados positivos, negativos e inconclusivos 
#(para tirar os em branco/vazios/NA)

lacen$Resultado<-lacen$'3º Campo Resultado'

lacen1<-subset (lacen, lacen$'Status Exame' =='Resultado Liberado')
lacen1<-subset (lacen, lacen$Resultado=='Coronavírus SARS-CoV2: Detectável' |lacen$Resultado=='Coronavírus SARS-CoV2: Não Detectável')

fio$Resultado<-fio$'1º Campo Resultado'

fio1<-subset (fio, fio$'Status Exame' =='Resultado Liberado')
fio1<-subset (fio, fio$Resultado=='Resultado: Detectável' |fio$Resultado=='Resultado: Inconclusivo' | fio$Resultado=='Resultado: Não Detectável')

##gerando bancos filtrados só com os campos desejados para unir as bases
lacen2<-subset(lacen1,select = c('Municipio do Solicitante','Data da Coleta', 'Status Exame','Resultado'))
fio2<-subset(fio1,select = c('Municipio do Solicitante','Data da Coleta', 'Status Exame','Resultado'))

##unindo as bases lacen e fiocrus com os mesmos campos/variáveis,
a<-rbind(lacen2,fio2)
str(a)

###fazendo table para agregar por mun e data gerando o campo FREQ
#B são todos os exames

b<-table(a$`Municipio do Solicitante`,a$`Data da Coleta`,a$Resultado)

##dizendo que é o banco B é um data frame
b<-as.data.frame(b)

#C são só os positivos
c<-subset(b,b$Var3=='Resultado: Detectável'|b$Var3=='Coronavírus SARS-CoV2: Detectável')

##somando o total de positivos
sum(b$Freq)


#agrupando por mun e data através de 2 bancos: 
#um com total de exames (B) chamando o campo/coluna gerado de Total e
#e outro só com os positivos (C) chamando o campo/coluna gerado de Positivo
library(dplyr)

d<-b %>% group_by (Var1,Var2) %>%
  summarise(Total=sum(Freq))

d1<-c %>% group_by (Var1,Var2) %>%
  summarise(Positivo=sum(Freq))

#retiranto os zeros de d e d1

d<-subset(d,d$Total>0)
d1<-subset(d1,d1$Positivo>0)

##criando uma "chave" com as colunas que quero para juntar novamente bancos(tabelas) d(Total de exames) e d1(Positivos)
d$chave<-paste(d$Var1,d$Var2)
d1$chave<-paste(d1$Var1,d1$Var2)

e<-full_join(d,d1,by='chave')

##eliminando as variáveis "chave" que criamos para poder juntar os bancos
e$chave<-NULL
e$Var1.y<-NULL
e$Var2.y<-NULL

##chamando os NA do campo Positivo de zero (apareceram NA no Positivo quando criamos a chave para juntar os bancos)
e$Positivo[is.na(e$Positivo)]<-0

  e
                  }
  
