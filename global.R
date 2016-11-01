#library(shiny)
#library(leaflet)
library(plyr)
library(dplyr)
#library(grid)
#library(vcd)
library(plotly)
#library(ggplot2)
library(googleVis)
library(RColorBrewer)
library(igraph)
library(d3Network)
library(rCharts)


#For Mac
dir()
setwd("~/Dropbox/JANO/2016/Conabio/JLarson/LastDataBase")

TableP <- read.csv("Maíz.csv", header = T, sep = ",")

#str(TableP)
names(TableP)
levels(TableP$Complejo_r)
names(TableP)
head(TableP)
dim(TableP)
names(TableP)
TableP$Estado
TTabla <- TableP %>%
  filter(!is.na(Raza_prima)) %>%
  filter(!is.na(latitude)) %>%
  filter(Estado != "ND")

levels(TTabla$Estado)
dim(TTabla)
names(TTabla)
TTabla$Anhio_Cole <- as.factor(TTabla$Anhio_Cole)
#TTabla[Tabla$Estado=="ND",]
str(TTabla)

levels(TTabla$Anhio_Cole)
levels(TTabla$Estado)
levels(TTabla$Complejo_r)
levels(TTabla$Raza_prima)


names(TTabla)
#TTabla <- TTabla %>%
#  mutate(Estado = revalue(Estado,c("AGUASCALIENTES" = "AGS"))) %>%
#  mutate(Estado = revalue(Estado,c("BAJA CALIFORNIA" = "BC"))) %>%
#  mutate(Estado = revalue(Estado,c("BAJA CALIFORNIA SUR" = "BCS"))) %>%
#  mutate(Estado = revalue(Estado,c("CAMPECHE" = "CAMP"))) %>%
#  mutate(Estado = revalue(Estado,c("CHIAPAS" = "CHPS"))) %>%
#  mutate(Estado = revalue(Estado,c("CHIHUAHUA" = "CHIH"))) %>%
#  mutate(Estado = revalue(Estado,c("COAHUILA DE ZARAGOZA" = "COAH"))) %>%
#  mutate(Estado = revalue(Estado,c("COLIMA" = "COL"))) %>%
#  mutate(Estado = revalue(Estado,c("DISTRITO FEDERAL" = "CDMX"))) %>%
#  mutate(Estado = revalue(Estado,c("DURANGO" = "DGO"))) %>%
#  mutate(Estado = revalue(Estado,c("ESTADO DE MÉXICO" = "MEX"))) %>%
#  mutate(Estado = revalue(Estado,c("GUANAJUATO" = "GTO"))) %>%
#  mutate(Estado = revalue(Estado,c("GUERRERO" = "GRO"))) %>%
#  mutate(Estado = revalue(Estado,c("HIDALGO" = "HGO"))) %>%
#  mutate(Estado = revalue(Estado,c("JALISCO" = "JAL"))) %>%
#  mutate(Estado = revalue(Estado,c("MICHOACÁN DE OCAMPO" = "MICH"))) %>%
#  mutate(Estado = revalue(Estado,c("MORELOS" = "MOR"))) %>%
#  mutate(Estado = revalue(Estado,c("NAYARIT" = "NAY"))) %>%
#  mutate(Estado = revalue(Estado,c("NUEVO LEÓN" = "NL"))) %>%
#  mutate(Estado = revalue(Estado,c("OAXACA" = "OAX"))) %>%
#  mutate(Estado = revalue(Estado,c("PUEBLA" = "PUE"))) %>%
#  mutate(Estado = revalue(Estado,c("QUERÉTARO DE ARTEAGA" = "QRO"))) %>%
#  mutate(Estado = revalue(Estado,c("QUINTANA ROO" = "QROO"))) %>%
#  mutate(Estado = revalue(Estado,c("SAN LUIS POTOSÍ" = "SLP"))) %>%
#  mutate(Estado = revalue(Estado,c("SINALOA" = "SIN"))) %>%
#  mutate(Estado = revalue(Estado,c("SONORA" = "SON"))) %>%
#  mutate(Estado = revalue(Estado,c("TABASCO" = "TAB"))) %>%
#  mutate(Estado = revalue(Estado,c("TAMAULIPAS" = "TAM"))) %>%
#  mutate(Estado = revalue(Estado,c("TLAXCALA" = "TLAX"))) %>%
#  mutate(Estado = revalue(Estado,c("VERACRUZ DE IGNACIO DE LA LLAVE" = "VER"))) %>%
#  mutate(Estado = revalue(Estado,c("YUCATÁN" = "YUC"))) %>%
#  mutate(Estado = revalue(Estado,c("ZACATECAS" = "ZAC"))) %>%
#  mutate(Complejo_racial = revalue(Complejo_racial,c("Chapalote" = "Chapalotes"))) %>%
#  mutate(Complejo_racial = revalue(Complejo_racial,c("Cónico" = "Cónicos"))) %>%
#  mutate(Complejo_racial = revalue(Complejo_racial,c("Dentados_tropicales" = "Dentados tropicales"))) %>%
#  mutate(Raza_primaria = revalue(Raza_primaria,c("Palomero Toluquenho" = "Palomero Toluqueño")))

#  mutate(Complejo_racial = revalue(Complejo_racial,c("Dentados_tropicales" = "Den_Trop"))) %>%
#  mutate(Complejo_racial = revalue(Complejo_racial,c("Ocho hileras" = "8_Hileras"))) %>%
#  mutate(Complejo_racial = revalue(Complejo_racial,c("Sierra Chihuahua" = "S_Chih"))) %>%
#  mutate(Complejo_racial = revalue(Complejo_racial,c("Tropicales precoces" = "Trop_Prec"))) %>%
#  mutate(Complejo_racial = revalue(Complejo_racial,c("Tropicales tardíos" = "Trop_Tar")))

levels(TTabla$Estado)
levels(TTabla$Complejo_r)
names(TTabla)
names(TTabla)[15] <- c("longitude")
names(TTabla)[16] <- c("latitude")

dim(TTabla)
nrow(TTabla)


TableL <- TTabla %>%
  select(Raza_prima, Complejo_r, Estado, Municipio, Localidad, NOMBRE, PROVBIOB) %>%
  mutate(Val1 = rep(1, nrow(TTabla)))
head(TableL)


dir()
Teoc <- read.csv("Teocintle.csv", sep = ",", header = T)
summary(Teoc)
summary(Teoc$NOMBRE)
#names(TableL2)
#levels(TableL2$Complejo_racial)
names(Teoc)
Teoc1 <- Teoc %>%
  select(Raza_prima, Complejo_r, Estado, Municipio, Localidad, NOMBRE, PROVBIOB) %>%
  mutate(Val1 = rep(1, nrow(Teoc))) %>%
  filter(!is.na(Complejo_r)) %>%
  filter(!is.na(Raza_prima))

summary(Teoc1)

head(Teoc1)
head(TableL2)

TableL2 <- rbind(TableL, Teoc1)
dim(TableL2)
names(TableL2)
head(TableL2)

#TableL2 <- TableL
attach(TableL2)
#TableLJJ <- aggregate(Val1 ~ Complejo_racial + Estado , FUN = sum, na.rm = T)
#TableLJJF <- aggregate(Val1 ~ Complejo_racial + Raza_primaria, FUN = sum, na.rm = T)

#TableLJJ <- aggregate(Val1 ~ Raza_prima + Complejo_r , FUN = sum, na.rm = T)
#TableLJJF <- aggregate(Val1 ~ Raza_prima + NOMBRE, FUN = sum, na.rm = T)

TableLJJ <- aggregate(Val1 ~  Complejo_r + Raza_prima , FUN = sum, na.rm = T)
TableLJJF <- aggregate(Val1 ~ Complejo_r + PROVBIOB, FUN = sum, na.rm = T)


#TableL1b <- aggregate(TableL1[,17], by = list(Raza_Primaria,Estado), FUN = sum, na.rm = T)
head(TableLJJ)
names(TableLJJ)[1] <- c("origin")
names(TableLJJ)[2] <- c("visit")
names(TableLJJ)[3] <- c("Val1")
head(TableLJJF)
names(TableLJJF)[2] <- c("origin")
names(TableLJJF)[1] <- c("visit")
names(TableLJJF)[3] <- c("Val1")
detach(TableL2)

Katcha <- rbind(TableLJJ,TableLJJF)
head(Katcha)
Katcha
#write.table(Katcha, file="KatchaTTT.txt", sep="\t")

TTT <- c(brewer.pal(8,"Dark2"))
TTT
TTT2 <- c(brewer.pal(9,"GnBu"))
#colors_link <- c('green', 'blue')
colors_link <- c(rep(TTT,2),TTT2,TTT2)

colors_link_array <- paste0("[", paste0("'", colors_link,"'", collapse = ','), "]")

#colors_node <- c('yellow', 'lightblue', 'red', 'black', 'brown')


TTT1 <- c(brewer.pal(3,"Set1"))
TTT1
colors_node <- c(rep(TTT1, 33))


colors_node_array <- paste0("[", paste0("'", colors_node,"'", collapse = ','), "]")


opts <- paste0("{
        link: { colorMode: 'source',
               colors: ", colors_link_array ," },
               node: { nodePadding: 5, colors: ", colors_node_array ,",label:{fontSize: 10}, 
                                  interactivity: false, width: 65 }
               }" )

opts

LL34 <- gvisSankey(Katcha, from = "origin", to = "visit", weight = "Val1",
                   options = list(
                     height = 850, width = 1000,
                     sankey = opts
                                      )
                   #  , chartid = "Sankey"
                   )
plot(LL34)

dir()
Katcha1 <- Katcha
#Katcha1 <- read.csv("KatchaTTT.csv", header = T)
head(Katcha1)
Katcha1

##########Function
# Was copied from: https://www.r-statistics.com/2012/01/merging-two-data-frame-objects-while-preserving-the-rows-order/

merge.with.order <- function(x,y, ..., sort = T, keep_order)
{
  # this function works just like merge, only that it adds the option to return the merged data.frame ordered by x (1) or by y (2)
  add.id.column.to.data <- function(DATA)
  {
    data.frame(DATA, id... = seq_len(nrow(DATA)))
  }
  # add.id.column.to.data(data.frame(x = rnorm(5), x2 = rnorm(5)))
  order.by.id...and.remove.it <- function(DATA)
  {
    # gets in a data.frame with the "id..." column.  Orders by it and returns it
    if (!any(colnames(DATA) == "id...")) stop("The function order.by.id...and.remove.it only works with data.frame objects which includes the 'id...' order column")
    
    ss_r <- order(DATA$id...)
    ss_c <- colnames(DATA) != "id..."
    DATA[ss_r, ss_c]
  }
  
  # tmp <- function(x) x==1; 1	# why we must check what to do if it is missing or not...
  # tmp()
  
  if (!missing(keep_order))
  {
    if (keep_order == 1) return(order.by.id...and.remove.it(merge(x = add.id.column.to.data(x),y = y,..., sort = FALSE)))
    if (keep_order == 2) return(order.by.id...and.remove.it(merge(x = x,y = add.id.column.to.data(y),..., sort = FALSE)))
    # if you didn't get "return" by now - issue a warning.
    warning("The function merge.with.order only accepts NULL/1/2 values for the keep_order variable")
  } else {return(merge(x = x,y = y,..., sort = sort))}
}
############

Katcha1
head(Katcha1)
dim(Katcha1)
Kat1 <- c(Katcha1$origin,Katcha1$visit)

KatNodes <- as.data.frame(as.vector(c(as.matrix(Katcha1$origin), as.matrix(Katcha1$visit))))
KatNodes
names(KatNodes) <- c("name")
 
KatNodes <- as.data.frame(levels(KatNodes$name))
names(KatNodes) <- c("name")
KatNodes$Numero <- as.numeric(KatNodes$name)
ValT <- length(KatNodes$Numero) - 1
KatNodes$Numero <- c(0:ValT)

head(KatNodes)
KatNodes1 <- as.data.frame(KatNodes[,1])
class(KatNodes1)
KatNodes1
names(KatNodes1) <- c("name")
head(Katcha1)

Primero1 <- as.data.frame(Katcha1$origin)
names(Primero1) <- c("name")
droplevels(Primero1)
HHH1 <- merge(Primero1,KatNodes, by = "name", all.x = T)
HHH1

HHH1 <- merge.with.order(Primero1,KatNodes, by = "name", all.x = T, sort = F ,keep_order = 1)
HHH1

Segundo1 <- as.data.frame(Katcha1$visit)
names(Segundo1) <- c("name")
HHH2 <- merge.with.order(Segundo1,KatNodes, by = "name", all.x = T, sort = F ,keep_order = 1)
HHH2


HHH3 <- data.frame(HHH1[,2],HHH2[,2],Katcha1$Val1)
head(HHH3)
names(HHH3) <- c("source", "target", "value")
HHH3

head(HHH3)
head(Katcha1)

str(HHH3)
str(KatNodes1)

d3Sankey(Links = HHH3, Nodes = KatNodes1, Source = "source",
         Target = "target", Value = "value", NodeID = "name",
         fontsize = 10, nodeWidth = 40, height = 700, width = 900, nodePadding = 3, file = "~/Desktop/TestSankey1.html")








