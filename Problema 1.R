####Enrique Franco Garcia####
####Problema 1####

library (igraph) #Cargo la libreria

####(a) Lea las redes en R y determine la distribucion de conectividades.####

# Leer las redes:
red_1 <- read.table ("1034808.txt") #Cargo la tabla
red_1 <- graph_from_data_frame(red_1) #La convierto en un objeto igraph para poder trabajar
red_1

red_2 <- read.table ("272624.txt") #Cargo la tabla
red_2 <- graph_from_data_frame(red_2) #La convierto en un objeto igraph para poder trabajar
red_2

red_3 <- read.table ("387662.txt") #Cargo la tabla
red_3 <- graph_from_data_frame(red_3) #La convierto en un objeto igraph para poder trabajar
red_3

#Distribucion de conectividades:

##RED 1:
conx_1 <- degree (red_1) #Esto me permite observar las conexiones que tienen entre ellas
hist (conx_1) #Asi tenemos una visualización por un histograma acerca del degree
conec_logat <- degree_distribution (red_1) #Así tengo la distribucion de conectividades a escala logatitmica
plot (conec_logat) #Con esto puedo graficarlo

##RED 2:##
conx_2 <- degree (red_2) #observamos las conexiones que tienen entre ellas
hist (conx_2) #una visualizacion por un histograma acerca del degree
conec_logat <- degree_distribution (red_2) #distribucion de conectividades a escala logatitmica
plot (conec_logat) #grafica 

##RED 3:##
conx_3 <- degree (red_3) #observamos las conexiones que tienen entre ellas
hist (conx_3) #una visualizacion por un histograma acerca del degree
conec_logat <- degree_distribution (red_3) #distribucion de conectividades a escala logatitmica
plot (conec_logat) #grafica 

####(b) Calcule el promedio de conectividades de cada red.####

##RED 1
dg_red1 <- degree (red_1) #Con esto hago la suma de tal manera que toma en cuenta todas las entradas y salidas
mean (dg_red1)#Me calcula el promedio tanto de entradas como de salidas

dg_red1_in <- degree (red_1, mode = "in") #Con "in" indico que unicamente quiero ver las entradas
mean (dg_red1_in) #Me calcula el promedio de las entradas

dg_red1_out <- degree (red_1, mode = "out") #Al contrario que el anterior aquí me da las salidas
mean (dg_red1_out) #Me calcula el promedio de las entradas

##RED 2
dg_red2 <- degree (red_2) #Con esto hago la suma de tal manera que toma en cuenta todas las entradas y salidas
mean (dg_red2) #Me calcula el promedio de entradas y salidas

dg_red2_in <- degree (red_2, mode = "in") #Con "in" indico que unicamente quiero ver las entradas
mean (dg_red2_in) #Me calcula el promedio de las entradas

dg_red2_out <- degree (red_2, mode = "out") #Al contrario que el anterior aquí me da las salidas
mean (dg_red2_out) #Me calcula el promedio de las entradas

##RED 3
dg_red3 <- degree (red_3) #Con esto hago la suma de tal manera que toma en cuenta todas las entradas y salidas
mean (dg_red3)#Me calcula el promedio tanto de entradas como de salidas

dg_red3_in <- degree (red_3, mode = "in") #Con "in" indico que unicamente quiero ver las entradas
mean (dg_red3_in) #Me calcula el promedio de las entradas

dg_red3_out <- degree (red_3, mode = "out") #Al contrario que el anterior aquí me da las salidas
mean (dg_red3_out) #Me calcula el promedio de las entradas

####(c) Calcule la densidad de cada red.####

####(d) Escribe una funcion que muestre el numero de componentes de la red cuando se desconectan los 10 nodos m ??as conectados. La funci?n de igraph components() puede ser  ?til.####

####(e) Calcule la robustez de las redes quitando los 10 prote?nas m ??as conectadas.####

#Quito las más conectadas:

##RED 1
rob_red1 <- degree (red_1) #Primero observo las conexiones que hay
sort (rob_red1, decreasing = T) #Ordeno desendentemente 
diez_red1 <- sort(rob_red1, decreasing = T) [1:10] #Una vez que ya lo ordene, usando corchetes selecciono las 10 mas conectadas
diez_red1 #Unicamente para visualizar cuales fueron las que se quitaron

##RED 2
rob_red2 <- degree (red_2) #Primero observo las conexiones que hay
sort (rob_red2, decreasing = T) #Ordeno desendentemente 
diez_red2 <- sort(rob_red2, decreasing = T) [1:10] #Una vez que ya lo ordene, usando corchetes selecciono las 10 mas conectadas
diez_red2 #Unicamente para visualizar cuales fueron las que se quitaron

##RED 3
rob_red3 <- degree (red_3) #Primero observo las conexiones que hay
sort (rob_red3, decreasing = T) #Ordeno desendentemente 
diez_red3 <- sort(rob_red3, decreasing = T) [1:10] #Una vez que ya lo ordene, usando corchetes selecciono las 10 mas conectadas
diez_red3 #Unicamente para visualizar cuales fueron las que se quitaron

# Calculo la robustez:
#Se me ocurre hacerlo a travese la longitud y el diametro de la red, así se puede saber la cantidad
#de nodos y el numero más grande de conexiones que hay
length(rob_red1)
length(rob_red2)
length(rob_red3)
diameter(red_1)
diameter(red_2)
diameter(red_3)
#De igual manera también podría ser con un histograma del degree de cada red
hist (degree (red_1))
hist (degree (red_2))
hist (degree (red_3))

####(f) Selecciona las 10 prote?nas mas importantes y determina su funci?n usando gene ontology (debido al tama?o de la muestra podr?a no salirte nada significativo en ese caso pon ese resultado)####
#Para seleccionar las proteinas mas importantes:
##RED 1
imp_red1 <- degree (red_1) #Primero observo las conexiones que hay
sort (imp_red1, decreasing = T) #Ordeno desendentemente 
dimp_red1 <- sort(rob_red1, decreasing = T) [1:10] #Una vez que ya lo ordene, usando corchetes selecciono las 10 mas conectadas
dimp_red1 #Me quedo unicamente con las 10 que más conexiones tienen

##RED 2
imp_red2 <- degree (red_2) #Primero observo las conexiones que hay
sort (imp_red2, decreasing = T) #Ordeno desendentemente 
diez_red2 <- sort(imp_red2, decreasing = T) [1:10] #Una vez que ya lo ordene, usando corchetes selecciono las 10 mas conectadas
diez_red2 #Me quedo unicamente con las 10 que más conexiones tienen

##RED 3
imp_red3 <- degree (red_3) #Primero observo las conexiones que hay
sort (imp_red3, decreasing = T) #Ordeno desendentemente 
diez_red3 <- sort(imp_red3, decreasing = T) [1:10] #Una vez que ya lo ordene, usando corchetes selecciono las 10 mas conectadas
diez_red3 #Me quedo unicamente con las 10 que más conexiones tienen

