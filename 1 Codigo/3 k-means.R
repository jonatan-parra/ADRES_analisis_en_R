# Instalación de paquetes
install.packages('DBI')
install.packages('RSQLite')

# Importar los paquetes
library(DBI)
library(RSQLite)
library(ggplot2)

#---------------------------------------
# Crear la conexión a la base de datos
#---------------------------------------
con <- dbConnect(SQLite(), "C:\\Users\\jon99\\Downloads\\ADRES prueba tecnica\\db\\base.db")

#--------------------------------------------------
# Obtener los datos
#--------------------------------------------------
dataset <- dbGetQuery(conn=con, 
                      statement=paste("
                          WITH 
                          a AS (
                          SELECT  depa_nombre, 
                          		count(*) as cant_vencimientos, 
                          		count(DISTINCT nits_nit) as nit_unicos, 
                          		SUM(CASE WHEN clpr_nombre = 'Instituciones Prestadoras de Servicios de Salud - IPS' THEN 1 ELSE 0 END) AS clpr_cant_IPS,
                          		SUM(CASE WHEN clpr_nombre = 'Transporte Especial de Pacientes' THEN 1 ELSE 0 END) AS clpr_cant_transporte_pacientes,
                          		SUM(CASE WHEN clpr_nombre = 'Profesional Independiente' THEN 1 ELSE 0 END) AS clpr_cant_profesional_independiente,
                          		
                          		SUM(CASE WHEN clase_persona = 'NATURAL' THEN 1 ELSE 0 END) AS clase_persona_cant_natural,
                          		SUM(CASE WHEN clase_persona = 'JURIDICO' THEN 1 ELSE 0 END) AS clase_persona_cant_juridico,
                          		
                          		SUM(CASE WHEN naju_nombre = 'Mixta' THEN 1 ELSE 0 END) AS naju_cant_mixta,
                          		SUM(CASE WHEN naju_nombre = 'Pública' THEN 1 ELSE 0 END) AS naju_cant_publica,
                          		SUM(CASE WHEN naju_nombre = 'Privada' THEN 1 ELSE 0 END) AS naju_cant_privada
                          			
                          FROM prestador 
                          WHERE  cast(SUBSTRING (fecha_vencimiento, 1, 4)  AS INT) = 2024
                          GROUP by depa_nombre
                          )
                          
                          SELECT * FROM A


                        ", sep=""))

nuevo <- data.frame(matrix(ncol = 1, nrow = 38))
colnames(nuevo) <- c('depa_nombre')

nuevo$depa_nombre = dataset$depa_nombre
dataset$depa_nombre = NULL

set.seed(700)
wcss <- vector()
for(i in 1:20){
  wcss[i] <- sum(kmeans(dataset, i)$withinss)
}




ggplot() + geom_point(aes(x = 1:20, y = wcss), color = 'blue') + 
  geom_line(aes(x = 1:20, y = wcss), color = 'blue') + 
  ggtitle("Método del Codo") + 
  xlab('Cantidad de Centroides k') + 
  ylab('WCSS')


set.seed(700)
kmeans <- kmeans(dataset, 5, iter.max = 1000, nstart = 10)

# Guardar cluste
dataset$cluster <- kmeans$cluster

dataset$depa_nombre = nuevo$depa_nombre
dataset
dataset[order(dataset$cluster, decreasing = FALSE),]
