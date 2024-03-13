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

# Listar las tablas que contiene la base de datos
as.data.frame(dbListTables(con))



#---------------------------------------
# Descripción del dataset
#---------------------------------------
prestador_df <- dbGetQuery(conn=con, statement=paste("SELECT * FROM prestador", sep=""))

nrow(prestador_df) # Cantidad de filas
ncol(prestador_df) # Cantidad de columnas
summary(prestador_df) # Resumen



#--------------------------------------------------
# Top de mas prestadores por departamento en el año 2024
#--------------------------------------------------
# Query para obtener los prestador para el año 2024
top_radicacion_df <- dbGetQuery(conn=con, 
                                statement=paste("SELECT depa_nombre, count(DISTINCT nits_nit) as cant_nit
                                              FROM prestador 
                                              WHERE
                                              SUBSTRING (fecha_radicacion, 1, 4) = '2024'
                                              GROUP by depa_nombre
                                              order by cant_nit DESC
                                              LIMIT 5", sep=""))


# Graficar top 
top_radicacion <-ggplot(data=top_radicacion_df, aes(x = reorder(depa_nombre, cant_nit), y=cant_nit)) +
  geom_bar(stat="identity",fill="green", alpha=0.6)+
  theme_minimal() + 
  xlab("Departamento") +
  ylab("Cantidad de prestadores (NIT únicos)") +
  ggtitle("Top de departamentos con mas prestadores radicados en el año 2024") +
  coord_flip() +
  geom_text(aes(label = cant_nit, y = cant_nit), size = 3)
top_radicacion





#--------------------------------------------------
# Cantidad de vencimiento de prestadores por año
#--------------------------------------------------
# Query para obtener los vencimiento por año
vencimientos_df <- dbGetQuery(conn=con, 
                              statement=paste("SELECT depa_nombre, SUBSTRING (fecha_vencimiento, 1, 4) AS anio_vencimiento, count(*) cant_vencimientos 
                                             FROM prestador 
                                             WHERE  depa_nombre in ('Bogotá D.C','Antioquia','Cali','Barranquilla') 
                                             GROUP by depa_nombre, anio_vencimiento", sep=""))

w <- ggplot(data=vencimientos_df, aes(x=anio_vencimiento, y= cant_vencimientos))
w + geom_point(size=5) +
  geom_smooth() +
  xlab("Año") +
  ylab("Cantidad de vencimientos") +
  ggtitle("Cantidad de vencimientos por año") +
  facet_grid(.~depa_nombre)






#--------------------------------------------------
# Evolutivo de prestadores por fecha de radicación desde el año 2019
#--------------------------------------------------
radicacion_df <- dbGetQuery(conn=con, 
                            statement=paste("SELECT depa_nombre, count(DISTINCT nits_nit) as cant_nit, SUBSTRING(fecha_radicacion, 1, 4) as anio_radicacion
                                      FROM prestador 
                                      WHERE SUBSTRING (fecha_radicacion, 1, 4) >= '2019'
                                      AND depa_nombre in ('Antioquia', 'Bogotá D.C', 'Cali', 'Valle del cauca')
                                      GROUP by depa_nombre, anio_radicacion
                                      order by anio_radicacion DESC", sep=""))

radicacion <- ggplot(data=radicacion_df, aes(x=anio_radicacion, y= cant_nit))

# Facetas
radicacion + geom_point(size=3) +
  geom_smooth() +
  xlab("Año") +
  ylab("Evolutivo de cantidad de radicaciones por año") +
  ggtitle("Cantidad de radicaciones por año") +
  facet_grid(.~depa_nombre)


#--------------------------------------------------
# Evolutivo de prestadores por fecha de radicación - clase persona
#--------------------------------------------------
radicacion_df <- dbGetQuery(conn=con, 
                            statement=paste("SELECT depa_nombre, count(DISTINCT nits_nit) as cant_nit, SUBSTRING(fecha_radicacion, 1, 4) as anio_radicacion, clase_persona
                                            FROM prestador 
                                            WHERE SUBSTRING (fecha_radicacion, 1, 4) >= '2020'
                                            AND depa_nombre in ('Antioquia', 'Bogotá D.C', 'Cali', 'Valle del cauca')
                                            GROUP by depa_nombre, anio_radicacion, clase_persona
                                            order by anio_radicacion DESC
                                            ", sep=""))

radicacion <- ggplot(data=radicacion_df, aes(x=anio_radicacion, y= cant_nit))

# Facetas
radicacion + geom_point(size=3) +
  geom_smooth() +
  xlab("Año") +
  ylab("Cantidad de NITs") +
  ggtitle("Evolutivo de cantidad de radicaciones por año y tipo de persona") +
  facet_grid(clase_persona~depa_nombre)









#--------------------------------------------------
# Evolutivo de prestadores por fecha de radicación - clase persona - naturaleza juridica
#--------------------------------------------------
radicacion_df <- dbGetQuery(conn=con, 
                            statement=paste("SELECT count(DISTINCT nits_nit) as cant_nit, SUBSTRING(fecha_radicacion, 1, 4) as anio_radicacion, clase_persona, naju_nombre
                                            FROM prestador 
                                            WHERE SUBSTRING (fecha_radicacion, 1, 4) >= '2018'
                                            GROUP by naju_nombre, anio_radicacion, clase_persona
                                            order by anio_radicacion DESC
                                            ", sep=""))

radicacion <- ggplot(data=radicacion_df, aes(x=anio_radicacion, y= cant_nit))

# Facetas
radicacion + geom_point(size=3) +
  geom_smooth() +
  xlab("Año") +
  ylab("Cantidad de radicaciones por año") +
  ggtitle("Evolutivo de cantidad de prestadores por clase persona y naturaleza juridica") +
  facet_grid(clase_persona~naju_nombre)






