# Instalación de paquetes
install.packages('DBI')
install.packages('RSQLite')

# Importar los paquetes
library(DBI)
library(RSQLite)
library(ggplot2)

#---------------------------------------
# Funciones de limpieza
#---------------------------------------

# Función para limpiar los nombre de los municipios, departamentos y regiones
limpiar_datos <- function(municipios_df) {      
  
  # Pasar a mayusculas
  municipios_df$Departamento <- toupper(municipios_df$Departamento) 
  municipios_df$Municipio <- toupper(municipios_df$Municipio) 
  municipios_df$Region <- toupper(municipios_df$Region) 
  
  # Quitar caracteres extraños
  municipios_df$Departamento <- gsub('[^[:alnum:] ]','',municipios_df$Departamento)
  municipios_df$Municipio <- gsub('[^[:alnum:] ]','',municipios_df$Municipio)
  municipios_df
  
  return(municipios_df)
}

# Función para limpiar los nombres de los departamentos
limpiar_columna_Departamento <- function(municipios_df) {      
  
  # Pasar a mayusculas
  municipios_df$Departamento <- toupper(municipios_df$Departamento) 
  
  # Quitar caracteres extraños
  municipios_df$Departamento <- gsub('[^[:alnum:] ]','',municipios_df$Departamento)
  municipios_df$Departamento <- gsub('  ',' ',municipios_df$Departamento)
  municipios_df$Departamento <- gsub('  ',' ',municipios_df$Departamento)
  
  return(municipios_df)
}



#---------------------------------------
# Crear la conexión a la base de datos
#---------------------------------------
con <- dbConnect(SQLite(), "C:\\Users\\jon99\\Downloads\\ADRES prueba tecnica\\db\\base.db")

# Listar las tablas que contiene la base de datos
as.data.frame(dbListTables(con))



#---------------------------------------
# Descripción del dataset
#---------------------------------------

# Obtener lista de municipios 
municipios_df <- dbGetQuery(conn=con, statement=paste("SELECT * FROM municipio", sep=""))

# Limpiar datos
municipios_df <- limpiar_datos(municipios_df)

municipios_df$Departamento = factor(municipios_df$Departamento)
municipios_df$Municipio = factor(municipios_df$Municipio)
municipios_df$Region = factor(municipios_df$Region)
municipios_df$Dep = factor(municipios_df$Dep)
municipios_df$Irural = factor(municipios_df$Irural)


#--------------------------------
# Caracteristicas del dataset 
#--------------------------------
nrow(municipios_df) # Cantidad de filas
ncol(municipios_df) # Cantidad de columnas
summary(municipios_df) # Resumen

#---------------------------------
# Top población por municipio
#---------------------------------

# Query para obtener lista de municipios con mayor población
top_municipios_poblacion <- dbGetQuery(conn=con, 
                                       statement=paste("SELECT Departamento, Poblacion 
                                              FROM municipio
                                              order by Poblacion desc
                                              limit 7", sep=""))

# Limpiar nombre
top_municipios_poblacion <- limpiar_columna_Departamento(top_municipios_poblacion)

# Graficar top municipios
top_minicipios <-ggplot(data=top_municipios_poblacion, aes(x = reorder(Departamento, Poblacion), y=Poblacion)) +
  geom_bar(stat="identity",fill="steelblue")+
  theme_minimal() + 
  xlab("Municipios") +
  ylab("Cantidad de habitantes") +
  ggtitle("Top municipios con mayor cantidad de habitantes") +
  coord_flip() +
  geom_text(aes(label = Poblacion, y = Poblacion), size = 3)
top_minicipios


#--------------------------------------------------
# Diagrama de caja -> superfcie de los municipios por departamento
#--------------------------------------------------
# Query para obtener la superficie de los municipios de los departamentos
departamentos_df <- dbGetQuery(conn=con, 
                               statement=paste("SELECT Departamento, Superficie 
                                             FROM municipio
                                             WHERE Dep in (17, 47,70, 73)
                                            ", sep=""))

departamentos_df <- limpiar_columna_Departamento(departamentos_df)


ggplot(departamentos_df, aes(x=as.factor(Departamento), y=Superficie)) + 
  geom_boxplot(fill="Green", alpha=0.2) + 
  xlab("Departamento") +
  ggtitle("Boxplot de superficie de municipios por departamento")  


#----------------------------------------
# Cantidad de departamentos por region
#----------------------------------------
cant_departamentos_df <- dbGetQuery(conn=con, 
                                    statement=paste("SELECT Region, count(DISTINCT Dep) as cant_departamentos  
                                                        FROM municipio 
                                                        GROUP by Region
                                                        order by cant_departamentos desc", sep=""))


# Graficar cantidad departamentos por región
cant_departamentos <-ggplot(data=cant_departamentos_df, aes(x = reorder(Region, -cant_departamentos), y=cant_departamentos)) +
  geom_bar(stat="identity",fill="blue", alpha=0.5)+
  theme_minimal(base_size = 12) + 
  xlab("Región") +
  ylab("Cantidad de departamentos") +
  ggtitle("Departamentos por región") +
  geom_text(aes(label = cant_departamentos, y = cant_departamentos), size = 3)
cant_departamentos










