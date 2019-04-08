# Main objective: Learn to make animated plots 
#     Secondary objective: Discover evolution of food vs sugarcane crops in Valle del Cauca
# Sources: https://www.datos.gov.co/Agricultura-y-Desarrollo-Rural/Superficie-Sembrada-en-Hect-reas-con-Cultivos-Tran/vs5v-e66i y https://www.datos.gov.co/Agricultura-y-Desarrollo-Rural/Superficie-Sembrada-por-hectareas-con-cultivos-per/v4ub-9eme

# Load libraries
library(data.table)
library(ggplot2)
library(gganimate)
library(xlsx)
library(stringi)

# load data
download.file(url = "https://www.datos.gov.co/api/views/vs5v-e66i/files/490bb58f-968f-4fc0-bc2b-4e1230ba4208?download=true&filename=Superficie%20Sembrada%20en%20Hectareas%20con%20Cultivos%20Transitorios%20en%20el%20Departamento%20del%20Valle%20del%20Cauca%20%20del%20a%C3%B1o%202000%20al%202018.xlsx", destfile = "sources/transitorios.xlsx", mode = "wb")

transitorios <- setDT(read.xlsx("sources/transitorios.xlsx", sheetIndex = 1, encoding = "UTF-8", stringsAsFactors = FALSE))

download.file(url = "https://www.datos.gov.co/api/views/v4ub-9eme/files/e7d11286-9847-4ecc-842f-a16cde3f9948?download=true&filename=Superficie%20Sembrada%20por%20Hectareas%20con%20cultivos%20permanentes%20en%20el%20departamento%20del%20valle%20del%20cauca%20del%20a%C3%B1o%202000%20al%202018.xlsx", destfile = "sources/permanentes.xlsx", mode = "wb")

permanentes <- setDT(read.xlsx("sources/permanentes.xlsx", sheetIndex = 1, encoding = "UTF-8", stringsAsFactors = FALSE))

# wrangle data transitorios
## take out non-ascii characters from column names

names(transitorios) <- stri_trans_general(stri_trans_tolower(names(transitorios)), id = "latin-ascii")

## str(transitorios) reveals that all columns are chr. That's because they repeated the table headers. Only first column (town) has to be chr, others should be numeric.

transitorios[, names(transitorios)[2:18] := lapply(.SD, as.numeric), .SDcols = 2:18]

### Remove full NA-coerced rows:

transitorios <- transitorios[!is.na(ano),]

## data is in wide format, convert to long format

transitorios <- melt(transitorios, id.vars = 1:2, variable.name = "cultivo", value.name = "hectareas")

## There are two values for same crops, under slightly different names (frijol.a, frijol.b). Presumably, each number comes from a different source (a and/or b), so summing them up would be wrong, just like dismissing either of them. I went for registering the maximum between them:

transitorios[, cultivo := gsub("([a-z]*)(\\.[a-z]*)", "\\1", cultivo)]
transitorios <- transitorios[, hectareas := max(hectareas, na.rm = TRUE), by = .(municipios, ano, cultivo)]

### Inf were introduced when looking for the maximum among two or more NA values. They are reverted to NA

transitorios[is.infinite(hectareas), hectareas := NA]


##################### ######### ############# ##########

# wrangle data permanentes

## take out non-ascii characters from column names

names(permanentes) <- stri_trans_general(stri_trans_tolower(names(permanentes)), id = "latin-ascii")

## str(permanentes) reveals that all columns are chr. That's because they repeated the table headers. Only first column (town) has to be chr, others should be numeric.

permanentes[, names(permanentes)[2:7] := lapply(.SD, as.numeric), .SDcols = 2:7]

### Remove full NA-coerced rows:

permanentes <- permanentes[!is.na(ano),]

## data is in wide format, convert to long format

permanentes <- melt(permanentes, id.vars = 1:2, variable.name = "cultivo", value.name = "hectareas")

## We need to fix the names:

permanentes[, cultivo := gsub("([a-z]*\\.*[a-z]*\\.*[a-z]*)(\\.*[0-9]\\.*)", "\\1", cultivo)]

permanentes[, cultivo := gsub("\\.\\.", "", cultivo)]

############## ################### ###########

# Assemble a properly formated table

## Bind both tables

cultivos <- rbind(transitorios, permanentes)

################ ######### ##########

# Make the static plot
static_plot <- ggplot(cultivos, aes(x = reorder(cultivo, -hectareas), y = hectareas/1000, fill = cultivo))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
  guides(fill = FALSE)+
  labs(y = "Miles de hectáreas", x = "Cultivo")

## Animate and write changing title

animacion <- static_plot+
  transition_states(ano, transition_length = 3, state_length = 2)+
  labs(title = "Hectareas por tipo de Cultivo en el Año {closest_state}", caption = "Fuente: datos.gov.co")

## Render GIF

animate(animacion)

## save image

anim_save("output/hectareas_anuales.gif")



