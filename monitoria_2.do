*-----------------------------------------------------------------*
* Curso: Evaluación de Impacto con Aplicaciones en Educación      *
* Monitoria 2                                                     *
* Profesor: Felipe Barrera                                        *
* Profesor Asistente: Carlos Bermúdez                             *   
* Fecha: junio 2024                                               *                  
*-----------------------------------------------------------------*

*------------------------------------------*
* Establecer ruta de carpeta de trabajo    *
*------------------------------------------*

global root  "/Users/carlosbermudez/Library/CloudStorage/OneDrive-Universidaddelrosario/TA_IE/BSSE_2024/EIAE_2024/Talleres/Taller_1"

use "$root/EjercicioStata", clear

*-----------------------*
*       Caso 4          *
*-----------------------*

* Pregunta 1
* Creamos la variable de cambio para cada hogar.  
bysort hhid (round): gen deltha_ophe = ophe - ophe[_n-1]

order hhid round ophe deltha_ophe 

* Pregunta 2

* Diferencia de medias. 
ttest deltha_ophe if eligible==1, by(treatcom) reverse 


*-----------------------*
*       Caso 5          *
*-----------------------*

* Pregunta 1
tab eligible if treatcom==1 & round==0
tab eligible if score>750 & treatcom==1 & round==0
tab eligible if score<=750 & treatcom==1 & round==0

* Alternativa
tw 	(hist score if eligible==1 & treatcom==1 & round==0, lcolor(red) color(none)) ///
	(hist score if eligible==0 & treatcom==1 & round==0, lcolor(blue) color(none)), ///
	xline(750, lcolor(black) lpattern(solid)) legend(label(1 "Eligibles") label(2 "No eligibles"))

* Pregunta 2
regress ophe eligible score if round==1 & treatcom==1, vce(cluster local) 

* Pregunta 3
cap drop yhat0
predict yhat0 if e(sample), xb
scatter yhat0 score, xline(750)
