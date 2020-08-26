:- use_module(library(pce)).

:- pce_image_directory('C:/Program Files/Correlativas/image').
:- pce_image_directory('C:/Archivos de Programa/Correlativas/image').

main :- principal.

principal :-
    new(P, dialog('ADS - Correlatividad')),

    nueva_imagen(P, _, fondito, point(100,200)),


    /*BOTONES*/
    new(Mostrar, button('Mostrar Correlativas',
                           and(message(@prolog,
                                       interfazMostrar)))),

%    new(Ingresar, button('Ingresar Materias',
 %                       and(message(@prolog, interfazIngresar)))),

    new(Cerrar, button('Cerrar',
                          and(message(P, free), message(P, destroy)))),


  %  send(P, append, Ingresar),

    send(P, append, Mostrar),

    send(P, append, Cerrar, below),

    send(P, open_centered).

/*comentarios---
 * message(Objeto, clear) <- limpia por ejemplo, un text box
    */


/*----------------LOGO UTN---------------*/
resource(fondito, image, image('utn.jpg')).

nueva_imagen(Ventana, Figura, Imagen, Posicion) :-
   new(Figura,  figure),
   new(Bitmap, bitmap(resource(Imagen),@on)),
   send(Bitmap, name, 1),
   send(Figura, display, Bitmap),
   send(Figura, status, 1),
   send(Ventana, display, Figura, Posicion).
/*---------------------------------------*/



interfazMostrar :-
    new(D, dialog('Los Simuladores')),

    new(L1, label(texto,'CONOZCA LAS CORRELATIVAS DE UNA MATERIA',
                  italic)),

    send(D, append, L1),

    %%text group para barra de texto
    send(D, append, new(AcaVaTexto,dialog_group('', box))),

   % send(D, append, new(TextoBlanco3, text('')), below),

    send(AcaVaTexto, append, new(Ingresar, /*Ingreso la materia*/
                        text_item('Materia:','ingrese una materia..')),next_row),

   % send(D, append, new(TextoBlanco, text('')), below),
   % send(D, append, new(TextoBlanco2, label(texto,'Seleccione tipo de consulta', italic)), next_row),



    /*botones "materiasCursadas, materiasAprobadas y materiasAprobadas2*/

    new(MateriasCursadas, button('Materias Cursadas',
                                 and(message(@prolog,
                                             buscarCorrelativasCC,
                                             Ingresar?selection)))),
    new(MateriasAprobadas, button('Materias Aprobadas',
                                  and(message(@prolog,
                                              buscarCorrelativasCA,
                                              Ingresar?selection)))),
    new(MateriasAprobadasRendir, button('Materias Aprobadas',
                                        and(message(@prolog,
                                                    buscarCorrelativasRA,
                                                    Ingresar?selection)))),
    new(ParaCursar, label(texto,  '         Requisitos para Cursar            ', bold)),
    new(ParaAprobar, label(texto, '         Requisitos para Firmar            ', bold)),

    new(Izquierda, dialog_group('', box)),
    new(Derecha, dialog_group('', group)),

    new(EspacioBlanco, label('')),

    %----------------------------------------------------------------------
    send(D, append, Izquierda),
    send(Izquierda, append, Derecha),


    send(Derecha, append, ParaAprobar),
    send(Derecha, append, MateriasAprobadasRendir),

    send(Izquierda, append, EspacioBlanco, below),

    send(Izquierda, append, ParaCursar),
    send(Izquierda, append, MateriasCursadas),
    send(Izquierda, append, MateriasAprobadas, below),



   /* send(D, append, button('Ok', and(message(@prolog,
                                           buscarCorrelativasDe,
                                           Ingresar?selection)))),

   */
    send(D, append, button('Volver', and(message(D, free),message(D, destroy))), below),

    send(D, open_centered).


/*----------------ACA SE INGRESARIAN LAS MATERIAS------------------ */

interfazIngresar :-
    new(D, dialog('Los Simuladores')),

    new(L1, label(texto,'CONOZCA LA DISPONIBILIDAD A BASE DE MATERIAS',

                  italic)),

    send(D, append, L1),

    send(D, append, new(Ingresar, /*Ingreso la materia*/
                        text_item(ingresar_materia,"una materia.."))),

    send(D, append, new(Selector, /*otra opcion: menu(class, choice)*/
                        menu('La materia fue:', marked))),

    send(D, append, new(Text,
                        list_browser)),
    send(Text, append,["Aca aparecerian las materias seleccionadas"]),

    send_list(Selector, append,
              [firmada,
               aprobada]),


    send(D, append, button('Ok', and(message(@prolog, muestra, Ingresar?selection),
                                     message(Ingresar, clear),
                                     message(@prolog,
                                            foa,
                                            Selector?selection)))),


    send(D, append, button('Cancel', and(message(D, free),message(D, destroy)))),

    send(D, open_centered).



/*---------------------------ACA CODIGO--------------------------------------*/

foa(Selector) :- /*muestra por consola si fue aprobada o firmada*/
    write(Selector),
    nl.
muestra(Ingresar) :-
    write(Ingresar),
    nl.
/*-----------------PRUEBA DE METER MATERIAS-------------------*/
/*primer año*/
materia(syo).
materia(am1).
materia(discreta).
materia(algebra).
materia(algoritmos).
materia(arquitecturaDeCompuradoras).
materia(quimica).
materia(ingenieriaYSociedad).
/*segundo año*/
materia(analisisDeSistemas).
materia(sistemasDeRepresentacion).
materia(am2).
materia(sintaxis).
materia(fisica1).
materia(paradigmas).
materia(ingles1).
materia(probabilidad).
/*tercer año*/
materia(disenioDeSistemas).
materia(sistemasOperativos).
materia(fisica2).
materia(economia).
materia(gestionDeDatos).
materia(ingles2).
materia(matematicaSuperior).
materia(legislacion).
/*cuarto año*/
materia(administracionDeRecursos).
materia(ingenieriaDeSoftware).
materia(teoriaDeControl).
materia(comunicaciones).
materia(redesDeInformacion).
materia(investigacionOperativa).
materia(simulacion).
/*quinto año*/
materia(proyectoFinal).
materia(inteligenciaArtificial).
materia(administracionGerencial).
materia(sistemasDeGestion).


/*Materias que deben ser aprobadas para aprobar una materia: correlativaA(materia, materiasQueDebenSerAprobadas)*/
correlativa(analisisDeSistemas, syo).
correlativa(analisisDeSistemas, algoritmos).
correlativa(am2, am1).
correlativa(am2, algebra).
correlativa(sintaxis, discreta).
correlativa(sintaxis, algoritmos).
correlativa(paradigmas, discreta).
correlativa(paradigmas, algoritmos).
correlativa(probabilidad, am1).
correlativa(probabilidad, algebra).
correlativa(disenioDeSistemas, analisisDeSistemas).
correlativa(disenioDeSistemas, paradigmas).
correlativa(sistemasOperativos, discreta).
correlativa(sistemasOperativos, algoritmos).
correlativa(sistemasOperativos, arquitecturaDeCompuradoras).
correlativa(fisica2, am1).
correlativa(fisica2, fisica1).
correlativa(economia, analisisDeSistemas).
correlativa(gestionDeDatos, analisisDeSistemas).
correlativa(gestionDeDatos, paradigmas).
correlativa(gestionDeDatos, sintaxis).
correlativa(ingles2, ingles1).
correlativa(matematicaSuperior, am2).
correlativa(legislacion, analisisDeSistemas).
correlativa(legislacion, ingenieriaYSociedad).
correlativa(administracionDeRecursos, disenioDeSistemas).
correlativa(administracionDeRecursos, sistemasOperativos).
correlativa(administracionDeRecursos, economia).
correlativa(ingenieriaDeSoftware, probabilidad).
correlativa(ingenieriaDeSoftware, disenioDeSistemas).
correlativa(ingenieriaDeSoftware, gestionDeDatos).
correlativa(teoriaDeControl, quimica).
correlativa(teoriaDeControl, matematicaSuperior).
correlativa(comunicaciones, arquitecturaDeCompuradoras).
correlativa(comunicaciones, am2).
correlativa(comunicaciones, fisica2).
correlativa(redesDeInformacion, sistemasOperativos).
correlativa(redesDeInformacion, comunicaciones).
correlativa(investigacionOperativa, probabilidad).
correlativa(investigacionOperativa, matematicaSuperior).
correlativa(simulacion, probabilidad).
correlativa(simulacion, matematicaSuperior).
correlativa(inteligenciaArtificial, investigacionOperativa).
correlativa(inteligenciaArtificial, simulacion).
correlativa(administracionGerencial, administracionDeRecursos).
correlativa(administracionGerencial, investigacionOperativa).
correlativa(sistemasDeGestion, administracionDeRecursos).
correlativa(sistemasDeGestion, investigacionOperativa).
correlativa(sistemasDeGestion, simulacion).
correlativa(proyectoFinal, Materia) :-
    materia(Materia),
    Materia \= proyectoFinal.

correlativaCC(analisisDeSistemas, syo).
correlativaCC(analisisDeSistemas, algoritmos).
correlativaCC(am2, am1).
correlativaCC(am2, algebra).
correlativaCC(sintaxis, discreta).
correlativaCC(sintaxis, algoritmos).
correlativaCC(paradigmas, discreta).
correlativaCC(paradigmas, algoritmos).
correlativaCC(disenioDeSistemas, analisisDeSistemas).
correlativaCC(disenioDeSistemas, paradigmas).
correlativaCC(sistemasOperativos, discreta).
correlativaCC(sistemasOperativos, algoritmos).
correlativaCC(sistemasOperativos, arquitecturaDeCompuradoras).
correlativaCC(fisica2, am1).
correlativaCC(fisica2, fisica1).
correlativaCC(economia, analisisDeSistemas).
correlativaCC(gestionDeDatos, analisisDeSistemas).
correlativaCC(gestionDeDatos, paradigmas).
correlativaCC(gestionDeDatos, sintaxis).
correlativaCC(matematicaSuperior, am2).
correlativaCC(legislacion, analisisDeSistemas).
correlativaCC(legislacion, ingenieriaYSociedad).
correlativaCC(administracionDeRecursos, disenioDeSistemas).
correlativaCC(administracionDeRecursos, sistemasOperativos).
correlativaCC(administracionDeRecursos, economia).
correlativaCC(ingenieriaDeSoftware, probabilidad).
correlativaCC(ingenieriaDeSoftware, disenioDeSistemas).
correlativaCC(ingenieriaDeSoftware, gestionDeDatos).
correlativaCC(teoriaDeControl, quimica).
correlativaCC(teoriaDeControl, matematicaSuperior).
correlativaCC(comunicaciones, arquitecturaDeCompuradoras).
correlativaCC(comunicaciones, am2).
correlativaCC(comunicaciones, fisica2).
correlativaCC(redesDeInformacion, sistemasOperativos).
correlativaCC(redesDeInformacion, comunicaciones).
correlativaCC(investigacionOperativa, probabilidad).
correlativaCC(investigacionOperativa, matematicaSuperior).
correlativaCC(simulacion, probabilidad).
correlativaCC(simulacion, matematicaSuperior).
correlativaCC(proyectoFinal, legislacion).
correlativaCC(proyectoFinal, administracionDeRecursos).
correlativaCC(proyectoFinal, redesDeInformacion).
correlativaCC(proyectoFinal, ingenieriaDeSoftware).
correlativaCC(inteligenciaArtificial, investigacionOperativa).
correlativaCC(inteligenciaArtificial, simulacion).
correlativaCC(administracionGerencial, administracionDeRecursos).
correlativaCC(administracionGerencial, investigacionOperativa).
correlativaCC(sistemasDeGestion, administracionDeRecursos).
correlativaCC(sistemasDeGestion, investigacionOperativa).
correlativaCC(sistemasDeGestion, simulacion).

correlativaCA(disenioDeSistemas, discreta).
correlativaCA(disenioDeSistemas, algoritmos).
correlativaCA(disenioDeSistemas, syo).
correlativaCA(economia, algoritmos).
correlativaCA(economia, syo).
correlativaCA(gestionDeDatos, discreta).
correlativaCA(gestionDeDatos, algoritmos).
correlativaCA(gestionDeDatos, syo).
correlativaCA(ingles2, ingles1).
correlativaCA(matematicaSuperior, am1).
correlativaCA(matematicaSuperior, algebra).
correlativaCA(legislacion, syo).
correlativaCA(legislacion, algoritmos).
correlativaCA(administracionDeRecursos, arquitecturaDeCompuradoras).
correlativaCA(administracionDeRecursos, ingles1).
correlativaCA(administracionDeRecursos, analisisDeSistemas).
correlativaCA(administracionDeRecursos, paradigmas).
correlativaCA(ingenieriaDeSoftware, analisisDeSistemas).
correlativaCA(ingenieriaDeSoftware, sintaxis).
correlativaCA(ingenieriaDeSoftware, paradigmas).
correlativaCA(teoriaDeControl, am2).
correlativaCA(teoriaDeControl, fisica2).
correlativaCA(comunicaciones, am1).
correlativaCA(comunicaciones, algebra).
correlativaCA(comunicaciones, fisica1).
correlativaCA(redesDeInformacion, discreta).
correlativaCA(redesDeInformacion, algoritmos).
correlativaCA(redesDeInformacion, arquitecturaDeCompuradoras).
correlativaCA(redesDeInformacion, am2).
correlativaCA(redesDeInformacion, fisica2).
correlativaCA(investigacionOperativa, am2).
correlativaCA(simulacion, am2).
correlativaCA(proyectoFinal, ingenieriaYSociedad).
correlativaCA(proyectoFinal, sistemasDeRepresentacion).
correlativaCA(proyectoFinal, probabilidad).
correlativaCA(proyectoFinal, sistemasOperativos).
correlativaCA(proyectoFinal, disenioDeSistemas).
correlativaCA(proyectoFinal, gestionDeDatos).
correlativaCA(proyectoFinal, economia).
correlativaCA(proyectoFinal, ingles2).
correlativaCA(proyectoFinal, comunicaciones).
correlativaCA(inteligenciaArtificial, probabilidad).
correlativaCA(inteligenciaArtificial, disenioDeSistemas).
correlativaCA(inteligenciaArtificial, matematicaSuperior).
correlativaCA(administracionGerencial, probabilidad).
correlativaCA(administracionGerencial, sistemasOperativos).
correlativaCA(administracionGerencial, disenioDeSistemas).
correlativaCA(administracionGerencial, matematicaSuperior).
correlativaCA(administracionGerencial, economia).
correlativaCA(sistemasDeGestion, probabilidad).
correlativaCA(sistemasDeGestion, sistemasOperativos).
correlativaCA(sistemasDeGestion, disenioDeSistemas).
correlativaCA(sistemasDeGestion, matematicaSuperior).
correlativaCA(sistemasDeGestion, economia).


/*Nombre de cada materia*/
es(syo,['Sistemas y Organizaciones', 'SyO', 'Sistemas y Organizaciones', 'sistemas y organizaciones', 'syo', 'SISTEMAS Y ORGANIZACIONES']).
es(am1, ['Análisis Matemático I', 'am1', 'Analisis Matematico I', 'Analisis Matematico 1', 'analisis matematico 1', 'analisis matematico I','ANALISIS MATEMATICO I']).
es(discreta, ['Matemática Discreta', 'discreta', 'matematica discreta', 'Matematica Discreta']).
es(algebra, ['Álgebra y Geometria Analítica', 'aga', 'ayga', 'AGA', 'Algebra y Geometria Analitica', 'algebra y geometria analitica','ALGEBRA Y GEOMETRIA ANALITICA']).
es(algoritmos, ['Algoritmos y Estructura de Datos', 'AyED', 'algoritmos', 'algoritmos y estructura de datos', 'ALGORITMOS Y ESTRUCTURA DE DATOS']).
es(arquitecturaDeCompuradoras, ['Arquitectura de Computadoras', 'arquitectura', 'arquitectura de computadoras', 'ARQUITECTURA DE COMPUTADORAS', 'Arquitectura de computadoras']).
es(quimica, ['Química', 'quimica', 'Quimica', 'qck', 'QUIMICA']).
es(ingenieriaYSociedad, ['Ingeniería y Sociedad', 'IyS', 'Ingenieria y Sociedad', 'Ingenieria y sociedad', 'ingenieria y sociedad', 'INGENIERIA Y SOCIEDAD']).
es(analisisDeSistemas, ['Análisis de Sistemas','AdS', 'ads', 'ADS', 'Analisis de Sistemas', 'analisis de sistemas', 'Analisis de sistemas', 'ANALISIS DE SISTEMAS']).
es(sistemasDeRepresentacion, ['Sistemas de Representacion','SdR','sdr','Sistemas de Representación',
                              'Sistemas de representacion','sistemas de representacion', 'SISTEMAS DE REPRESENTACION']).
es(am2, ['Análisis Matemático II', 'am2','amII','Analisis Matematico 2', 'analisis matematico 2', 'analisis 2', 'AM2', 'analisis matematico II',
         'Analisis Matematico II', 'analisis matematico ii', 'ANALISIS MATEMATICO II']).
es(sintaxis, ['Sintaxis y Semántica de los Lenguajes', 'sintaxis', 'Sintaxis', 'SSdL', 'SSDL', 'SINTAXIS Y SEMANTICA DE LOS LENGUAJES',
              'Sintaxis y Semantica de los Lenguajes', 'sintaxis y semantica de los lenguajes', 'sintaxis y semantica','sintaxis y semantica de lenguajes']).
es(fisica1, ['Física I', 'f2', 'Fisica 1', 'Fisica I', 'Física 1', 'fisica i', 'fisica 1', 'FISICA I', 'FISICA 1']).
es(paradigmas, ['Paradigmas de Programación','paradigmas','paradigmas de programacion',
                'Paradigmas de Programacion', 'paradigmas de programación', 'Paradigmas', 'PARADIGMAS DE PROGRAMACION']).
es(ingles1, ['Inglés I','Ingles 1', 'Ingles I', 'Inglés 1','ingles 1', 'ingles I', 'ingles i','INGLES 1','INGLES I']).
es(probabilidad, ['Probabilidad y Estadística', 'proba', 'probabilidad', 'probabilidad y estadistica',
                  'Probabilidad y Estadistica', 'Probabilidad y estadistica', 'PROBABILIDAD Y ESTADISTICA']).
es(disenioDeSistemas, ['Diseño de Sistemas','DdS','Disenio de Sistemas', 'Diseño de sistemas', 'diseño de sistemas', 'DISEÑO DE SISTEMAS']).
es(sistemasOperativos, ['Sistemas Operativos','SO','SISTEMAS OPERATIVOS','sistemas operativos', 'Sistemas operativos']).
es(fisica2, ['Física II', 'Fisica 2', 'Fisica II', 'fisica 2', 'fisica II', 'fisica ii', 'FISICA II']).
es(economia, ['Economía', 'ECONOMIA', 'economia', 'Economia', 'economía']).
es(gestionDeDatos, ['Gestión de Datos','GdD', 'Gestion de Datos', 'gestion de datos', 'gestión de datos', 'GESTION DE DATOS', 'GESTIÓN DE DATOS']).
es(ingles2, ['Ingles II', 'ingles 2', 'Ingles 2', 'ingles ii', 'INGLES II', 'Inglés II', 'Inglés 2']).
es(matematicaSuperior, ['Matemática Superior', 'matematica superior', 'MATEMATICA SUPERIOR', 'Matematica Superior', 'Matematica superior','MATEMÁTICA SUPERIOR']).
es(legislacion, ['Legislación','legislacion', 'Legislacion', 'LEGISLACION', 'LEGISLACIÓN', 'legis']).
es(administracionDeRecursos, ['Administración de Recursos', 'ADMINISTRACION DE RECURSOS', 'administracion de recursos',
                              'Administracion de recursos', 'Administración de Recursos','ADMINISTRACIÓN DE RECURSOS']).
es(ingenieriaDeSoftware, ['Ingeniería de Software', 'IdS', 'Ingenieria en software', 'Ingenieria de software', 'Ingenieria de Software',
                          'ingenieria de software', 'INGENIERIA DE SOFTWARE', 'INGENIERÍA DE SOFTWARE']).
es(teoriaDeControl, ['Teoría de Control','TdC', 'TEORIA DE CONTROL', 'Teoria de Control', 'teoria de control', 'Teoria de control']).
es(comunicaciones, ['Comunicaciones', 'comunicaciones', 'COMUNICACIONES', 'comunicacion', 'COMUNICACION']).
es(redesDeInformacion, ['Redes de Información', 'redes de informacion', 'Redes de Informacion', 'REDES DE INFORMACION', 'Redes de informacion', 'RdI','rdi']).
es(investigacionOperativa, ['Investigación Operativa', 'investigacion operativa', 'INVESTIGACION OPERATIVA', 'INVESTIGACIÓN OPERATIVA',
                            'Investigacion operativa', 'investigación operativa']).
es(simulacion, ['Simulación', 'simulacion', 'simulación', 'SIMULACION','SIMULACIÓN']).
es(proyectoFinal, ['Proyecto Final','PROYECTO FINAL', 'proyecto final', 'Proyecto final']).
es(inteligenciaArtificial, ['Inteligencia Artificial', 'Inteligencia artificial', 'inteligencia artificial', 'INTELIGENCIA ARTIFICIAL']).
es(administracionGerencial, ['Administración Gerencial', 'ADMINISTRACION GERENCIAL', 'ADMIN GERENCIAL', 'admin gerencial',
                             'administracion gerencial', 'administración gerencial']).
es(sistemasDeGestion, ['Sistemas de Gestión','SdG','Sistemas de Gestion', 'sdg', 'SISTEMAS DE GESTION', 'sistemas de gestion', 'sistemas de gestión']).

/*Año de las materias: anio(Materia, AñoDeLaMateria)*/
anio(syo, primero).
anio(am1, primero).
anio(discreta, primero).
anio(algoritmos, primero).
anio(arquitecturaDeCompuradoras, primero).
anio(algebra, primero).
anio(quimica, primero).
anio(ingenieriaYSociedad, primero).
anio(analisisDeSistemas, segundo).
anio(sistemasDeRepresentacion, segundo).
anio(am2, segundo).
anio(sintaxis, segundo).
anio(fisica1, segundo).
anio(paradigmas, segundo).
anio(ingles1, segundo).
anio(probabilidad, segundo).
anio(disenioDeSistemas, tercero).
anio(sistemasOperativos, tercero).
anio(fisica2, tercero).
anio(economia, tercero).
anio(gestionDeDatos, tercero).
anio(ingles2, tercero).
anio(matematicaSuperior, tercero).
anio(legislacion, tercero).
anio(administracionDeRecursos, cuarto).
anio(ingenieriaDeSoftware, cuarto).
anio(teoriaDeControl, cuarto).
anio(comunicaciones, cuarto).
anio(redesDeInformacion, cuarto).
anio(investigacionOperativa, cuarto).
anio(simulacion, cuarto).
anio(proyectoFinal, quinto).
anio(inteligenciaArtificial, quinto).
anio(administracionGerencial, quinto).
anio(sistemasDeGestion, quinto).

/*MATERIAS QUE SE DEBEN HABER APROBADO PARA CURSAR UNA MATERIA*/
buscarCorrelativasCA(Ingresar) :-
    tieneQueAprobarMateriasDe(Ingresar, _, _, _, _, _).

tieneQueAprobarMateriasDe(MateriaString, _, _, _, _, _) :-
    es(Materia, Posibilidades),
    member(MateriaString, Posibilidades),
    seNecesitaAprobarA(Materia, ListaMaterias),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  primero),  Primero),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  segundo),  Segundo),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  tercero),  Tercero),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  cuarto),   Cuarto),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  quinto),   Quinto),

    new(Y, dialog('REQUISITOS PARA CURSAR: Materias Aprobadas')),
    new(L1, label(texto, 'Materias de primer año', bold)),
    new(L2, label(texto, 'Materias de segundo año', bold)),
    new(L3, label(texto, 'Materias de tercer año', bold)),
    new(L4, label(texto, 'Materias de cuarto año', bold)),
    new(L5, label(texto, 'Materias de quinto año', bold)),
    send(Y, append, new(C1, dialog_group(texts, group))),
    send(C1, append, L1),
    send(C1, append, new(List1, browser), below),
    send_list(List1, append, Primero),
    send(Y, append, new(C2, dialog_group(texts, group)), right),
    send(C2, append, L2, right),
    send(C2, append, new(List2, browser)),
    send_list(List2, append, Segundo),
    send(Y, append, new(C3, dialog_group(texts, group)), right),
    send(C3, append, L3,right),
    send(C3, append, new(List3, browser)),
    send_list(List3, append, Tercero),
    send(Y, append, new(C4, dialog_group(texts, group)), right),
    send(C4, append, L4, right),
    send(C4, append, new(List4, browser)),
    send_list(List4, append, Cuarto),
    send(Y, append, new(C5, dialog_group(texts, group)), right),
    send(C5, append, L5, right),
    send(C5, append, new(List5, browser)),
    send_list(List5, append, Quinto),
    send(Y, append, button(volver,
                               and(message(Y, free), message(Y, destroy)))),
    send(Y, open_centered).

seNecesitaAprobarA(Materia, Aprobadas) :-
    findall(Aprobada, (materia(Aprobada),tieneRelacionAprobadaA(Materia, Aprobada)), MateriasNecesarias),
    list_to_set(MateriasNecesarias, Aprobadas).

tieneRelacionAprobadaA(Materia, Aprobada) :-
    correlativaCA(Materia, Aprobada).
tieneRelacionAprobadaA(Materia, Aprobada) :-
    correlativaCA(Materia, UnaMateria),
    tieneRelacionAprobadaA(UnaMateria, Aprobada).


/*MATERIAS QUE SE DEBEN HABER CURSADO PARA CURSAR UNA MATERIA*/

buscarCorrelativasCC(Ingresar) :-
    tieneQueCursarMateriasDe(Ingresar, _, _, _, _, _).

tieneQueCursarMateriasDe(MateriaString, _, _, _, _, _) :-
    es(Materia, Posibilidades),
    member(MateriaString, Posibilidades),
    seNecesitaCursar(Materia, ListaMaterias),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  primero),  Primero),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  segundo),  Segundo),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  tercero),  Tercero),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  cuarto),   Cuarto),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  quinto),   Quinto),


    new(X, dialog('REQUISITOS PARA CURSAR - Materias Cursadas')),

    new(L1, label(texto, 'Materias de primer año', bold)),
    new(L2, label(texto, 'Materias de segundo año', bold)),
    new(L3, label(texto, 'Materias de tercer año', bold)),
    new(L4, label(texto, 'Materias de cuarto año', bold)),
    new(L5, label(texto, 'Materias de quinto año', bold)),

    send(X, append, new(C1, dialog_group(texts, group))),
    send(C1, append, L1),
    send(C1, append, new(List1, browser), below),
    send_list(List1, append, Primero),

    send(X, append, new(C2, dialog_group(texts, group)), right),
    send(C2, append, L2, right),
    send(C2, append, new(List2, browser)),
    send_list(List2, append, Segundo),

    send(X, append, new(C3, dialog_group(texts, group)), right),
    send(C3, append, L3,right),
    send(C3, append, new(List3, browser)),
    send_list(List3, append, Tercero),

    send(X, append, new(C4, dialog_group(texts, group)), right),
    send(C4, append, L4, right),
    send(C4, append, new(List4, browser)),
    send_list(List4, append, Cuarto),

    send(X, append, new(C5, dialog_group(texts, group)), right),
    send(C5, append, L5, right),
    send(C5, append, new(List5, browser)), /*o list_browser*/
    send_list(List5, append, Quinto),

    /*send(W, append, new(_, text_item(materia, prolog(Algo)))),*/

    send(X, append, button(volver,
                               and(message(X, free), message(X, destroy)))),

    send(X, open_centered).

seNecesitaCursar(Materia, Cursadas) :-
    findall(Cursada, (materia(Cursada),tieneRelacionCursada(Materia, Cursada)), MateriasNecesarias),
    list_to_set(MateriasNecesarias, Cursadas).

tieneRelacionCursada(Materia, Cursada) :-
    correlativaCC(Materia, Cursada).
tieneRelacionCursada(Materia, Cursada) :-
    correlativaCC(Materia, UnaMateria),
    tieneRelacionCursada(UnaMateria, Cursada).





/*CORRELATIVAS QUE SE NECESITAN PARA APROBAR UNA MATERIA*/

buscarCorrelativasRA(Ingresar) :-
    tieneMateriasDe(Ingresar, _, _, _, _, _).


tieneMateriasDe(MateriaString, Primero, Segundo, Tercero, Cuarto, Quinto) :-
    es(LaMateria, Posibilidades),
    member(MateriaString, Posibilidades),
    seNecesitaAprobar(LaMateria, ListaMaterias),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  primero),  Primero),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  segundo),  Segundo),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  tercero),  Tercero),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  cuarto),   Cuarto),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  quinto),   Quinto),


    new(W, dialog('REQUISITOS PARA FIRMAR: Materias Aprobadas')),

    new(L1, label(texto, 'Materias de primer año', bold)),
    new(L2, label(texto, 'Materias de segundo año', bold)),
    new(L3, label(texto, 'Materias de tercer año', bold)),
    new(L4, label(texto, 'Materias de cuarto año', bold)),
    new(L5, label(texto, 'Materias de quinto año', bold)),

    send(W, append, new(C1, dialog_group(texts, group))),
    send(C1, append, L1),
    send(C1, append, new(List1, browser), below),
    send_list(List1, append, Primero),

    send(W, append, new(C2, dialog_group(texts, group)), right),
    send(C2, append, L2, right),
    send(C2, append, new(List2, browser)),
    send_list(List2, append, Segundo),

    send(W, append, new(C3, dialog_group(texts, group)), right),
    send(C3, append, L3,right),
    send(C3, append, new(List3, browser)),
    send_list(List3, append, Tercero),

    send(W, append, new(C4, dialog_group(texts, group)), right),
    send(C4, append, L4, right),
    send(C4, append, new(List4, browser)),
    send_list(List4, append, Cuarto),

    send(W, append, new(C5, dialog_group(texts, group)), right),
    send(C5, append, L5, right),
    send(C5, append, new(List5, browser)), /*o list_browser*/
    send_list(List5, append, Quinto),

    /*send(W, append, new(_, text_item(materia, prolog(Algo)))),*/

    send(W, append, button(volver,
                               and(message(W, free), message(W, destroy)))),

    send(W, open_centered).


seNecesitaAprobar(Materia, Aprobadas) :-
    findall(Aprobada, (materia(Aprobada), tieneRelacion(Materia, Aprobada)), MateriasNecesarias),
    list_to_set(MateriasNecesarias, Aprobadas).

tieneRelacion(Materia, Aprobada) :-
    correlativa(Materia, Aprobada).
tieneRelacion(Materia, Aprobada) :-
    correlativa(Materia, UnaMateria),
    tieneRelacion(UnaMateria, Aprobada).

pertenezcaA(ListaMaterias, NombreMateria, Anio) :-
    anio(UnaMateria, Anio),
    member(UnaMateria, ListaMaterias),
    es(UnaMateria, ListaNombres),
    nth0(0, ListaNombres, NombreMateria).


save(Exe):- qsave_program(Exe,[stand_alone(true), goal(main)]).

