:- use_module(library(pce)).

:- pce_image_directory('C:/Program Files/Correlativas').
:- pce_image_directory('C:/Archivos de Programa/Correlativas').



principal :-
    new(P, dialog('ADS - Correlatividad')),

    nueva_imagen(P, _, fondito, point(100,200)),


    /*BOTONES*/
    new(Mostrar, button('Mostrar Correlativas',
                           and(message(@prolog,
                                       interfazMostrar)))),

    new(Ingresar, button('Ingresar Materias',
                        and(message(@prolog, interfazIngresar)))),

    new(Cerrar, button('Cerrar',
                          and(message(P, free), message(P, destroy)))),


    send(P, append, Ingresar),

    send(P, append, Mostrar),

    send(P, append, Cerrar, below),

    send(P, open_centered).




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

    new(L1, label(texto,'INGRESE UNA MATERIA PARA CONOCER SUS CORRELATIVAS',
                  italic)),

    send(D, append, L1),

    send(D, append, new(Ingresar, /*Ingreso la materia*/
                        text_item(ingresar_materia,'ingrese una materia..'))),


    send(D, append, button('Ok', and(message(@prolog,
                                           buscarCorrelativasDe,
                                           Ingresar?selection),
                                     message(Ingresar, clear)))),


    send(D, append, button('Cancel', and(message(D, free),message(D, destroy)))),

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
/*primer a�o*/
materia(syo).
materia(am1).
materia(discreta).
materia(algebra).
materia(algoritmos).
materia(arquitecturaDeCompuradoras).
materia(quimica).
materia(ingenieriaYSociedad).
/*segundo a�o*/
materia(analisisDeSistemas).
materia(sistemasDeRepresentacion).
materia(am2).
materia(sintaxis).
materia(fisica1).
materia(paradigmas).
materia(ingles1).
materia(probabilidad).
/*tercer a�o*/
materia(disenioDeSistemas).
materia(sistemasOperativos).
materia(fisica2).
materia(economia).
materia(gestionDeDatos).
materia(ingles2).
materia(matematicaSuperior).
materia(legislacion).
/*cuarto a�o*/
materia(administracionDeRecursos).
materia(ingenieriaDeSoftware).
materia(teoriaDeControl).
materia(comunicaciones).
materia(redesDeInformacion).
materia(investigacionOperativa).
materia(simulacion).
/*quinto a�o*/
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

/*Nombre de cada materia*/
es(syo, "Sistemas y Organizaciones").
es(am1, "An�lisis Matem�tico I").
es(discreta, "Matem�tica Discreta").
es(algebra, "�lgebra y Geometria Anal�tica").
es(quimica, "Qu�mica").
es(ingenieriaYSociedad, "Ingenier�a y Sociedad").
es(analisisDeSistemas, "An�lisis de Sistemas").
es(sistemasDeRepresentacion, "Sistemas de Representacion").
es(am2, "An�lisis Matem�tico II").
es(sintaxis, "Sintaxis y Sem�ntica de los Lenguajes").
es(fisica1, "F�sica I").
es(paradigmas, "Paradigmas de Programaci�n").
es(ingles1, "Ingl�s I").
es(probabilidad, "Probabilidad y Estad�stica").
es(disenioDeSistemas, "Dise�o de Sistemas").
es(sistemasOperativos, "Sistemas Operativos").
es(fisica2, "F�sica II").
es(economia, "Econom�a").
es(gestionDeDatos, "Gesti�n de Datos").
es(ingles2, "Ingles II").
es(matematicaSuperior, "Matem�tica Superior").
es(legislacion, "Legislaci�n").
es(administracionDeRecursos, "Administraci�n de Recursos").
es(ingenieriaDeSoftware, "Ingenier�a de Software").
es(teoriaDeControl, "Teor�a de Control").
es(comunicaciones, "Comunicaciones").
es(redesDeInformacion, "Redes de Informaci�n").
es(investigacionOperativa, "Investigaci�n Operativa").
es(simulacion, "Simulaci�n").
es(proyectoFinal, "Proyecto Final").
es(inteligenciaArtificial, "Inteligencia Artificial").
es(administracionGerencial, "Administraci�n Gerencial").
es(sistemasDeGestion, "Sistemas de Gesti�n").


/*A�o de las materias: anio(Materia, A�oDeLaMateria)*/
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



buscarCorrelativasDe(Ingresar) :-
    tieneMateriasDe(Ingresar, _, _, _, _, _).


tieneMateriasDe(LaMateria, Primero, Segundo, Tercero, Cuarto, Quinto) :-
    seNecesitaAprobar(LaMateria, ListaMaterias),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  primero),  Primero),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  segundo),  Segundo),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  tercero),  Tercero),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  cuarto),   Cuarto),
    findall(NombreMateria, pertenezcaA(ListaMaterias, NombreMateria,  quinto),   Quinto),


    new(W, dialog('Materias')),

    new(L1, label(texto, 'Materias de primer a�o', bold)),
    new(L2, label(texto, 'Materias de segundo a�o', bold)),
    new(L3, label(texto, 'Materias de tercer a�o', bold)),
    new(L4, label(texto, 'Materias de cuarto a�o', bold)),
    new(L5, label(texto, 'Materias de quinto a�o', bold)),

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
    es(UnaMateria, NombreMateria).


save(Exe):- pce_autoload_all, qsave_program(Exe,[ emulator(swi('bin/xpce-stub.exe')), stand_alone(true), goal(main) ]).
