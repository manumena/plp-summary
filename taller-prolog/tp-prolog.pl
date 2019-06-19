:- dynamic si/1, no/1, atributos/2.

adivinarPersonaje :- atributos(P, A), satisfaceAtributos(A), mostrarPersonaje(P), borraRespuestas, !.
adivinarPersonaje :- leerPersonaje(P), leerAtributos(AS), atributosSatisfechos(ASatisfechos), append(AS, ASatisfechos, ACS), agregarPersonaje(P, ACS). % No olvidar que ACS puede tener repetidos
adivinarPersonaje :- borraRespuestas.

% agregarPersonaje(+Nombre, +Atributos).
agregarPersonaje(N, A) :- not(atributosExistenEnUnPersonaje(A)), asserta(atributos(N, A)).
% ¿Es lo mismo utilizar asserta y assertz en este caso? ¿Por que?
% No, cambia el orden de las preguntas

%atributosExistenEnUnPersonaje(+Atributos).
atributosExistenEnUnPersonaje(AS) :- sort(AS, AS_sorted), atributos(_, XS), sort(XS, AExistentes_sorted), iguales(AS_sorted, AExistentes_sorted).

iguales([], []).
iguales([H1|R1], [H2|R2]):- H1 = H2, iguales(R1, R2).

% mostrarPersonaje(+Nombre).
mostrarPersonaje(Nombre) :- write(Nombre), write('\n').

borraRespuestas :- retractall(si(_)), retractall(no(_)).

% atributos(?Nombre, ?Atributos).

atributos('Axel', ['Fachero', 'Anteojudo']).
atributos('Gus', ['Loco', 'Vista de lince O_o', 'Skater']).
atributos('Flor', ['Skater', 'Verdecita']).
atributos('Manu', ['Skater', 'Fachero']).

% satisfaceAtributos(+Atributos).
satisfaceAtributos([]).
satisfaceAtributos([X|XS]) :- satisface(X), satisfaceAtributos(XS).

% satisface(+Atributo).
satisface(A) :- si(A), !.
satisface(A) :- no(A), !, fail.
satisface(A) :- pregunta(A), satisface(A).

% pregunta(+Atributo).
pregunta(A) :- mostrarPregunta(A), leerRespuesta(R), guardarRespuesta(A,R).

% mostrarPregunta(+Atributo).
mostrarPregunta(A) :- write(A), write('?\n').

% leerRespuesta(-Respuesta).
leerRespuesta(R) :- read(R).

% leerPersonaje(-Personaje).
leerPersonaje(P) :- write('Nombre del personaje: '), read(P).

%leerAtributos(-Atributos).
leerAtributos(AS) :- write('Lista de atributos del personaje: '), read(AS).

%atributosSatisfechos(-Atributos).
atributosSatisfechos(AS) :- setof(X, si(X), AS).

% guardarRespuesta(+Atributo, +Respuesta).
guardarRespuesta(A, R) :- R == 'si', !, assertz(si(A)).
guardarRespuesta(A, R) :- R == 'no', !, assertz(no(A)).
guardarRespuesta(A, R) :- write('Respuesta inválida. Se pregunta nuevamente.\n'), pregunta(A).

% ¿Que ocurre cuando el juego no conoce el personaje? 
% En nuestro caso solo borra las respuestas y retorna true al hacerlo.
% ¿Como influye el orden de los atributos en el predicado atributos(Personaje, Atributos)?
% Es el orden de las preguntas.

%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%
test(1) :- true.
tests :- forall(between(1,1,N), test(N)). % Hacer mejores tests y cambiar 1 por la cantidad de tests que tengan.

