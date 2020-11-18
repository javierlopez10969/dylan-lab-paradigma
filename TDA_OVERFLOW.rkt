#lang racket
(require "funciones.rkt")
(require "TDAFecha.rkt")
;EJEMPLO 2
;sin usuarios.
(define stackoverflow2
  (list
   ;0 tiene todos los usuarios y contraseñas
   (list  )
   )
)
;EJemplo de stack 
;EJEMPLO 1
;con 1 usuario
;TDA stackoverflow
(define stackoverflow
  (list
  ;0
  ;tiene todos los usuarios y contraseñas
  ;TDA usuario contraseña
  ;LISTA
  ;TDA base de datos de usuarios y contraseñas
  ;LISTA X LISTAS
  (list (list "juan01" "clave123" 0)(list "diego02" "clave123" 0))
  ;1
  ;Penultimo reward
  ;ultimo estado de pregunta
  ;TDA PREGUNTA 1/0 es respondida o no.
  (list (list 1 "pregunta" "juan01" (list "etiquetas" "C" "universidad") "10/12/2020" 0 1 ))
  ;2
  ;TDA Respuestas
  (list (list 1 (list 1 "diego02" "12/12/2020"  "respuesta" (list "etiquetas" "c"))))
  ;3
  ;Sección de usario con sesión abierta
  (list)
  )
)
;Constructor
(define construirStack (lambda () (list (list) (list) (list) (list))))
;Pertenencia de stackoverflow.
;Ejemplo:(esStackoverflow? stackoverflow)
(define (esStackoverflow? stackoverflow)
  (if (list? stackoverflow)
      (and (list? (selectorDato stackoverflow 0))
          (list? (selectorDato stackoverflow 1))
          (list? (selectorDato stackoverflow 2)))
  #f
  )
)

;selectores
(define (GetListaDeUsuarios stack) (selectorDato stack 0))
(define (GetListaDePreguntas stack) (selectorDato stack 1))
(define (GetListaDeRespuestas stack) (selectorDato stack 2))
(define (GetActiveUsuario stack) (selectorDato stack 3))
(define (GetUsuarioLogeado stack) (selectorDato (selectorDato stack 3) 0))

(define (logear stack user password)
  (cambiarDato stack 3(list user password)))

(define (deslogear stack)
  (cambiarDato stack 3(list)))

;Obtener cuanto reward da cierta pregunta dado el id de esta
;EJEMPLO  :(getRewardPregunta (GetListaDePreguntas stackoverflow) 1)
(define getRewardPregunta (lambda (lista idPregunta)
                            (selectorDato (selectorDato lista (getIndicePreguntaID lista idPregunta)) 5)))

;Obtener el usuario de una respuesta
;Ejemplo  : (getUsuarioAnswer (GetListaDeRespuestas stackoverflow) 1 1)
(define getUsuarioAnswer
(lambda(lista idPregunta idRespuesta)
  (define buscarUsuario
    (lambda (lista idRespuesta)
      (cond
        ;Hasta que llege a ser nulo
        [( null? lista) #f]
        ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
        [(equal? (car(car lista)) idRespuesta) (selectorDato (car lista) 1) ]
        [else(buscarUsuario (cdr lista) idRespuesta)])))
  (cond
    ;Hasta que llege a ser nulo
    [( null? lista) #f]
    ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
    [(equal? (car(car lista)) idPregunta) (buscarUsuario (cdr(car lista)) idRespuesta) ]
    [else(getUsuarioAnswer (cdr lista) idPregunta idRespuesta)])))
;(getIndiceUsuario stackoverflow "juan01")
(define getIndiceUsuario
  (lambda (stack usuario)
    (define buscarIndice
      (lambda (lista usuario i)
        (cond
          ;Hasta que llege a ser nulo
          [( null? lista) -1]
          ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
          [(equal? (car(car lista)) usuario) i]
          [else (buscarIndice (cdr lista) usuario (+ i 1))])))
    (buscarIndice (GetListaDeUsuarios stack) usuario 0)))
;(asignarRewardUsuario stackoverflow "juan01" 100)
(define asignarRewardUsuario (lambda (stack usuario reward)
                               (cambiarDato stack 0 (cambiarDato (GetListaDeUsuarios stack) (getIndiceUsuario stack usuario) (cambiarDato (selectorDato (GetListaDeUsuarios stack) (getIndiceUsuario stack usuario)) 2 reward ))) ))



(define preguntaCorresponde?
  (lambda (lista id user)
    (cond
      ;Hasta que llege a ser nulo
      [( null? lista) #f]
      ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
      [(equal? (car(car lista)) id) (equal? (selectorDato (car lista) 2) user) ]
      [else(preguntaCorresponde? (cdr lista) id user)])))


(define existePreguntaID?(lambda(lista id)
                            (cond
                              ;Hasta que llege a ser nulo
                              [( null? lista) #f]
                              ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
                              [(equal? (car(car lista)) id) #t ]
                              [else(existePreguntaID? (cdr lista) id)])))

;Entrada : lista de respuestas
(define existeRespuestaID?(lambda(lista idPregunta idRespuesta)
                            (cond
                              ;Hasta que llege a ser nulo
                              [( null? lista) #f]
                              ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
                              [(equal? (car(car lista)) idPregunta) (existePreguntaID? (cdr(car lista)) idRespuesta) ]
                              [else(existeRespuestaID? (cdr lista) idPregunta idRespuesta)])))

;Get indice de pregunta en una lista de respuestas
; EJEMPLO : (getIndicePreguntaID (GetListaDeRespuestas stackoverflow) 1)
(define getIndicePreguntaID (lambda(lista id)
                (define buscar (lambda(lista id i)
                            (cond
                              ;Hasta que llege a ser nulo
                              [( null? lista) #f]
                              ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
                              [(equal? (car(car lista)) id) i ]
                              ;Caso recursivo
                              [else(buscar (cdr lista) id (+ i 1))])))
               (buscar lista id 0)))

(define asignarRewardPreguntaID(lambda (lista id reward stack i)
                            (cond
                              ;Hasta que llege a ser nulo
                              [( null? lista) #f]
                              ;Si no es nulo sigo buscando el ultimo elemento (null) de la lista
                              [(equal? (car(car lista)) id) (cambiarDato stack 1
                              (cambiarDato (GetListaDePreguntas stack) i (asignarReward (car lista) reward)) ) ]
                              [else(asignarRewardPreguntaID (cdr lista) id reward stack (+ i 1))])
                            ))
;modificador reward dado una pregunta
(define asignarReward (lambda (pregunta reward)
                        (cambiarDato pregunta 5 reward)))

;Responder pregunta segun el id
(define responderPreguntaID (lambda (stack listaRespuestas id respuesta date etiquetas)
                              
                               ;caso de que la lista de respeustas a la pregunta este vacía o no exista la respuesta aún
                               (if (or (null? listaRespuestas) (equal? (getIndicePreguntaID listaRespuestas id) #f))
                                   (cambiarDato stack 2
                                    (añadirDato listaRespuestas (list id (list 1 (GetUsuarioLogeado stack) date respuesta etiquetas))))
                                   ;Caso de que si hayan respuestas
                                   (cambiarDato stack 2
                                   (cambiarDato listaRespuestas (getIndicePreguntaID listaRespuestas id)
                                    (añadirDato (selectorDato listaRespuestas (getIndicePreguntaID listaRespuestas id)) (list (length (selectorDato listaRespuestas (getIndicePreguntaID listaRespuestas id))) (GetUsuarioLogeado stack) date respuesta etiquetas)))))))

;REGISTER
;Funcion register
;Ejemplo: (register stackoverflow "user01" "pass01")
;Ejemplo: (register stackoverflow "juan01" "pass01")
;Ejemplo: (register (construirStack) "user01" "pass01")
(define register(lambda(stackoverflow user pass)
                  (if (and (esStackoverflow? stackoverflow)(string? user)(string? pass))
                      (if (existNameUser? stackoverflow user)
                      "Ya existe este nombre de usuario\n"
                      (cambiarDato stackoverflow 0 (añadirDato(selectorDato stackoverflow 0)(list user pass))))
                      "No corresponden a usuario y pass\n"
                  )
               )
 )
;Funcion para ver si el usuario se repite.
;Funcion para comprobar si existe un usuario.
;ejemplo : (existNameUser? stackoverflow "diego02")
(define existNameUser?(lambda(stackoverflow user)
                        (if(null? (selectorDato stackoverflow 0))
                           #f
                           (buscadorNameUser (selectorDato stackoverflow 0) user))))
;Funcion para ver si el usuario se repite.
(define buscadorNameUser(lambda(lista user)
                           (cond
                              ;Hasta que llege a ser nulo
                              [( null? lista) #f]
                              ;Si no es nulo añado uno más a la cuenta del longitud y sigo buscando el ultimo elemento (null) de la lista
                              [(equal? (car(car lista)) user) #t]
                              [else(buscadorNameUser (cdr lista) user)])))

(define buscadorUserPassword(lambda(lista user password)
                           (cond
                              ;Hasta que llege a ser nulo
                              [( null? lista) #f]
                              ;Si no es nulo añado uno más a la cuenta del longitud y sigo buscando el ultimo elemento (null) de la lista
                              [(equal? (car(car lista)) user) (equal? (car(cdr(car lista))) password)]
                              [else(buscadorNameUser (cdr lista) user)])))

           

;LOGIN que es como un main
;Ejemplo : (login stackoverflow "juan01" "pass" ask)
;Ejemplo : (login stackoverflow "juan01" "clave123" ask)
(define login (lambda (stack username password operation)
                (if (and (esStackoverflow? stack) (string? username) (string? password))
                    (if (buscadorNameUser (selectorDato stack 0) username)
                        (if (buscadorUserPassword (selectorDato stack 0) username password)
                           (cond
                             ;ask
                             [(equal? operation ask)
                              (ask (logear stack username password))]
                             ;reward
                             [(equal? operation reward)
                              (reward (logear stack username password))]
                             ;answer
                             [(equal? operation answer)
                              (answer (logear stack username password))]
                             ;acept
                             [(equal? operation accept)
                              (accept (logear stack username password))]
                             [else
                              (display "Comando Invalido\n")])
                           "Contraseña incorrecta\n"
                         )
                        "El usuario entregado no se encuentra registrado\n"
                        )
                    "Datos entregados erroneos"
                    )))
;ask
;recorrido :list ->stack X date X string X string list
; Ejemplo :  (((login stackoverflow "juan01" "clave123" ask)(date))"question?" "C#")
; Ejemplo : (((login stackoverflow "juan01" "clave123" ask)(fecha 1 2 3))"question?" "C#")

(define (ask stack)(lambda (date)
               (lambda (question .labels)
                 (if (not (null? (GetActiveUsuario stack)))
                 (deslogear (cambiarDato stack 1 (añadirDato
                  (GetListaDePreguntas stack)
                  (list (+ (length (GetListaDePreguntas stack)) 1)
                  question (GetUsuarioLogeado stack)
                  (list .labels) date 0
                  ) )))
                 "No hay usario logeado \n"))))

;REWARD
;(((login stackoverflow "juan01" "clave123" reward) 1) 300)
(define (reward stack)(lambda(id)
                 (lambda (reward)
                   (if (not (null? (GetActiveUsuario stack)))
                   (if (existePreguntaID? (GetListaDePreguntas stack) id)
                       (if (preguntaCorresponde? (GetListaDePreguntas stack) id (GetUsuarioLogeado stack))
                       (deslogear (asignarRewardPreguntaID (GetListaDePreguntas stack) id reward stack 0))
                        "El usuario entregado, no ha formulado dicha pregunta" )
                       "No existen preguntas")
                   "No hay usario logeado \n"))))

;ANSWER
;EJEMPLO : ((((login stackoverflow "juan01" "clave123" answer) (date)) 1) "mi respuesta" "C")
(define (answer stack)(lambda (date)
                        (lambda(id)
                          (lambda (answer .labels)
                            (if (existePreguntaID? (GetListaDePreguntas stack) id)
                                (deslogear (responderPreguntaID stack (GetListaDeRespuestas stack) id answer date (list .labels)))
                                "No existe pregunta con tal ID")))))

;ACEPT
;(((login stackoverflow "juan01" "clave123" accept) 1) 1)
;(((login (((login stackoverflow "juan01" "clave123" reward) 1) 300) "juan01" "clave123" accept) 1) 1)
;(getUsuarioAnswer (GetListaDeRespuestas stackoverflow) 1 1)
(define (accept stack)
  (lambda (idPregunta)
    (lambda(idRespuesta)
      ;Existe usuario logeado
      (if (not (null? (GetActiveUsuario stack)))
       ;pregunta corresponde   
      (if(preguntaCorresponde? (GetListaDePreguntas stack) idPregunta (GetUsuarioLogeado stack))
         ;Si existe respuesta
         (if (existeRespuestaID? (GetListaDeRespuestas stack) idPregunta idRespuesta)
             (deslogear (asignarRewardUsuario
              ;Se asigna el reward al usuario de la respuesta
             (asignarRewardUsuario stack (getUsuarioAnswer (GetListaDeRespuestas stack) idPregunta idRespuesta) (+ 15 (getRewardPregunta (GetListaDePreguntas stack) idPregunta)))
             ;Se asigna el reward al usuario de la pregunta
             (GetUsuarioLogeado stack) (- 2 (getRewardPregunta (GetListaDePreguntas stack) idPregunta)) ))
             "No existe la respuesta entregada")
         "Pregunta no corresponde a usuario logeado\n")
      "No hay usario logeado \n"))))
                        