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
  (list (list "juan01" "clave123")(list "diego02" "clave123"))
  ;1
  ;TDA PREGUNTA 1/0 es respondida o no.
  (list (list 1 "pregunta" "juan01" (list "etiquetas" "C" "universidad") "10/12/2020" 1 ))
  ;2
  ;TDA Respuestas
  (list (list 1 (list "diego02" "12/12/2020"  "respuesta" (list "etiquetas" "c"))))
  ;3
  ;Sección de usario con sesión abierta
  (list)
  )
)
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
(define (GetistaDeUsarios stack) (selectorDato stack 0))
(define (GetListaDePreguntas stack) (selectorDato stack 1))
(define (GetListaDeRespuestas stack) (selectorDato stack 2))
(define (GetActiveUsuario stack) (selectorDato stack 3))
(define (GetUsuarioLogeado stack) (selectorDato (selectorDato stack 3) 0))

(define (logear stack user password)
  (cambiarDato stack 3(list user password)))

(define (deslogear stack)
  (cambiarDato stack 3(list)))


;REGISTER

;Funcion register
;Ejemplo: (register stackoverflow "user01" "pass01")
;Ejemplo: (register stackoverflow "juan01" "pass01")
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
;ask
;recorrido :list ->stack X date X string X string list
; Ejemplo :  (((login stackoverflow "juan01" "clave123" ask)(date))"question?" "C#")
; Ejemplo : (((login stackoverflow "juan01" "clave123" ask)(fecha 1 2 3))"question?" "C#")

(define (ask stack)(lambda (date)
               (lambda (question .labels)
                 (if (not (null? (GetActiveUsuario stack)))
                 (cambiarDato stack 1 (añadirDato
                  (GetListaDePreguntas stack)
                  (list (+ (length (GetListaDePreguntas stack)) 1)
                  question (GetUsuarioLogeado stack)
                  (list .labels) date 0
                  ) ))
                 "No hay usario logeado \n"))))
               


;LOGIN
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
                             [else
                              (display "Comando Invalido\n")])
                           "Contraseña incorrecta\n"
                         )
                        "El usuario entregado no se encuentra registrado\n"
                        )
                    "Datos entregados erroneos"
                    )))
