! 2020 - 30 - 5
! mover.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! FORMA PARTE MONTECARLO
! Calcula la energia de un sistema de 
! particulas cargadas

! Codificación del texto: UTF-8
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: 
! gfortran -Wall -pedantic -std=f95 -c mover.f90

FUNCTION mover(Sistema,N)
  IMPLICIT NONE
  
  ! Definimos variables de entrada
  REAL(8), INTENT(IN):: Sistema(:,:) ! posisciones del sistema de particulas
  INTEGER(8), INTENT(IN):: N! número de particulas
  ! variables internas
  REAL(8), dimension(2,N):: mover, Sis2
  REAL(8):: azm,pol,a,b,c,d,e
  INTEGER(8):: Num
  
  Sis2 = Sistema
  ! llamamos los numeros aleatorios
  
  CALL RANDOM_NUMBER(a)
  CALL RANDOM_NUMBER(b)
  CALL RANDOM_NUMBER(c)
  CALL RANDOM_NUMBER(d)
  CALL RANDOM_NUMBER(e)
  
  ! Escogemos la particula a mover
  Num = 1+INT(a*N,8)
  ! Angulos a mover
  azm = b/1000.0
  pol = c/1000.0
  
  ! signo
  azm = azm*((-1)**(INT(d*2)))
  pol = pol*((-1)**(INT(e*2)))
  ! movemos la particula selecionada
  Sis2(1,Num) = Sis2(1,Num) + pol
  Sis2(2,Num) = Sis2(2,Num) + azm
  ! actualizamos el sistema
  mover=Sis2

END FUNCTION mover
