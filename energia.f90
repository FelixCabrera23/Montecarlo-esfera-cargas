! 2020 - 30 - 5
! energia.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! FORMA PARTE MONTECARLO
! Calcula la energia de un sistema de 
! particulas cargadas

! Codificación del texto: UTF-8
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: 
! gfortran -Wall -pedantic -std=f95 -c energia.f90

FUNCTION energia(Sistema,N)
  IMPLICIT NONE
  
  ! Definimos variables de entrada
  REAL(8), INTENT(IN):: Sistema(:,:) ! posisciones del sistema de particulas
  INTEGER(8), INTENT(IN):: N! número de particulas
  
  ! variables internas
  REAL(8):: energia,s,k,x1,x2,y1,y2,z1,z2
  INTEGER(8):: i,j
  REAL(8), DIMENSION(2)::p1,p2
  ! iniciamos las variables
  k = 9e9*(0.001**2) ! energia para particulas de carga 0.001 Coulombs
  energia = 0
  ! Ciclamos para cada par de particulas posible
  DO i=1, N
    p1 = Sistema(:,i)
    DO j=1,N
      IF (i==j) CYCLE
      p2 = Sistema(:,j)
      ! Esto lo pasa a cartecianas
      x1=SIN(p1(1))*COS(p1(2))
      y1=SIN(p1(1))*SIN(p1(2))
      z1=COS(p1(1))
      x2=SIN(p2(1))*COS(p2(2))
      y2=SIN(p2(1))*SIN(p2(2))
      z2=COS(p2(1))
      
      s = SQRT(((x1-x2)**2)+((y1-y2)**2)+((z1-z2)**2))
      energia = energia + k/s
    END DO
  END DO
  
  energia = energia/2.0


END FUNCTION energia
