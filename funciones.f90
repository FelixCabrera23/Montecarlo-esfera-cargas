! 2020 - 30 - 5
! funciones.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! FORMA PARTE DE MONTECARLO
! MODULO DE INTERFASE DE FUNCIÓNES

! Codificación del texto: UTF-8
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008

! REQUIERE:
! FX.f90

! Instrucciones de compilación:
! gfortran -Wall -pedantic -std=f95 -c energia.f90
! gfortran -Wall -pedantic -std=f95 -c mover.f90
! gfortran -Wall -pedantic -std=f95 -c funciones.f90

MODULE funciones

  ! Funcion energia
  INTERFACE
    FUNCTION energia(Sistema,N)
      IMPLICIT NONE
      REAL(8), INTENT(IN):: Sistema(:,:)
      INTEGER(8), INTENT(IN):: N
      REAL(8):: energia
    END FUNCTION energia
  END INTERFACE

  ! Funcion mover
  INTERFACE
    FUNCTION mover(Sistema,N)
      IMPLICIT NONE
      REAL(8), INTENT(IN):: Sistema(:,:) ! posisciones del sistema de particulas
      INTEGER(8), INTENT(IN):: N
      REAL(8), dimension(2,N):: mover
    END FUNCTION mover
  END INTERFACE

END MODULE
