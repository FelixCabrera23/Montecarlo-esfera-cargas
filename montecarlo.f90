! 2020 - 25 - 5
! montecarlo.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! Simulación montecarlo para determinar 
! la configuración de menor energia entre particulas
! cargadas en la superficie de una esfera.

! Codificación del texto: UTF-8
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008

! instrucciones de compilación
! gfortran -Wall -pedantic -std=f95 -c energia.f90
! gfortran -Wall -pedantic -std=f95 -c mover.f90
! gfortran -Wall -pedantic -std=f95 -c funciones.f90
! gfortran -Wall -pedantic -std=f95 -c montecarlo.f90
! gfortran -Wall -pedantic -std=f95 -o montecarlo montecarlo.o funciones.o energia.o mover.o
! ./montecarlo
PROGRAM montecarlo

  USE funciones
  IMPLICIT NONE
  
  ! variables princiales
  REAL(8), ALLOCATABLE:: Energias(:), Polares(:,:)
  REAL(8), ALLOCATABLE:: Particulas(:,:), Part_mov(:,:)
  REAL(8):: E,En,a,b,c
  
  ! variables auxiliares
  INTEGER(8):: i,j,k,l,N,sem,pasos,dat
  INTEGER(4), DIMENSION(33):: semilla
  REAL(8):: pi,W
  
  ! Variables de control
  INTEGER(4) :: err
  
    ! Abrimos el archivo de configuración (12)
  OPEN (12, FILE='Parametros.config', STATUS='old', IOSTAT=err)
  IF (err .ne. 0) STOP 'Parametros.config is missing'

  READ(12,*) N      ! numero de particulas
  READ(12,*) sem    ! semilla
  READ(12,*) pasos  ! numero maximo de pasos
  READ(12,*) dat    ! Cantidad de datos a guardar
  CLOSE(12)
  
  ! colocamos la semilla
  
  IF (sem /= 0) THEN
    ! Seleccionamos una semilla
    semilla = INT(sem,4)
    ! Ponemos la semilla
    CALL RANDOM_SEED (put=semilla)
  END IF  
  
  ! iniciamos variables
  pi = 3.14159265359
  ALLOCATE(Energias(pasos),Particulas(2,N),Part_mov(2,N),Polares(2*N,pasos+1))

  ! Esto coloca las particulas aleatoriamente en la esfera
  DO i=1, N
    CALL RANDOM_NUMBER(a)
    CALL RANDOM_NUMBER(b)
    Particulas(1,i)=a*pi  ! Angulo polar
    Particulas(2,i)=b*2.0*pi ! Angulo azimutal
  END DO
  ! Esto guarda los datos de la configuración inical
  DO j=1, N
    Polares(1+(j-1)*2,1) = Particulas(1,j)
    Polares(2+(j-1)*2,1) = Particulas(2,j)
  END DO
  ! Calculamos la energia inicial
  E = energia(Particulas,N)
  Energias(1) = E
    
  ! Abrimos el acrchivo donde escribiremos los resultados
  OPEN (16, FILE='Polares.dat', STATUS='new',IOSTAT=err)
  IF (err .ne. 0) STOP 'Polares.dat exists already'
  WRITE(16,*)E, Polares(:,1)


  PRINT *, '***********************************************************************'
  PRINT *, '                    MONTECARLO'
  PRINT *, 'Calculando configuración de menor energia'
  PRINT *, 'Particulas =',N,', Pasos maximos =',pasos
  PRINT *, 'Procesando...'


  ! Aqui empieza montecarlo
  l=0
  i=1
  k=2
  DO WHILE (i<pasos)
    ! Calculamos la probabilidad de aceptar un paso de mayor energia
    W=0.5*EXP(-(0.001*REAL(i,8))) ! densidad de probabilidad exponencial
  
    Part_mov = Particulas
    Part_mov = mover(Part_mov,N)
    En = energia(Part_mov,N)
       
    
    ! Esta parte acepta el paso si la energia baja
    IF (En < E) THEN
      Particulas = Part_mov ! Confirmamos el movimiento
      E = En                ! Guardamos la nueva energia
      Energias(i+1)=E       ! Guardamos en lista de energias
      i = i + 1             ! Aumentamos el contador de pasos motecarlo
      l = 0

      ! Esto guarda los datos en la variable
      DO j=1, N
        Polares(1+(j-1)*2,k) = Particulas(1,j)
        Polares(2+(j-1)*2,k) = Particulas(2,j)
      END DO
      k = k+1

      CYCLE
    ! Esta parte acepta si cumple con la probabilidad
    ELSE
      CALL RANDOM_NUMBER(c)
      IF (c<=W) THEN
        Particulas = Part_mov ! Confirmamos el movimiento
        E = En                ! Guardamos la nueva energia
        Energias(i+1)=E       ! Guardamos en lista de energias
        i = i + 1             ! Aumentamos el contador de pasos motecarlo

        ! Esto guarda los datos en un archivo
        DO j=1, N
          Polares(1+(j-1)*2,k) = Particulas(1,j)
          Polares(2+(j-1)*2,k) = Particulas(2,j)
        END DO
        k = k+1
        CYCLE
      END IF
    END IF
    l=l+1 ! Cantidad de pasos rechazados
    IF (MOD(l,INT(pasos/10,8))==0) THEN
      WRITE(*, fmt='(1x,a,i0)', advance='no') '%'
    END IF
    IF (l>(pasos/3)) EXIT
  END DO
  PRINT *,' '
  PRINT *,'El proceso se detuvo despues de',i,'pasos montecarlo.'
  
  ! Guardar los datos en el archivo
  k=INT((i/dat),8)
  j=1
  i=1
  DO WHILE (i<=dat)
    WRITE (16,*) Energias(j),Polares(:,j)
    j=j+k
    i=i+1
  END DO
  CLOSE(16)
  DEALLOCATE(Energias,Particulas,Part_mov,Polares)
  
  PRINT *, '***********************************************************************'
END PROGRAM montecarlo
