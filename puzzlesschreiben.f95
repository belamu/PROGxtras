PROGRAM puzzleschreiben
  ! Dieses Programm schreibt ein Puzzle in eine Datei.
  ! Diese Puzzle können von Programmen für die Aufgabe 17 von Blatt 8
  ! Nummerierung vom SoSe 2016) gelöst werden.
  ! Das Programm nimmt drei Argumente:
  ! 1) Die Breite des Puzzles
  ! 2) Die Höhe des Puzzles
  ! 3) Die Beschriftung des Puzzles (wenn es zu wenig Zeichen sind, werden die restlichen mit Leerzeichen aufgefüllt)
  ! wenn das Puzzle mehr als 1000 Teile hat und mehr als 1000 Zeichen angegeben werden, werden trotzdem nur 1000 Teile beschriftet. Euren nächsten Roman könnt ihr hier also nicht verewigen. Sorry. (Bei Bedarf ersetze 1000 unten durch einen größeren Wert.)
  ! Also eine beispielhafte Nutzung ist
  ! f95 puzzleerzeugen.f96 -o pe
  ! ./pe 5 3 "Puzzle sindTOLL"

  IMPLICIT NONE

  TYPE teil
    ! ein Puzzleteil
    ! oben, rechts, unten, links
    INTEGER, DIMENSION(4) :: seiten = 0
    CHARACTER :: beschriftung = " "
  END TYPE

  INTEGER, PARAMETER :: NORTH = 4, EAST = 1, WEST = 3, SOUTH = 2, unitnr = 30, zufaellig = 30
  INTEGER :: pmEins, spalte, zeile, error
  INTEGER :: width = 0, heigth = 0
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: zeilenbuchten, spaltenbuchten
  INTEGER, DIMENSION(:), ALLOCATABLE :: buchtenliste
  TYPE(teil), DIMENSION(:,:), ALLOCATABLE :: teile
  CHARACTER(len=20) :: format
  CHARACTER, DIMENSION(:), ALLOCATABLE :: puzzletext
  CHARACTER(len=1000) :: text
  ! bei einer Zeichenkette wüsste ich nicht wie ich einzelne Zeichen ansprechen könnte

  
  CALL GETARG(2, text)
  READ(UNIT=text, FMT=*, iostat = error) heigth
  if (error /= 0. .OR. heigth < 1) THEN
    STOP "Höhenangabe konnte nicht als natürliche Zahl geparst werden."
  ELSE
    WRITE(*,*) "Höhe", heigth
  END IF

  CALL GETARG(1, text)
  READ(UNIT=text, FMT=*, iostat = error) width
  if (error /= 0. .OR. width < 1) THEN
    STOP "Breitenangabe konnte nicht als natürliche Zahl geparst werden."
  ELSE
    WRITE (*,*) "Breite", width
  END IF
  
  CALL GETARG(3, text)

  ALLOCATE(zeilenbuchten(width-1, heigth))
  ALLOCATE(spaltenbuchten(width, heigth-1))
  ALLOCATE(buchtenliste(SIZE(zeilenbuchten) + SIZE(spaltenbuchten)))
  ALLOCATE(teile(width, heigth))
  ALLOCATE(puzzletext(width*heigth))

  ! jedes Zeichen wird einzeln gelesen und in ein Element geschrieben
  WRITE(format, "(A,I0,A)") "(", width*heigth, "(A1))"
  ! WRITE(*,"(A,I0,A)") "Beschriftung des Puzzles (", width*heigth, " Zeichen inkl. Leerzeichen)"  
  READ(text,format) teile%beschriftung

  ! rechteckig ausgeben 
  WRITE(*,*) "Das Puzzle wird mit folgendem Text beschrieben:"
  WRITE(format, "(A,I0,A)") "(", width, "A)"
  WRITE(*,format) teile%beschriftung

  buchtenliste = permutation(SIZE(buchtenliste))
  ! zwischenspeichern, damit alle Nummern über alle Buchten verteilt werden
  ! und es keine getrennte Nummerierung für senkrechte und waagerechte Buchten gibt
  zeilenbuchten = reshape(buchtenliste(:SIZE(zeilenbuchten)), SHAPE(zeilenbuchten))
  spaltenbuchten = reshape(buchtenliste(SIZE(zeilenbuchten) + 1:), SHAPE(spaltenbuchten))
  WRITE(format, "(A,I0,A)") "(", width-1, "(I3))"
  WRITE(*,*) "Zeilenbuchten"  
  WRITE(*, format) zeilenbuchten
  WRITE(*,*) "Spaltenbuchten"
  WRITE(format, "(A,I0,A)") "(", width, "(I3))"  
  WRITE(*, format) spaltenbuchten

! -------------
! die Buchten in die Teile schreiben.
! erst für die waagerechten Buchten, dann die senkrechten
! -------------
  DO spalte = 1, width-1
    DO zeile = 1, heigth
      pmEins = INT(RAND()*2) *2-1
      teile(spalte, zeile)%seiten(EAST) = pmEins*zeilenbuchten(spalte, zeile)
      teile(spalte + 1, zeile)%seiten(WEST) = - pmEins*zeilenbuchten(spalte, zeile)
    END DO
  END DO
  DO spalte = 1, width
    DO zeile = 1, heigth-1
      pmEins = INT(RAND()*2) *2-1
      teile(spalte, zeile)%seiten(SOUTH) = pmEins*spaltenbuchten(spalte, zeile)
      teile(spalte, zeile + 1)%seiten(NORTH) = - pmEins*spaltenbuchten(spalte, zeile)
    END DO
  END DO

  WRITE(*,*) "Teile vor dem Verdrehen"
  CALL putTeile()
  
  ! drehe Teile zufaellig
  ! schreibe Text auf die Teile
  WRITE(*,*) 
  do zeile = 1, width
    do spalte = 1, heigth
      ! Eckteile werden nicht gedreht:
      if (.NOT. ((zeile == 1 .OR. zeile == width) .AND. (spalte == 1 .OR. spalte == heigth))) then
      ! zufaellig 0 bis 4 Seiten weit rotiert
        teile(zeile, spalte)%seiten = CSHIFT(teile(zeile, spalte)%seiten, INT(RAND()*4))
      end if
      ! teile(zeile, spalte)%beschriftung = 
    END DO
  END DO

  WRITE(*,*) "Teile nach dem Verdrehen"
  CALL putTeile()

  ! schreibe Teile in zufaelliger Reihenfolge in eine Datei
  DO pmEins = 1, 30 ! pmEins und format hier missbraucht
    WRITE(format, "(A,I0,A)") "puzzle", pmEins, ".puz"
    WRITE(*,*) format
    OPEN(UNIT=unitnr, file=format, status="NEW", iostat=error, action="WRITE")
    IF (error == 0) EXIT
    IF (pmEins == 30) error = 1 ! I know, bad code, make it better
    ! otherwise try next file
  END DO

  IF (error == 0) THEN
    ! Breite und Hoehe in die Datei schreiben
    WRITE(unitnr, *) heigth, width
    buchtenliste = permutation(SIZE(teile))
    DO pmEins = 1, SIZE(buchtenliste)
      ! zeilenweise nummerieren
      CALL saveTeil(teile(&
        & MOD(buchtenliste(pmEins)-1, width) + 1, &
        & (buchtenliste(pmEins)-1)/width + 1 ) )
    END DO
  ELSE
    WRITE(*,*) "Konnte keine Datei öffnen. Überprüfe, ob es eine der &
    & Dateien puzzle1-30.puz noch nicht gibt und ich Schreibrechte habe."
  END IF
  CLOSE(unitnr)

  CONTAINS

  FUNCTION permutation(groesse)
    INTEGER :: groesse, i, index, tauscher, tauschwert
    INTEGER, DIMENSION(groesse) :: permutation
    INTEGER, DIMENSION(8) :: time
    CALL DATE_AND_TIME(VALUES=time)
    ! Pseudozufall mithilfe der aktuellen Uhrzeit
    tauscher = RAND(SUM(time))
    ! permutation = (i = 1, groesse)
    permutation = (/ (i, i = 1, groesse) /)
    DO i=1, zufaellig ! so viele Durchgaenge an Vertauschungen
      DO index = 1, groesse ! jede Position mal vertauschen
        tauscher = 1 + groesse * RAND()
        ! WRITE(*,*) tauscher
        tauschwert = permutation(tauscher)
        permutation(tauscher) = permutation(index)
        permutation(index) = tauschwert
      END DO
    END DO
  END FUNCTION

  SUBROUTINE putTeile()
    WRITE(format, "(A,I0,A)") "(", width, "(4(I3),1X,A,2X))"
    WRITE(*,*) "Teile"  
    WRITE(*, format) teile
  END SUBROUTINE

  SUBROUTINE saveTeil(puzzleteil)
    TYPE(teil) :: puzzleteil
    WRITE(unitnr, ("(4(I6,1X),A,A,A)")) puzzleteil%seiten, '"', &
      & puzzleteil%beschriftung, '"'
  END SUBROUTINE

END PROGRAM