module analyzerSintac
    use token_module
    implicit none
    private
    public :: parser

    type(token), dimension(:) , allocatable :: tokensList(:)

    ! Variable para recorrer los tokens
    integer :: numPreAnalisis
    ! Variable para almacenar el token actual
    type(token) :: preAnalisis

    contains

    !Funcion del metodo parea
    subroutine match(typeToken)
        integer, intent(in) :: typeToken
        if (preAnalisis%tipo /= typeToken) then
            print *, "Error de sintaxis: Se esperaba ", trim(getTypeToken(typeToken)), " en lugar de ", trim(preAnalisis%lexema),preAnalisis%tipo, preAnalisis%row
            stop
        end if

        if (numPreAnalisis < size(tokensList)) then
            numPreAnalisis = numPreAnalisis + 1
            preAnalisis = tokensList(numPreAnalisis)
        else
            print *, "Fin del archivo"
            stop
        end if

    end subroutine match

    !produccion de inicio
    subroutine parser(tokens)
        type(token), dimension(:), intent(in) :: tokens
        tokensList = tokens
        numPreAnalisis = 1
        preAnalisis = tokensList(numPreAnalisis)

        !Inicio de la produccion
        call I()

    
        
    end subroutine parser

    ! * Produccion de inicio de bloques
    subroutine I()
        
        call match(MENOR_QUE)
        call match(EXCLAMACION)
        call match(GUION)
        call match(GUION)
        call S()
        
    end subroutine I

    ! * Produccion de Identificacion de bloques
    subroutine S()
        if (preAnalisis%tipo == PALC_CONTROL) then
            call match(PALC_CONTROL)
            call C()

        else if (preAnalisis%tipo == PALC_PROPIEDADES) then
            call match(PALC_PROPIEDADES)
            call P()

        else if (preAnalisis%tipo == PALC_COLOCACION) then
            call match(PALC_COLOCACION)
            call B()

        else
            print *, "Error de sintaxis: Se esperaba un bloque de control, propiedades o colocacion en lugar de ", preAnalisis%lexema
            stop
        end if
        print *, "Se ha reconocido un bloque de ", preAnalisis%lexema
        call I()
    end subroutine S

    ! ! Produccion bloques de control
    recursive subroutine C()

        if (preAnalisis%tipo == COMENTARIO_L) call COM()
        

        call match (PAL_CLAVE)
        call match (IDENTIFICADOR)
        call match (PUNTO_COMA)

        if (preAnalisis%tipo == COMENTARIO_L) call COM()
         

        if (preAnalisis%tipo == PALC_CONTROL) then
            call match (PALC_CONTROL)
            call F()
        
        else 
            call C()

        end if

    end subroutine C


    ! ! Produccion bloques de propiedades
    recursive subroutine P()
        if (preAnalisis%tipo == COMENTARIO_L) call COM()

        call match (IDENTIFICADOR)
        call match (PUNTO)
        call PC()
        call match (PUNTO_COMA)

        if (preAnalisis%tipo == COMENTARIO_L) call COM()

        if (preAnalisis%tipo == PALC_PROPIEDADES) then
            call match (PALC_PROPIEDADES)
            call F()

        else 
            call P()

        end if

    end subroutine P

    ! ? Produccion de propiedades de control
    subroutine PC()
        if (preAnalisis%tipo == PALC_NUM) then
            call match (PALC_NUM)
            call match (PARENTESIS_A)
            call match (NUMERO)
            call match (PARENTESIS_C)

        else if (preAnalisis%tipo == PALC_COLOR) then
            call match (PALC_COLOR)
            call match (PARENTESIS_A)
            call match (NUMERO)
            call match (COMA)
            call match (NUMERO)
            call match (COMA)
            call match (NUMERO)
            call match (PARENTESIS_C)

        else if (preAnalisis%tipo == PALC_TEXTO) then
            call match (PALC_TEXTO)
            call match (PARENTESIS_A)
            call match (CADENA)
            call match (PARENTESIS_C)

        else
            print *, "Error de sintaxis: Se esperaba un numero, color o texto en lugar de ", preAnalisis%lexema
            stop
        end if
    end subroutine PC

    ! ! Produccion bloques de colocacion
    recursive subroutine B()
        if (preAnalisis%tipo == COMENTARIO_L) call COM()

        call match (IDENTIFICADOR)
        call match (PUNTO)
        call BC()
        call match (PUNTO_COMA)

        if (preAnalisis%tipo == COMENTARIO_L) call COM()

        if (preAnalisis%tipo == PALC_COLOCACION) then
            call match (PALC_COLOCACION)
            call F()

        else 
            call B()

        end if

    end subroutine B

    ! ? Produccion de propiedades de colocacion
    subroutine BC()
        if (preAnalisis%tipo == PALC_POS) then
            call match (PALC_POS)
            call match (PARENTESIS_A)
            call match (NUMERO)
            call match (COMA)
            call match (NUMERO)
            call match (PARENTESIS_C)

        else if (preAnalisis%tipo == PALC_ADD) then
            call match (PALC_ADD)
            call match (PARENTESIS_A)
            call match (IDENTIFICADOR)
            call match (PARENTESIS_C)

        else
            print *, "Error de sintaxis: Se esperaba una propiedad de colocacion o de agregacion ", preAnalisis%lexema
            stop
        end if
    end subroutine BC

    ! ! Produccion fin de bloques
    subroutine F()
        call match(GUION)
        call match(GUION)
        call match(MAYOR_QUE)
        call I()
    end subroutine F

    ! ! Produccion de comentarios
    recursive subroutine COM()
        
        call match(COMENTARIO_L)
        
        if (preAnalisis%tipo == COMENTARIO_L) then
            call COM()
        end if

    end subroutine COM


end module analyzerSintac