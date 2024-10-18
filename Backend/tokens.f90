! * Adminstracion de tokens

module token_module
    implicit none
    public :: PAL_CLAVE, IDENTIFICADOR, NUMERO, CADENA ,MAYOR_QUE , MENOR_QUE , EXCLAMACION , GUION, PUNTO , PUNTO_COMA , COMA , PARENTESIS_A , PARENTESIS_C , COMENTARIO_L
    public :: PALC_CONTROL, PALC_PROPIEDADES, PALC_COLOCACION, PALC_NUM, PALC_COLOR, PALC_TEXTO, PALC_POS ,PALC_ADD
    integer, parameter :: PAL_CLAVE = 0
    integer, parameter :: IDENTIFICADOR = 1
    integer, parameter :: NUMERO = 2
    integer, parameter :: CADENA = 3
    integer, parameter :: PUNTO = 4
    integer, parameter :: PUNTO_COMA = 5
    integer, parameter :: COMA = 6
    integer, parameter :: PARENTESIS_A = 7
    integer, parameter :: PARENTESIS_C = 8
    integer, parameter :: COMENTARIO_L = 9
    integer, parameter :: MAYOR_QUE = 10
    integer, parameter :: MENOR_QUE = 11
    integer, parameter :: EXCLAMACION = 12
    integer, parameter :: GUION = 13

    ! Tokens espeficicos
    integer, parameter :: PALC_CONTROL = 14
    integer, parameter :: PALC_PROPIEDADES = 15
    integer, parameter :: PALC_COLOCACION = 16
    integer, parameter :: PALC_NUM = 17
    integer, parameter :: PALC_COLOR = 18
    integer, parameter :: PALC_TEXTO = 19
    integer, parameter :: PALC_POS = 20
    integer, parameter :: PALC_ADD = 21



 
    type :: token
        integer :: tipo
        character(len=100) :: lexema
        integer :: row
        integer :: col
    end type token

    type :: error
        character(len=:),allocatable :: caracter
        character(len=100) :: descripcion
        integer :: fila
        integer :: columna
    end type error

contains

    subroutine addToken (typeToken , valor , row, col , tokens)
        implicit none
        integer, intent(in) :: typeToken
        character(len=*), intent(inout) :: valor
        integer, intent(in) :: row
        integer, intent(in) :: col
        type(token), dimension(:), allocatable, intent(inout) :: tokens
        type(token) :: newToken
        type(token),  dimension(:), allocatable :: temp_tokens
        integer :: actualSize

        newToken%tipo = typeToken
        newToken%lexema = valor
        newToken%row = row
        newToken%col = col

        actualSize = size(tokens)

        allocate(temp_tokens(actualSize + 1))

        if (actualSize > 0) then
            temp_tokens(1:actualSize) = tokens
        end if

        temp_tokens(actualSize + 1) = newToken

        call move_alloc(temp_tokens, tokens)

        valor = ""
    end subroutine addToken

    subroutine addError (character , description , row, col , errors)
        implicit none
        character(len=*), intent(inout) :: character
        character(len=*), intent(inout) :: description
        integer, intent(in) :: row
        integer, intent(in) :: col
        type(error), dimension(:), allocatable, intent(inout) :: errors
        type(error) :: newError
        type(error),  dimension(:), allocatable :: temp_errors
        integer :: actualSize

        newError%caracter = character
        newError%descripcion = description
        newError%fila = row
        newError%columna = col

        actualSize = size(errors)

        allocate(temp_errors(actualSize + 1))

        if (actualSize > 0) then
            temp_errors(1:actualSize) = errors
        end if

        temp_errors(actualSize + 1) = newError
        call move_alloc(temp_errors, errors)

        character = ""
    
    end subroutine addError

    function getTypeToken(typeToken) result (res)
        integer, intent(in) :: typeToken
        character(len=100) :: res

        res = ""
        select case (typeToken)
            case (PAL_CLAVE)
                res = "Palabra clave"
            case (IDENTIFICADOR)
                res = "Identificador"
            case (NUMERO)
                res = "Numero"
            case (CADENA)
                res = "Cadena"
            case (PUNTO)
                res = "Punto"
            case (PUNTO_COMA)
                res = "Punto y coma"
            case (COMA)
                res = "Coma"
            case (PARENTESIS_A)
                res = "Parentesis abierto"
            case (PARENTESIS_C)
                res = "Parentesis cerrado"
            case (COMENTARIO_L)
                res = "Comentario"
            case (MAYOR_QUE)
                res = "Mayor que"
            case (MENOR_QUE)
                res = "Menor que"
            case (EXCLAMACION)
                res = "Exclamacion"
            case (GUION)
                res = "Guion"
            case (PALC_CONTROL)
                res = "Palabra clave Control"
            case (PALC_PROPIEDADES)
                res = "Palabra clave Propiedades"
            case (PALC_COLOCACION)
                res = "Palabra clave Colocacion"
            case (PALC_NUM)
                res = "Palabra clave Size"
            case (PALC_COLOR)
                res = "Palabra clave Color"
            case (PALC_TEXTO)
                res = "Palabra clave Texto"
            case (PALC_POS)
                res = "Palabra clave Posicion"
            case (PALC_ADD)
                res = "Palabra clave Add"
                
        end select


    end function getTypeToken

end module token_module