! * Adminstracion de tokens

module token_module
    implicit none
    public :: PAL_CLAVE, IDENTIFICADOR, NUMERO, CADENA ,MAYOR_QUE , MENOR_QUE , EXCLAMACION , GUION, PUNTO , PUNTO_COMA , COMA , PARENTESIS_A , PARENTESIS_C , COMENTARIO_L , COMENTARIO_BLOQUE
    
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
    integer, parameter :: COMENTARIO_BLOQUE = 10
    integer, parameter :: MAYOR_QUE = 11
    integer, parameter :: MENOR_QUE = 12
    integer, parameter :: EXCLAMACION = 13
    integer, parameter :: GUION = 14

 
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

end module token_module