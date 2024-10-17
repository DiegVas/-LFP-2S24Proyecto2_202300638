program analyzerLex
    use token_module

    implicit none
    ! ! Decalaracion de variables

    ! ? Lista te tokens
    type(token), dimension(:),allocatable :: tokensList
    ! ? Lista de errores
    type(error), dimension(:), allocatable :: errorList

    ! ? Variables para el manejo de archivos
    integer :: unit, iostat, lenght
    character(len=100) :: buffer

    ! ? Variables para el manejo de cadenas
    character(len=:), allocatable :: content
    

    ! ? Variables para el analisis lexico
    integer :: state, token_index, token_capacity, error_count, error_index, numInitSimbol, k
    character(len=100) :: current_lexema
    character(len=100) :: lexema, description
    integer :: line_actual, column_actual, typeToken
    character(len=1) :: charLine


    ! * Inicializacion de variables
    column_actual = 0
    line_actual = 1
    error_count = 0
    error_Index = 0
    state = 0
    numInitSimbol = 0
    current_lexema = ''
    token_Index = 0
    token_capacity = 0  ! ? Tamaño inicial del arreglo de tokens
    content = '' ! ? Inicializar la cadena de contenido
    k = 1 ! ? Inicializar el índice de la cadena

    allocate(tokensList(token_capacity)) ! ? Inicializar la lista de tokens
    allocate(errorList(token_capacity)) ! ? Inicializar la lista de errores


    ! !  Abrir el archivo
    unit = 10
    open(unit, file='./test/entrada1.LFP', status='old', action='read', iostat=iostat)
    
    if (iostat /= 0) then
        print *, 'Error al abrir el archivo'
        stop
    end if

    do
        read(unit, '(A)', iostat=iostat) buffer
        if (iostat /= 0) exit
        content = trim(content) // trim(buffer) // char(10)
    end do
    

    ! ! Guardar el tamaño de la cadena
    lenght = len_trim(content)

    ! ! Cerrar el archivo
    close(unit)


    ! * Recorrer el contenido del archivo

    do while (k <= lenght)
            ! ? Obtener el caracter actual
            charLine = content(k:k)
            select case(state)
                case(0) ! * Estado inicial

                    if(charLine == char(10)) then
                        ! ! Ignorar Salto de linea            
                        line_actual = line_actual + 1
                        column_actual = 1

                    else if ( charLine == char(32) .OR. charLine == " " .or. charLine == char(9)) then
                        ! ! Ignorar espacios en blanco                       
                    else if ( charLine == "/" ) then
                        ! ! Inicio de comentario
                        state = 1
                        current_lexema = charLine
                    
                    else if ( charLine >= 'a' .and. charLine <= 'z' .or. charLine >= "A" .and. charLine <="Z" ) then
                        ! ! Inicio de identificador
                        state = 5
                        current_lexema = charLine
                    
                    else if (charLine == "<" .or. charLine == ">" .or. charLine == "!" .or. charLine == "-" .or. charLine == "," .or. charLine == "." .or. charLine == "(" .or. charLine == ")" .or. charLine == ";") then
                        ! ! Ignorar caracteres relevantes
                        state = 6
                        current_lexema = charLine

                    else if (charLine == '"') then
                        ! ! Inicio de una cadena
                        state = 7
                        current_lexema = charLine
                    else if (charLine >= "0" .and. charLine <= "9") then
                        ! ! Inicio de un numero
                        state = 8
                        current_lexema = charLine
                    else
                        description = "Caracter no reconocido"
                        error_count = error_count + 1
                        call addError(charLine, description, line_actual, column_actual, errorList)
                    end if

                case (1) ! * Estado de inicio de comentario
                
                    if (charLine == "/") then
                        state = 2
                        current_lexema = trim(current_lexema) // charLine

                    else if (charLine == "*") then
                        state = 3
                        current_lexema = trim(current_lexema) // charLine

                    else
                        description = "Se esperaba un / o * para iniciar un comentario"
                        call addError(charLine , description, line_actual, column_actual, errorList)
                        state = 0

                    end if
                
                case (2) ! * Estado de comentario de una linea
                    if (charLine /= char(10)) then
                        current_lexema = trim(current_lexema) // charLine

                    else
                        ! ! Guardar el token
                        lexema = 'Comentario de una linea'
                        call addToken(COMENTARIO_L, current_lexema, line_actual, column_actual, tokensList)
                        state = 0
                        k = k - 1  ! Retroceder un carácter para reevaluar

                    end if
                
                case (3) ! * Estado de inicio de comentario multilinea

                    if (charLine /= "*" ) then
                        current_lexema = trim(current_lexema) // charLine

                    else
                        state = 4
                        current_lexema = trim(current_lexema) // charLine

                    end if
                
                case (4) ! * Estado de comentario multilinea
                    if (charLine == "/") then
                        current_lexema = trim(current_lexema) // charLine
                        ! ! Guardar el token
                        lexema = 'Comentario multilinea'
                        call addToken(COMENTARIO_BLOQUE, current_lexema, line_actual, column_actual,tokensList)
                        state = 0

                    else
                        state = 6
                        current_lexema = trim(current_lexema) // charLine
                    end if
                
                case (5) ! * Estado de identificacion identificadores
                    if (charLine >= "a" .and. charLine <= "z" .or. charLine >= "A" .and. charLine <= "Z") then
                        current_lexema = trim(current_lexema) // charLine
                    else
                        if (current_lexema == "Contenedor" .or. current_lexema == "Boton" .or. current_lexema == "Clave" .or. current_lexema == "Etiqueta" .or. current_lexema == "Texto" .or. current_lexema == "Controles" .or. current_lexema == "Propiedades" .or. current_lexema == "Colocacion"  ) then 
                            call addToken(PAL_CLAVE, current_lexema, line_actual, column_actual,tokensList)
                        else 
                            call addToken(IDENTIFICADOR, current_lexema, line_actual, column_actual,tokensList)
                        end if
                        
                        ! ! Reiniciar el estado
                        state = 0
                        k = k - 1  ! Retroceder un carácter para reevaluar
                    end if
                
                case (6) ! * Estado de simbolos
                    if (current_lexema == ",") typeToken = COMA
                    if (current_lexema == ".") typeToken = PUNTO
                    if (current_lexema == "(") typeToken = PARENTESIS_A
                    if (current_lexema == ")") typeToken = PARENTESIS_C
                    if (current_lexema == ";") typeToken = PUNTO_COMA
                    if (current_lexema == ">") typeToken = MAYOR_QUE
                    if (current_lexema == "<") typeToken = MENOR_QUE
                    if (current_lexema == "!") typeToken = EXCLAMACION
                    if (current_lexema == "-") typeToken = GUION

                    call addToken(typeToken, current_lexema, line_actual, column_actual,tokensList)

                    ! ! Reiniciar el estado
                    state = 0
                    k = k - 1  ! Retroceder un carácter para reevaluar

                case (7) ! * Estado de cadena
                    if (charLine /= '"') then
                        current_lexema = trim(current_lexema) // charLine

                    else
                        current_lexema = trim(current_lexema) // charLine

                        ! ! Guardar el token
                        call addToken(CADENA, current_lexema, line_actual, column_actual,tokensList)
                        state = 0

                    end if 
                case (8) ! * Estado de numero
                    if (charLine >= "0" .and. charLine <= "9") then
                        current_lexema = trim(current_lexema) // charLine

                    else
                        ! ? Guardar el token
                        call addToken(NUMERO, current_lexema, line_actual, column_actual,tokensList)

                        ! ! Reiniciar el estado
                        state = 0
                        k = k - 1  ! Retroceder un carácter para reevaluar
                    end if
                
                end select

            k = k + 1
            column_actual = column_actual + 1
    end do

    ! * Imprimir los tokens
    print *, "Tokens"
    do token_index = 1, size(tokensList)
        print *, "Tipo: ", tokensList(token_index)%tipo, " Lexema: ", tokensList(token_index)%lexema, " Fila: ", tokensList(token_index)%row, " Columna: ", tokensList(token_index)%col
    end do

    print *, "Errores"
    do error_index = 1, size(errorList)
        print *, "Caracter: ", errorList(error_index)%caracter, " Descripcion: ", errorList(error_index)%descripcion, " Fila: ", errorList(error_index)%fila, " Columna: ", errorList(error_index)%columna
    end do


end program