program analyzerSintac
    use token_module

    implicit none
    ! ! Decalaracion de variables
    integer :: unit, iostat, lenght
    character(len=100) :: buffer, line

    ! ? Variables para el manejo de cadenas
    character(len=:), allocatable :: content
    character(len=1) :: charLine

    ! ? Variables para el analisis lexico
    integer :: state,token_count, token_index, token_capacity, error_count, error_index, numInitSimbol, k
    character(len=100) :: current_lexema
    character(len=100) :: lexema, description
    integer :: line_actual, column_actual
    integer :: num
    character(len=1), parameter :: relevant_chars(4) = [char(123), char(125), char(46), char(58)]
    character(len=10), parameter :: reserved_words(7) = ['grafica   ', 'nombre    ', 'continente', 'pais      ', 'poblacion ', 'saturacion', 'bandera   ']


    ! ? Variables para el manejo de tokens
    type(token), allocatable :: tokens(:)
    type(error), allocatable :: errors(:)

    ! * Inicializacion de variables
    column_actual = 0
    line_actual = 1
    error_count = 0
    error_Index = 0
    state = 0
    numInitSimbol = 0
    current_lexema = ''
    token_Index = 0
    token_capacity = 100  ! ? Tamaño inicial del arreglo de tokens
    content = '' ! ? Inicializar la cadena de contenido
    allocate(tokens(token_capacity)) ! ? Inicializar el arreglo de tokens
    allocate(errors(token_capacity)) ! ? Inicializar el arreglo de errores
    k = 1 ! ? Inicializar el índice de la cadena

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
    content = trim(content)
    

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
                    ! ! Salto de linea            
                    line_actual = line_actual + 1
                    column_actual = 1
                
                else if ( charLine == char(60) ) then

                    ! ! Inicio de Caracteristicas

                    state = 1
                    current_lexema = charLine
                
                else if ( charLine == char(32) .OR. charLine == " " .or. charLine == char(9)) then
                    ! ! Ignorar espacios en blanco
                
                else if ( charLine == "/" ) then
                    ! ! Inicio de comentario
                    state = 4
                    current_lexema = charLine
                
                else if ( charLine >= 'a' .and. charLine <= 'z' .or. charLine >= "A" .and. charLine <="Z" ) then
                    ! ! Inicio de identificador
                    state = 8
                    current_lexema = charLine
                
                else if (charLine == "," .or. charLine == "." .or. charLine == "(" .or. charLine == ")" .or. charLine == ";") then
                    ! ! Ignorar caracteres relevantes
                    state = 9
                    current_lexema = charLine

                else if (charLine == '"') then
                    ! ! Inicio de una cadena
                    state = 10
                    current_lexema = charLine
                else if (charLine >= "0" .and. charLine <= "9") then
                    ! ! Inicio de un numero
                    state = 11
                    current_lexema = charLine
                else if (charLine == "-") then
                    ! ! fin  de una caracteristica
                    state = 12
                    current_lexema = charLine
                    numInitSimbol = numInitSimbol + 1
                else
                    description = "Caracter no reconocido"
                    error_count = error_count + 1
                    call add_error(error_index,token_capacity, errors,charLine, description, line_actual, column_actual)
                end if

            case(1) ! * Estado de inicio de caracteristicas
                ! ? Inicio de una caracteristica
                if(charLine == "!") then
                   
                    state = 2
                    current_lexema = trim(current_lexema) // charLine
                else

                    description = "Se esperaba un ! para iniciar una caracteristica"
                    error_count = error_count + 1
                    call add_error(error_index,token_capacity, errors,charLine, description, line_actual, column_actual)        
                    state = 0

                end if

            case(2) ! * Estado de seguimiento de caracteristicas
                if ( charLine /= "-" .and. numInitSimbol == 2) then
                    state = 3
                    k = k-1
                    numInitSimbol = 0
                    

                else if (charLine == "-") then
                    numInitSimbol = numInitSimbol + 1
                    current_lexema = trim(current_lexema) // charLine
                
                else
                    description = "Se esperaba menos - para iniciar una caracteristica"
                    error_count = error_count + 1
                    call add_error(error_index,token_capacity, errors,charLine, description, line_actual, column_actual)
                    state = 0
                    numInitSimbol = 0
                    k = k - 1

                end if


            case(3) ! * Estado de identificacion de caracteristica
                if (charLine >= "a" .and. charLine <= "z" .or. charLine >= "A" .and. charLine <= "Z") then
                    current_lexema = trim(current_lexema) // charLine

                else
                    ! ? Guardar el token

                     
                    ! ? Verificar si es una palabra reservada

                        if (current_lexema == "<!--Controles" .or. current_lexema == "<!--Propiedades" .or. current_lexema == "<!--Colocacion") then
                            lexema = 'Palabra reservada'
                            call resize_tokens(token_Index, token_capacity, tokens)
                            call save_token(token_Index, tokens, trim(lexema), current_lexema, line_actual, column_actual)
                        else
                            lexema = 'Identificador no reconocido'
                            call add_error(error_Index, token_capacity, errors,trim(current_lexema),lexema, line_actual, column_actual)
                            error_count = error_count + 1
                        end if
                        


                    ! ! Reiniciar el estado
                    state = 0
                    k = k - 1  ! Retroceder un carácter para reevaluar
                end if
            case (4) ! * Estado de inicio de comentario
               
                if (charLine == "/") then
                    state = 5
                    current_lexema = trim(current_lexema) // charLine
                else if (charLine == "*") then
                    state = 6
                    current_lexema = trim(current_lexema) // charLine
                else
                    description = "Se esperaba un / o * para iniciar un comentario"
                    error_count = error_count + 1
                    call add_error(error_index,token_capacity, errors,charLine, description, line_actual, column_actual)
                    state = 0
                end if
            
            case (5) ! * Estado de comentario de una linea
                if (charLine /= char(10)) then
                    
                    current_lexema = trim(current_lexema) // charLine
                else
                    ! ! Guardar el token
                     call resize_tokens(token_Index, token_capacity, tokens)
                    lexema = 'Comentario de una linea'
                    call save_token(token_Index, tokens, trim(lexema), current_lexema, line_actual, column_actual)
                    state = 0
                    k = k - 1  ! Retroceder un carácter para reevaluar}
                end if
            
            case (6) ! * Estado de inicio de comentario multilinea
                if (charLine /= "*" ) then
                    current_lexema = trim(current_lexema) // charLine
                else
                    state = 7
                    current_lexema = trim(current_lexema) // charLine
                end if
            
            case (7) ! * Estado de comentario multilinea
                if (charLine == "/") then
                    current_lexema = trim(current_lexema) // charLine
                      ! ! Guardar el token
                    call resize_tokens(token_Index, token_capacity, tokens)
                    lexema = 'Comentario multilinea'
                    call save_token(token_Index, tokens, trim(lexema), current_lexema, line_actual, column_actual)
                    state = 0
                else
                    state = 6
                    current_lexema = trim(current_lexema) // charLine
                end if
            
            case (8) ! * Estado de identificacion identificadores
                if (charLine >= "a" .and. charLine <= "z" .or. charLine >= "A" .and. charLine <= "Z") then
                    current_lexema = trim(current_lexema) // charLine
                else
                    lexema = 'Identificador'
                    if (current_lexema == "Contenedor" .or. current_lexema == "Boton" .or. current_lexema == "Clave" .or. current_lexema == "Etiqueta" .or. current_lexema == "Texto") lexema = 'Palabra reservada'
                    call resize_tokens(token_Index, token_capacity, tokens)
                    call save_token(token_Index, tokens, trim(lexema), current_lexema, line_actual, column_actual)
                    
                    ! ! Reiniciar el estado

                    state = 0
                    k = k - 1  ! Retroceder un carácter para reevaluar
                end if
            
            case (9) ! * Estado de simbolos
                if (current_lexema == ",") lexema = 'separador'
                if (current_lexema == ".") lexema = 'punto'
                if (current_lexema == "(") lexema = 'simbolo de apertura'
                if (current_lexema == ")") lexema = "simbolo de cierre"
                if (current_lexema == ";") lexema = "simbolo de fin de instruccion"
                call resize_tokens(token_Index, token_capacity, tokens)
                call save_token(token_Index, tokens, trim(lexema), current_lexema, line_actual, column_actual)
                ! ! Reiniciar el estado
                state = 0
                k = k - 1  ! Retroceder un carácter para reevaluar

            case (10) ! * Estado de cadena
                if (charLine /= '"') then
                    current_lexema = trim(current_lexema) // charLine
                else
                    current_lexema = trim(current_lexema) // charLine

                    
                    ! ! Guardar el token
                    call resize_tokens(token_Index, token_capacity, tokens)
                    lexema = 'Cadena'
                    call save_token(token_Index, tokens, trim(lexema), current_lexema, line_actual, column_actual)
                    state = 0

                end if 
            case (11) ! * Estado de numero
                if (charLine >= "0" .and. charLine <= "9") then
                    current_lexema = trim(current_lexema) // charLine
                else
                    ! ? Guardar el token
                     call resize_tokens(token_Index, token_capacity, tokens)
                    lexema = 'Numero'
                    call save_token(token_Index, tokens, trim(lexema), current_lexema, line_actual, column_actual)
                    ! ! Reiniciar el estado
                    state = 0
                    k = k - 1  ! Retroceder un carácter para reevaluar
                end if
            
            
            
            case (12) ! * Estado de finalizacion de caracteristica
                if (charLine == ">" .and. numInitSimbol == 2) then
                    state = 0
                    current_lexema = trim(current_lexema) // charLine
                    call resize_tokens(token_Index, token_capacity, tokens)
                    lexema = 'Fin de caracteristica'
                    call save_token(token_Index, tokens, trim(lexema), current_lexema, line_actual, column_actual)
                    numInitSimbol = 0


                else if (charLine == "-") then
                    numInitSimbol = numInitSimbol + 1
                    current_lexema = trim(current_lexema) // charLine
                else
                    description = "Se esperaba menos de un - para finalizar una caracteristica"
                    error_count = error_count + 1
                    call add_error(error_index,token_capacity, errors,charLine, description, line_actual, column_actual)
                    state = 0
                    k = k - 1
                end if
            end select

        k = k + 1
        column_actual = column_actual + 1
end do

do error_index = 1, error_count
    print *, "Error ", error_index, ":"
    print *, "Caracter: ", errors(error_index)%caracter
    print *, "Descripcion: ", errors(error_index)%descripcion
    print *, "Fila: ", errors(error_index)%fila
    print *, "Columna: ", errors(error_index)%columna
end do

print *, "Tokens encontrados: ", token_index

do token_count = 1,token_index
    print *, "Lexema: ", tokens(token_count)%lexema
    print *, "Tipo: ", tokens(token_count)%tipo
end do

end program