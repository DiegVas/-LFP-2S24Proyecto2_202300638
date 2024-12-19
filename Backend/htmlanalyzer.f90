module htmlanalyzer
    use token_module
    use TypeControl
    public :: analizeHtml
    
    type(htmlControl), dimension(:), allocatable :: controls
    type(TagControl), dimension(:), allocatable :: tagControls
    type(ButtonControl), dimension(:), allocatable :: buttonControls
    type(CheckControl), dimension(:), allocatable :: checkControls
    type(RadioControl), dimension(:), allocatable :: radioControls
    type(TextControl), dimension(:), allocatable :: textControls
    type(TextAreaControl), dimension(:), allocatable :: textAreaControls
    type(KeyControl), dimension(:), allocatable :: keyControls
    type(ContainerControl), dimension(:), allocatable :: containerControls


    
    contains
    subroutine analizeHtml(tokens)
        implicit none

        type(token), dimension(:), intent(in) :: tokens
        type(htmlControl) :: newControl
        
        integer :: i, indexref,indexrefControl, typePropertie, Temp_r,Temp_g,Temp_b,Temp_Numeric, Tempx, Tempy
        character(len=100) :: Temp_TextVal
        logical :: blockControl, blockPropertie, blockColocation, isContainter

        allocate(controls(0))
        allocate(tagControls(0))
        allocate(buttonControls(0))
        allocate(checkControls(0))
        allocate(radioControls(0))
        allocate(textControls(0))
        allocate(textAreaControls(0))
        allocate(keyControls(0))
        allocate(containerControls(0))


        i = 1
        indexref = 0
        typePropertie = 0
        indexrefControl = 0
        blockControl = .false.
        blockPropertie = .false.
        blockColocation = .false.
        isContainter = .false.

        do while (i <= size(tokens))
            if (tokens(i)%tipo == PALC_CONTROL ) then
                
                ! * Seccion de control
                blockControl = .not. blockControl
                i = i + 1
            end if
            if (tokens(i)%tipo == PALC_PROPIEDADES ) then

                ! * Seccion de propiedades
                blockPropertie = .not. blockPropertie
                i = i + 1
            end if

            if (tokens(i)%tipo == PALC_COLOCACION ) then

                ! * Seccion de colocacion
                blockColocation = .not. blockColocation
                i = i + 1
            end if

            if (blockControl) then
                
                if (tokens(i)%tipo == PAL_CLAVE) then
                    
                    ! * Seccion de tipo de control
                    newControl%control = tokens(i)%lexema
                    newControl%typeControl = getTypeControl(tokens(i)%lexema)
                
                else if (tokens(i)%tipo == IDENTIFICADOR) then
                    
                    ! * Seccion de id de control
                    newControl%id = tokens(i)%lexema
                    ! * Seccion de index de control de acuerdo a la lista de su tipo
                    newControl%indexRef = addTypeControl(newControl%typeControl, tagControls, buttonControls, checkControls, radioControls, textControls, textAreaControls, keyControls, containerControls)
                    call addHtmlControl(newControl,controls)
                end if 
                
            else if (blockPropertie) then

                if (tokens(i)%tipo == IDENTIFICADOR) then

                    ! * Buscar el id del control
                    indexref = getIndexControl(tokens(i)%lexema, controls) 

                else if (tokens(i)%tipo >= 17 .and. tokens(i)%tipo <= 24 .and. tokens(i)%tipo /= 20) then
                    
                    typePropertie = getTypePropietie(tokens(i)%lexema)

                    ! * Agregar propiedades de acuerdo al tipo de control
                    select case(typePropertie)
                        case(Width, Height, Group,Marck)
                            read(tokens(i+2)%lexema, *) Temp_Numeric
                            call setNumeric(controls(indexref)%indexRef,typePropertie,controls(indexref)%typeControl, Temp_Numeric,tagControls,containerControls, checkControls, radioControls )
                            i = i + 4
                        case(TextVal, Aling)
                            Temp_TextVal = tokens(i+2)%lexema
                            call setText(controls(indexref)%indexRef,typePropertie,controls(indexref)%typeControl, Temp_TextVal, buttonControls, checkControls, radioControls, textControls, textAreaControls, keyControls, tagControls)
                            i = i + 4
                            ! * Seccion de propiedades de texto
                        case(ColorLetter, ColorBackground)
                            read(tokens(i+2)%lexema, *) Temp_r
                            read(tokens(i+4)%lexema, *) Temp_g
                            read(tokens(i+6)%lexema, *) Temp_b
                            call setColor(controls(indexref)%indexRef,typePropertie,controls(indexref)%typeControl,Temp_r, Temp_g, Temp_b, tagControls, containerControls)
                            i = i + 8
                    end select

                    ! * Seccion de propiedades
                    !call addPropertie(tokens(i)%lexema, indexref, tagControls, buttonControls, checkControls, radioControls, textControls, textAreaControls, keyControls, containerControls)
                end if
            else if (blockColocation) then
                if (tokens(i)%tipo == IDENTIFICADOR) then
                    if (tokens(i)%lexema == "this") then
                        indexrefControl = getIndexControl(tokens(i+4)%lexema, controls)
                        controls(indexrefControl)%isContainer = .false.
                        i = i + 6
                    else
                        indexref = getIndexControl(tokens(i)%lexema, controls)
                        indexrefControl = getIndexControl(tokens(i+2)%lexema, controls)
                    end if
                    
                else if(tokens(i)%tipo == 20) then
                    ! * Seccion de posicion
                    read(tokens(i+2)%lexema, *) Tempx
                    read(tokens(i+4)%lexema, *) Tempy 
                    call setPosicion(indexref, controls, Tempx, Tempy)
                    i = i + 6
                else if(tokens(i)%tipo == 26) then
                    ! * Seccion de add
                    indexrefControl = getIndexControl(tokens(i+2)%lexema, controls)
                    call addControlToContainer(controls(indexref)%indexRef, controls(indexrefControl),containerControls)
 
                    i = i + 4

                end if
            end if
            i = i + 1
        end do

        do i = 1, size(controls)
        end do

        call generate_html_css()

    end subroutine analizeHtml

    subroutine generate_html_css()
        integer :: i, html_unit, css_unit
        character(len=100) :: html_filename, css_filename

        html_filename = "output.html"
        css_filename = "styles.css"

        ! Open HTML file
        open(newunit=html_unit, file=html_filename, status="replace", action="write")
        
        ! Write HTML header
        write(html_unit, '(a)') "<!DOCTYPE html>"
        write(html_unit, '(a)') "<html lang='en'>"
        write(html_unit, '(a)') "<head>"
        write(html_unit, '(a)') "    <meta charset='UTF-8'>"
        write(html_unit, '(a)') "    <meta name='viewport' content='width=device-width, initial-scale=1.0'>"
        write(html_unit, '(a)') "    <title>Generated Page</title>"
        write(html_unit, '(a)') "    <link rel='stylesheet' href='styles.css'>"
        write(html_unit, '(a)') "</head>"
        write(html_unit, '(a)') "<body>"

        ! Generate HTML content
        do i = 1, size(controls)
            if (.not. controls(i)%isContainer) then
                call write_html_element(html_unit, controls(i))
            end if
          
        end do

        ! Close HTML file
        write(html_unit, '(a)') "</body>"
        write(html_unit, '(a)') "</html>"
        close(html_unit)

        ! Open CSS file
        open(newunit=css_unit, file=css_filename, status="replace", action="write")

        ! Generate CSS content
        do i = 1, size(controls)
            call write_css_rules(css_unit, controls(i))
        end do

        ! Close CSS file
        close(css_unit)

    end subroutine generate_html_css

    subroutine write_html_element(unit, control)
        integer, intent(in) :: unit
        type(htmlControl), intent(in) :: control
        character(len=100) :: element_start, element_end

        select case (control%typeControl)
            case (Tag)
                element_start = "<label id='" // trim(control%id) // "'>" // trim(tagControls(control%indexRef)%text)
                element_end = "</label>"
            case (Button)
                element_start = "<input type='submit' id='" // trim(control%id) // "' value='"// trim(buttonControls(control%indexRef)%text) // "'"
                element_end = "'>"
            case (Check)
                element_start = "<input type='checkbox' id='" // trim(control%id) // "'>"
                element_end = ""
            case (Radio)
                element_start = "<input type='radio' id='" // trim(control%id) // "'>" // trim(radioControls(control%indexRef)%text)
                element_end = ""
            case (Text)
                element_start = "<input type='text' id='" // trim(control%id) // "'>"
                element_end = ""
            case (TextArea)
                element_start = "<textarea id='" // trim(control%id) // "'>"
                element_end = "</textarea>"
            case (Key)
                element_start = "<input type='password' id='" // trim(control%id) // "'>"
                element_end = ""
            case (Container)
                element_start = "<div id='" // trim(control%id) // "'>"
                element_end = "</div>"
        end select

        write(unit, '(a)') trim(element_start)
        if (control%typeControl == Container) then
            call write_container_contents(unit, control)
        end if
        write(unit, '(a)') trim(element_end)
    end subroutine write_html_element

    subroutine write_container_contents(unit, container)
        integer, intent(in) :: unit
        type(htmlControl), intent(in) :: container
        integer :: i

        do i = 1, size(containerControls(container%indexRef)%controls)
            call write_html_element(unit, containerControls(container%indexRef)%controls(i))
        end do
    end subroutine write_container_contents

    subroutine write_css_rules(unit, control)
        integer, intent(in) :: unit
        type(htmlControl), intent(in) :: control

        write(unit, '(a)') "#" // trim(control%id) // " {"
        write(unit, '(a,i0,a,i0,a)') "    position: absolute;"
        write(unit, '(a,i0,a)') "    left: ", control%posX, "px;"
        write(unit, '(a,i0,a)') "    top: ", control%posY, "px;"

        select case (control%typeControl)
            case (Tag)
                call write_tag_css(unit, tagControls(control%indexRef))
            case (Container)
                call write_container_css(unit, containerControls(control%indexRef))
            ! Add cases for other control types as needed
        end select

        write(unit, '(a)') "}"
    end subroutine write_css_rules

    subroutine write_tag_css(unit, tag)
        integer, intent(in) :: unit
        type(TagControl), intent(in) :: tag

        write(unit, '(a,i0,a)') "    width: ", tag%width, "px;"
        write(unit, '(a,i0,a)') "    height: ", tag%height, "px;"
        write(unit, '(a,i0,a,i0,a,i0,a)') "    color: rgb(", tag%Tr, ",", tag%Tg, ",", tag%Tb, ");"
        if (tag%r /= 0 .or. tag%g /= 0 .or. tag%b /= 0) then
            write(unit, '(a,i0,a,i0,a,i0,a)') "    background-color: rgb(", tag%r, ",", tag%g, ",", tag%b, ");"
        end if
    end subroutine write_tag_css

    subroutine write_container_css(unit, container)
        integer, intent(in) :: unit
        type(ContainerControl), intent(in) :: container

        write(unit, '(a,i0,a)') "    width: ", container%width, "px;"
        write(unit, '(a,i0,a)') "    height: ", container%height, "px;"
        write(unit, '(a,i0,a,i0,a,i0,a)') "    background-color: rgb(", container%r, ",", container%g, ",", container%b, ");"
    end subroutine write_container_css


end module htmlanalyzer