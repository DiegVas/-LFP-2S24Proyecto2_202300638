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
        integer :: typzeContrl
        
        integer :: i
        logical :: blockControl, blockPropertie, blockColocation

        i = 1
        blockControl = .false.
        blockPropertie = .false.
        blockColocation = .false.

        do while (i <= size(tokens))
            if (tokens(i)%tipo == PALC_CONTROL ) then
                blockControl = .not. blockControl
                i = i + 1
            end if
            if (tokens(i)%tipo == PALC_PROPIEDADES ) then
                blockPropertie = .not. blockPropertie
                i = i + 1
            end if

            if (tokens(i)%tipo == PALC_COLOCACION ) then
                blockColocation = .not. blockColocation
                i = i + 1
            end if

            if (tokens(i)%tipo == COMENTARIO_L ) i = i + 1

            if (blockControl) then
                print *, "Control: ", tokens(i)%lexema , tokens(i)%tipo
                if (tokens(i)%tipo == PAL_CLAVE) print *, "Clave: ", tokens(i)%lexema
                
            else if (blockPropertie) then
                print *, "Propiedades: ", tokens(i)%lexema
            else if (blockColocation) then
                print *, "Colocacion: ", tokens(i)%lexema
            end if


            


            i = i + 1
        end do

    end subroutine analizeHtml

end module htmlanalyzer