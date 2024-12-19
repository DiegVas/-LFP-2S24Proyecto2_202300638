module TypeControl
    implicit none
    ! ! Definicion de los tipos de controles
    public :: Tag, Button, Check, Radio, Text, TextArea, Key, Container

    integer, parameter :: Tag = 1
    integer, parameter :: Button = 2
    integer, parameter :: Check = 3
    integer, parameter :: Radio = 4
    integer, parameter :: Text = 5
    integer, parameter :: TextArea = 6
    integer, parameter :: Key = 7
    integer, parameter :: Container = 8

    ! ! Definicion de las propiedades de los controles

    public :: ColorLetter, TextVal, Aling, ColorBackground, Marck, Group, Height, Width
    integer, parameter :: ColorLetter = 9
    integer, parameter :: TextVal = 10
    integer, parameter :: Aling = 11
    integer, parameter :: ColorBackground = 12
    integer, parameter :: Marck = 13
    integer, parameter :: Group = 14
    integer, parameter :: Height = 15
    integer, parameter :: Width = 16


    ! ! Definicion de los tipos de controles con sus propiedades
    type :: htmlControl
        character(len=100) :: id
        character(len=100) :: control
        integer :: posX, posY
        integer :: typeControl
        integer :: indexRef
        logical :: isContainer
        integer :: indexContainer
    end type htmlControl

    type :: TagControl
        integer :: Tr,Tg,Tb
        integer :: r,g,b
        integer :: height, width
        character(len=100) :: text
    end type TagControl

    type :: ButtonControl
        character(len=100) :: text
        character(len=100) :: Align
    end type ButtonControl

    type :: CheckControl
        character(len=100) :: text
        logical :: value
        integer :: NoGroup
    end type CheckControl

    type :: RadioControl
        character(len=100) :: text
        logical :: value
        integer :: NoGroup
        character(len=100) :: Group
    end type RadioControl

    type :: TextControl
        character(len=100) :: text
        character(len=100) :: Align
    end type TextControl

    type :: TextAreaControl
        character(len=100) :: text
    end type TextAreaControl

    type :: KeyControl
        character(len=100) :: Text
        character(len=100) :: Align
    end type KeyControl

    type :: ContainerControl
        character(len=100) :: id
        integer :: r,g,b
        integer :: height, width
        type(htmlControl) ,dimension(:), allocatable :: controls
    end type ContainerControl

contains
    ! ! Definicion de los metodos para agregar controles dependiendo de su tipo
    subroutine addHtmlControl(newControl, controls)
        implicit none
        type(htmlControl), dimension(:), allocatable, intent(inout) :: controls
        type(htmlControl), intent(inout) :: newControl
        type(htmlControl), dimension(:), allocatable :: temp_controls
        integer :: actualSize

        actualSize = size(controls)

        allocate(temp_controls(actualSize + 1))

        if (actualSize > 0) then
            temp_controls(1:actualSize) = controls
        end if

        newControl%posX = 0
        newControl%posY = 0
        temp_controls(actualSize + 1) = newControl

        call move_alloc(temp_controls, controls)
    end subroutine addHtmlControl

    subroutine addTagControl(Tr, Tg, Tb, r, g, b,height, width,  tags)
        implicit none
        integer, intent(in), optional :: Tr, Tg, Tb, r, g, b, height, width
        type(TagControl), dimension(:), allocatable, intent(inout) :: tags
        type(TagControl) :: newTag
        type(TagControl), dimension(:), allocatable :: temp_tags
        integer :: actualSize

        if (present(Tr)) newTag%Tr = Tr
        if (present(Tg)) newTag%Tg = Tg
        if (present(Tb)) newTag%Tb = Tb
        if (present(r)) newTag%r = r
        if (present(g)) newTag%g = g
        if (present(b)) newTag%b = b
        if (present(height)) newTag%height = height
        if (present(width)) newTag%width = width

        actualSize = size(tags)

        allocate(temp_tags(actualSize + 1))

        if (actualSize > 0) then
            temp_tags(1:actualSize) = tags
        end if

        newTag%text = ""
        newTag%Tr = 0
        newTag%Tg = 0
        newTag%Tb = 0
        newTag%r = 0
        newTag%g = 0
        newTag%b = 0

        temp_tags(actualSize + 1) = newTag

        

        call move_alloc(temp_tags, tags)
    end subroutine addTagControl

    subroutine addButtonControl(text, Align, buttons)
        implicit none
        character(len=*), intent(in), optional :: text, Align
        type(ButtonControl), dimension(:), allocatable, intent(inout) :: buttons
        type(ButtonControl) :: newButton
        type(ButtonControl), dimension(:), allocatable :: temp_buttons
        integer :: actualSize

        if (present(text)) newButton%text = text
        if (present(Align)) newButton%Align = Align

        actualSize = size(buttons)

        allocate(temp_buttons(actualSize + 1))

        if (actualSize > 0) then
            temp_buttons(1:actualSize) = buttons
        end if

        temp_buttons(actualSize + 1) = newButton

        call move_alloc(temp_buttons, buttons)
    end subroutine addButtonControl

    subroutine addCheckControl(text, value, NoGroup, checks)
        implicit none
        character(len=*), intent(in), optional :: text
        logical, intent(in), optional :: value
        integer, intent(in), optional :: NoGroup
        type(CheckControl), dimension(:), allocatable, intent(inout) :: checks
        type(CheckControl) :: newCheck
        type(CheckControl), dimension(:), allocatable :: temp_checks
        integer :: actualSize

        if (present(text)) newCheck%text = text
        if (present(value)) newCheck%value = value
        if (present(NoGroup)) newCheck%NoGroup = NoGroup

        actualSize = size(checks)

        allocate(temp_checks(actualSize + 1))

        if (actualSize > 0) then
            temp_checks(1:actualSize) = checks
        end if

        temp_checks(actualSize + 1) = newCheck

        call move_alloc(temp_checks, checks)
    end subroutine addCheckControl

    subroutine addRadioControl(text, value, NoGroup, radios)
        implicit none
        character(len=*), intent(in), optional :: text
        logical, intent(in), optional :: value
        integer, intent(in), optional :: NoGroup
        type(RadioControl), dimension(:), allocatable, intent(inout) :: radios
        type(RadioControl) :: newRadio
        type(RadioControl), dimension(:), allocatable :: temp_radios
        integer :: actualSize

        if (present(text)) newRadio%text = text
        if (present(value)) newRadio%value = value
        if (present(NoGroup)) newRadio%NoGroup = NoGroup

        actualSize = size(radios)

        allocate(temp_radios(actualSize + 1))

        if (actualSize > 0) then
            temp_radios(1:actualSize) = radios
        end if

        temp_radios(actualSize + 1) = newRadio

        call move_alloc(temp_radios, radios)
    end subroutine addRadioControl

    subroutine addTextControl(text, Align, texts)
        implicit none
        character(len=*), intent(in), optional :: text, Align
        type(TextControl), dimension(:), allocatable, intent(inout) :: texts
        type(TextControl) :: newText
        type(TextControl), dimension(:), allocatable :: temp_texts
        integer :: actualSize

        if (present(text)) newText%text = text
        if (present(Align)) newText%Align = Align

        actualSize = size(texts)

        allocate(temp_texts(actualSize + 1))

        if (actualSize > 0) then
            temp_texts(1:actualSize) = texts
        end if

        temp_texts(actualSize + 1) = newText

        call move_alloc(temp_texts, texts)
    end subroutine addTextControl

    subroutine addTextAreaControl(text, textAreas)
        implicit none
        character(len=*), intent(in), optional :: text
        type(TextAreaControl), dimension(:), allocatable, intent(inout) :: textAreas
        type(TextAreaControl) :: newTextArea
        type(TextAreaControl), dimension(:), allocatable :: temp_textAreas
        integer :: actualSize

        if (present(text)) newTextArea%text = text

        actualSize = size(textAreas)

        allocate(temp_textAreas(actualSize + 1))

        if (actualSize > 0) then
            temp_textAreas(1:actualSize) = textAreas
        end if

        temp_textAreas(actualSize + 1) = newTextArea

        call move_alloc(temp_textAreas, textAreas)
    end subroutine addTextAreaControl

    subroutine addKeyControl(Text, Align, keys)
        implicit none
        character(len=*), intent(in), optional :: Text, Align
        type(KeyControl), dimension(:), allocatable, intent(inout) :: keys
        type(KeyControl) :: newKey
        type(KeyControl), dimension(:), allocatable :: temp_keys
        integer :: actualSize

        if (present(Text)) newKey%Text = Text
        if (present(Align)) newKey%Align = Align

        actualSize = size(keys)

        allocate(temp_keys(actualSize + 1))

        if (actualSize > 0) then
            temp_keys(1:actualSize) = keys
        end if

        temp_keys(actualSize + 1) = newKey

        call move_alloc(temp_keys, keys)
    end subroutine addKeyControl

    subroutine addContainerControl(id, r, g, b, height, width, controls, containers)
        implicit none
        character(len=*), intent(in), optional :: id
        integer, intent(in), optional :: r, g, b, height, width
        type(htmlControl), dimension(:), allocatable, intent(in), optional :: controls
        type(ContainerControl), dimension(:), allocatable, intent(inout) :: containers
        type(ContainerControl) :: newContainer
        type(ContainerControl), dimension(:), allocatable :: temp_containers
        integer :: actualSize

        if (present(id)) newContainer%id = id
        if (present(r)) newContainer%r = r
        if (present(g)) newContainer%g = g
        if (present(b)) newContainer%b = b
        if (present(height)) newContainer%height = height
        if (present(width)) newContainer%width = width
        if (present(controls)) newContainer%controls = controls

        actualSize = size(containers)

        allocate(temp_containers(actualSize + 1))

        if (actualSize > 0) then
            temp_containers(1:actualSize) = containers
        end if

        allocate(newContainer%controls(0))

        temp_containers(actualSize + 1) = newContainer

        call move_alloc(temp_containers, containers)
    end subroutine addContainerControl

    subroutine addControlToContainer(containerIndex, newControl, containerControls)
        implicit none
        integer, intent(in) :: containerIndex
        type(htmlControl), intent(in) :: newControl
        type(ContainerControl), dimension(:), allocatable, intent(inout) :: containerControls
        type(htmlControl), dimension(:), allocatable :: temp_controls
        integer :: actualSize

        ! Ensure the container index is within bounds
        if (containerIndex > size(containerControls) .or. containerIndex < 1) then
            print *, "Error: containerIndex out of bounds"
            return
        end if

        ! Get the current size of the controls array
        actualSize = size(containerControls(containerIndex)%controls)

        ! Allocate a temporary array with one additional element
        allocate(temp_controls(actualSize + 1))

        ! Copy existing controls to the temporary array
        if (actualSize > 0) then
            temp_controls(1:actualSize) = containerControls(containerIndex)%controls
        end if

        ! Add the new control to the temporary array
        temp_controls(actualSize + 1) = newControl

        ! Move the temporary array to the controls array
        call move_alloc(temp_controls, containerControls(containerIndex)%controls)
    end subroutine addControlToContainer

    function getTypeControl(typeControl) result(type)
        implicit none
        character(len=100), intent(in) :: typeControl
        integer :: type

        select case (typeControl)
            case ("Contenedor")
                type = Container
            case ("Boton")
                type = Button
            case ("Etiqueta")
                type = Tag
            case ("Check")
                type = Check
            case ("Radio")
                type = Radio
            case ("Texto")
                type = Text
            case ("AreaTexto")
                type = TextArea
            case ("Clave")
                type = Key

        end select


    end function getTypeControl

    function addTypeControl(typeControl,tagControls,buttonControls,checkControls,radioControls,textControls,textAreaControls,keyControls, containerControls ) result(index)
        implicit none
        integer, intent(in) :: typeControl
        integer :: index
        type(TagControl), dimension(:), allocatable, intent(inout) :: tagControls
        type(ButtonControl), dimension(:), allocatable, intent(inout) :: buttonControls
        type(CheckControl), dimension(:), allocatable, intent(inout) :: checkControls
        type(RadioControl), dimension(:), allocatable, intent(inout) :: radioControls
        type(TextControl), dimension(:), allocatable, intent(inout) :: textControls
        type(TextAreaControl), dimension(:), allocatable, intent(inout) :: textAreaControls
        type(KeyControl), dimension(:), allocatable, intent(inout) :: keyControls
        type(ContainerControl), dimension(:), allocatable, intent(inout) :: containerControls

        select case (typeControl)
            case (Tag)
                call addTagControl(tags=tagControls)
                index = size(tagControls)
            case (Button)
                call addButtonControl(buttons=buttonControls)
                index = size(buttonControls)
            case (Check)
                call addCheckControl(checks=checkControls)
                index = size(checkControls)
            case (Radio)
                call addRadioControl(radios=radioControls)
                index = size(radioControls)
            case (Text)
                call addTextControl(texts=textControls)
                index = size(textControls)
            case (TextArea)
                call addTextAreaControl(textAreas=textAreaControls)
                index = size(textAreaControls)
            case (Key)
                call addKeyControl(keys=keyControls)
                index = size(keyControls)
            case (Container)
                call addContainerControl(containers=containerControls)
                index = size(containerControls)

        end select

    end function addTypeControl

    function getIndexControl(id, controls) result(index)
        implicit none
        character(len=100), intent(in) :: id
        type(htmlControl), dimension(:), intent(in) :: controls
        integer :: index, i

        index = 0

        do i = 1, size(controls)
            if (controls(i)%id == id) then
                index = i
                exit
            end if
        end do

    end function getIndexControl

    function getTypePropietie(propietie) result(type)
        implicit none
        character(len=100), intent(in) :: propietie
        integer :: type

        select case (propietie)
            case ("setColorLetra")
                type = ColorLetter
            case ("setTexto")
                type = TextVal
            case ("setAlineacion")
                type = Aling
            case ("setColorFondo")
                type = ColorBackground
            case ("setMarcar")
                type = Marck
            case ("setGrupo")
                type = Group
            case ("setAlto")
                type = Height
            case ("setAncho")
                type = Width
        end select

    end function getTypePropietie

    subroutine setColor(index,typePropertie,typeControl,r, g, b, tags, containers)
        implicit none
        integer, intent(in) :: r, g, b, index, typePropertie, typeControl
        type(TagControl), dimension(:), intent(inout) :: tags
        type(ContainerControl), dimension(:), intent(inout) :: containers

        if (typeControl == Tag) then
            if (typePropertie == ColorLetter) then
                tags(index)%Tr = r
                tags(index)%Tg = g
                tags(index)%Tb = b
            else if (typePropertie == ColorBackground) then
                tags(index)%r = r
                tags(index)%g = g
                tags(index)%b = b
            end if
        else if (typeControl == Container) then
            containers(index)%r = r
            containers(index)%g = g
            containers(index)%b = b
        end if

    end subroutine setColor

    subroutine setText(index, typePropertie,typeControl, TextV, botonList, checkList, RadioList, TextList, AreaTexList, KeyList, tags)
        integer, intent(in) :: index, typePropertie,typeControl
        character(len=100), intent(in) :: TextV
        type(ButtonControl), dimension(:), intent(inout) :: botonList
        type(CheckControl), dimension(:), intent(inout) :: checkList
        type(RadioControl), dimension(:), intent(inout) :: RadioList
        type(TextControl), dimension(:), intent(inout) :: TextList
        type(TextAreaControl), dimension(:), intent(inout) :: AreaTexList
        type(KeyControl), dimension(:), intent(inout) :: KeyList
        type(TagControl), dimension(:), intent(inout) :: tags

        if (typeControl == Button) then
            if (typePropertie == TextVal) then
                botonList(index)%text = TextV
            else if (typePropertie == Aling) then
                botonList(index)%Align = TextV
            end if
        else if (typeControl == Check) then
            checkList(index)%text = TextV
        else if (typeControl == Radio) then
            if (typePropertie == TextVal) then
            RadioList(index)%text = TextV
            else if (typePropertie == Marck) then
                RadioList(index)%Group = TextV
            end if
        else if (typeControl == Text) then
            if (typePropertie == TextVal) then
                TextList(index)%text = TextV
            else if (typePropertie == Aling) then
                TextList(index)%Align = TextV
            end if
        else if (typeControl == TextArea) then
            AreaTexList(index)%text = TextV
        else if (typeControl == Key) then
            if (typePropertie == TextVal) then
            KeyList(index)%Text = TextV
            else if (typePropertie == Aling) then
                KeyList(index)%Align = TextV
            end if
        else if (typeControl == Tag) then
            tags(index)%text = TextV
        end if
    
    end subroutine setText

    subroutine setNumeric(index, typePropertie, typeControl, numeric,tags, containers, checks, radios )
        integer, intent(in) :: index, typePropertie, typeControl, numeric
        type(TagControl), dimension(:), intent(inout) :: tags
        type(ContainerControl), dimension(:), intent(inout) :: containers
        type(CheckControl), dimension(:), intent(inout) :: checks
        type(RadioControl), dimension(:), intent(inout) :: radios

        if (typeControl == Tag) then
            if (typePropertie == Height) then
                tags(index)%height = numeric
            else if (typePropertie == Width) then
                tags(index)%width = numeric
            end if
        else if (typeControl == Container) then
            if (typePropertie == Height) then
                containers(index)%height = numeric
            else if (typePropertie == Width) then
                containers(index)%width = numeric         
            end if
        else if (typeControl == Check) then
            if (typePropertie == Group) then
                checks(index)%NoGroup = numeric
            end if
        else if (typeControl == Radio) then
            if (typePropertie == Group) then
                radios(index)%NoGroup = numeric
            end if
        end if

    end subroutine setNumeric

    subroutine setPosicion(index, controlers, x, y)
        integer, intent(in) :: index, x,y
        type(htmlControl), dimension(:), intent(inout) :: controlers

        controlers(index)%posX = x
        controlers(index)%posY = y

    end subroutine setPosicion

end module TypeControl