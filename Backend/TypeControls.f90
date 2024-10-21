module TypeControl
    implicit none
    public :: Tag, Button, Check, Radio, Text, TextArea, Key, Container

    integer, parameter :: Tag = 1
    integer, parameter :: Button = 2
    integer, parameter :: Check = 3
    integer, parameter :: Radio = 4
    integer, parameter :: Text = 5
    integer, parameter :: TextArea = 6
    integer, parameter :: Key = 7
    integer, parameter :: Container = 8

    type :: htmlControl
        character(len=100) :: id
        character(len=100) :: control
        integer :: typeControl
        integer :: indexRef
    end type htmlControl

    type :: TagControl
        integer :: Tr,Tg,Tb
        integer :: r,g,b
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
        integer, dimension(:), allocatable :: controls
    end type ContainerControl

contains

    subroutine addHtmlControl(id, control, typeControl, indexRef, controls)
        implicit none
        character(len=*), intent(in) :: id, control
        integer, intent(in) :: typeControl, indexRef
        type(htmlControl), dimension(:), allocatable, intent(inout) :: controls
        type(htmlControl) :: newControl
        type(htmlControl), dimension(:), allocatable :: temp_controls
        integer :: actualSize

        newControl%id = id
        newControl%control = control
        newControl%typeControl = typeControl
        newControl%indexRef = indexRef

        actualSize = size(controls)

        allocate(temp_controls(actualSize + 1))

        if (actualSize > 0) then
            temp_controls(1:actualSize) = controls
        end if

        temp_controls(actualSize + 1) = newControl

        call move_alloc(temp_controls, controls)
    end subroutine addHtmlControl

    subroutine addTagControl(Tr, Tg, Tb, r, g, b, tags)
        implicit none
        integer, intent(in) :: Tr, Tg, Tb, r, g, b
        type(TagControl), dimension(:), allocatable, intent(inout) :: tags
        type(TagControl) :: newTag
        type(TagControl), dimension(:), allocatable :: temp_tags
        integer :: actualSize

        newTag%Tr = Tr
        newTag%Tg = Tg
        newTag%Tb = Tb
        newTag%r = r
        newTag%g = g
        newTag%b = b

        actualSize = size(tags)

        allocate(temp_tags(actualSize + 1))

        if (actualSize > 0) then
            temp_tags(1:actualSize) = tags
        end if

        temp_tags(actualSize + 1) = newTag

        call move_alloc(temp_tags, tags)
    end subroutine addTagControl

   subroutine addButtonControl(text, Align, buttons)
    implicit none
    character(len=*), intent(in) :: text, Align
    type(ButtonControl), dimension(:), allocatable, intent(inout) :: buttons
    type(ButtonControl) :: newButton
    type(ButtonControl), dimension(:), allocatable :: temp_buttons
    integer :: actualSize

    newButton%text = text
    newButton%Align = Align

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
        character(len=*), intent(in) :: text
        logical, intent(in) :: value
        integer, intent(in) :: NoGroup
        type(CheckControl), dimension(:), allocatable, intent(inout) :: checks
        type(CheckControl) :: newCheck
        type(CheckControl), dimension(:), allocatable :: temp_checks
        integer :: actualSize

        newCheck%text = text
        newCheck%value = value
        newCheck%NoGroup = NoGroup

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
        character(len=*), intent(in) :: text
        logical, intent(in) :: value
        integer, intent(in) :: NoGroup
        type(RadioControl), dimension(:), allocatable, intent(inout) :: radios
        type(RadioControl) :: newRadio
        type(RadioControl), dimension(:), allocatable :: temp_radios
        integer :: actualSize

        newRadio%text = text
        newRadio%value = value
        newRadio%NoGroup = NoGroup

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
        character(len=*), intent(in) :: text, Align
        type(TextControl), dimension(:), allocatable, intent(inout) :: texts
        type(TextControl) :: newText
        type(TextControl), dimension(:), allocatable :: temp_texts
        integer :: actualSize

        newText%text = text
        newText%Align = Align

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
        character(len=*), intent(in) :: text
        type(TextAreaControl), dimension(:), allocatable, intent(inout) :: textAreas
        type(TextAreaControl) :: newTextArea
        type(TextAreaControl), dimension(:), allocatable :: temp_textAreas
        integer :: actualSize

        newTextArea%text = text

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
        character(len=*), intent(in) :: Text, Align
        type(KeyControl), dimension(:), allocatable, intent(inout) :: keys
        type(KeyControl) :: newKey
        type(KeyControl), dimension(:), allocatable :: temp_keys
        integer :: actualSize

        newKey%Text = Text
        newKey%Align = Align

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
        character(len=*), intent(in) :: id
        integer, intent(in) :: r, g, b, height, width
        integer, dimension(:), allocatable, intent(in) :: controls
        type(ContainerControl), dimension(:), allocatable, intent(inout) :: containers
        type(ContainerControl) :: newContainer
        type(ContainerControl), dimension(:), allocatable :: temp_containers
        integer :: actualSize

        newContainer%id = id
        newContainer%r = r
        newContainer%g = g
        newContainer%b = b
        newContainer%height = height
        newContainer%width = width
        newContainer%controls = controls

        actualSize = size(containers)

        allocate(temp_containers(actualSize + 1))

        if (actualSize > 0) then
            temp_containers(1:actualSize) = containers
        end if

        temp_containers(actualSize + 1) = newContainer

        call move_alloc(temp_containers, containers)
    end subroutine addContainerControl


end module TypeControl