module analyzerSintac
    use token_module
    implicit none
    private
    public :: parser

    type(token), dimension(:) , allocatable :: tokens(:)

    ! Variable para recorrer los tokens
    integer :: numPreAnalisis
    ! Variable para almacenar el token actual
    type(token) :: preAnalisis

end module analyzerSintac