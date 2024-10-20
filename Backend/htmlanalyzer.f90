module htmlanalyzer
    implicit none
    public :: analizeHtml
    
    contains
    subroutine analizeHtml(content)
        character(len=*) , intent(in) :: content

        character(len=1000) :: line
        character(len=1000) :: control_Type, control_id

        integer :: io, pos , start, endLine
        
        open(unit=20, file="output.html", status="replace", action="write")

        write(20,*) "<html>"
        write(20,*) "<head>"
        write(20,*) "<title>HTML Analyzer</title>"
        write(20,*) "</head>"
        write(20,*) "<body>"

        start = 1

        do
            endLine = index(content(start:), char(10))
            if (endLine == 0) then
                endLine = len(content)
            else
                endLine = endLine + start - 1
            end if

            print *, index(line, "<!--Controles")

        end do

    end subroutine analizeHtml
end module htmlanalyzer