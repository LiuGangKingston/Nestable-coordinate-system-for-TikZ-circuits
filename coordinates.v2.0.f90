!   This is
!   https://github.com/LiuGangKingston/Nestable-coordinate-system-for-Tikz-circuits.git
!            Version 2.0
!   free for non-commercial use.
!   Please send us emails for any problems/suggestions/comments.
!   Please be advised that none of us accept any responsibility
!   for any consequences arising out of the usage of this
!   software, especially for damage.
!   For usage, please refer to the README file.
!   This code was written by
!        Gang Liu (gl.cell@outlook)
!                 (http://orcid.org/0000-0003-1575-9290)
!          and
!        Shiwei Huang (huang937@gmail.com)
!   Copyright (c) 2021
!
!
module data
    implicit none
    integer  :: ii, ix, iy, p
    integer,           parameter :: theinputtunnel  = 11
    integer,           parameter :: theoutputtunnel = 12
    character (len=*), parameter :: theinputfile    = 'in.dat'
    character (len=*), parameter :: macroset = '\pgfmathsetmacro{'
    character (len=*), parameter :: macrosets= '\pgfmathsetmacro{\'
    character (len=*), parameter :: spacing = 'spacing'
    character (len=*), parameter :: additional = ' + 0.0 }'
    character (len=*), parameter :: coordinate = '\coordinate ('
    character (len=1)            :: aaa = 'a'
contains


subroutine filegenerating(thefile, thetunnel, xxx, yyy, ppp, firstx, totalx, firsty, totaly)
    implicit none
    integer    :: thetunnel, totalx, totaly
    character (len=1) :: firstx, firsty
    integer           :: firstxi, firstyi
    character (len=*) :: thefile, xxx, yyy, ppp

    firstxi = ichar(firstx)
    firstyi = ichar(firsty)

    open(thetunnel, file=thefile)

    write(thetunnel,'(a)') '% https://github.com/LiuGangKingston/Nestable-coordinate-system-for-Tikz-circuits.git'
    write(thetunnel,'(a)') '% https://github.com/LiuGangKingston/Nestable-coordinate-system-for-Tikz-circuits.git'
    write(thetunnel,'(a)')
    write(thetunnel,'(a)')

    write(thetunnel,'(a,i2,a)') macroset//'\total'//xxx//'}{',totalx,'}'
    write(thetunnel,'(a,i2,a)') macroset//'\total'//yyy//'}{',totaly,'}'
    write(thetunnel,'(a)') macrosets//xxx//spacing//'}{1}'
    write(thetunnel,'(a)') macrosets//yyy//spacing//'}{1}'
    write(thetunnel,'(a)') macrosets//xxx//firstx//'}{-8}'
    write(thetunnel,'(a)') macrosets//yyy//firsty//'}{-8}'

    write(thetunnel,*)
    do ix = 1, totalx-1
        write(thetunnel,'(a)') macrosets//xxx//char(firstxi+ix)//'}{\'//xxx//char(firstxi-1+ix)//' + \'//xxx//spacing//additional
    end do

    write(thetunnel,*)
    do iy = 1, totaly-1
        write(thetunnel,'(a)') macrosets//yyy//char(firstyi+iy)//'}{\'//yyy//char(firstyi-1+iy)//' + \'//yyy//spacing//additional
    end do

    write(thetunnel,*)
    do ix = 1, totalx
    do iy = 1, totaly
        write(thetunnel,'(a)') coordinate//ppp//char(firstxi-1+ix)//char(firstyi-1+iy)//') at (\'//xxx//char(firstxi-1+ix)//&
                                                                                           &', \'//yyy//char(firstyi-1+iy)//');'
    end do
    end do

    write(thetunnel,*)
    write(thetunnel,'(a)') '%\gangprintcoordinateat{(0,0)}{The last coordinate values: }'//&
                   &'{($('//ppp//char(96+totalx)//char(96+totaly)//')$)}; '

    write(thetunnel,*)
    close(thetunnel)

    return
end subroutine filegenerating

end module


program coordinates
    use data
    implicit none
    integer            :: totalx = 26
    integer            :: totaly = 26
    integer            :: regenerate, l, m, n
    integer            :: line_number = 0
    integer, parameter :: key_max = 1000
    integer, parameter :: alength = 10 * key_max
    character (len=1)  :: firstx  = 'a'
    character (len=1)  :: firsty  = 'a'
    character (len=3)  :: xendkey = 'xxx'
    character (len=3)  :: yendkey = 'yyy'
    character (len=3)  :: pendkey = 'ppp'
    character (len=alength)  :: thekeyword = ' '
    character (len=alength)  :: awordp= ' '
    character (len=alength)  :: tpword= ' '

    write(*,*) 'If you want all the files: '
    write(*,*) '                "coordinates.tex" '
    write(*,*) '                "coorda.tex" '
    write(*,*) '                "coordb.tex" '
    write(*,*) '                 ...          '
    write(*,*) '                "coordz.tex" '
    write(*,*) '   to be (re-)generated, '
    write(*,*) 'please reply with 1. Otherwise, any other number.'
    read(*,*) regenerate
    if(regenerate.eq.1) then
        call filegenerating('coordinates.tex', theoutputtunnel, xendkey, yendkey, pendkey, firstx, totalx, firsty, totaly)
        do ii = 1, 26
           aaa = char(96+ii)
           call filegenerating('coord'//aaa//'.tex', theoutputtunnel, aaa//xendkey, aaa//yendkey, aaa//pendkey, &
                                                                                         & firstx, totalx, firsty, totaly)
        end do
    end if

    line_number = 0
    open(theinputtunnel, file=theinputfile)
    manyotherfiles: do

        !write(*,*) 'If you need a file called "coordANY.tex"  '
        !write(*,*) '   which containes definitions            '
        !write(*,*) '         \pgfmathsetmacro{\ANYxxxX}{...}  '
        !write(*,*) '                 ...                      '
        !write(*,*) '         \pgfmathsetmacro{\ANYyyyY}{...}  '
        !write(*,*) '                 ...                      '
        !write(*,*) '         \coordinate (ANYpppXY) at (\ANYxxxX, \ANYyyyY);    '
        !write(*,*) '                 ...                      '
        !write(*,*) '   where  both the upper case "X" and "Y" runs from "a" to "z", '
        !write(*,*) 'please input the key word as the "ANY"  (but no quotes please).'
        !write(*,*) 'Only English letters are accepeted for it, lower case suggestd.'
        !write(*,*) 'NO MORE THAN ', alength, ' IN LENGTH, please.'
        !write(*,*) 'Anything other than English letters means to stop this code run.'
        !write(*,*) 'Your input as the "ANY" now: '

        line_number = line_number + 1
        awordp = ' '
        read(theinputtunnel, '(a)', end = 100)  awordp
        thekeyword = ' '
        thekeyword = adjustl(awordp)
        l = len_trim(thekeyword)
        if ((l.le.0) .or. (l.gt.key_max)) then
            write(*,*) 'The string in line number: ', line_number, ' of the file '//theinputfile
            write(*,*) 'would be used as the KEY-WORD in this coordinate system.'
            write(*,*) 'Since either no such word or more than ', key_max, ' letters, then stopped.'
            stop
        end if
        do m = 1, l
            if ((ichar(thekeyword(m:m)).lt.97) .or. (ichar(thekeyword(m:m)).gt.122)) then
               write(*,*) 'The string in line number: ', line_number, ' of the file '//theinputfile
               write(*,*) 'would be used as the KEY-WORD in this coordinate system.'
               write(*,*) 'However only lower English letters are allowed.'
               write(*,*) 'Since something other than lower English letters is found, then stopped.'
               stop
            end if
        end do

        line_number = line_number + 1
        awordp = ' '
        read(theinputtunnel, '(a)', end = 100)  awordp
        tpword = ' '
        tpword = adjustl(awordp)
        firstx = tpword(1:1)
        if ((ichar(firstx).lt.97) .or. (ichar(firstx).gt.122)) then
            write(*,*) 'The first character in line number: ', line_number, ' of the file '//theinputfile
            write(*,*) 'would be used as the first point in the x-direction in this coordinate system.'
            write(*,*) 'However it must be lower English letter.'
            write(*,*) 'Since it is ', firstx, ', then stopped.'
            stop
        end if

        line_number = line_number + 1
        read(theinputtunnel, *, end = 100)  totalx
        if ((totalx.lt.1) .or. (totalx.gt.(123-ichar(firstx)))) then
            write(*,*) 'The integer in line number: ', line_number, ' of the file '//theinputfile
            write(*,*) 'would be used as the total number of points in the x-direction in this coordinate system.'
            write(*,*) 'However it must be positive and no more than the number of letters between '//firstx//' and z.'
            write(*,*) 'Since it is ', totalx, ', then stopped.'
            stop
        end if

        line_number = line_number + 1
        awordp = ' '
        read(theinputtunnel, '(a)', end = 100)  awordp
        tpword = ' '
        tpword = adjustl(awordp)
        firsty = tpword(1:1)
        if ((ichar(firsty).lt.97) .or. (ichar(firsty).gt.122)) then
            write(*,*) 'The first character in line number: ', line_number, ' of the file '//theinputfile
            write(*,*) 'would be used as the first point in the y-direction in this coordinate system.'
            write(*,*) 'However it must be lower English letter.'
            write(*,*) 'Since it is ', firsty, ', then stopped.'
            stop
        end if

        line_number = line_number + 1
        read(theinputtunnel, *, end = 100)  totaly
        if ((totaly.lt.1) .or. (totaly.gt.(123-ichar(firsty)))) then
            write(*,*) 'The integer in line number: ', line_number, ' of the file '//theinputfile
            write(*,*) 'would be used as the total number of points in the y-direction in this coordinate system.'
            write(*,*) 'However it must be positive and no more than the number of letters between '//firsty//' and z.'
            write(*,*) 'Since it is ', totaly, ', then stopped.'
            stop
        end if


        do m = 1, l
             n = iachar(thekeyword(m:m))
             if ((n.le.64) .or. ((n.ge.91).and.(n.le.96)) .or. (n.ge.123)) stop
        end do
        call filegenerating('coord'//thekeyword(1:l)//'.tex', theoutputtunnel, thekeyword(1:l)//xendkey, &
                                                    &thekeyword(1:l)//yendkey, thekeyword(1:l)//pendkey, &
                                                                          & firstx, totalx, firsty, totaly)
    end do manyotherfiles

    100 stop

end program

