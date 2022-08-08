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
!   Copyright (c) 2021 - 2022
!
!
module data
    implicit none
    integer,           parameter :: theinputtunnel  = 11
    integer,           parameter :: theoutputtunnel = 12
    integer,           parameter :: keylengthlimit  = 1000
    integer,           parameter :: linewidelimit   = 10 * keylengthlimit
    character (len=*), parameter :: xendkey         = 'xxx'
    character (len=*), parameter :: yendkey         = 'yyy'
    character (len=*), parameter :: pendkey         = 'ppp'
    character (len=*), parameter :: theinputfile    = 'in.dat'
    character (len=*), parameter :: macroset        = '\pgfmathsetmacro{'
    character (len=*), parameter :: macrosets       = '\pgfmathsetmacro{\'
    character (len=*), parameter :: spacing         = 'spacing'
    character (len=*), parameter :: additional      = ' + 0.0 }'
    character (len=*), parameter :: coordinate      = '\coordinate ('
contains


subroutine filegenerating(thefile, thetunnel, xxx, yyy, ppp, firstx, totalx, firsty, totaly)
    implicit none
    character (len=*) :: thefile, xxx, yyy, ppp
    character (len=1) :: firstx, firsty
    integer           :: thetunnel, totalx, totaly
    integer           :: firstxi, firstyi, ix, iy

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

    firstxi = ichar(firstx)
    firstyi = ichar(firsty)

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
                             &'{($('//ppp//char(firstxi-1+totalx)//char(firstyi-1+totaly)//')$)}; '

    write(thetunnel,*)
    close(thetunnel)

    return
end subroutine filegenerating

end module


program coordinates
    use data
    implicit none
    integer :: linenumber = 0
    integer :: totalx, totaly, regenerate, ii, l, m, n
    character (len=linewidelimit) :: thekeyword, wordtmp, tmpword
    character (len=1) :: aaa, firstx, firsty

    write(*,*) 'If you want all the files: '
    write(*,*) '                "coordinates.tex" '
    write(*,*) '                "coorda.tex" '
    write(*,*) '                "coordb.tex" '
    write(*,*) '                 ...          '
    write(*,*) '                "coordz.tex" '
    write(*,*) '   to be (re-)generated, '
    write(*,*) 'please reply with 1. Otherwise, any other integer value.'
    read(*,*) regenerate
    if(regenerate.eq.1) then
        call filegenerating('coordinates.tex', theoutputtunnel, xendkey, yendkey, pendkey, 'a', 26, 'a', 26)
        do ii = 1, 26
           aaa = char(96+ii)
           call filegenerating('coord'//aaa//'.tex', theoutputtunnel, aaa//xendkey, aaa//yendkey, aaa//pendkey, 'a', 26, 'a', 26)
        end do
    end if

    linenumber = 0
    open(theinputtunnel, file=theinputfile)
    manyotherfiles: do

        linenumber = linenumber + 1
        wordtmp = ' '
        read(theinputtunnel, '(a)', end = 100)  wordtmp
        thekeyword = ' '
        thekeyword = adjustl(wordtmp)
        l = len_trim(thekeyword)
        if ((l.le.0) .or. (l.gt.keylengthlimit)) then
            write(*,*) 'The string in line number: ', linenumber, ' of the file '//theinputfile
            write(*,*) 'would be used as the KEY-WORD in this coordinate system.'
            write(*,*) 'Since either no such word or more than ', keylengthlimit, ' letters, then stopped.'
            stop
        end if

        do m = 1, l
             n = iachar(thekeyword(m:m))
             if ((n.le.96) .or. (n.ge.123)) then
               write(*,*) 'The string in line number: ', linenumber, ' of the file '//theinputfile
               write(*,*) 'would be used as the KEY-WORD in this coordinate system.'
               write(*,*) 'However only lower-case English letters are allowed.'
               write(*,*) 'Since other character: "'//thekeyword(m:m)//'" is found, then stopped.'
               stop
            end if
        end do

        linenumber = linenumber + 1
        wordtmp = ' '
        read(theinputtunnel, '(a)', end = 100)  wordtmp
        tmpword = ' '
        tmpword = adjustl(wordtmp)
        firstx = tmpword(1:1)
        if ((ichar(firstx).lt.97) .or. (ichar(firstx).gt.122)) then
            write(*,*) 'The first character in line number: ', linenumber, ' of the file '//theinputfile
            write(*,*) 'would be used as the first point in the x-direction in this coordinate system.'
            write(*,*) 'However it must be lower-case English letter.'
            write(*,*) 'Since it is "', firstx, '", then stopped.'
            stop
        end if

        linenumber = linenumber + 1
        read(theinputtunnel, *, end = 100)  totalx
        if ((totalx.lt.1) .or. (totalx.gt.(123-ichar(firstx)))) then
            write(*,*) 'The first integer in line number: ', linenumber, ' of the file '//theinputfile
            write(*,*) 'would be used as the total number of points in the x-direction in this coordinate system.'
            write(*,*) 'However it must be positive and no more than the number of letters between '//firstx//' and z.'
            write(*,*) 'Since it is ', totalx, ', then stopped.'
            stop
        end if

        linenumber = linenumber + 1
        wordtmp = ' '
        read(theinputtunnel, '(a)', end = 100)  wordtmp
        tmpword = ' '
        tmpword = adjustl(wordtmp)
        firsty = tmpword(1:1)
        if ((ichar(firsty).lt.97) .or. (ichar(firsty).gt.122)) then
            write(*,*) 'The first character in line number: ', linenumber, ' of the file '//theinputfile
            write(*,*) 'would be used as the first point in the y-direction in this coordinate system.'
            write(*,*) 'However it must be lower-case English letter.'
            write(*,*) 'Since it is "', firsty, '", then stopped.'
            stop
        end if

        linenumber = linenumber + 1
        read(theinputtunnel, *, end = 100)  totaly
        if ((totaly.lt.1) .or. (totaly.gt.(123-ichar(firsty)))) then
            write(*,*) 'The first integer in line number: ', linenumber, ' of the file '//theinputfile
            write(*,*) 'would be used as the total number of points in the y-direction in this coordinate system.'
            write(*,*) 'However it must be positive and no more than the number of letters between '//firsty//' and z.'
            write(*,*) 'Since it is ', totaly, ', then stopped.'
            stop
        end if


        call filegenerating('coord'//thekeyword(1:l)//'.tex', theoutputtunnel, thekeyword(1:l)//xendkey, &
                                                    &thekeyword(1:l)//yendkey, thekeyword(1:l)//pendkey, &
                                                                          & firstx, totalx, firsty, totaly)
    end do manyotherfiles

    100 stop

end program

