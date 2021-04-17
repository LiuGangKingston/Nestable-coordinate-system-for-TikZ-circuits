!   This is
!   https://github.com/LiuGangKingston/Nestable-coordinate-system-for-Tikz-circuits.git
!            Version 1.0
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
    integer  :: totalx=26, totaly=26, ii, ix, iy, p
    integer,          parameter  :: theoutputtunnel = 12
    character (len=*), parameter :: macroset = '\pgfmathsetmacro{'
    character (len=*), parameter :: macrosets= '\pgfmathsetmacro{\'
    character (len=*), parameter :: spacing = 'spacing'
    character (len=*), parameter :: additional = ' + 0.0 }'
    character (len=*), parameter :: coordinate = '\coordinate ('
    character (len=1)            :: aaa = 'a'
contains


subroutine filegenerating(thefile, thetunnel, xxx, yyy, ppp)
    implicit none
    integer    :: thetunnel
    character (len=*) :: thefile, xxx, yyy, ppp

    open(thetunnel, file=thefile)

    write(thetunnel,'(a)') 'https://github.com/LiuGangKingston/Nestable-coordinate-system-for-Tikz-circuits.git'
    write(thetunnel,'(a)') 'https://github.com/LiuGangKingston/Nestable-coordinate-system-for-Tikz-circuits.git'
    write(thetunnel,'(a)') 
    write(thetunnel,'(a)') 

    write(thetunnel,'(a)') macroset//'\total'//xxx//'}{26}'
    write(thetunnel,'(a)') macroset//'\total'//yyy//'}{26}'
    write(thetunnel,'(a)') macrosets//xxx//spacing//'}{1}'
    write(thetunnel,'(a)') macrosets//yyy//spacing//'}{1}'
    write(thetunnel,'(a)') macrosets//xxx//'a}{-8}'
    write(thetunnel,'(a)') macrosets//yyy//'a}{-8}'

    write(thetunnel,*)
    do ix = 1, totalx-1
        write(thetunnel,'(a)') macrosets//xxx//char(97+ix)//'}{\'//xxx//char(96+ix)//' + \'//xxx//spacing//additional
    end do

    write(thetunnel,*)
    do iy = 1, totaly-1
        write(thetunnel,'(a)') macrosets//yyy//char(97+iy)//'}{\'//yyy//char(96+iy)//' + \'//yyy//spacing//additional
    end do

    write(thetunnel,*)
    do ix = 1, totalx
    do iy = 1, totaly
        write(thetunnel,'(a)') coordinate//ppp//char(96+ix)//char(96+iy)//') at (\'//xxx//char(96+ix)//&
                              &', \'//yyy//char(96+iy)//');'
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
    integer            :: regenerate, l, m, n
    integer, parameter :: alength = 1000
    character (len=3)  :: xendkey = 'xxx'
    character (len=3)  :: yendkey = 'yyy'
    character (len=3)  :: pendkey = 'ppp'
    character (len=alength)  :: aword = ' '
    character (len=alength)  :: awordp= ' '

    write(*,*) 'If you want all the files: '
    write(*,*) '                "coordinates.tex" '
    write(*,*) '                "coorda.tex" '
    write(*,*) '                "coordb.tex" '
    write(*,*) '                 ...          '
    write(*,*) '                "coordz.tex" '
    write(*,*) '   to be generated or re-generated, '
    write(*,*) 'please reply with 1. Otherwise, any other number.'
    read(*,*) regenerate
    if(regenerate.eq.1) then
        call filegenerating('coordinates.tex', theoutputtunnel, xendkey, yendkey, pendkey)
        do ii = 1, 26
           aaa = char(96+ii)
           call filegenerating('coord'//aaa//'.tex', theoutputtunnel, aaa//xendkey, aaa//yendkey, aaa//pendkey)
        end do
    end if

    manyotherfiles: do
        write(*,*) 'If you need a file called "coordANY.tex"  '
        write(*,*) '   which containes definitions            '
        write(*,*) '         \pgfmathsetmacro{\ANYxxxX}{...}  '
        write(*,*) '                 ...                      '
        write(*,*) '         \pgfmathsetmacro{\ANYyyyY}{...}  '
        write(*,*) '                 ...                      '
        write(*,*) '         \coordinate (ANYpppXY) at (\ANYxxxX, \ANYyyyY);    '
        write(*,*) '                 ...                      '
        write(*,*) '   where  both the upper case "X" and "Y" runs from "a" to "z", '
        write(*,*) 'please input the key word as the "ANY"  (but no quotes please).'
        write(*,*) 'Only English letters are accepeted for it, lower case suggestd.'
        write(*,*) 'NO MORE THAN ', alength, ' IN LENGTH, please.'
        write(*,*) 'Anything other than English letters means to stop this code run.'
        write(*,*) 'Your input as the "ANY" now: '
        read(*,*)  awordp
        aword = adjustl(awordp)
        l = len_trim(aword)
        if (l.le.0) stop
        do m = 1, l
             n = iachar(aword(m:m))
             if ((n.le.64) .or. ((n.ge.91).and.(n.le.96)) .or. (n.ge.123)) stop
        end do
        call filegenerating('coord'//aword(1:l)//'.tex', theoutputtunnel, aword(1:l)//xendkey, &
                                                        &aword(1:l)//yendkey, aword(1:l)//pendkey)
    end do manyotherfiles

    stop

end program

