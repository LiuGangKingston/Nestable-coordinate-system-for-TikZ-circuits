/*
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
*/





#include <iostream>
#include <fstream>
#include <iomanip>
#include <vector>
#include <string>


namespace nestable_coordinate_system
{
    inline static const std::string xendkey      {"xxx"};
    inline static const std::string yendkey      {"yyy"};
    inline static const std::string pendkey      {"ppp"};
    inline static const std::string theinputfile {"in.dat"};
    inline static const std::string macroset     {"\\pgfmathsetmacro{"};
    inline static const std::string macrosets    {"\\pgfmathsetmacro{\\"};
    inline static const std::string spacing      {"spacing"};
    inline static const std::string additional   {" + 0.0 }"};
    inline static const std::string coordinate   {"\\coordinate ("};
    inline static const std::string whitespaces  {" \t"};
    inline static const std::string legalletters {"abcdefghijklmnopqrstuvwxyz"};


    inline void filegenerating(const std::string thefile, const std::string truexxx,
                               const std::string trueyyy, const std::string trueppp,
                               const char firstx,         const int totalx,
                               const char firsty,         const int totaly          ) noexcept
    {
        static std::ofstream nestablefile;
        int firstxi, firstyi, ix, iy;
        char txo, txp, tyo, typ;

        nestablefile.open(thefile);
        nestablefile << "% https://github.com/LiuGangKingston/Nestable-coordinate-system-for-Tikz-circuits.git" << std::endl;
        nestablefile << "% https://github.com/LiuGangKingston/Nestable-coordinate-system-for-Tikz-circuits.git" << std::endl;

        nestablefile << std::endl;
        nestablefile << std::endl;
        nestablefile << "%\\input{coordinates}" << std::endl;
        nestablefile << std::endl;
        nestablefile << std::endl;
        nestablefile << macroset << "\\total" << truexxx << "}{" << totalx << "}" << std::endl;
        nestablefile << macroset << "\\total" << trueyyy << "}{" << totaly << "}" << std::endl;
        nestablefile << macrosets << truexxx << spacing << "}{1}" << std::endl;
        nestablefile << macrosets << trueyyy << spacing << "}{1}" << std::endl;
        nestablefile << macrosets << truexxx << firstx << "}{-8}" << std::endl;
        nestablefile << macrosets << trueyyy << firsty << "}{-8}" << std::endl;

        nestablefile << std::endl;
        txo = firstx;
        for (ix = 1; ix < totalx; ix++)
        {
            txp = firstx + ix;
            nestablefile << macrosets << truexxx << txp << "}{\\" << truexxx << txo
                         << " + \\" << truexxx << spacing << additional << std::endl;
            txo = txp;
        }

        nestablefile << std::endl;
        tyo = firsty;
        for (iy = 1; iy < totaly; iy++)
        {
            typ = firsty + iy;
            nestablefile << macrosets << trueyyy << typ << "}{\\" << trueyyy << tyo
                         << " + \\" << trueyyy << spacing << additional << std::endl;
            tyo = typ;
        }

        nestablefile << std::endl;
        for (ix = 0; ix < totalx; ix++)
        {
            txp = firstx + ix;
            for (iy = 0; iy < totaly; iy++)
            {
                typ = firsty + iy;
                nestablefile << coordinate << trueppp << txp << typ << ") at (\\"
                             << truexxx << txp << ", \\" << trueyyy << typ << ");" << std::endl;
            }
        }

        nestablefile << std::endl;
        txp = firstx + totalx - 1;
        typ = firsty + totaly - 1;
        nestablefile << "%\\gangprintcoordinateat{(0,0)}{The last coordinate values: }{($("
                     << trueppp << txp << typ << ")$)}; " << std::endl;

        nestablefile << std::endl;
        nestablefile.close();

    }


}




void generatingdefaulfiles()
{
    using namespace nestable_coordinate_system;

    std::string aaa;
    int regenerate = 0;

    std::cout << "If you want all the following 27 files: " << std::endl;
    std::cout << "                \"coordinates.tex\" " << std::endl;
    std::cout << "                \"coorda.tex\" " << std::endl;
    std::cout << "                \"coordb.tex\" " << std::endl;
    std::cout << "                 ...          " << std::endl;
    std::cout << "                \"coordz.tex\" " << std::endl;
    std::cout << "   to be (re-)generated, " << std::endl;
    std::cout << "please input 1. Otherwise, any other integer value." << std::endl;
    std::cin  >> regenerate;
    if (regenerate == 1)
    {
        filegenerating("coordinates.tex", xendkey, yendkey, pendkey, 'a', 26, 'a', 26);

        for (int ii = 0; ii < 26; ii++)
        {
            aaa = 'a' + ii;
            filegenerating("coord" + aaa + ".tex", aaa + xendkey, aaa + yendkey, aaa + pendkey, 'a', 26, 'a', 26);
        }

        std::cout << "The above 27 coordinate system files were (re-)generated. "
                  << std::endl << std::endl;
    }
    else
    {
        std::cout << "None of the above 27 coordinate system files were touched. "
                  << std::endl << std::endl;
    }


}




void generatingadditionalfiles()
{
    using namespace nestable_coordinate_system;

    std::size_t left, right;
    std::string aline, thekeyword;
    char firstxpoint, firstypoint;
    int newfiles = 0, linenumber = 0, totalxpoints, totalypoints;

    std::ifstream theinput(theinputfile);
    if (theinput.fail())
    {
        std::cout << " Failed in openning the " << theinputfile << " file, then stopped. " << std::endl;
        exit(1);
    }

    while (true)
    {
        linenumber++;
        getline(theinput, aline);
        left = aline.find_first_not_of(whitespaces);
        if (left == std::string::npos)
        {
            std::cout << " No string at line #:" << linenumber
                      << " of the " << theinputfile << " file as KEYWORD, then stopped. " << std::endl;
            exit(1);
        }
        right = aline.find_last_not_of(whitespaces);
        thekeyword = aline.substr(left, right + 1);
        // middle = thekeyword.find_first_not_of(legalletters); // seeming not working.
        for (const auto& ch : thekeyword)
        {
            if (ch < 'a' || ch > 'z')
            {
                std::cout << "The string in line #: " << linenumber << " of the " << theinputfile << " file "
                          << "would be used as the KEY-WORD in this coordinate system. "
                          << "However only lower-case English letters are allowed. "
                          << "Since other character: " << ch <<" is found, then stopped. "
                          << std::endl;
                exit(1);
            }
        }


        linenumber++;
        getline(theinput, aline);
        left = aline.find_first_not_of(whitespaces);
        if (left == std::string::npos)
        {
            std::cout << " Nothing at line #:" << linenumber
                      << " of the " << theinputfile << " file as the first point in the x-direction, then stopped. " << std::endl;
            exit(1);
        }
        firstxpoint = aline[left];
        if (firstxpoint < 'a' || firstxpoint > 'z')
        {
            std::cout << "The first character in line #: " << linenumber << " of the " << theinputfile << " file "
                      << "would be used as the first point in the x-direction. "
                      << "However only lower-case English letters are allowed. "
                      << "Since it is " << firstxpoint <<", then stopped. "
                      << std::endl;
            exit(1);
        }


        linenumber++;
        totalxpoints = -1;
        // theinput >> totalxpoints;
        getline(theinput, aline);
        totalxpoints = std::stoi(aline);
        if (totalxpoints < 1 || totalxpoints > 'z' - firstxpoint + 1)
        {
            std::cout << "The number in line #: " << linenumber << " of the " << theinputfile << " file "
                      << "would be used as the total number of points in the x-direction. "
                      << "Since it is " << totalxpoints <<", too big/small, then stopped. "
                      << std::endl;
            exit(1);
        }


        linenumber++;
        getline(theinput, aline);
        left = aline.find_first_not_of(whitespaces);
        if (left == std::string::npos)
        {
            std::cout << " Nothing at line #:" << linenumber
                      << " of the " << theinputfile << " file as the first point in the y-direction, then stopped. " << std::endl;
            exit(1);
        }
        firstypoint = aline[left];
        if (firstypoint < 'a' || firstypoint > 'z')
        {
            std::cout << "The first character in line #: " << linenumber << " of the " << theinputfile << " file "
                      << "would be used as the first point in the y-direction. "
                      << "However only lower-case English letters are allowed. "
                      << "Since it is " << firstypoint <<", then stopped. "
                      << std::endl;
            exit(1);
        }


        linenumber++;
        totalypoints = -1;
        // theinput >> totalypoints;
        getline(theinput, aline);
        totalypoints = std::stoi(aline);
        if (totalypoints < 1 || totalypoints > 'z' - firstypoint + 1)
        {
            std::cout << "The number in line #: " << linenumber << " of the " << theinputfile << " file "
                      << "would be used as the total number of points in the y-direction. "
                      << "Since it is " << totalypoints <<", too big/small, then stopped. "
                      << std::endl;
            exit(1);
        }


        newfiles++;
        filegenerating("coord" + thekeyword + ".tex", thekeyword + xendkey, thekeyword + yendkey,
                       thekeyword + pendkey, firstxpoint, totalxpoints, firstypoint, totalypoints);
        std::cout << "New file #: " << newfiles << " was generated as: "
                  << "coord" + thekeyword + ".tex" << " based on: "
                  << firstxpoint << ' ' << totalxpoints << ' ' << firstypoint << ' ' << totalypoints  << '.'
                  << std::endl << std::endl;

    }

    theinput.close();


}




int main(void)
{
    generatingdefaulfiles();

    generatingadditionalfiles();

    return 0;
}


