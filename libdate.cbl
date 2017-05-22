      *-----------------------------------------------------------------
      *--- DESCRIPTION -------------------------------------------------
      *--- Copyright Vin0u@2016 ----------------------------------------
      *--- Library for manage date in COBOL ----------------------------
      *-----------------------------------------------------------------
      *--- Entry list :
      *--- - libdat_addday : allow you to add days to a date, give you
      *    the resulting date in libdat-datrst
      *    - libdat_subday : allow you to subtract days from a date,
      *    give you the resulting date un libdat-datrst
      *    - libdat_nbrday : calculate the number of days beetween two
      *    dates, give the result in libdat-nbrrst
      *    - libdat_dat2str: give you the full date string of the datin1
      *    date in libdat-strrst
      *-----------------------------------------------------------------
       identification division.
       program-id.    libdate.
       author.        Vin0u.
       date-written.  10-06-2016.

      * -----------------------
      * --- Common variable ---
      * -----------------------
       data division.
       working-storage section.
      *
       77  w-nbrday                    pic 9(3).

      * ---------------------------
      * --- Structured variable ---
      * ---------------------------
       01  w-reddat                    pic 9(8).
       01  r-reddat              redefines w-reddat.
           02 r-year                   pic 99.
           02 r-month                  pic 99.
           02 r-day                    pic 99.

      * --- Constants ---
       01  t-maxday.
           02 filler                   pic XX    value 31.
           02 c-fevday                 pic XX.
           02 filler                   pic X(  ) value
             "31303130313031303130".
      *
       01  r-maxday               redefines t-maxday.
           02 c-maxday                 pic 99 occurs 12.
      *
       01  w-mthlng                    pic x(  ).
       01  r-mthlng               redefines w-mthlng.
           02 c-mthlng               occurs 12.
      *
       01  w-mthsho                    pic x(  ).
       01  r-mthsho               redefines w-mthsho.
           02 c-mthsho               occurs 12.
      *
       01  w-daylng                    pic x(240).
       01  r-daylng               redefines w-mthlng.
           02 c-daylng               occurs 12.
      *
       01  w-daysho                    pic x(60).
       01  r-daysho               redefines w-mthsho.
           02 c-daysho               occurs 12.

      * --- constant language copy ---
           copy "c-frelng.cpy".
           copy "c-englng.cpy".

      * -----------------
      * --- Link copy ---
      * -----------------
       linkage section.
      *
           copy libdat-buffer.

      * -----------------------
      * --- Loading library ---
      * -----------------------
       procedure division using libdat-buffer.

      * --- Calcul february day max ---
todo
           exit program.

      * --------------------------
      * --- Add days to a date ---
      * --- Result in datrst   ---
      * --------------------------
       entry lbdate_addday using libdat-buffer.
      
      * --- Entry control ---
           if libdat_datin1 = 0
              move 1                   to libdat-return
              exit program
           end-if.
           move libdat-datin1          to w-ctrdat.
           perform ctr-datein thru end-ctr-datein.
           if w-ctrdat not = 0
              move 1                   to libdat-return
              exit program
           end-if.
           if libdat-nbrday = 0
              move 3                   to libdat-return
              move libdat-datin1       to libdat-datrst
              exit program
           end-if.

      * --- adding days ----
           move libdat-nbrday          to w-nbrday.
           move libdat-datin1          to w-reddat.
       bou-addday.
           add  1                      to r-day.
           if r-day > c-maxday(r-month)
              move  1                  to r-day
              add   1                  to r-month
              if r-month > 12
                 add  1                to r-year
                 move 1                to r-month
              end-if.
           end-if.
           subtract 1                from w-nbrday.
           if w-nbrday > 0             go to bou-addday.
           move w-reddat               to libdat-datrst.
           move  0                     to libdat-return.
           exit program.


      * ---------------------------------
      * --- subtract days from a date ---
      * --- Result in datrst          ---
      * ---------------------------------
       entry lbdate_subday using libdat-buffer.
      
      * --- Entry control ---
           if libdat_datin1 = 0
              move 1                   to libdat-return
              exit program
           end-if.
           move libdat-datin1          to w-ctrdat.
           perform ctr-datein thru end-ctr-datein.
           if w-ctrdat not = 0
              move 1                   to libdat-return
              exit program
           end-if.
           if libdat-nbrday = 0
              move 3                   to libdat-return
              move libdat-datin1       to libdat-datrst
              exit program
           end-if.

      * --- subtracting days ----
           move libdat-nbrday          to w-nbrday.
           move libdat-datin1          to w-reddat.
       bou-subday.
           subtract 1                from r-day.
           if r-day < 1
              subtract 1             from r-month
              if r-month < 1
                 subtract 1          from r-year
                 move    12            to r-month
              end-if
              move  c-maxday(r-month)  to r-day
           end-if.
           subtract 1                from w-nbrday.
           if w-nbrday > 0             go to bou-subday.
           move w-reddat               to libdat-datrst.
           move  0                     to libdat-return.
           exit program.

      * ------------------------------------
      * --- Number days beetween 2 dates ---
      * ------------------------------------
       entry libdat_nbrday using libdat-buffer.
      *
      * --- Entry control ---
           if libdat_datin1 = 0
              move 1                   to libdat-return
              exit program
           end-if.
           move libdat-datin1          to w-ctrdat.
           perform ctr-datein thru end-ctr-datein.
           if w-ctrdat not = 0
              move 1                   to libdat-return
              exit program
           end-if.
           if libdat_datin2 = 0
              move 2                   to libdat-return
              exit program
           end-if.
           move libdat-datin2          to w-ctrdat.
           perform ctr-datein thru end-ctr-datein.
           if w-ctrdat not = 0
              move 2                   to libdat-return
              exit program
           end-if.
           if libdat-datin1 = libdat-datin2
              move libdat-datin1       to lidat-datrst
              move 0                   to libdat-nbrrst
              move 0                   to libdat-return
              exit program
           end-if.

      * --- Sel first date ---
           if libdat-datin1 > libdat-datin2
              move libdat-datin2       to w-reddat
              move libdat-datin1       to libdat-datrst
            else
              move libdat-datin1       to w-reddat
              move libdat-datin2       to libdat-datrst
           end-if.

      * --- adding days ----
           move 0                      to libdat-nbrrst.
       bou-nbrday.
           add  1                      to r-day.
           if r-day > c-maxday(r-month)
              move  1                  to r-day
              add   1                  to r-month
              if r-month > 12
                 add  1                to r-year
                 move 1                to r-month
              end-if.
           end-if.
           ADD  1                      to libdat-nbrrst.
           if w-reddat not = libdat-datrst 
                                       go to bou-nbrday.
           move  0                     to libdat-return.
           exit program.

      * ----------------------
      * --- Date to string ---
      * ----------------------
       entry libdat_dat2str using libdat-buffer.
      *
      * --- Entry control ---
           move  9                     to libdat-return.
           if libdat_datin1 = 0
              move 1                   to libdat-return
              exit program
           end-if.
           move libdat-datin1          to w-ctrdat.
           perform ctr-datein thru end-ctr-datein.
           if w-ctrdat not = 0
              move 1                   to libdat-return
              exit program
           end-if.
           if libdat-codlng not = "FR" and not = "EN"
               move 4                   to libdat-return
               move "FR"                to libdat-codlng
           end-if.
           if libdat-format not = "F" and not = "L"
                        and not = "S" and not = "E"
                        and not = "U"
              move 5                    to libdat-return
              move "F"                  to libdat-format
           end-if.
           initialize                      libdat-strrst.

      * --- Prepare lngcod ---
           evaluate libdat-codlng
            when "FR" move c-frmthl     to w-mthlng
                      move c-frmths     to w-mthsho
                      move c-frdayl     to w-daylng
                      move c-frdays     to w-daysho
            when "EN" move c-enmthl     to w-mthlng
                      move c-enmths     to w-mthsho
                      move c-endayl     to w-daylng
                      move c-endays     to w-daysho
           end-evaluate.
           move libdat-datin1           to w-reddat.

      * --- Prepare result ---
           evaluate libdat-format
            when "F" string r-day       delimited by size
                           "/"          delimited by size
                            r-month     delimited by size
                           "/"          delimited by size
                            r-year      delimited by size
                                      into libdat-strrst
            when "L" perform cal-libday thru end-cal-libday
                     string w-libday    delimited by spaces
                           " "          delimited by size
                            r-day       delimited by size
                           " "          delimited by size
                            c-mthlng(r-month)
                                        delimited by spaces
                           " "          delimited by size
                            r-year      delimited by size
                                      into libdat-strrst
            when "S" perform cal-libday thru end-cal-libday
                     string w-libday    delimited by spaces
                           " "          delimited by size
                            r-day       delimited by size
                           " "          delimited by size
                            c-mthsho(r-month)
                                        delimited by spaces
                           " "          delimited by size
                            r-year      delimited by size
                                      into libdat-strrst
            when "E" string r-year      delimited by size
                           "/"          delimited by size
                            r-month     delimited by size
                           "/"          delimited by size
                            r-day       delimited by size
                                      into libdat-strrst
            when "U" string r-month     delimited by size
                           "/"          delimited by size
                            r-day       delimited by size
                           "/"          delimited by size
                            r-year      delimited by size
                                      into libdat-strrst
           end-evaluate.
           move 0                       to libdat-return.
           exit program.
