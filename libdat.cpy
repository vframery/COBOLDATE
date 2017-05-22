      *-----------------------------------------------------------------
      *--- Copy for libdat library -------------------------------------
      *--- All date format are SSYYMMDD --------------------------------
      *-----------------------------------------------------------------
       01  libdat-buffer.
           02 libdat-datin1            pic 9(8).
           02 libdat-datin2            pic 9(8).
           02 libdat-nbrday            pic 9(3).
      * --- (FR)ench default, (EN)glish
           02 libdat-codlng            pic X(2).
      * --- (F)rench format (DD/MM/AAAA) default
      * --- (L)ong format (ex: Monday 12 january 2016)
      * --- (S)hort format (ex: Mon. 12 jan. 16)
      * --- (E)nglish format (AAAA/MM/DD)
      * --- (U)s format (MM/DD/AAAA)
           02 libdat-format            pic X.
           02 libdat-nbrrst            pic 9(3).
           02 libdat-datrst            pic 9(8).
           02 libdat-strrst            pic X(30).
      * --- 0 - No error encourted
      * --- 1 - datin1 error
      * --- 2 - datin2 error no result
      * --- 3 - nbrday error no result
      * --- 4 - codlng error use default
      * --- 5 - format error use default
      * --- 6 - overflow more than 999 day result
           02 libdat-return            PIC 9.
