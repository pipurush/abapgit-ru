*&---------------------------------------------------------------------*
*& Include          ZJVAN_JV_AUDIT_REPORT_SUB_01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Author          : Ajitesh Kumar
* Creation Date   : 28-02-2019
* Transaction     : N/A
* Technical design: TSD_ETP_I_R02_11.2.6_JV Audit Report_V1.0
* Description     : JV Audit Report
*----------------------------------------------------------------------*
* Modification Information (Most Recent on Top)
*----------------------------------------------------------------------*
* Date            : 28-02-2019
* Author          : Ajitesh Kumar
* Change request  : N/A
* Transport number: S4DK900556
* Description     : Initial Development
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .
  TYPES: BEGIN OF lt_jvdata,
           objectclas TYPE cdhdr-objectclas,
           tcode      TYPE cdhdr-tcode,
           objectid   TYPE cdhdr-objectid,
           changenr   TYPE cdpos-changenr,
           tabname    TYPE cdpos-tabname,
           tabkey     TYPE cdpos-tabkey,
           fname      TYPE cdpos-fname,
           chngind    TYPE cdchngind,
           value_new  TYPE cdpos-value_new,
           value_old  TYPE cdpos-value_old,
           username   TYPE cdhdr-username,
           udate      TYPE cdhdr-udate,
           utime      TYPE cdhdr-utime,
           joa        TYPE t8j9a-joa,
* Begin of change DF867 spalit 09/03/2019
           flddescp   TYPE as4text,
* End of change DF867 spalit 09/03/2019
         END OF lt_jvdata,
         BEGIN OF lt_jadata,
           bukrs      TYPE t8j9a-bukrs,
           joa        TYPE t8j9a-joa,
           egrup      TYPE t8j9a-egrup,
           opshare    TYPE t8j9a-opshare,
           nopshare   TYPE t8j9a-nopshare,
           partn      TYPE jv_partn,
           eqshare    TYPE t8j9c-eqshare,
           sopshare   TYPE string,
           snopshare  TYPE string,
           seqshare   TYPE string,
           ownsusp    TYPE t8j9c-ownsusp,
           ownsusper  TYPE t8j9c-ownsusper,
           ownsusyear TYPE t8j9c-ownsusyear,
           ownunsus   TYPE t8j9c-ownunsus, "changed
           ownususper TYPE t8j9c-ownususper,
           ownususyr  TYPE t8j9c-ownususyr,
           egroupsus  TYPE t8j9a-egroupsus,
           grpsusper  TYPE t8j9a-grpsusper,
           grpsusyear TYPE t8j9a-grpsusyear,
           egroupusus TYPE t8j9a-egroupusus,
           grpususper TYPE t8j9a-grpususper,
           grpususyer TYPE t8j9a-grpususyer,
         END OF lt_jadata.

  DATA: lit_jvdata TYPE TABLE OF lt_jvdata,
        lt_dfies   TYPE STANDARD TABLE OF dfies,
        lwa_dfies  TYPE dfies,
        lit_jadata TYPE TABLE OF lt_jadata,
        lwa_jvdata TYPE lt_jvdata,
        lwa_jadata TYPE lt_jadata,
        wa_fjadata TYPE t_jadata.

  DATA: l_tabkey     TYPE cdpos-tabkey,
        l_len        TYPE i,
        l_strlength  TYPE i,
        l_objid      TYPE string,
        l_key        TYPE string,
        l_new        TYPE string,
        l_share      TYPE i,
        l_tshare(16) TYPE c.
  RANGES: r_objectid  FOR cdhdr-objectid.
  CONSTANTS: lc_11       TYPE i VALUE 11,
             lc_13       TYPE i VALUE 13,
             lc_15       TYPE i VALUE 15,
             lc_99999999 TYPE i VALUE 99999999.
  DATA: lv_date         TYPE sydatum,
        lv_datechar(10) TYPE c,
        lv_fdate        TYPE fdate,
* Begin of change 9454163 ssen 30/04/2020
        lv_busp         TYPE BU_PARTNER.
* End of change 9454163 ssen 30/04/2020

  IF p_audit = abap_true.

* Begin of change DF867 spalit 08/29/2019
    IF s_joa[] IS NOT INITIAL.
      SELECT bukrs,joa
             FROM t8ju
             INTO TABLE @DATA(lt_t8ju)
             WHERE bukrs IN @s_bukrs
             AND   joa IN @s_joa .
      IF sy-subrc  = 0 .
        SORT lt_t8ju BY joa.
      ELSE.
        MESSAGE TEXT-c44 TYPE 'I'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
* End of change DF867 spalit 08/29/2019

    LOOP AT s_bukrs .
      r_objectid-sign = 'I'.
      r_objectid-option = 'CP'.
* Begin of change DF867 spalit 08/29/2019
      IF lt_t8ju[] IS NOT INITIAL.
        LOOP AT lt_t8ju INTO DATA(ls_t8ju) WHERE bukrs = s_bukrs-low.
          r_objectid-low = 'E' && s_bukrs-low && ls_t8ju-joa && '*'.
          APPEND r_objectid.
        ENDLOOP.
      ELSE.
* End of change DF867 spalit 08/29/2019
        r_objectid-low = 'E' && s_bukrs-low && '*'.
        APPEND r_objectid.
* Begin of change DF867 spalit 08/29/2019
      ENDIF.
* End of change DF867 spalit 08/29/2019
    ENDLOOP.
* Begin of change DF867 spalit 08/29/2019
* If only JOA is given
    IF lt_t8ju[] IS NOT INITIAL AND r_objectid[] IS INITIAL.
      DELETE ADJACENT DUPLICATES FROM lt_t8ju COMPARING joa.
      LOOP AT lt_t8ju INTO DATA(ls_t8juobj) .
        r_objectid-sign = 'I'.
        r_objectid-option = 'CP'.
        r_objectid-low = 'E' && '*' && ls_t8juobj-joa && '*'.
        APPEND r_objectid.
      ENDLOOP.
    ENDIF.
* End of change DF867 spalit 08/29/2019

* Begin of change 9454163 ssen 30/04/2020
  IF p_mhits IS NOT INITIAL.

    SELECT cdhdr~objectclas
           cdhdr~tcode
           cdhdr~objectid
           cdpos~changenr
           cdpos~tabname
           cdpos~tabkey
           cdpos~fname
           cdpos~chngind
           cdpos~value_new
           cdpos~value_old
           cdhdr~username
           cdhdr~udate
           cdhdr~utime
      UP TO p_mhits ROWS
      INTO TABLE lit_jvdata
      FROM cdhdr
      JOIN cdpos
      ON ( cdhdr~changenr = cdpos~changenr )
      WHERE cdhdr~tcode IN ('GJA1','GJV1','GJA2','GJV2') AND
            cdhdr~objectclas IN ('JV_JOA','JV_MASTER') AND
            cdhdr~objectid IN r_objectid AND
            cdhdr~username IN s_uname AND
            cdhdr~udate IN s_dat.
  ELSE.
    SELECT cdhdr~objectclas
           cdhdr~tcode
           cdhdr~objectid
           cdpos~changenr
           cdpos~tabname
           cdpos~tabkey
           cdpos~fname
           cdpos~chngind
           cdpos~value_new
           cdpos~value_old
           cdhdr~username
           cdhdr~udate
           cdhdr~utime
      INTO TABLE lit_jvdata
      FROM cdhdr
      JOIN cdpos
      ON ( cdhdr~changenr = cdpos~changenr )
      WHERE cdhdr~tcode IN ('GJA1','GJV1','GJA2','GJV2') AND
            cdhdr~objectclas IN ('JV_JOA','JV_MASTER') AND
            cdhdr~objectid IN r_objectid AND
            cdhdr~username IN s_uname AND
            cdhdr~udate IN s_dat.

  ENDIF.

    IF sy-subrc  NE 0 .
      MESSAGE TEXT-102 TYPE 'I'.
      LEAVE LIST-PROCESSING .
    ENDIF.
    SORT lit_jvdata BY objectid changenr.

    LOOP AT lit_jvdata INTO lwa_jvdata.

      l_objid = lwa_jvdata-objectid.
      l_strlength = strlen( l_objid ) - 6.
      IF l_strlength GE 0.
        lwa_jvdata-joa = l_objid+l_strlength(6).
      ENDIF.

* Begin of change DF867 spalit 09/03/2019
      IF lwa_jvdata-tabname IS NOT INITIAL AND lwa_jvdata-fname NE 'KEY'.
        CALL FUNCTION 'DDIF_FIELDINFO_GET'
          EXPORTING
            tabname        = lwa_jvdata-tabname
            fieldname      = lwa_jvdata-fname
*           LANGU          = SY-LANGU
*           LFIELDNAME     = ' '
*           ALL_TYPES      = ' '
*           GROUP_NAMES    = ' '
*           UCLEN          =
*           DO_NOT_WRITE   = ' '
*         IMPORTING
*           X030L_WA       =
*           DDOBJTYPE      =
*           DFIES_WA       =
*           LINES_DESCR    =
          TABLES
            dfies_tab      = lt_dfies
*           FIXED_VALUES   =
          EXCEPTIONS
            not_found      = 1
            internal_error = 2
            OTHERS         = 3.
        IF sy-subrc = 0.
          READ TABLE lt_dfies INTO lwa_dfies INDEX 1.
          lwa_jvdata-flddescp = lwa_dfies-fieldtext.
        ENDIF.
        CLEAR:lt_dfies,lwa_dfies.
      ENDIF.
* End of change DF867 spalit 09/03/2019

      IF lwa_jvdata-tabname = 'T8J9A' AND lwa_jvdata-fname = 'KEY'.
        lwa_jvdata-fname = TEXT-083.
        l_tabkey = lwa_jvdata-tabkey.
        l_strlength = strlen( l_tabkey ).
        l_len = l_strlength - 10.
        IF l_len GE 0.
          lwa_jvdata-value_new = l_tabkey+10(l_len).
        ENDIF.
        CLEAR l_tabkey.
      ELSEIF lwa_jvdata-tabname = 'T8J9A' AND lwa_jvdata-fname = 'EGROUPSUS'.
        lwa_jvdata-fname = TEXT-097.
        l_tabkey = lwa_jvdata-tabkey.
        l_strlength = strlen( l_tabkey ) - 3.
        IF l_strlength GE 0.
          l_key = l_tabkey+l_strlength(3).
          IF lwa_jvdata-value_new IS NOT INITIAL.
            l_new = lwa_jvdata-value_new.
            CONCATENATE l_key '-' l_new INTO lwa_jvdata-value_new.
          ELSEIF lwa_jvdata-value_old IS NOT INITIAL.
            l_new = lwa_jvdata-value_old.
            CONCATENATE l_key '-' l_new INTO lwa_jvdata-value_old.
          ENDIF.
        ENDIF.
        CLEAR l_tabkey.
      ELSEIF lwa_jvdata-tabname = 'T8J9C' AND lwa_jvdata-fname = 'OWNSUSP'.
        lwa_jvdata-fname = TEXT-c01.
      ELSEIF lwa_jvdata-tabname = 'T8J9C' AND lwa_jvdata-fname = 'OWNSUSYEAR'.
        lwa_jvdata-fname = TEXT-c02.
      ELSEIF lwa_jvdata-tabname = 'T8J9C' AND lwa_jvdata-fname = 'KEY'.
        lwa_jvdata-fname = TEXT-c03.
        l_tabkey = lwa_jvdata-tabkey.
        l_strlength = strlen( l_tabkey ).
        l_len = l_strlength - lc_13.
        IF l_len GE 0.
          lwa_jvdata-value_new = l_tabkey+13(l_len).
        ENDIF.
        CLEAR l_tabkey.
      ELSEIF lwa_jvdata-tabname = 'T8JUT' AND lwa_jvdata-fname = 'KEY'.
        lwa_jvdata-fname = TEXT-c04.
        lwa_jvdata-value_new = lwa_jvdata-tabkey(3).
        CLEAR l_tabkey.
      ELSEIF lwa_jvdata-tabname = 'T8JUT' AND lwa_jvdata-fname = 'JTEXT'.
        lwa_jvdata-fname = TEXT-c05.
      ELSEIF lwa_jvdata-tabname = 'T8J9A' AND lwa_jvdata-fname = 'GRPUSUSPER'.
        lwa_jvdata-fname = TEXT-c06.
      ELSEIF lwa_jvdata-tabname = 'T8J9A' AND lwa_jvdata-fname = 'GRPUSUSYER'.
        lwa_jvdata-fname = TEXT-c07.
      ELSEIF lwa_jvdata-tabname = 'T8J9A' AND lwa_jvdata-fname = 'GRPSUSPER'.
        lwa_jvdata-fname = TEXT-c08.
      ELSEIF lwa_jvdata-tabname = 'T8J9A' AND lwa_jvdata-fname = 'GRPSUSYER'.
        lwa_jvdata-fname = TEXT-c09.
      ELSEIF lwa_jvdata-tabname = 'T8J9A' AND lwa_jvdata-fname = 'OPSHARE'.
        lwa_jvdata-fname = TEXT-c37.
      ELSEIF lwa_jvdata-tabname = 'T8J9A' AND lwa_jvdata-fname = 'NOPSHARE'.
        lwa_jvdata-fname = TEXT-c38.
      ELSEIF lwa_jvdata-tabname = 'T8JC2' AND lwa_jvdata-fname = 'KEY'.
        lwa_jvdata-fname = TEXT-c39.
        l_tabkey = lwa_jvdata-tabkey.
        l_strlength = strlen( l_tabkey ).
        l_len = l_strlength - lc_15.
        IF l_len GE 0.
          lwa_jvdata-value_new = l_tabkey+15(l_len).
        ENDIF.
        CLEAR l_tabkey.
      ELSEIF lwa_jvdata-tabname = 'T8JQ' AND lwa_jvdata-fname = 'KEY'.
        lwa_jvdata-fname = TEXT-c40.
        l_tabkey = lwa_jvdata-tabkey.
        l_strlength = strlen( l_tabkey ).
        l_len = l_strlength - lc_15.
        IF l_len GE 0.
          lwa_jvdata-value_new = l_tabkey+15(l_len).
        ENDIF.
        CLEAR l_tabkey.
      ELSEIF lwa_jvdata-tabname = 'T8JQ' AND lwa_jvdata-fname = 'EQSHARE'.
        lwa_jvdata-fname = TEXT-c41.
        l_tabkey = lwa_jvdata-tabkey.
        l_strlength = strlen( l_tabkey ).
        l_len = l_strlength - 16.
        IF l_len GE 0.
          l_key = l_tabkey+16(l_len).
          IF lwa_jvdata-value_new IS NOT INITIAL.
            l_new = lwa_jvdata-value_new.
            CONCATENATE l_key '-' l_new '%' INTO lwa_jvdata-value_new.
          ELSEIF lwa_jvdata-value_old IS NOT INITIAL.
            l_new = lwa_jvdata-value_old.
            CONCATENATE l_key '-' l_new '%' INTO lwa_jvdata-value_old.
          ENDIF.
        ENDIF.
        CLEAR l_tabkey.
      ELSEIF lwa_jvdata-tabname = 'T8JQ' AND lwa_jvdata-fname = 'OWNSUSPER'.
        lwa_jvdata-fname = TEXT-c08.
      ELSEIF lwa_jvdata-tabname = 'T8JQ' AND lwa_jvdata-fname = 'OWNSUSYEAR'.
        lwa_jvdata-fname = TEXT-c09.
      ELSEIF lwa_jvdata-tabname = 'T8JQ' AND lwa_jvdata-fname = 'OWNUSUSPER'.
        lwa_jvdata-fname = TEXT-c10.
      ELSEIF lwa_jvdata-tabname = 'T8JQ' AND lwa_jvdata-fname = 'OWNUSUYR'.
        lwa_jvdata-fname = TEXT-c11.
      ELSEIF lwa_jvdata-tabname = 'T8JQ' AND lwa_jvdata-fname = 'INTTYPE'.
        lwa_jvdata-fname = TEXT-c12.
      ELSEIF lwa_jvdata-tabname = 'T8JF' AND lwa_jvdata-fname = 'KEY'.
        lwa_jvdata-fname = TEXT-c13.
        l_tabkey = lwa_jvdata-tabkey.
        l_strlength = strlen( l_tabkey ) - 3.
        IF l_strlength GE 0.
          l_key = l_tabkey+l_strlength(3).
          IF lwa_jvdata-value_new IS NOT INITIAL.
            l_new = lwa_jvdata-value_new.
            CONCATENATE l_key '-' l_new INTO lwa_jvdata-value_new.
          ELSEIF lwa_jvdata-value_old IS NOT INITIAL.
            l_new = lwa_jvdata-value_old.
            CONCATENATE l_key '-' l_new INTO lwa_jvdata-value_old.
          ENDIF.
        ENDIF.
        CLEAR l_tabkey.
      ELSEIF lwa_jvdata-tabname = 'T8JF' AND lwa_jvdata-fname = 'GRPUSUSPER'.
        lwa_jvdata-fname = TEXT-c08.
      ELSEIF lwa_jvdata-tabname = 'T8JF' AND lwa_jvdata-fname = 'GRPUSUSYER'.
        lwa_jvdata-fname = TEXT-c09.
      ELSEIF lwa_jvdata-tabname = 'T8J9A' AND lwa_jvdata-fname = 'GRPUSUSYER'.
        lwa_jvdata-fname = TEXT-c09.
      ELSEIF lwa_jvdata-tabname = 'T8J9B' AND lwa_jvdata-fname = 'EGTEXT'.
        lwa_jvdata-fname = TEXT-c15.
        l_tabkey = lwa_jvdata-tabkey.
        l_strlength = strlen( l_tabkey ) - 3.
        IF l_strlength GE 0.
          l_key = l_tabkey+l_strlength(3).
          IF lwa_jvdata-value_new IS NOT INITIAL.
            l_new = lwa_jvdata-value_new.
            CONCATENATE l_key '-' l_new INTO lwa_jvdata-value_new.
          ELSEIF lwa_jvdata-value_old IS NOT INITIAL.
            l_new = lwa_jvdata-value_old.
            CONCATENATE l_key '-' l_new INTO lwa_jvdata-value_old.
          ENDIF.
        ENDIF.
        CLEAR l_tabkey.
      ELSEIF lwa_jvdata-tabname = 'T8J9B' AND lwa_jvdata-fname = 'KEY'.
        lwa_jvdata-fname = TEXT-c13.
      ELSEIF lwa_jvdata-tabname = 'T8JF' AND lwa_jvdata-fname = 'EGROUPSUS'.
        lwa_jvdata-fname = TEXT-c14.
        l_tabkey = lwa_jvdata-tabkey.
        l_strlength = strlen( l_tabkey ) - 3.
        IF l_strlength GE 0.
          lwa_jvdata-value_new = l_tabkey+l_strlength(3).
        ENDIF.
        CLEAR l_tabkey.
      ELSEIF lwa_jvdata-tabname = 'T8JFT' AND lwa_jvdata-fname = 'EGTEXT'.
        lwa_jvdata-fname = TEXT-c15.
        l_tabkey = lwa_jvdata-tabkey.
        l_strlength = strlen( l_tabkey ) - 3.
        IF l_strlength GE 0.
          l_key = l_tabkey+l_strlength(3).
          IF lwa_jvdata-value_new IS NOT INITIAL.
            l_new = lwa_jvdata-value_new.
            CONCATENATE l_key '-' l_new INTO lwa_jvdata-value_new.
          ELSEIF lwa_jvdata-value_old IS NOT INITIAL.
            l_new = lwa_jvdata-value_old.
            CONCATENATE l_key '-' l_new INTO lwa_jvdata-value_old.
          ENDIF.
        ENDIF.
        CLEAR l_tabkey.
      ELSEIF lwa_jvdata-tabname = 'T8JG' AND lwa_jvdata-fname = 'KEY'.
        lwa_jvdata-fname = TEXT-c17.
        DATA: l_num TYPE i.
        l_tabkey = lwa_jvdata-tabkey.
        l_strlength = strlen( l_tabkey ).
        l_len = l_strlength - 16.
*        l_strlength = strlen( l_tabkey ) - l_len.
        IF l_len GE 0.
          l_new = l_tabkey+16(l_len).
          CLEAR l_len.
          l_len = strlen( l_new ).
          IF l_len EQ 8.
            MOVE l_new TO l_num.
            l_num = lc_99999999 - l_num.
            MOVE l_num TO l_new.
            CONCATENATE l_new+0(4) '/' l_new+4(2) '/' l_new+6(2) INTO lwa_jvdata-value_new.
          ENDIF.
        ENDIF.
        CLEAR l_tabkey.
        CLEAR l_num.
      ELSEIF lwa_jvdata-tabname = 'T8JVT' AND lwa_jvdata-fname = 'VTEXT'.
        lwa_jvdata-fname = TEXT-c18.
      ELSEIF lwa_jvdata-tabname = 'T8JV' AND lwa_jvdata-fname = 'JOA'.
* Begin of change DF867   spalit
        lwa_jvdata-fname = TEXT-c42.
      ELSEIF lwa_jvdata-tabname = 'T8JV' AND lwa_jvdata-fname = 'BMETHOD'.
        lwa_jvdata-fname = TEXT-c43.
* End  of change DF867   spalit
      ELSEIF lwa_jvdata-tabname = 'T8JV' AND lwa_jvdata-fname = 'KEY'.
        lwa_jvdata-fname = TEXT-c19.
      ELSEIF lwa_jvdata-tabname = 'T8JV' AND lwa_jvdata-fname = 'OUTPAYTERM'.
        lwa_jvdata-fname = TEXT-c20.
      ELSEIF lwa_jvdata-tabname = 'T8JG' AND lwa_jvdata-fname = 'EGRUP'.
        lwa_jvdata-fname = TEXT-c21.
        DATA: l_str TYPE string.
        l_tabkey = lwa_jvdata-tabkey.
        l_strlength = strlen( l_tabkey ).
        l_len = l_strlength - lc_13.
*        l_strlength = strlen( l_tabkey ) - l_len.
        IF l_len GE 0.
          l_new = l_tabkey+13(l_len).
          CLEAR l_len.
          l_len = strlen( l_new ).
          IF l_len EQ lc_11.
            MOVE l_new+3(8) TO l_num.
            l_num = lc_99999999 - l_num.
            MOVE l_num TO l_str.
            CONCATENATE l_new+0(3) '-' l_str+0(4) '/' l_new+5(2) '/' l_new+7(2) INTO lwa_jvdata-value_new.
          ENDIF.
        ENDIF.
        CLEAR l_tabkey.
        CLEAR l_num.
        CLEAR l_str.
      ELSEIF lwa_jvdata-tabname = 'T8J8H' AND lwa_jvdata-fname = 'FRDEPTH'.
        lwa_jvdata-fname = TEXT-c22.
      ELSEIF lwa_jvdata-tabname = 'T8J8H' AND lwa_jvdata-fname = 'TODEPTH'.
        lwa_jvdata-fname = TEXT-c23.
      ELSEIF lwa_jvdata-tabname = 'T8J8I' AND lwa_jvdata-fname = 'KEY'.
        lwa_jvdata-fname = TEXT-c24.
        l_tabkey = lwa_jvdata-tabkey.
        l_strlength = strlen( l_tabkey ).
        l_len = l_strlength - 16.
        IF l_len GE 0.
          lwa_jvdata-value_new = l_tabkey+16(l_len).
        ENDIF.
        CLEAR l_tabkey.
      ELSEIF lwa_jvdata-tabname = 'T8J8H' AND lwa_jvdata-fname = 'KEY'.
        lwa_jvdata-fname = TEXT-c25.
        l_tabkey = lwa_jvdata-tabkey.
        l_strlength = strlen( l_tabkey ).
        l_len = l_strlength - 10.
        IF l_len GE 0.
          lwa_jvdata-value_new = l_tabkey+10(l_len).
        ENDIF.
        CLEAR l_tabkey.
      ELSEIF lwa_jvdata-tabname = 'T8J8F' AND lwa_jvdata-fname = 'KEY'.
        lwa_jvdata-fname = TEXT-c26.
        l_tabkey = lwa_jvdata-tabkey.
        l_strlength = strlen( l_tabkey ).
        l_len = l_strlength - 10.
        IF l_len GE 0.
          lwa_jvdata-value_new = l_tabkey+10(l_len).
        ENDIF.
        CLEAR l_tabkey.
      ELSEIF lwa_jvdata-tabname = 'T8J8F' AND lwa_jvdata-fname = 'MONTHLY'.
        lwa_jvdata-fname = TEXT-c27.
      ELSEIF lwa_jvdata-tabname = 'T8J8F' AND lwa_jvdata-fname = 'RATE'.
        lwa_jvdata-fname = TEXT-c28.
      ELSEIF lwa_jvdata-tabname = 'T8J8F' AND lwa_jvdata-fname = 'TODEPTH'.
        lwa_jvdata-fname = TEXT-c29.
      ELSEIF lwa_jvdata-tabname = 'T8J8F' AND lwa_jvdata-fname = 'FRDEPTH'.
        lwa_jvdata-fname = TEXT-c30.
      ELSEIF lwa_jvdata-tabname = 'T8JU' AND lwa_jvdata-fname = 'PER1'.
        lwa_jvdata-fname = TEXT-c31.
      ELSEIF lwa_jvdata-tabname = 'T8JU' AND lwa_jvdata-fname = 'PER2'.
        lwa_jvdata-fname = TEXT-c32.
      ELSEIF lwa_jvdata-tabname = 'T8JU' AND lwa_jvdata-fname = 'ESCAL'.
        lwa_jvdata-fname = TEXT-c33.
      ELSEIF lwa_jvdata-tabname = 'T8JU' AND lwa_jvdata-fname = 'CONTRACT'.
        lwa_jvdata-fname = TEXT-f30.
      ELSEIF lwa_jvdata-tabname = 'T8JU' AND lwa_jvdata-fname = 'CONRULE'.
        lwa_jvdata-fname = TEXT-c34.
      ELSEIF lwa_jvdata-tabname = 'T8JU' AND lwa_jvdata-fname = 'KEY'.
        lwa_jvdata-fname = TEXT-c35.
      ELSEIF lwa_jvdata-tabname = 'T8JV' AND lwa_jvdata-fname = 'VACTIVE'.
        lwa_jvdata-fname = TEXT-c36.
      ENDIF.

      MODIFY lit_jvdata FROM lwa_jvdata.
      IF lwa_jvdata-tabname = 'T8J9A'.
        wa_opshare-bukrs  = lwa_jvdata-tabkey+3(4).
        wa_opshare-joa  = lwa_jvdata-tabkey+7(6).
        wa_opshare-egrup = lwa_jvdata-tabkey+13(3).
        APPEND wa_opshare TO it_opshare.
        CLEAR wa_opshare.
      ELSEIF lwa_jvdata-tabname = 'T8J9C'.
        wa_opshare-bukrs  = lwa_jvdata-tabkey+3(4).
        wa_opshare-joa  = lwa_jvdata-tabkey+7(6).
        wa_opshare-egrup = lwa_jvdata-tabkey+13(3).
        wa_opshare-partn = lwa_jvdata-tabkey+16(10).
        APPEND wa_opshare TO it_opshare.
        CLEAR wa_opshare.
      ENDIF.

    ENDLOOP.

    DELETE lit_jvdata WHERE joa NOT IN s_joa.

    IF it_opshare[] IS NOT INITIAL.
      SELECT t8j9c~bukrs,
          t8j9c~joa,
          t8j9c~egrup,
          t8j9a~opshare,
          t8j9a~nopshare,
          t8j9c~partn,
          t8j9c~eqshare,
          t8j9a~egroupsus,
          t8j9a~grpsusper,
          t8j9a~grpsusyear,
          t8j9a~egroupusus,
          t8j9a~grpususper,
          t8j9a~grpususyer,
          t8j9c~ownsusp,
          t8j9c~ownsusper,
          t8j9c~ownsusyear,
          t8j9c~ownunsus,
          t8j9c~ownususper,
          t8j9c~ownususyr
        INTO CORRESPONDING FIELDS OF TABLE @lit_jadata
        FROM t8j9c
        LEFT OUTER JOIN t8j9a
        ON ( t8j9a~joa = t8j9c~joa AND t8j9a~bukrs = t8j9c~bukrs AND t8j9a~egrup = t8j9c~egrup )
        FOR ALL ENTRIES IN @it_opshare
        WHERE t8j9a~bukrs = @it_opshare-bukrs
        AND   t8j9a~egrup = @it_opshare-egrup
        AND   t8j9a~joa = @it_opshare-joa
          AND t8j9c~partn = @it_opshare-partn.
    ENDIF.

    LOOP AT lit_jvdata INTO lwa_jvdata.
      IF lwa_jvdata-tabname = 'T8JFT'.
        CLEAR lwa_jvdata.
        CONTINUE.
      ENDIF.

*      IF lwa_jvdata-tcode = 'GJA1'.
*        IF lwa_jvdata-objectclas = 'JV_JOA'.
      CLEAR l_key.
      CLEAR l_strlength.
      CLEAR l_tabkey.
*      SELECT t8j9c~bukrs,
*          t8j9c~joa,
*          t8j9c~egrup,
*          t8j9a~opshare,
*          t8j9a~nopshare,
*          t8j9c~partn,
*          t8j9c~eqshare,
*          t8j9a~egroupsus,
*          t8j9a~grpsusper,
*          t8j9a~grpsusyear,
*          t8j9a~egroupusus,
*          t8j9a~grpususper,
*          t8j9a~grpususyer,
*          t8j9c~ownsusp,
*          t8j9c~ownsusper,
*          t8j9c~ownsusyear,
*          t8j9c~ownunsus,
*          t8j9c~ownususper,
*          t8j9c~ownususyr
*        INTO CORRESPONDING FIELDS OF TABLE @lit_jadata
*        FROM t8j9c
*        LEFT OUTER JOIN t8j9a
*        ON ( t8j9a~joa = t8j9c~joa AND t8j9a~bukrs = t8j9c~bukrs AND t8j9a~egrup = t8j9c~egrup )
*        WHERE t8j9a~joa = @lwa_jvdata-joa
*          AND t8j9c~partn = @lwa_jvdata-value_new+3(10).

*          LOOP AT lit_jadata INTO lwa_jadata.
*            IF lwa_jadata-opshare NE 0.
*              l_share = lwa_jadata-opshare.
*              l_tshare = l_share.
*              CONCATENATE l_tshare '%' INTO lwa_jadata-sopshare.
*              CLEAR l_share.
*              CLEAR l_tshare.
*            ELSE.
*              lwa_jadata-sopshare = lwa_jadata-opshare.
*              CLEAR lwa_jadata-sopshare.
*            ENDIF.
*            IF lwa_jadata-nopshare NE 0.
*              l_share = lwa_jadata-nopshare.
*              l_tshare = l_share.
*              CONCATENATE l_tshare '%' INTO lwa_jadata-snopshare.
*              CLEAR l_share.
*              CLEAR l_tshare.
*            ELSE.
*              lwa_jadata-snopshare = lwa_jadata-nopshare.
*              CLEAR lwa_jadata-snopshare.
*            ENDIF.
*            IF lwa_jadata-eqshare NE 0.
*              l_share = lwa_jadata-eqshare.
*              l_tshare = l_share.
*              CONCATENATE l_tshare '%' INTO lwa_jadata-seqshare.
*              CLEAR l_share.
*              CLEAR l_tshare.
*            ELSE.
*              lwa_jadata-seqshare = lwa_jadata-eqshare.
*              CLEAR lwa_jadata-seqshare.
*            ENDIF.
*            MODIFY lit_jadata FROM lwa_jadata.
*            CLEAR lwa_jadata.
*          ENDLOOP.
*          ENDIF.
*        ENDIF.
*      ENDIF.
      IF lwa_jvdata-chngind = 'I'.
        lwa_jvdata-tcode = TEXT-074.
      ELSEIF lwa_jvdata-chngind = 'U'.
        lwa_jvdata-tcode = TEXT-076.
      ELSEIF lwa_jvdata-chngind = 'D' OR lwa_jvdata-chngind = 'E'.
        lwa_jvdata-tcode = TEXT-079.
      ENDIF.

      MODIFY lit_jvdata FROM lwa_jvdata.

      wa_fjadata-tcode = lwa_jvdata-tcode.
      wa_fjadata-joa = lwa_jvdata-joa.
      wa_fjadata-tabname = lwa_jvdata-tabname.
***********************************************************

      IF lwa_jvdata-tabname = 'T8JU'.
* Begin of change 9454163 ssen 30/04/2020
*        CONCATENATE lwa_jvdata-tabkey+3(4) lwa_jvdata-tabkey+7(6) INTO wa_fjadata-tabkey SEPARATED BY ' '.
        MOVE lwa_jvdata-tabkey+3(4) TO wa_fjadata-tabkey1.
        MOVE lwa_jvdata-tabkey+7(6) TO wa_fjadata-tabkey2.
      ELSEIF lwa_jvdata-tabname = 'T8JUT'.
*        CONCATENATE  lwa_jvdata-tabkey+4(4) lwa_jvdata-tabkey+8(6) INTO wa_fjadata-tabkey SEPARATED BY ' '  .
        MOVE lwa_jvdata-tabkey+4(4) TO wa_fjadata-tabkey1.
        MOVE lwa_jvdata-tabkey+8(6) TO wa_fjadata-tabkey2.
      ELSEIF lwa_jvdata-tabname =  'T8J8F'.
*        CONCATENATE lwa_jvdata-tabkey+3(4) lwa_jvdata-tabkey+7(6) lwa_jvdata-tabkey+13(6) INTO wa_fjadata-tabkey SEPARATED BY ' ' .
        MOVE lwa_jvdata-tabkey+3(4)  TO wa_fjadata-tabkey1.
        MOVE lwa_jvdata-tabkey+7(6)  TO wa_fjadata-tabkey2.
        MOVE lwa_jvdata-tabkey+13(6) TO wa_fjadata-tabkey3.
      ELSEIF lwa_jvdata-tabname = 'T8J8H'.
        MOVE lwa_jvdata-tabkey+3(4)  TO wa_fjadata-tabkey1.
        MOVE lwa_jvdata-tabkey+7(6)  TO wa_fjadata-tabkey2.
        MOVE lwa_jvdata-tabkey+13(6) TO wa_fjadata-tabkey3.
      ELSEIF lwa_jvdata-tabname = 'T8J8I'.
*        CONCATENATE lwa_jvdata-tabkey+3(4) lwa_jvdata-tabkey+7(6) lwa_jvdata-tabkey+13(8) INTO wa_fjadata-tabkey SEPARATED BY ' '.
        MOVE lwa_jvdata-tabkey+3(4)  TO wa_fjadata-tabkey1.
        MOVE lwa_jvdata-tabkey+7(6)  TO wa_fjadata-tabkey2.
        MOVE lwa_jvdata-tabkey+13(8) TO wa_fjadata-tabkey3.
      ELSEIF lwa_jvdata-tabname = 'T8J9A'.
*        CONCATENATE lwa_jvdata-tabkey+3(4) lwa_jvdata-tabkey+7(6) lwa_jvdata-tabkey+13(3) INTO wa_fjadata-tabkey SEPARATED BY ' '.
        MOVE lwa_jvdata-tabkey+3(4)  TO wa_fjadata-tabkey1.
        MOVE lwa_jvdata-tabkey+7(6)  TO wa_fjadata-tabkey2.
        MOVE lwa_jvdata-tabkey+13(3) TO wa_fjadata-tabkey3.
      ELSEIF lwa_jvdata-tabname =  'T8J9B'.
        MOVE lwa_jvdata-tabkey+4(4)  TO wa_fjadata-tabkey1.
        MOVE lwa_jvdata-tabkey+8(6)  TO wa_fjadata-tabkey2.
        MOVE lwa_jvdata-tabkey+14(3) TO wa_fjadata-tabkey3.
      ELSEIF lwa_jvdata-tabname =  'T8J9C'.
        IF lwa_jvdata-chngind = 'E' .
*          CONCATENATE lwa_jvdata-tabkey+3(4)  lwa_jvdata-tabkey+7(6)  lwa_jvdata-tabkey+13(3) lwa_jvdata-tabkey+16(10) INTO wa_fjadata-tabkey SEPARATED BY ' '.
          MOVE lwa_jvdata-tabkey+3(4)   TO wa_fjadata-tabkey1.
          MOVE lwa_jvdata-tabkey+7(6)   TO wa_fjadata-tabkey2.
          MOVE lwa_jvdata-tabkey+13(3)  TO wa_fjadata-tabkey3.
          MOVE lwa_jvdata-tabkey+16(10) TO wa_fjadata-tabkey4.
        ELSE.
*          CONCATENATE lwa_jvdata-tabkey+3(4)  lwa_jvdata-tabkey+7(6)  lwa_jvdata-tabkey+13(3) lwa_jvdata-tabkey+16(10) INTO wa_fjadata-tabkey SEPARATED BY ' '.
          MOVE lwa_jvdata-tabkey+3(4)   TO wa_fjadata-tabkey1.
          MOVE lwa_jvdata-tabkey+7(6)   TO wa_fjadata-tabkey2.
          MOVE lwa_jvdata-tabkey+13(3)  TO wa_fjadata-tabkey3.
          MOVE lwa_jvdata-tabkey+16(10) TO wa_fjadata-tabkey4.
        ENDIF.
      ELSEIF lwa_jvdata-tabname =  'T8JV'.
*        CONCATENATE lwa_jvdata-tabkey+3(4)  lwa_jvdata-tabkey+7(6) INTO wa_fjadata-tabkey SEPARATED BY ' ' .
         MOVE lwa_jvdata-tabkey+3(4) TO wa_fjadata-tabkey1.
         MOVE lwa_jvdata-tabkey+7(6) TO wa_fjadata-tabkey2.
      ELSEIF lwa_jvdata-tabname = 'T8JVT'.
*        CONCATENATE lwa_jvdata-tabkey+4(4)  lwa_jvdata-tabkey+8(6) INTO wa_fjadata-tabkey SEPARATED BY ' ' .
        MOVE lwa_jvdata-tabkey+4(4) TO wa_fjadata-tabkey1.
        MOVE lwa_jvdata-tabkey+8(6) TO wa_fjadata-tabkey2.
      ELSEIF lwa_jvdata-tabname = 'T8JF'.
*        CONCATENATE lwa_jvdata-tabkey+3(4)  lwa_jvdata-tabkey+7(6)  lwa_jvdata-tabkey+13(3) INTO  wa_fjadata-tabkey SEPARATED BY ' ' .
        MOVE lwa_jvdata-tabkey+3(4)  TO wa_fjadata-tabkey1.
        MOVE lwa_jvdata-tabkey+7(6)  TO wa_fjadata-tabkey2.
        MOVE lwa_jvdata-tabkey+13(3) TO wa_fjadata-tabkey3.
      ELSEIF lwa_jvdata-tabname = 'T8JG'.
        lv_fdate = lwa_jvdata-tabkey+16(23).
        CONVERT INVERTED-DATE lv_fdate INTO DATE lv_date.

*        CONCATENATE lwa_jvdata-tabkey+3(4)  lwa_jvdata-tabkey+7(6)  lwa_jvdata-tabkey+13(3) INTO  wa_fjadata-tabkey SEPARATED BY  ' '.
        MOVE lwa_jvdata-tabkey+3(4)  TO wa_fjadata-tabkey1.
        MOVE lwa_jvdata-tabkey+7(6)  TO wa_fjadata-tabkey2.
        MOVE lwa_jvdata-tabkey+13(3) TO wa_fjadata-tabkey3.
        CONCATENATE   lv_date+4(2)  '/'   lv_date+6(2)  '/'  lv_date+0(4) INTO lv_datechar.
*        CONCATENATE  wa_fjadata-tabkey lv_datechar   INTO wa_fjadata-tabkey  SEPARATED BY ' ' .
        MOVE lv_datechar   TO wa_fjadata-tabkey4.
      ELSEIF lwa_jvdata-tabname = 'T8JQ'.
*        CONCATENATE lwa_jvdata-tabkey+3(4) lwa_jvdata-tabkey+7(6) lwa_jvdata-tabkey+13(3) lwa_jvdata-tabkey+16(10) INTO  wa_fjadata-tabkey SEPARATED BY ' '.
        MOVE lwa_jvdata-tabkey+3(4)   TO wa_fjadata-tabkey1.
        MOVE lwa_jvdata-tabkey+7(6)   TO wa_fjadata-tabkey2.
        MOVE lwa_jvdata-tabkey+13(3)  TO wa_fjadata-tabkey3.
        MOVE lwa_jvdata-tabkey+16(10) TO wa_fjadata-tabkey4.
      ELSEIF lwa_jvdata-tabname = 'T8JC2'.
*        CONCATENATE lwa_jvdata-tabkey+3(4) lwa_jvdata-tabkey+7(6) lwa_jvdata-tabkey+13(3) lwa_jvdata-tabkey+16(3) INTO wa_fjadata-tabkey SEPARATED BY ' '.
        MOVE lwa_jvdata-tabkey+3(4)  TO wa_fjadata-tabkey1.
        MOVE lwa_jvdata-tabkey+7(6)  TO wa_fjadata-tabkey2.
        MOVE lwa_jvdata-tabkey+13(3) TO wa_fjadata-tabkey3.
        MOVE lwa_jvdata-tabkey+16(3) TO wa_fjadata-tabkey4.

      ENDIF.

      IF wa_fjadata-tabkey4 IS NOT INITIAL.
        lv_busp = wa_fjadata-tabkey4.

        SELECT SINGLE name_org1 FROM but000 INTO wa_fjadata-BUSNAME
          WHERE PARTNER = lv_busp.
      ENDIF.
********************************************************************
* Begin of change DF867 spalit 08/27/2019
      IF lwa_jvdata-chngind NE 'I'.
        IF lwa_jvdata-flddescp IS NOT INITIAL.
          wa_fjadata-fname = lwa_jvdata-flddescp.
        ELSE.
* End of change DF867 spalit 08/27/2019
          wa_fjadata-fname = lwa_jvdata-fname.
* Begin of change DF867 spalit 08/27/2019
        ENDIF.
* End of change DF867 spalit 08/27/2019
        wa_fjadata-value_new = lwa_jvdata-value_new.
* Begin of change DF867 spalit 08/27/2019
      ENDIF.
* End of change 9454163 ssen 30/04/2020
* End of change DF867 spalit 08/27/2019
      wa_fjadata-value_old = lwa_jvdata-value_old.
      wa_fjadata-username = lwa_jvdata-username.
      wa_fjadata-udate = lwa_jvdata-udate.
      wa_fjadata-utime = lwa_jvdata-utime.
*      IF lit_jadata IS NOT INITIAL.
*        LOOP AT lit_jadata INTO lwa_jadata.
      IF lwa_jvdata-tabname = 'T8J9A'.
        READ TABLE lit_jadata INTO lwa_jadata WITH KEY bukrs = lwa_jvdata-tabkey+3(4)
                                                       joa    = lwa_jvdata-tabkey+7(6)
                                                       egrup  = lwa_jvdata-tabkey+13(3).
        IF sy-subrc  = 0 .
          IF lwa_jadata-opshare NE 0.
            l_tshare = lwa_jadata-opshare.
            CONDENSE l_tshare NO-GAPS.
            CONCATENATE l_tshare '%' INTO wa_fjadata-sopshare.
            CLEAR l_share.
            CLEAR l_tshare.
          ELSE.
            wa_fjadata-sopshare = lwa_jadata-opshare.
            CLEAR lwa_jadata-sopshare.
          ENDIF.
          IF lwa_jadata-nopshare NE 0.
            l_tshare = lwa_jadata-nopshare.
            CONDENSE l_tshare NO-GAPS.
            CONCATENATE l_tshare '%' INTO wa_fjadata-snopshare.
            CLEAR l_share.
            CLEAR l_tshare.
          ELSE.
            wa_fjadata-snopshare = lwa_jadata-nopshare.
            CLEAR lwa_jadata-snopshare.
          ENDIF.
        ENDIF.
      ELSEIF lwa_jvdata-tabname = 'T8J9C'.
        READ TABLE lit_jadata INTO lwa_jadata WITH KEY bukrs = lwa_jvdata-tabkey+3(4)
                                                       joa    = lwa_jvdata-tabkey+7(6)
                                                       egrup  = lwa_jvdata-tabkey+13(3)
                                                       partn = lwa_jvdata-tabkey+16(10).
        IF sy-subrc  = 0 .
          IF lwa_jadata-eqshare NE 0.
            l_tshare = lwa_jadata-eqshare.
            CONDENSE l_tshare NO-GAPS.
            CONCATENATE l_tshare '%' INTO wa_fjadata-seqshare.
            CLEAR l_share.
            CLEAR l_tshare.
          ELSE.
            wa_fjadata-seqshare = lwa_jadata-eqshare.
            CLEAR lwa_jadata-seqshare.
          ENDIF.

        ENDIF.

      ENDIF.
*      wa_fjadata-sopshare = lwa_jadata-sopshare.
*      wa_fjadata-snopshare = lwa_jadata-snopshare.
*      wa_fjadata-seqshare = lwa_jadata-seqshare.
      wa_fjadata-egroupsus = lwa_jadata-egroupsus.
      wa_fjadata-grpsusper = lwa_jadata-grpsusper.
      wa_fjadata-grpsusyear = lwa_jadata-grpsusyear.
      wa_fjadata-egroupusus = lwa_jadata-egroupusus.
      wa_fjadata-grpususper = lwa_jadata-grpususper.
      wa_fjadata-grpususyer = lwa_jadata-grpususyer.
      wa_fjadata-ownsusp = lwa_jadata-ownsusp.
      wa_fjadata-ownsusper = lwa_jadata-ownsusper.
      wa_fjadata-ownsusyear = lwa_jadata-ownsusyear.
      wa_fjadata-ownunsus = lwa_jadata-ownunsus.
      wa_fjadata-ownususper = lwa_jadata-ownususper.
      wa_fjadata-ownususyr = lwa_jadata-ownususyr.
      APPEND wa_fjadata TO it_fjadata.
      CLEAR: lwa_jadata.
*        ENDLOOP.
*      ELSE.
* Begin of change DF867 spalit 08/27/2019
*      APPEND wa_fjadata TO it_fjadata.
* End  of change DF867 spalit 08/27/2019

*      ENDIF.
      CLEAR wa_fjadata.

      CLEAR: lwa_jvdata,lv_date,lv_datechar.
    ENDLOOP.



  ELSEIF p_equity = abap_true.
    SELECT a~bukrs,
           a~joa,
           a~egrup
      INTO TABLE @it_ueqgdata
      FROM t8j9a AS a LEFT JOIN t8jg AS g
      ON ( a~bukrs = g~bukrs AND a~joa = g~vname AND a~egrup = g~egrup )
      WHERE g~egrup IS NULL.
    SORT it_ueqgdata BY bukrs joa egrup.

  ELSEIF p_joajv = abap_true.
    SELECT joa vname
      INTO TABLE it_mjjdata
      FROM t8jv
      WHERE t8jv~joa <> t8jv~vname
      AND vclass <> 'NN'.

  ELSEIF p_jvadoi = abap_true.

    """ START DAMIEN CHANGES DEFECT 219 """"
    DATA: temp TYPE STANDARD TABLE OF t_mcndata.
*   Get WI with mismatched netting
    SELECT oiu_do_do~bukrs
           oiu_do_do~own_no
           oiu_do_do~vname
           oiu_do_do~doi_no
           oiu_do_do~eff_from_dt
           oiu_do_do~eff_to_dt
           t8jo~convnetind
           oiu_do_do~jib_offs_fl
           oiu_do_do~own_int_type_cd
      INTO TABLE it_mcndata
      FROM t8jo
      JOIN t8jq
        ON t8jo~partn = t8jq~partn
       AND t8jo~bukrs = t8jq~bukrs
      JOIN oiu_do_do
        ON t8jq~vname = oiu_do_do~vname
       AND t8jq~partn = oiu_do_do~own_no
       AND t8jq~bukrs = oiu_do_do~bukrs
      JOIN oiu_cm_pintty
        ON oiu_do_do~own_int_type_cd = oiu_cm_pintty~own_int_type_cd
       AND oiu_cm_pintty~int_cat_cd = 'W'
     WHERE t8jo~convnetind <> oiu_do_do~jib_offs_fl
       AND oiu_cm_pintty~int_cat_cd = 'W'.

*   Get RI with netting checked
    SELECT oiu_do_do~bukrs
           oiu_do_do~own_no
           oiu_do_do~vname
           oiu_do_do~doi_no
           oiu_do_do~eff_from_dt
           oiu_do_do~eff_to_dt
           oiu_do_do~jib_offs_fl
           oiu_do_do~own_int_type_cd
      INTO CORRESPONDING FIELDS OF TABLE temp
      FROM oiu_do_do
      JOIN oiu_cm_pintty
        ON oiu_do_do~own_int_type_cd = oiu_cm_pintty~own_int_type_cd
       AND oiu_cm_pintty~int_cat_cd = 'R'
     WHERE oiu_do_do~jib_offs_fl = 'X'.

*   Combine RI and WI tables for display
    APPEND LINES OF temp TO it_mcndata.

    SORT it_mcndata BY bukrs own_no vname doi_no own_int_type_cd eff_from_dt.
    DELETE ADJACENT DUPLICATES FROM it_mcndata COMPARING ALL FIELDS.
    """ END DAMIEN CHANGES DEFECT 219 """"

*  ELSEIF p_exinn = abap_true.
*
*    SELECT t8ju~bukrs
*           t8ju~joa
*           t8jv~vname
*           t8ju~aclass
*           t8jv~vclass
*      INTO TABLE it_exinn
*      FROM t8ju
*      JOIN t8jv
*      ON ( t8ju~bukrs = t8jv~bukrs AND t8ju~joa = t8jv~joa )
*      WHERE t8ju~aclass <> t8jv~vclass.


  ENDIF.

ENDFORM.





*&---------------------------------------------------------------------*
*& Form SUB_FIELDCAT_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> G_FIELDCAT
*&---------------------------------------------------------------------*
FORM sub_fieldcat_init TABLES   p_fieldcat TYPE slis_t_fieldcat_alv.

  IF p_audit = abap_true.

    PERFORM sub_fill_fieldcat
*                                fieldname      key     key_sel
*                  ref_tabname   text_fieldname do_sum  no_out
*                  reptext_ddic  outputlen      input   sp_group
*                  ref_field     fname          tname   tech
*                  no_zero
              TABLES p_fieldcat
              USING:               space          'X'      'X'
                     space         space          space    space
                     TEXT-104      space          space    space
                     space         TEXT-002    TEXT-008 space
                     space,

                                   space          'X'      'X'
                     space         space          space    space
                     TEXT-009      space          space    space
                     space         TEXT-010    TEXT-008 space
                     space,

                                    space          'X'      'X'
                     space         space          space    space
                     TEXT-011  space          space    space
                     space         TEXT-012    TEXT-008 space
                     space,
* Begin of change 9454163 ssen 30/04/2020
*                                    space          'X'      'X'
*                     space         space          space    space
*                     TEXT-114  space          space    space
*                     space         TEXT-113    TEXT-008  space
*                     space,

                                    space          'X'      'X'
                     space         space          space    space
                     TEXT-119  space          space    space
                     space         TEXT-115    TEXT-008  space
                     space,

                                    space          'X'      'X'
                     space         space          space    space
                     TEXT-120  space          space    space
                     space         TEXT-116    TEXT-008  space
                     space,

                                    space          'X'      'X'
                     space         space          space    space
                     TEXT-121  space          space    space
                     space         TEXT-117    TEXT-008  space
                     space,

                                    space          'X'      'X'
                     space         space          space    space
                     TEXT-122  space          space    space
                     space         TEXT-118    TEXT-008  space
                     space,

                                    space          'X'      'X'
                     space         space          space    space
                     TEXT-123  space          space    space
                     space         TEXT-124    TEXT-008  space
                     space,
* End of change 9454163 ssen 30/04/2020

                                    space          'X'      'X'
                     space         space          space    space
                     TEXT-013  space          space    space
                     space         TEXT-014    TEXT-008 space
                     space,

                                    space          'X'      'X'
                     space         space          space    space
                     TEXT-015  space          space    space
                     space         TEXT-016    TEXT-008 space
                     space,

                                    space          'X'      'X'
                     space         space          space    space
                     TEXT-017  space          space    space
                     space         TEXT-018    TEXT-008 space
                     space,

                                    space          'X'      'X'
                     space         space          space    space
                     TEXT-019  space          space    space
                     space         TEXT-020    TEXT-008 space
                     space,

                                    space          'X'      'X'
                     space         space          space    space
                     TEXT-021  space          space    space
                     space         TEXT-022    TEXT-008 space
                     space,

                                    space          'X'      'X'
                     space         space          space    space
                     TEXT-023  space          space    space
                     space         TEXT-024    TEXT-008 space
                     space,

                     space          'X'      'X'
                     space         space          space    space
                     TEXT-025  space          space    space
                     space         TEXT-026    TEXT-008 space
                     space,

                     space          'X'      'X'
                     space         space          space    space
                     TEXT-027  space          space    space
                     space         TEXT-028    TEXT-008 space
                     space,

                     space          'X'      'X'
                     space         space          space    space
                     TEXT-029  space          space    space
                     space         TEXT-030    TEXT-008 space
                     space,

                     space          'X'      'X'
                     space         space          space    space
                     TEXT-031  space          space    space
                     space         TEXT-032    TEXT-008 space
                     space,

                     space          'X'      'X'
                     space         space          space    space
                     TEXT-033  space          space    space
                     space         TEXT-034    TEXT-008 space
                     space,

                     space          'X'      'X'
                     space         space          space    space
                     TEXT-035  space          space    space
                     space         TEXT-036    TEXT-008 space
                     space,

                     space          'X'      'X'
                     space         space          space    space
                     TEXT-037  space          space    space
                     space         TEXT-038    TEXT-008 space
                     space,

                     space          'X'      'X'
                     space         space          space    space
                     TEXT-039  space          space    space
                     space         TEXT-040    TEXT-008 space
                     space,

                     space          'X'      'X'
                     space         space          space    space
                     TEXT-041  space          space    space
                     space         TEXT-042    TEXT-008 space
                     space,

                     space          'X'      'X'
                     space         space          space    space
                     TEXT-043  space          space    space
                     space         TEXT-044    TEXT-008 space
                     space,

                     space          'X'      'X'
                     space         space          space    space
                     TEXT-045  space          space    space
                     space         TEXT-046    TEXT-008 space
                     space,

                     space          'X'      'X'
                     space         space          space    space
                     TEXT-048  space          space    space
                     space         TEXT-047    TEXT-008 space
                     space,

                     space          'X'      'X'
                     space         space          space    space
                     TEXT-049  space          space    space
                     space         TEXT-050    TEXT-008 space
                     space,

                     space          'X'      'X'
                     space         space          space    space
                     TEXT-039  space          space    space
                     space         TEXT-051    TEXT-008 space
                     space,

                     space          'X'      'X'
                     space         space          space    space
                     TEXT-041  space          space    space
                     space         TEXT-052    TEXT-008 space
                     space.

  ELSEIF p_equity = abap_true.

    PERFORM sub_fill_fieldcat
*                                fieldname      key     key_sel
*                  ref_tabname   text_fieldname do_sum  no_out
*                  reptext_ddic  outputlen      input   sp_group
*                  ref_field     fname          tname   tech
*                  no_zero
              TABLES p_fieldcat
              USING:               space          'X'      'X'
                     space         space          space    space
                     TEXT-053  space          space    space
                     space         TEXT-060    TEXT-055 space
                     space,

                                    space          'X'      'X'
                     space         space          space    space
                     TEXT-082  space          space    space
                     space         TEXT-010    TEXT-055 space
                     space,

                                    space          'X'      'X'
                     space         space          space    space
                     TEXT-083  space          space    space
                     space         TEXT-084    TEXT-055 space
                     space.

  ELSEIF p_joajv = abap_true.

    PERFORM sub_fill_fieldcat
*                                fieldname      key     key_sel
*                  ref_tabname   text_fieldname do_sum  no_out
*                  reptext_ddic  outputlen      input   sp_group
*                  ref_field     fname          tname   tech
*                  no_zero
              TABLES p_fieldcat
              USING:               space          'X'      'X'
                     space         space          space    space
                     TEXT-056  space          space    space
                     space         TEXT-057    TEXT-058 space
                     space,

                                   space          'X'      'X'
                     space         space          space    space
                     TEXT-091       space          space    space
                     space         TEXT-092    TEXT-058 space
                     space.

  ELSEIF p_jvadoi = abap_true.

    PERFORM sub_fill_fieldcat
*                                fieldname      key     key_sel
*                  ref_tabname   text_fieldname do_sum  no_out
*                  reptext_ddic  outputlen      input   sp_group
*                  ref_field     fname          tname   tech
*                  no_zero
          TABLES p_fieldcat
          USING:               space          'X'      'X'
                 space         space          space    space
                 TEXT-059  space          space    space
                 space          TEXT-060    TEXT-073 space
                 space,

                               space          'X'      'X'
                 space         space          space    space
                  TEXT-064  space          space    space
                 space          TEXT-065    TEXT-073 space
                 space,

                               space          'X'      'X'
                 space         space          space    space
                  TEXT-066  space          space    space
                 space          TEXT-054    TEXT-073 space
                 space,

                 space          'X'      'X'
                 space         space          space    space
                 TEXT-111      space          space    space
                 space         TEXT-112     TEXT-073 space
                 space,

                               space          'X'      'X'
                 space         space          space    space
                  TEXT-068  space          space    space
                 space          TEXT-067    TEXT-073 space
                 space,

                               space          'X'      'X'
                 space         space          space    space
                  TEXT-069  space          space    space
                 space          TEXT-070    TEXT-073 space
                 space,

                               space          'X'      'X'
                 space         space          space    space
                  TEXT-071  space          space    space
                 space          TEXT-072    TEXT-073 space
                 space,

                               space          'X'      'X'
                 space         space          space    space
                 TEXT-090      space          space    space
                 space          TEXT-099    TEXT-073 space
                 space,

                               space          'X'      'X'
                 space         space          space    space
                 TEXT-101    space          space    space
                 space          TEXT-100    TEXT-073 space
                 space.


*  ELSEIF p_exinn = abap_true.
*
*    PERFORM sub_fill_fieldcat
**                                fieldname      key     key_sel
**                  ref_tabname   text_fieldname do_sum  no_out
**                  reptext_ddic  outputlen      input   sp_group
**                  ref_field     fname          tname   tech
**                  no_zero
*              TABLES p_fieldcat
*              USING:               space          'X'      'X'
*                     space         space          space    space
*                     TEXT-059  space          space    space
*                     space         TEXT-060    TEXT-061 space
*                     space,
*
*                                   space          'X'      'X'
*                     space         space          space    space
*                     TEXT-063      space          space    space
*                     space         TEXT-057    TEXT-061 space
*                     space,
*
*                                    space          'X'      'X'
*                     space         space          space    space
*                     TEXT-062  space          space    space
*                     space         TEXT-054    TEXT-061 space
*                     space,
*
*                                    space          'X'      'X'
*                     space         space          space    space
*                     TEXT-093      space          space    space
*                     space         TEXT-094    TEXT-061 space
*                     space,
*
*                                    space          'X'      'X'
*                     space         space          space    space
*                     TEXT-095      space          space    space
*                     space         TEXT-096    TEXT-061 space
*                     space.

  ENDIF.





ENDFORM.




*&---------------------------------------------------------------------*
*& Form SUB_FILL_FIELDCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_FIELDCAT
*&      --> SPACE
*&      --> P_
*&      --> P_
*&      --> SPACE
*&      --> SPACE
*&      --> SPACE
*&      --> SPACE
*&      --> P_
*&      --> SPACE
*&      --> SPACE
*&      --> SPACE
*&      --> SPACE
*&      --> P_
*&      --> P_
*&      --> SPACE
*&      --> SPACE
*&---------------------------------------------------------------------*
FORM sub_fill_fieldcat TABLES   p_fieldcat TYPE slis_t_fieldcat_alv
                       USING                   cfieldname
                                key            key_sel
                                ref_tabname    text_fieldname
                                do_sum         no_out
                                reptext_ddic   outputlen
                                input sp_group ref_field
                                fname          tname
                                tech           nozero.

  DATA: l_fldcat TYPE slis_fieldcat_alv.

  g_num = g_num + 1.
  CLEAR p_fieldcat.
  l_fldcat-col_pos        = g_num.
  l_fldcat-fieldname      = cfieldname.
  l_fldcat-key            = key.
  l_fldcat-key_sel        = key_sel.
  l_fldcat-ref_tabname    = ref_tabname.
  l_fldcat-text_fieldname = text_fieldname.
  l_fldcat-do_sum         = do_sum.
  l_fldcat-no_out         = no_out.
  l_fldcat-reptext_ddic   = reptext_ddic.
  l_fldcat-outputlen      = outputlen.
  l_fldcat-input          = input.
  l_fldcat-sp_group       = sp_group.
  l_fldcat-ref_fieldname  = cfieldname.
  l_fldcat-fieldname      = fname.
  l_fldcat-tabname        = tname.
  l_fldcat-tech           = tech.
  l_fldcat-no_zero        = nozero.

  APPEND l_fldcat TO p_fieldcat.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_ALV_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_alv_display .
  CONSTANTS: c_e TYPE char1 VALUE 'E',
             c_i TYPE char1 VALUE 'I'.
  IF p_audit = abap_true.
    IF it_fjadata IS NOT INITIAL.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program     = sy-repid
          i_callback_top_of_page = 'TOP-OF-PAGE'
*         is_layout              = g_layout
          it_fieldcat            = g_fieldcat[]
*         it_special_groups      = g_group[]
*         it_sort                = g_sort[]
          i_save                 = 'X'
*         is_variant             = g_variant
*         it_events              = g_events[]
*         is_print               = g_print
        TABLES
          t_outtab               = it_fjadata.
    ELSE.
      MESSAGE TEXT-102 TYPE c_i.
      LEAVE LIST-PROCESSING .
    ENDIF.

  ELSEIF p_equity = abap_true.
    IF it_ueqgdata IS NOT INITIAL.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program     = sy-repid
          i_callback_top_of_page = 'TOP-OF-PAGE'
          it_fieldcat            = g_fieldcat[]
*         it_special_groups      = g_group[]
*         it_sort                = g_sort[]
          i_save                 = 'X'
*         is_variant             = g_variant
*         it_events              = g_events[]
*         is_print               = g_print
        TABLES
          t_outtab               = it_ueqgdata.
    ELSE.
      MESSAGE TEXT-102 TYPE c_i.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ELSEIF p_joajv = abap_true.
    IF it_mjjdata IS NOT INITIAL.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program = sy-repid
*         is_layout          = g_layout
          it_fieldcat        = g_fieldcat[]
*         it_special_groups  = g_group[]
*         it_sort            = g_sort[]
          i_save             = 'X'
*         is_variant         = g_variant
*         it_events          = g_events[]
*         is_print           = g_print
        TABLES
          t_outtab           = it_mjjdata.
    ELSE.
      MESSAGE TEXT-102 TYPE c_i.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ELSEIF p_jvadoi = abap_true.
    IF it_mcndata IS NOT INITIAL.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program = sy-repid
*         is_layout          = g_layout
          it_fieldcat        = g_fieldcat[]
*         it_special_groups  = g_group[]
*         it_sort            = g_sort[]
          i_save             = 'X'
*         is_variant         = g_variant
*         it_events          = g_events[]
*         is_print           = g_print
        TABLES
          t_outtab           = it_mcndata.
    ELSE.
      MESSAGE TEXT-102 TYPE c_i.
      LEAVE LIST-PROCESSING.
    ENDIF.

*  ELSEIF p_exinn = abap_true.
*    IF it_exinn IS NOT INITIAL.
*      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*        EXPORTING
*          i_callback_program = g_repid
*         is_layout          = g_layout
*          it_fieldcat        = g_fieldcat[]
*         it_special_groups  = g_group[]
*         it_sort            = g_sort[]
*         i_save             = g_save
*         is_variant         = g_variant
*         it_events          = g_events[]
*         is_print           = g_print
*        TABLES
*          t_outtab           = it_exinn.
*     ELSE.
*       MESSAGE TEXT-102 TYPE c_e.
*     ENDIF.

  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUILD_ALV_HEADER
*&---------------------------------------------------------------------*
FORM build_alv_header .
* Begin of change 9454163 ssen 06/05/2020
DATA: ls_uname LIKE LINE OF s_uname,
      ls_dat   LIKE LINE OF s_dat,
      ls_bukrs LIKE LINE OF s_dat,
      ls_joa   LIKE LINE OF s_dat,
      lv_uname TYPE string,
      lv_dat   TYPE string,
      lv_bukrs TYPE string,
      lv_joa   TYPE string,
      lv_lines TYPE char4.
* End of change 9454163 ssen 06/05/2020
  IF p_audit = abap_true.
    wa_listheader-typ  = 'H'.
    wa_listheader-info = TEXT-098.
    APPEND wa_listheader TO it_listheader.
    CLEAR wa_listheader.
* Begin of change 9454163 ssen 30/04/2020
        wa_listheader-typ  = 'S'.
*    IF s_uname[] IS NOT INITIAL.
      IF S_UNAME-LOW IS NOT INITIAL AND S_UNAME-HIGH IS NOT INITIAL.
        CONCATENATE 'Username(s):' ' ' S_UNAME-LOW '-' S_UNAME-HIGH INTO wa_listheader-info.
*      ELSEIF S_UNAME-LOW IS NOT INITIAL.
       ELSEIF s_uname[] IS NOT INITIAL.
         CLEAR lv_lines.
         DESCRIBE TABLE s_uname[] LINES lv_lines.
         IF lv_lines = 1.
           CONCATENATE 'Username(s):' ' ' S_UNAME-LOW INTO wa_listheader-info.
        ELSEIF lv_lines GT 1.
         LOOP AT s_uname INTO ls_uname.
           IF sy-tabix = 1.
             CONCATENATE ls_uname-low ',' INTO lv_uname.
            ELSE.
              CONCATENATE lv_uname ls_uname-low ',' INTO lv_uname.
           ENDIF.
           CLEAR ls_uname.
         ENDLOOP.
        CONCATENATE 'Username(s):'  lv_uname INTO wa_listheader-info.
        ENDIF.
      ENDIF.
    APPEND wa_listheader TO it_listheader.
    CLEAR wa_listheader.
*    ENDIF.

     wa_listheader-typ  = 'S'.
*    IF s_dat[] IS NOT INITIAL.
     IF S_DAT-LOW IS NOT INITIAL AND S_DAT-HIGH IS NOT INITIAL.
      CONCATENATE 'Date Range: ' S_DAT-LOW '-' S_DAT-HIGH INTO wa_listheader-info.
*     ELSEIF S_DAT-LOW IS NOT INITIAL.
      ELSEIF s_dat[] IS NOT INITIAL.
        CLEAR lv_lines.
        DESCRIBE TABLE s_dat[] LINES lv_lines.
        IF lv_lines = 1.
        CONCATENATE 'Date Range: ' S_DAT-LOW INTO wa_listheader-info.
        ELSEIF lv_lines GT 1.
          LOOP AT s_dat INTO ls_dat.
           IF sy-tabix = 1.
             CONCATENATE ls_dat-low ',' INTO lv_dat.
            ELSE.
              CONCATENATE lv_dat ls_dat-low ',' INTO lv_dat.
           ENDIF.
           CLEAR ls_dat.
         ENDLOOP.
         CONCATENATE 'Date Range: ' lv_dat INTO wa_listheader-info.
      ENDIF.
     ENDIF.
    APPEND wa_listheader TO it_listheader.
    CLEAR wa_listheader.
*    ENDIF.

    wa_listheader-typ  = 'S'.
*    IF s_bukrs[] IS NOT INITIAL.
     IF S_BUKRS-LOW IS NOT INITIAL AND S_BUKRS-HIGH IS NOT INITIAL.
      CONCATENATE 'Company Code: ' S_BUKRS-LOW '-' S_BUKRS-HIGH INTO wa_listheader-info.
*     ELSEIF S_BUKRS-LOW IS NOT INITIAL.
     ELSEIF s_bukrs[] IS NOT INITIAL.
       CLEAR lv_lines.
       DESCRIBE TABLE s_bukrs[] LINES lv_lines.
       IF lv_lines = 1.
         CONCATENATE 'Company Code: ' S_BUKRS-LOW INTO wa_listheader-info.
       ELSEIF lv_lines GT 1.
         LOOP AT s_bukrs INTO ls_bukrs.
           IF sy-tabix = 1.
             CONCATENATE ls_bukrs-low ',' INTO lv_bukrs.
            ELSE.
              CONCATENATE lv_bukrs ls_bukrs-low ',' INTO lv_bukrs.
           ENDIF.
           CLEAR ls_bukrs.
         ENDLOOP.
         CONCATENATE 'Company Code: ' lv_bukrs INTO wa_listheader-info.
       ENDIF.
     ENDIF.
    APPEND wa_listheader TO it_listheader.
    CLEAR wa_listheader.
*    ENDIF.

    wa_listheader-typ  = 'S'.
*    IF s_joa[] IS NOT INITIAL.
     IF S_JOA-LOW IS NOT INITIAL AND S_JOA-HIGH IS NOT INITIAL.
      CONCATENATE 'Joint Operating Agreement: ' S_JOA-LOW '-' S_JOA-HIGH INTO wa_listheader-info.
*     ELSEIF S_JOA-LOW IS NOT INITIAL.
     ELSEIF s_joa[] IS NOT INITIAL.
       CLEAR lv_lines.
       DESCRIBE TABLE s_uname[] LINES lv_lines.
         IF lv_lines = 1.
           CONCATENATE 'Joint Operating Agreement: ' S_JOA-LOW INTO wa_listheader-info.
          ELSEIF lv_lines GT 1.
            LOOP AT s_joa INTO ls_joa.
           IF sy-tabix = 1.
             CONCATENATE ls_joa-low ',' INTO lv_joa.
            ELSE.
              CONCATENATE lv_joa ls_joa-low ',' INTO lv_joa.
           ENDIF.
           CLEAR ls_joa.
         ENDLOOP.
         CONCATENATE 'Joint Operating Agreement: ' lv_joa INTO wa_listheader-info.
         ENDIF.
     ENDIF.
    APPEND wa_listheader TO it_listheader.
    CLEAR wa_listheader.
*    ENDIF.
* End of change 9454163 ssen 30/04/2020

  ELSEIF p_equity = abap_true.
    wa_listheader-typ  = 'H'.
    wa_listheader-info = TEXT-006.
    APPEND wa_listheader TO it_listheader.
    CLEAR wa_listheader.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
FORM top-of-page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_listheader.

ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*& Form SUB_F4HELP_USERNAME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_f4help_username .
  TYPES: BEGIN OF lt_unmdata,
           username TYPE cdhdr-username,
         END OF lt_unmdata.

  DATA: lit_unmdata TYPE TABLE OF lt_unmdata.

  CONSTANTS: c_e TYPE c VALUE 'E',
             c_w TYPE c VALUE 'W'.

  SELECT DISTINCT username
    INTO TABLE lit_unmdata
    FROM cdhdr ORDER BY username.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      retfield        = TEXT-020
*     PVALKEY         = ' '
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = TEXT-088
      stepl           = 0
*     WINDOW_TITLE    =
*     VALUE           = ' '
      value_org       = 'S'
*     MULTIPLE_CHOICE = ' '
*     DISPLAY         = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM   = ' '
*     CALLBACK_METHOD =
*     MARK_TAB        =
*   IMPORTING
*     USER_RESET      =
    TABLES
      value_tab       = lit_unmdata
*     FIELD_TAB       =
*     RETURN_TAB      =
*     DYNPFLD_MAPPING =
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE TEXT-103 TYPE c_e DISPLAY LIKE c_w.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_F4HELP_JOA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_f4help_joa .
  TYPES: BEGIN OF lt_joadata,
           objectid TYPE cdhdr-objectid, "t8j9a-joa,
         END OF lt_joadata.

  DATA: lit_joadata TYPE TABLE OF lt_joadata,
        lwa_joadata TYPE lt_joadata,
        l_oid       TYPE cdhdr-objectid,
        l_strlen    TYPE i.

  CONSTANTS: c_e TYPE c VALUE 'E',
             c_w TYPE c VALUE 'W'.

  SELECT DISTINCT objectid
    INTO TABLE lit_joadata
    FROM cdhdr
    WHERE objectclas IN ('JV_JOA','JV_MASTER').

  LOOP AT lit_joadata INTO lwa_joadata.
    l_oid = lwa_joadata-objectid.
    l_strlen = strlen( l_oid ) - 6.
    IF l_strlen GE 0.
      lwa_joadata-objectid = l_oid+l_strlen(6).
    ENDIF.
    MODIFY lit_joadata FROM lwa_joadata.
  ENDLOOP.

  SORT lit_joadata BY objectid.
  DELETE ADJACENT DUPLICATES FROM lit_joadata COMPARING objectid.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      retfield        = TEXT-010
*     PVALKEY         = ' '
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = TEXT-089
      stepl           = 0
*     WINDOW_TITLE    =
*     VALUE           = ' '
      value_org       = 'S'
*     MULTIPLE_CHOICE = ' '
*     DISPLAY         = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM   = ' '
*     CALLBACK_METHOD =
*     MARK_TAB        =
*   IMPORTING
*     USER_RESET      =
    TABLES
      value_tab       = lit_joadata
*     FIELD_TAB       =
*     RETURN_TAB      =
*     DYNPFLD_MAPPING =
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE TEXT-103 TYPE c_e DISPLAY LIKE c_w.
  ENDIF.
ENDFORM.
