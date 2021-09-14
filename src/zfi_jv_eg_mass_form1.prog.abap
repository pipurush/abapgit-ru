*&---------------------------------------------------------------------*
*& Include          ZFI_JV_EG_MASS_FORM1
*&---------------------------------------------------------------------*

FORM sub_validate.

*Validate Company Code
  SELECT SINGLE bukrs
    FROM t001
    INTO lv_bukrs
    WHERE bukrs EQ p_bukrs.
  IF sy-subrc NE 0.
    SET CURSOR FIELD 'P_BUKRS'.
    MESSAGE e000(zmsg) WITH 'Invalid Company Code'(036).
  ENDIF.

*Validate Old Partner Number
  IF p_ptrold NE 'PDC'.
    SELECT  partn
            UP TO 1 ROWS
            FROM t8jo
            INTO lv_partn
            WHERE partn = p_ptrold.
    ENDSELECT.
    IF sy-subrc NE 0.
      SET CURSOR FIELD 'P_PTROLD'.
      MESSAGE e000(zmsg) WITH 'Invalid Partner'(037).
    ENDIF.
  ENDIF.
*Validate Venture
  IF NOT s_vname[] IS INITIAL.
    SELECT  SINGLE vname
            FROM t8jg
            INTO lv_vname
            WHERE vname IN s_vname.
    IF sy-subrc NE 0.
      SET CURSOR FIELD 'S_VNAME'.
      MESSAGE e000(zmsg) WITH 'Invalid Venture'(038).
    ENDIF.
  ENDIF.

*Validate Equity Type
  IF NOT s_etype[] IS INITIAL.
    SELECT  SINGLE etype
            FROM t8jg
            INTO lv_etype
            WHERE etype IN s_etype.
    IF sy-subrc NE 0.
      SET CURSOR FIELD 'S_ETYPE'.
      MESSAGE e000(zmsg) WITH 'Invalid Equity Type'(039).
    ENDIF.
  ENDIF.

*Validate Split % is < 100
  IF p_split > 100 OR p_split <= 0.
    SET CURSOR FIELD 'P_SPLIT'.
    MESSAGE e000(zmsg) WITH TEXT-040. "'Split % should be > 100'.
  ENDIF.

  PERFORM sub_validate_npartners.

*Validate NewPartner(s) and % are entered together

ENDFORM.                    " SUB_VALIDATE

*&---------------------------------------------------------------------*
*&      Form  sub_FETCH_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sub_fetch_data .


  v_totalsplit =  p_splt1 + p_splt2 + p_splt3 + p_splt4 + p_splt5 +
                  p_splt6 + p_splt7 + p_splt8 + p_splt9 + p_splt10.
  IF v_totalsplit <> 100.
    MESSAGE i000(zmsg) WITH 'The new ownership percentage does not sum to 100%'(041).
    EXIT.
  ENDIF.

*Fetch all Ventures/Equity for the Old Partner from T8JQ (Joint Venture Owner Equity)
  IF p_ptrold EQ 'PDC'.
    SELECT   bukrs
             joa
             egrup
             opshare
             nopshare
     FROM   t8j9a
     INTO TABLE it_t8j9a
     WHERE bukrs = p_bukrs
     AND joa IN s_vname.
    IF sy-subrc = 0 .
      SORT it_t8j9a BY bukrs joa egrup .
      SELECT bukrs
             vname
             egrup
             partn
             eqshare
        FROM t8jq
        INTO TABLE it_allpartners
        WHERE bukrs  EQ p_bukrs
        AND   vname  IN s_vname.
*    AND   partn  = p_ptrold.
      IF sy-subrc = 0.
        it_t8jq[] = it_allpartners[].
        IF it_t8jq[] IS NOT INITIAL.
* Begin of change HPC9438387 Spalit 01/21/2019
          SORT it_t8jq BY bukrs vname egrup.
          LOOP AT   it_t8j9a INTO DATA(lw_t8j9atp) .
            READ TABLE it_t8jq  INTO DATA(ls_t8jqtp) WITH KEY  bukrs = lw_t8j9atp-bukrs
                                                               vname = lw_t8j9atp-joa
                                                               egrup = lw_t8j9atp-egrup
                                                               BINARY SEARCH.
            IF sy-subrc  NE 0 .
              wa_t8jq-partn = 'PDC'.
              wa_t8jq-eqshare = lw_t8j9atp-opshare.
              wa_t8jq-bukrs = lw_t8j9atp-bukrs.
              wa_t8jq-vname = lw_t8j9atp-joa.
              wa_t8jq-egrup = lw_t8j9atp-egrup.
              APPEND wa_t8jq TO it_t8jq.
            ENDIF.
          ENDLOOP.
* End of change HPC9438387 Spalit 01/21/2019
          SORT it_t8jq BY bukrs vname egrup partn.
          DELETE ADJACENT DUPLICATES FROM it_t8jq COMPARING bukrs vname egrup.
          LOOP AT it_t8jq ASSIGNING FIELD-SYMBOL(<fs_t8jq>).
            READ TABLE it_t8j9a INTO DATA(lw_t8j9a) WITH KEY bukrs = <fs_t8jq>-bukrs
                                                             joa = <fs_t8jq>-vname
                                                             egrup = <fs_t8jq>-egrup
                                                             BINARY SEARCH.
            IF sy-subrc  = 0.
              <fs_t8jq>-partn = 'PDC'.
              <fs_t8jq>-eqshare = lw_t8j9a-opshare.
            ENDIF.
          ENDLOOP.
        ELSE.
          MESSAGE i000(zmsg) WITH 'No data found for given selection'(042).
          LEAVE LIST-PROCESSING.
        ENDIF.
* Begin of  DF1095 spalit 09/26/2019
      ELSE.
        LOOP AT   it_t8j9a INTO DATA(lw_t8j9atmp) .
          APPEND INITIAL LINE TO it_t8jq ASSIGNING  FIELD-SYMBOL(<fs_t8jqtmp>).
          <fs_t8jqtmp>-partn = 'PDC'.
          <fs_t8jqtmp>-eqshare = lw_t8j9atmp-opshare.
          <fs_t8jqtmp>-bukrs = lw_t8j9atmp-bukrs.
          <fs_t8jqtmp>-vname = lw_t8j9atmp-joa.
          <fs_t8jqtmp>-egrup = lw_t8j9atmp-egrup.
        ENDLOOP.
        SORT it_t8jq BY bukrs vname egrup partn.
        DELETE ADJACENT DUPLICATES FROM it_t8jq COMPARING bukrs vname egrup.
* End of  DF1095 spalit 09/26/2019
      ENDIF.

    ENDIF.

  ELSE.
    SELECT bukrs
           vname
           egrup
           partn
           eqshare
      FROM t8jq
      INTO TABLE it_allpartners
      WHERE bukrs  EQ p_bukrs
      AND   vname  IN s_vname.
*    AND   partn  = p_ptrold.
    IF sy-subrc = 0.
      it_t8jq[] = it_allpartners[].
      DELETE it_t8jq WHERE partn NE p_ptrold.
      IF it_t8jq[] IS NOT INITIAL.
        SORT it_t8jq BY bukrs vname egrup partn.
      ELSE.
        MESSAGE i000(zmsg) WITH 'No data found for given selection'(042).
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
  ENDIF.
* Begin of change HPC9438387 Spalit 01/21/2019
  DELETE it_t8jq WHERE eqshare IS  INITIAL.
  IF it_t8jq[] IS INITIAL.
    MESSAGE i000(zmsg) WITH 'No data found for given selection'(046).
    LEAVE LIST-PROCESSING.
  ENDIF.
* End of change HPC9438387 Spalit 01/21/2019

*For all entries of IT_T8JQ, fetch Venture Descriptions from T8JVT(Joint Venture Description)
  IF NOT it_t8jq[] IS INITIAL.
    SELECT  spras
            bukrs
            vname
            vtext
      FROM t8jvt
      INTO TABLE it_t8jvt
      FOR ALL ENTRIES IN it_t8jq
      WHERE bukrs = it_t8jq-bukrs
      AND  vname = it_t8jq-vname.
    IF sy-subrc = 0.
      SORT it_t8jvt BY spras bukrs vname.
    ENDIF.

*fetch Equity Group Description from T8JFT (Investment Group Description)
    SELECT
        bukrs
        vname
        egrup
        egtxt
        FROM t8jft
        INTO TABLE it_t8jft
        FOR ALL ENTRIES IN it_t8jq
        WHERE bukrs EQ it_t8jq-bukrs
          AND vname EQ it_t8jq-vname
          AND egrup EQ it_t8jq-egrup .
    IF sy-subrc = 0.
      SORT it_t8jft BY bukrs vname .
    ENDIF.

    CLEAR it_npartners.
    IF NOT p_nptr1 IS INITIAL AND NOT p_splt1 IS INITIAL.
      PERFORM sub_new_partners USING p_nptr1 p_splt1 p_stat1.
    ENDIF.

    IF NOT p_nptr2 IS INITIAL AND NOT p_splt2 IS INITIAL.
      PERFORM sub_new_partners USING p_nptr2 p_splt2 p_stat2.
    ENDIF.

    IF NOT p_nptr3 IS INITIAL AND NOT p_splt3 IS INITIAL.
      PERFORM sub_new_partners USING p_nptr3 p_splt3 p_stat3.
    ENDIF.

    IF NOT p_nptr4 IS INITIAL AND NOT p_splt4 IS INITIAL.
      PERFORM sub_new_partners USING p_nptr4 p_splt4 p_stat4.
    ENDIF.

    IF NOT p_nptr5 IS INITIAL AND NOT p_splt5 IS INITIAL.
      PERFORM sub_new_partners USING p_nptr5 p_splt5 p_stat5.
    ENDIF.

    IF NOT p_nptr6 IS INITIAL AND NOT p_splt6 IS INITIAL.
      PERFORM sub_new_partners USING p_nptr6 p_splt6 p_stat6.
    ENDIF.

    IF NOT p_nptr7 IS INITIAL AND NOT p_splt7 IS INITIAL.
      PERFORM sub_new_partners USING p_nptr7 p_splt7 p_stat7.
    ENDIF.

    IF NOT p_nptr8 IS INITIAL AND NOT p_splt8 IS INITIAL.
      PERFORM sub_new_partners USING p_nptr8 p_splt8 p_stat8.
    ENDIF.

    IF NOT p_nptr9 IS INITIAL AND NOT p_splt9 IS INITIAL.
      PERFORM sub_new_partners USING p_nptr9 p_splt9 p_stat9.
    ENDIF.

    IF NOT p_nptr10 IS INITIAL AND NOT p_splt10 IS INITIAL.
      PERFORM sub_new_partners USING p_nptr10 p_splt10 p_stat10.
    ENDIF.

    DATA(lt_partn) = it_npartners[].
    APPEND INITIAL LINE TO lt_partn ASSIGNING  FIELD-SYMBOL(<fs_new_part>).
    <fs_new_part>-new_partner = p_ptrold.
    SORT lt_partn BY new_partner.
    DELETE ADJACENT DUPLICATES FROM lt_partn COMPARING new_partner.
    SELECT
     partner
     name_org1
     FROM but000
     INTO TABLE it_but000
     FOR ALL ENTRIES IN lt_partn
     WHERE  partner EQ lt_partn-new_partner .
    IF sy-subrc = 0.
      SORT it_but000 BY partner.
    ENDIF.

  ENDIF.

*Fetch all Equity groups from T8JG for the BUKRS/VNAME/ETYPE entered on s-s
  SELECT
   bukrs
   vname
   etype
   fdate
   egrup
   FROM t8jg
   INTO  TABLE it_t8jg
   WHERE bukrs EQ p_bukrs
     AND vname IN s_vname
     AND etype IN s_etype.
  IF sy-subrc = 0.
    SORT it_t8jg BY bukrs vname etype .
    DELETE it_t8jg WHERE fdate IS INITIAL.
  ENDIF.

*Covert Invert Date into Regular Date
  LOOP AT it_t8jg ASSIGNING FIELD-SYMBOL(<lfs_t8jg>).
    CONVERT INVERTED-DATE <lfs_t8jg>-fdate INTO DATE <lfs_t8jg>-vfrom.
  ENDLOOP.

*  SORT IT_T8JG BY BUKRS VNAME ETYPE EGRUP VFROM ASCENDING.
  SORT it_t8jg BY bukrs vname etype vfrom ASCENDING.

  LOOP AT it_t8jg ASSIGNING <lfs_t8jg> WHERE vto IS INITIAL.
    v_tabix = sy-tabix + 1.
    READ TABLE it_t8jg INTO wa_t8jg INDEX v_tabix.
    IF sy-subrc = 0.
      IF <lfs_t8jg>-bukrs = wa_t8jg-bukrs
        AND <lfs_t8jg>-vname = wa_t8jg-vname
        AND <lfs_t8jg>-etype = wa_t8jg-etype.
*        AND <LFS_T8JG>-EGRUP = WA_T8JG-EGRUP.
        <lfs_t8jg>-vto = wa_t8jg-vfrom - 1.
      ELSE.
        <lfs_t8jg>-vto = '99991231'.
      ENDIF.
    ELSE.
      <lfs_t8jg>-vto = '99991231'.
    ENDIF.
    CLEAR v_tabix .
  ENDLOOP.

ENDFORM.                    " sub_FETCH_data


*&---------------------------------------------------------------------*
*&      Form  SUB_FLDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sub_fldcat.

  CLEAR wa_pfldcat.

  wa_pfldcat-col_pos = 1.
  wa_pfldcat-fieldname = 'FLAG'.   " name of field from internal table
  wa_pfldcat-tabname = 'IT_PLIST'. " internal table name
  wa_pfldcat-checkbox = 'X'.       " print as checkbox
  wa_pfldcat-edit = 'X'.           " make field open for input
  wa_pfldcat-seltext_l = ' '.      " header information
  APPEND wa_pfldcat TO it_pfldcat.   " append field catalog internal table
  CLEAR wa_pfldcat.                " clear field catalog work area

  wa_pfldcat-col_pos  = 2.
  wa_pfldcat-fieldname = 'VNAME'.
  wa_pfldcat-tabname = 'IT_PLIST'.
  wa_pfldcat-seltext_l = 'Venture'(013).
  APPEND wa_pfldcat TO it_pfldcat.
  CLEAR wa_pfldcat.

  wa_pfldcat-col_pos  = 3.
  wa_pfldcat-fieldname = 'VTEXT'.
  wa_pfldcat-tabname = 'IT_PLIST'.
  wa_pfldcat-seltext_l = 'Venture Name'(014).
  APPEND wa_pfldcat TO it_pfldcat.
  CLEAR wa_pfldcat.


  wa_pfldcat-col_pos  = 4.
  wa_pfldcat-fieldname = 'EGRUP'.
  wa_pfldcat-tabname = 'IT_PLIST'.
  wa_pfldcat-seltext_l = 'Equity Group'(015).
  APPEND wa_pfldcat TO it_pfldcat.
  CLEAR wa_pfldcat.

  wa_pfldcat-col_pos  = 5.
  wa_pfldcat-fieldname = 'EGTXT'.
  wa_pfldcat-tabname = 'IT_PLIST'.
  wa_pfldcat-seltext_l = 'Equity Group Description'(016).
  APPEND wa_pfldcat TO it_pfldcat.
  CLEAR wa_pfldcat.

  wa_pfldcat-col_pos  = 6.
  wa_pfldcat-fieldname = 'OPARTN'.
  wa_pfldcat-tabname = 'IT_PLIST'.
  wa_pfldcat-seltext_l = 'Partner Number'(017).
  APPEND wa_pfldcat TO it_pfldcat.
  CLEAR wa_pfldcat.


  wa_pfldcat-col_pos  = 7.
  wa_pfldcat-fieldname = 'OLDPART_NAME'.
  wa_pfldcat-tabname = 'IT_PLIST'.
  wa_pfldcat-seltext_l = 'Partner Name'(018).
  APPEND wa_pfldcat TO it_pfldcat.
  CLEAR wa_pfldcat.

  wa_pfldcat-col_pos  = 8.
  wa_pfldcat-fieldname = 'EQSHARE'.
  wa_pfldcat-tabname = 'IT_PLIST'.
  wa_pfldcat-seltext_l = 'Equity share of Partner'(019).
  APPEND wa_pfldcat TO it_pfldcat.
  CLEAR wa_pfldcat.

  wa_pfldcat-col_pos  = 9.
  wa_pfldcat-fieldname = 'VFROM'.
  wa_pfldcat-tabname = 'IT_PLIST'.
  wa_pfldcat-seltext_l = 'Valid From'(020).
  APPEND wa_pfldcat TO it_pfldcat.
  CLEAR wa_pfldcat.

  wa_pfldcat-col_pos  = 9.
  wa_pfldcat-fieldname = 'VTO'.
  wa_pfldcat-tabname = 'IT_PLIST'.
  wa_pfldcat-seltext_l = 'Valid To'(021).
  APPEND wa_pfldcat TO it_pfldcat.
  CLEAR wa_pfldcat.

  wa_pfldcat-col_pos  = 10.
  wa_pfldcat-fieldname = 'ETYPE'.
  wa_pfldcat-tabname = 'IT_PLIST'.
  wa_pfldcat-seltext_l = 'Equity Type'(022).
  APPEND wa_pfldcat TO it_pfldcat.
  CLEAR wa_pfldcat.

  wa_playout-colwidth_optimize = 'X'.
  wa_playout-zebra             = 'X'.

ENDFORM.                    " FLDCAT

*&---------------------------------------------------------------------*
*& Form SUB_POPULATE_VALUE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM sub_populate_value.

*Populate Effective Date as 1st Day of current Month
  v_date = sy-datum.
  CONCATENATE sy-datum+0(4)
              sy-datum+4(2)
              c_01
              INTO v_date.
  WRITE v_date TO p_edate.


  w_line-key = 'NS'.
  w_line-text = 'Not Suspense'.
  APPEND w_line TO it_stat.

  w_line-key = 'S'.
  w_line-text = 'Suspense'.
  APPEND w_line TO it_stat.


  w_line-key = 'OPS'.
  w_line-text = 'Old Par Suspense'.
  APPEND w_line TO it_stat.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SUB_DISP_PLIST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM sub_disp_plist .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       = ' '
*     I_BUFFER_ACTIVE          = ' '
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'PF_STATUS_SET'
      i_callback_user_command  = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      is_layout                = wa_playout
      it_fieldcat              = it_pfldcat[]
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
*     IT_SORT                  =
*     IT_FILTER                =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
*     I_SAVE                   = ' '
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT                 =
*     IS_REPREP_ID             =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        = 0
*     I_HTML_HEIGHT_END        = 0
*     IT_ALV_GRAPHICS          =
*     IT_HYPERLINK             =
*     IT_ADD_FIELDCAT          =
*     IT_EXCEPT_QINFO          =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = it_plist[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SUB_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM sub_prepare .

  LOOP AT it_t8jq INTO wa_t8jq.

    LOOP AT it_t8jg INTO wa_t8jg WHERE  bukrs = wa_t8jq-bukrs AND
                                        vname = wa_t8jq-vname AND
                                        egrup = wa_t8jq-egrup.
*      IF p_edate >= wa_t8jg-vfrom.
*      IF wa_t8jg-vto  >= p_edate AND wa_t8jg-vfrom <= p_edate.
      IF wa_t8jg-vto  >= p_edate .
*      WA_PLIST-BUKRS = WA_T8JQ-BUKRS.
        wa_plist-vfrom = wa_t8jg-vfrom.
        wa_plist-vto   = wa_t8jg-vto.
        wa_plist-etype = wa_t8jg-etype.

        wa_plist-vname = wa_t8jq-vname.
        READ TABLE it_t8jvt INTO wa_t8jvt WITH KEY  bukrs = wa_t8jq-bukrs
                                                    vname = wa_t8jq-vname.
        IF sy-subrc = 0.
          wa_plist-vtext = wa_t8jvt-vtext.
        ENDIF.

        wa_plist-egrup = wa_t8jq-egrup.
        READ TABLE it_t8jft INTO wa_t8jft WITH KEY  bukrs = wa_t8jq-bukrs
                                                    vname = wa_t8jq-vname
                                                    egrup = wa_t8jq-egrup.
        IF sy-subrc = 0.
          wa_plist-egtxt = wa_t8jft-egtxt.
        ENDIF.

        wa_plist-opartn = wa_t8jq-partn.
        READ TABLE it_but000 INTO wa_but000 WITH KEY  partner = wa_t8jq-partn BINARY SEARCH.
        IF sy-subrc = 0.
          wa_plist-oldpart_name = wa_but000-name_org1 . "Partner Name
        ENDIF.

        wa_plist-eqshare = wa_t8jq-eqshare.

        APPEND wa_plist TO it_plist.
        CLEAR: wa_plist,
               wa_t8jg.
      ENDIF.
    ENDLOOP.
    CLEAR wa_t8jq.
  ENDLOOP.

ENDFORM.

FORM pf_status_set USING extab TYPE slis_t_extab.

  SET PF-STATUS 'Z_PF_EG_MASS1'.

ENDFORM.

FORM user_command USING okcode TYPE sy-ucomm
                  CHANGING l_selfield TYPE slis_selfield.

  CASE okcode.

    WHEN 'EXE1'.

* to reflect the data changed into internal table
      DATA : ref_grid TYPE REF TO cl_gui_alv_grid.

      CLEAR:  it_split,
*              it_npartners,
              it_slist.

      IF ref_grid IS INITIAL.
        CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
          IMPORTING
            e_grid = ref_grid.
      ENDIF.

      IF NOT ref_grid IS INITIAL.
        CALL METHOD ref_grid->check_changed_data.

      ENDIF.

***********************
SELECT *
FROM T8JV
INTO TABLE @DATA(lt_t8jv)
FOR ALL ENTRIES IN @it_plist
WHERE bukrs = @lv_bukrs
AND VNAME = @it_plist-vname.

*******************************

      LOOP AT it_plist  ASSIGNING FIELD-SYMBOL(<lfs_plist3>)  WHERE flag = 'X'.
        wa_split-flag      = <lfs_plist3>-flag.
        wa_split-vname     = <lfs_plist3>-vname.
        wa_split-vtext     = <lfs_plist3>-vtext.
        wa_split-egrup     = <lfs_plist3>-egrup.
        wa_split-egtxt     = <lfs_plist3>-egtxt.
        wa_split-opartn    = <lfs_plist3>-opartn.
        wa_split-oldpart_name = <lfs_plist3>-oldpart_name.
        wa_split-eqshare   = <lfs_plist3>-eqshare.
        wa_split-vfrom     = <lfs_plist3>-vfrom.
        wa_split-vto       = <lfs_plist3>-vto.
        wa_split-etype     = <lfs_plist3>-etype.

********************************************
      READ TABLE lt_t8jv INTO DATA(ls_t8jv) WITH KEY bukrs = lv_bukrs vname =  <lfs_plist3>-vname.
      IF sy-subrc IS INITIAL.
         wa_split-vname = ls_t8jv-joa.
        CLEAR ls_t8jv.
      ENDIF.
*********************************************

        APPEND wa_split TO it_split.
        CLEAR wa_split.
      ENDLOOP.


      DATA(it_parttmp) = it_allpartners[].

      CALL FUNCTION 'ZJVA_MASSCHANGE_SPLITLOGIC1'
        EXPORTING
          iv_splitseltab  = it_npartners
          iv_oldpart      = p_ptrold
          iv_edate        = p_edate
          iv_partnernames = it_but000
          iv_descp        = p_egdesc
        IMPORTING
          ev_newsplittab  = it_slist
        CHANGING
          iv_allpartners  = it_parttmp
          iv_splittab     = it_split.


* refresh the ALV Grid output from internal table
      l_selfield-refresh = 'X'.

* now all the selected records by the user at run-time are appended into
* into a new internal table which can now be used to processed as per the
* user requirements
      DATA : lv_line_count TYPE i.

      DESCRIBE TABLE it_slist LINES lv_line_count.
      IF lv_line_count GE 1.
        PERFORM sub_slist.
      ENDIF.

    WHEN 'SELECT_ALL'.
* to select all the records displayed in ALV Grid
      LOOP AT it_plist ASSIGNING FIELD-SYMBOL(<lfs_plist>).
        <lfs_plist>-flag = 'X'.
      ENDLOOP.
* refresh the ALV Grid output from internal table
      l_selfield-refresh = 'X'.

    WHEN 'DSELECTALL'.
* to deselect all the records displayed in ALV Grid
      LOOP AT it_plist ASSIGNING FIELD-SYMBOL(<lfs_plist1>).
        <lfs_plist1>-flag = ''.
      ENDLOOP.
* refresh the ALV Grid output from internal table
      l_selfield-refresh = 'X'.

    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE PROGRAM.

    WHEN 'EXIT'.
      LEAVE PROGRAM.

  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SUB_SLIST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM sub_slist .

  CLEAR: wa_sfldcat,
         it_sfldcat[].

  wa_sfldcat-col_pos   = 1.
  wa_sfldcat-fieldname = 'BUKRS'.
  wa_sfldcat-tabname   = 'IT_SLIST'.
  wa_sfldcat-seltext_l = 'Company Code'(023).
  APPEND wa_sfldcat TO it_sfldcat.
  CLEAR wa_sfldcat.

*  wa_sfldcat-col_pos   = 2.
*  wa_sfldcat-fieldname = 'VNAME'.
*  wa_sfldcat-tabname   = 'IT_SLIST'.
*  wa_sfldcat-seltext_l = 'Joint Venture'(024).
*  APPEND wa_sfldcat TO it_sfldcat.
*  CLEAR wa_sfldcat.

wa_sfldcat-col_pos   = 2.
  wa_sfldcat-fieldname = 'VNAME'.
  wa_sfldcat-tabname   = 'IT_SLIST'.
  wa_sfldcat-seltext_l = 'JOA'(024).
  APPEND wa_sfldcat TO it_sfldcat.
  CLEAR wa_sfldcat.

  wa_sfldcat-col_pos   = 3.
  wa_sfldcat-fieldname = 'ETYPE'.
  wa_sfldcat-tabname   = 'IT_SLIST'.
  wa_sfldcat-seltext_l = 'Equity Type'(022).
  APPEND wa_sfldcat TO it_sfldcat.
  CLEAR wa_sfldcat.

  wa_sfldcat-col_pos   = 4.
  wa_sfldcat-fieldname = 'EDATE'.
  wa_sfldcat-tabname   = 'IT_SLIST'.
  wa_sfldcat-seltext_l = 'Valid From'(020).
  APPEND wa_sfldcat TO it_sfldcat.
  CLEAR wa_sfldcat.


  wa_sfldcat-col_pos   = 5.
  wa_sfldcat-fieldname = 'EGRUP'.
  wa_sfldcat-tabname   = 'IT_SLIST'.
  wa_sfldcat-seltext_l = 'Equity Group'(015).
  APPEND wa_sfldcat TO it_sfldcat.
  CLEAR wa_sfldcat.

  wa_sfldcat-col_pos   = 6.
  wa_sfldcat-fieldname = 'VTEXT'.
  wa_sfldcat-tabname   = 'IT_SLIST'.
  wa_sfldcat-seltext_l = 'JV Description'(025).
  APPEND wa_sfldcat TO it_sfldcat.
  CLEAR wa_sfldcat.

  wa_sfldcat-col_pos   = 7.
  wa_sfldcat-fieldname = 'EGTXT'.
  wa_sfldcat-tabname   = 'IT_SLIST'.
  wa_sfldcat-seltext_l = 'Investment Group Desc'(026).
  APPEND wa_sfldcat TO it_sfldcat.
  CLEAR wa_sfldcat.

  wa_sfldcat-col_pos   = 8.
  wa_sfldcat-fieldname = 'OLD_PARTNER'.
  wa_sfldcat-tabname   = 'IT_SLIST'.
  wa_sfldcat-seltext_l = 'Old Partner'(027).
  APPEND wa_sfldcat TO it_sfldcat.
  CLEAR wa_sfldcat.

  wa_sfldcat-col_pos   = 9.
  wa_sfldcat-fieldname = 'OLDPART_NAME'.
  wa_sfldcat-tabname   = 'IT_SLIST'.
  wa_sfldcat-seltext_l = 'Old Partner Name'(028).
  APPEND wa_sfldcat TO it_sfldcat.
  CLEAR wa_sfldcat.

  wa_sfldcat-col_pos  = 10.
  wa_sfldcat-fieldname = 'OLD_SHARE'.
  wa_sfldcat-tabname   = 'IT_SLIST'.
  wa_sfldcat-seltext_l = 'Equity Share - Old Partner'(029).
  APPEND wa_sfldcat TO it_sfldcat.
  CLEAR wa_sfldcat.

  wa_sfldcat-col_pos   = 11.
  wa_sfldcat-fieldname = 'OLD_SHARSPLIT'.
  wa_sfldcat-tabname   = 'IT_SLIST'.
  wa_sfldcat-seltext_l = 'Old Partner Split'(030).
  APPEND wa_sfldcat TO it_sfldcat.
  CLEAR wa_sfldcat.

  wa_sfldcat-col_pos   = 12.
  wa_sfldcat-fieldname = 'NEW_PARTNER'.
  wa_sfldcat-tabname   = 'IT_SLIST'.
  wa_sfldcat-seltext_l = 'New Partner'(031).
  APPEND wa_sfldcat TO it_sfldcat.
  CLEAR wa_sfldcat.

  wa_sfldcat-col_pos   = 13.
  wa_sfldcat-fieldname = 'NEWPART_NAME'.
  wa_sfldcat-tabname   = 'IT_SLIST'.
  wa_sfldcat-seltext_l = 'New Partner Name'(032).
  APPEND wa_sfldcat TO it_sfldcat.
  CLEAR wa_sfldcat.

  wa_sfldcat-col_pos   = 14.
  wa_sfldcat-fieldname = 'NEW_SHARE'.
  wa_sfldcat-tabname   = 'IT_SLIST'.
  wa_sfldcat-seltext_l = 'Equity Share - New Partner'(033).
  APPEND wa_sfldcat TO it_sfldcat.
  CLEAR wa_sfldcat.

  wa_sfldcat-col_pos   = 15.
  wa_sfldcat-fieldname = 'MESSAGE'.
  wa_sfldcat-tabname   = 'IT_SLIST'.
  wa_sfldcat-seltext_l = 'Message'(044).
  APPEND wa_sfldcat TO it_sfldcat.
  CLEAR wa_sfldcat.


  wa_slayout-colwidth_optimize = 'X'.
  wa_slayout-zebra             = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       = ' '
*     I_BUFFER_ACTIVE          = ' '
      i_callback_program       = sy-repid
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      is_layout                = wa_slayout
      it_fieldcat              = it_sfldcat[]
      i_callback_pf_status_set = 'PF_2STATUS_SET'
      i_callback_user_command  = 'USER_COMMAND_2'
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
*     IT_SORT                  =
*     IT_FILTER                =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
*     I_SAVE                   = ' '
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT                 =
*     IS_REPREP_ID             =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        = 0
*     I_HTML_HEIGHT_END        = 0
*     IT_ALV_GRAPHICS          =
*     IT_HYPERLINK             =
*     IT_ADD_FIELDCAT          =
*     IT_EXCEPT_QINFO          =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = it_slist[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SUB_NEW_PARTNERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM sub_new_partners  USING    p_p_nptr1
                                p_p_splt1
                                p_p_stat1.

  wa_npartners-new_partner    = p_p_nptr1.
  wa_npartners-split          = p_p_splt1.
  wa_npartners-old_partsplit  = p_split.
  wa_npartners-bukrs          = p_bukrs.

  APPEND  wa_npartners TO it_npartners.
  CLEAR   wa_npartners.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SUB_BAPI_UPDATE_FIELD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM sub_bapi_update_field  USING  l_field l_flag.

  CLEAR: l_flag.
  CHECK NOT l_field IS INITIAL.

  l_flag = 'X'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SUB_TOGGLE_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM sub_toggle_screen .

  LOOP AT SCREEN.
    IF p_c1 IS INITIAL.
      IF screen-group1 = 'G2'
        OR screen-group1 = 'G3'
          OR screen-group1 = 'G4'
            OR screen-group1 = 'G5'
              OR screen-group1 = 'G6'
                OR screen-group1 = 'G7'
                  OR screen-group1 = 'G8'
                    OR screen-group1 = 'G9'
                      OR screen-group1 = 'G10'.
        screen-input = 0.
        screen-active = 0.
        CLEAR: p_nptr2, p_splt2, p_nptr3, p_splt3, p_nptr4, p_splt4, p_nptr5, p_splt5,
               p_nptr6, p_splt6, p_nptr7, p_splt7, p_nptr8, p_splt8, p_nptr9, p_splt9, p_nptr10, p_splt10.
      ENDIF.
    ELSE.
      IF p_c2 IS INITIAL.
        IF screen-group1 = 'G2'.
          screen-input = 1.
          screen-active = 1.
        ENDIF.
        IF screen-group1 = 'G3'
          OR screen-group1 = 'G4'
            OR screen-group1 = 'G5'
              OR screen-group1 = 'G6'
                OR screen-group1 = 'G7'
                  OR screen-group1 = 'G8'
                    OR screen-group1 = 'G9'
                      OR screen-group1 = 'G10'.
          screen-input = 0.
          screen-active = 0.
          CLEAR: p_nptr3, p_splt3, p_nptr4, p_splt4, p_nptr5, p_splt5,
                 p_nptr6, p_splt6, p_nptr7, p_splt7, p_nptr8, p_splt8, p_nptr9, p_splt9, p_nptr10, p_splt10.
          CLEAR: p_c3, p_c4, p_c5, p_c6, p_c7, p_c8, p_c9.
        ENDIF.
      ELSE.
        IF p_c3 IS INITIAL.
          IF screen-group1 = 'G3'.
            screen-input = 1.
            screen-active = 1.
          ENDIF.
          IF screen-group1 = 'G4'
             OR screen-group1 = 'G5'
              OR screen-group1 = 'G6'
                OR screen-group1 = 'G7'
                  OR screen-group1 = 'G8'
                    OR screen-group1 = 'G9'
                      OR screen-group1 = 'G10'.
            screen-input = 0.
            screen-active = 0.
            CLEAR: p_nptr4, p_splt4, p_nptr5, p_splt5,
                   p_nptr6, p_splt6, p_nptr7, p_splt7, p_nptr8, p_splt8, p_nptr9, p_splt9, p_nptr10, p_splt10.
            CLEAR: p_c4, p_c5, p_c6, p_c7, p_c8, p_c9.
          ENDIF.
        ELSE.
          IF p_c4 IS INITIAL.
            IF screen-group1 = 'G4'.
              screen-input = 1.
              screen-active = 1.
            ENDIF.
            IF screen-group1 = 'G5'
              OR screen-group1 = 'G6'
                OR screen-group1 = 'G7'
                  OR screen-group1 = 'G8'
                    OR screen-group1 = 'G9'
                      OR screen-group1 = 'G10'.
              screen-input = 0.
              screen-active = 0.
              CLEAR: p_nptr5, p_splt5,
                     p_nptr6, p_splt6, p_nptr7, p_splt7, p_nptr8, p_splt8, p_nptr9, p_splt9, p_nptr10, p_splt10.
              CLEAR: p_c5, p_c6, p_c7, p_c8, p_c9.
            ENDIF.
          ELSE.
            IF p_c5 IS INITIAL.
              IF screen-group1 = 'G5'.
                screen-input = 1.
                screen-active = 1.
              ENDIF.
              IF screen-group1 = 'G6'
                  OR screen-group1 = 'G7'
                    OR screen-group1 = 'G8'
                      OR screen-group1 = 'G9'
                        OR screen-group1 = 'G10'.
                screen-input = 0.
                screen-active = 0.
                CLEAR: p_nptr6, p_splt6, p_nptr7, p_splt7, p_nptr8, p_splt8, p_nptr9, p_splt9, p_nptr10, p_splt10.
                CLEAR: p_c6, p_c7, p_c8, p_c9.
              ENDIF.
            ELSE.
              IF p_c6 IS INITIAL.
                IF screen-group1 = 'G6'.
                  screen-input = 1.
                  screen-active = 1.
                ENDIF.
                IF screen-group1 = 'G7'
                    OR screen-group1 = 'G8'
                      OR screen-group1 = 'G9'
                        OR screen-group1 = 'G10'.
                  screen-input = 0.
                  screen-active = 0.
                  CLEAR: p_nptr7, p_splt7, p_nptr8, p_splt8, p_nptr9, p_splt9, p_nptr10, p_splt10.
                  CLEAR: p_c7, p_c8, p_c9.
                ENDIF.
              ELSE.
                IF p_c7 IS INITIAL.
                  IF screen-group1 = 'G7'.
                    screen-input = 1.
                    screen-active = 1.
                  ENDIF.
                  IF screen-group1 = 'G8'
                      OR screen-group1 = 'G9'
                        OR screen-group1 = 'G10'.
                    screen-input = 0.
                    screen-active = 0.
                    CLEAR: p_nptr8, p_splt8, p_nptr9, p_splt9, p_nptr10, p_splt10.
                    CLEAR: p_c8, p_c9.
                  ENDIF.
                ELSE.
                  IF p_c8 IS INITIAL.
                    IF screen-group1 = 'G8'.
                      screen-input = 1.
                      screen-active = 1.
                    ENDIF.
                    IF screen-group1 = 'G9'
                        OR screen-group1 = 'G10'.
                      screen-input = 0.
                      screen-active = 0.
                      CLEAR: p_nptr9, p_splt9, p_nptr10, p_splt10.
                      CLEAR: p_c9.
                    ENDIF.
                  ELSE.
                    IF p_c9 IS INITIAL.
                      IF screen-group1 = 'G9'.
                        screen-input = 1.
                        screen-active = 1.
                      ENDIF.
                      IF screen-group1 = 'G10'.
                        screen-input = 0.
                        screen-active = 0.
                        CLEAR: p_nptr10, p_splt10.
*                        CLEAR: P_C10.
                      ENDIF.
                    ENDIF.  "IF P_C9 IS INITIAL.
                  ENDIF.  "IF P_C8 IS INITIAL.
                ENDIF. "IF P_C7 IS INITIAL.
              ENDIF. "IF P_C6 IS INITIAL.
            ENDIF.  "IF P_C5 IS INITIAL.
          ENDIF. "IF P_C4 IS INITIAL.
        ENDIF. "IF P_C3 IS INITIAL.
      ENDIF. "IF P_C2 IS INITIAL.
    ENDIF. " IF P_C1 IS INITIAL.
    MODIFY SCREEN.
  ENDLOOP.


  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_STAT1'
      values = it_stat.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_STAT2'
      values = it_stat.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_STAT3'
      values = it_stat.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_STAT4'
      values = it_stat.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_STAT5'
      values = it_stat.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_STAT6'
      values = it_stat.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_STAT7'
      values = it_stat.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_STAT8'
      values = it_stat.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_STAT9'
      values = it_stat.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_STAT10'
      values = it_stat.




ENDFORM.

FORM pf_2status_set USING extab TYPE slis_t_extab.

  SET PF-STATUS 'Z_PF_EG_MASS2'.

ENDFORM.

FORM user_command_2 USING okcode TYPE sy-ucomm
                  CHANGING l_selfield TYPE slis_selfield.

  CASE okcode.
*Save Output in custom table ZJVA_CHG_ID
    WHEN 'EXE2'.
      PERFORM sub_insert_chgid.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE PROGRAM.

    WHEN 'EXIT'.
      LEAVE PROGRAM.

  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SUB_VALIDATE_NPARTNERS
*&---------------------------------------------------------------------*
*& Throw Error message incase New Partner is Repeated
*&---------------------------------------------------------------------*
FORM sub_validate_npartners .

  DATA: lv_old TYPE i,
        lv_new TYPE i.

  CLEAR it_npartner[].

  PERFORM sub_add_partners USING p_nptr1.
  PERFORM sub_add_partners USING p_nptr2.
  PERFORM sub_add_partners USING p_nptr3.
  PERFORM sub_add_partners USING p_nptr4.
  PERFORM sub_add_partners USING p_nptr5.
  PERFORM sub_add_partners USING p_nptr6.
  PERFORM sub_add_partners USING p_nptr7.
  PERFORM sub_add_partners USING p_nptr8.
  PERFORM sub_add_partners USING p_nptr9.
  PERFORM sub_add_partners USING p_nptr10.

  SORT it_npartner  BY partner.

  DESCRIBE TABLE it_npartner LINES lv_old.

  DELETE ADJACENT DUPLICATES FROM it_npartner  COMPARING partner.

  DESCRIBE TABLE it_npartner LINES lv_new.
  IF lv_old <> lv_new.
    SET CURSOR FIELD 'P_NPTR1'.
    MESSAGE e000(zmsg) WITH 'Dont repeat New Partners'(043).
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SUB_ADD_PARTNERS
*&---------------------------------------------------------------------*
*& Collect all New Partners
*&---------------------------------------------------------------------*
FORM sub_add_partners  USING    p_p_nptr1 TYPE t8jo-partn.

  IF NOT p_p_nptr1 IS INITIAL AND p_p_nptr1 NE 'PDC'.

    SELECT  partn
              UP TO 1 ROWS
              FROM t8jo
              INTO lv_partn
              WHERE partn = p_p_nptr1.
    ENDSELECT.
    IF sy-subrc NE 0.
      SET CURSOR FIELD 'P_P_NPTR1'.
      MESSAGE e000(zmsg) WITH 'Invalid Partner'(037).
    ENDIF.

    wa_npartner = p_p_nptr1.
    APPEND wa_npartner TO it_npartner.
    CLEAR wa_npartner.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SUB_INSERT_CHGID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM sub_insert_chgid .

  DATA: lwa_zjva_chg_id TYPE zjva_chg_id.

  DATA: lv_count  TYPE i VALUE 1.

  DELETE it_slist WHERE status  = 'E'.

  IF it_slist[] IS NOT INITIAL.
*Get the Change ID

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZJV_CHGID'
*       QUANTITY                = '1'
*       SUBOBJECT               = ' '
*       TOYEAR                  = '0000'
*       IGNORE_BUFFER           = ' '
      IMPORTING
        number                  = v_chg_id
*       QUANTITY                =
*       RETURNCODE              =
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    LOOP AT it_slist  ASSIGNING <fs_slist>.

      lwa_zjva_chg_id-chgid = v_chg_id.
      lwa_zjva_chg_id-chgdate = sy-datum.
      lwa_zjva_chg_id-chgtime = sy-uzeit.
      lwa_zjva_chg_id-userid = sy-uname.
      lwa_zjva_chg_id-zcount = lv_count.
      MOVE-CORRESPONDING <fs_slist> TO lwa_zjva_chg_id.
      lwa_zjva_chg_id-chgstatus = 'N'.
      INSERT zjva_chg_id FROM lwa_zjva_chg_id.
      CLEAR: lwa_zjva_chg_id.
      lv_count = lv_count + 1.
    ENDLOOP.

    MESSAGE i000(zmsg) WITH 'Change id'(034) v_chg_id 'created'(035).

    LEAVE TO SCREEN 0.
  ELSE.
    MESSAGE i000(zmsg) WITH 'No Records to process'(045) .
    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.
