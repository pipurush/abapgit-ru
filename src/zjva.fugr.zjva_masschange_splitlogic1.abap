FUNCTION ZJVA_MASSCHANGE_SPLITLOGIC1.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_SPLITSELTAB) TYPE  ZJVA_SPLITSELTYPE
*"     REFERENCE(IV_OLDPART) TYPE  JV_PARTN
*"     REFERENCE(IV_EDATE) TYPE  SYDATUM
*"     REFERENCE(IV_PARTNERNAMES) TYPE  ZJVA_PARTNERNAMESTYPE
*"     REFERENCE(IV_DESCP) TYPE  LTEXT_8JF
*"  EXPORTING
*"     REFERENCE(EV_NEWSPLITTAB) TYPE  ZJVA_NEWSPLITTYPE
*"  CHANGING
*"     REFERENCE(IV_ALLPARTNERS) TYPE  ZJVA_T8JQTYPE
*"     REFERENCE(IV_SPLITTAB) TYPE  ZJVA_SPLITTABTYPE
*"--------------------------------------------------------------------
  FIELD-SYMBOLS:<fs_split>     TYPE zjva_splittab,
                <fs_new_split> TYPE zjva_newsplittab.
  TYPES: BEGIN OF ty_t8jg,   "'1
           bukrs TYPE t8jg-bukrs,      "Company Code
           vname TYPE t8jg-vname,    "Joint venture
           etype TYPE t8jg-etype,   "Equity type
           fdate TYPE t8jg-fdate,      "Inverted Valid from Date
           egrup TYPE t8jg-egrup,  "Equity group
         END OF ty_t8jg.
  TYPES : BEGIN OF ty_vname,
            vname TYPE jv_joa,
          END OF ty_vname.
  DATA: it_t8jg  TYPE STANDARD TABLE OF ty_t8jg,
        it_vname TYPE STANDARD TABLE OF ty_vname,
        wa_vname TYPE ty_vname,
        wa_t8jg  TYPE ty_t8jg.

  DATA: lv_oldsplit      TYPE int2,
        lv_bukrs         TYPE     bukrs,
        ls_partnername   TYPE zjva_partnernames,
        lv_oldsplitshare TYPE jv_eshare,
        wa_splitseltab   TYPE zjva_splitseltab.

* read the CC and split info sent from selection screen
  READ TABLE iv_splitseltab INTO wa_splitseltab INDEX 1.
  IF sy-subrc  = 0 .
    lv_oldsplit = wa_splitseltab-old_partsplit.
    lv_bukrs = wa_splitseltab-bukrs.
  ENDIF.

* Select the operative,non perative shares
  SELECT      bukrs,
              joa,
              egrup,
              opshare,
              nopshare
      FROM   t8j9a
      INTO TABLE @DATA(lt_t8j9a)
      FOR ALL ENTRIES IN @iv_splittab
      WHERE bukrs = @lv_bukrs
      AND joa = @iv_splittab-vname.
*      AND egrup = @iv_splittab-egrup.
  IF sy-subrc = 0 .
    SORT lt_t8j9a BY bukrs joa egrup.
  ENDIF.

* Find out the  dates of the equity grpoups
  DATA(lt_records) = iv_splittab.
  SORT lt_records BY  vname etype .
  DELETE ADJACENT DUPLICATES FROM lt_records COMPARING vname etype .
  IF lt_records[] IS NOT INITIAL.
    SELECT bukrs
    vname
    etype
    fdate
    egrup
      FROM t8jg
      INTO TABLE it_t8jg
      FOR ALL ENTRIES IN lt_records
      WHERE bukrs = lv_bukrs
      AND  vname = lt_records-vname.
*      AND  etype = lt_records-etype.
    IF sy-subrc = 0 .
      SORT  it_t8jg BY bukrs vname egrup .
    ENDIF.
  ENDIF.

* Find out the JV for whoch groups don't have dates
  LOOP AT  lt_t8j9a INTO DATA(lw_vname) .
    READ TABLE it_t8jg INTO DATA(lw_group) WITH KEY bukrs = lv_bukrs
                                                vname = lw_vname-joa
                                                egrup = lw_vname-egrup
                                                BINARY SEARCH.
    IF sy-subrc  NE 0 .
      READ TABLE iv_splittab ASSIGNING <fs_split> WITH KEY  vname = lw_vname-joa.
      IF sy-subrc  = 0 .
        <fs_split>-indicator = 'E'.
        MODIFY iv_splittab  FROM <fs_split> TRANSPORTING indicator WHERE vname = lw_vname-joa.
*      wa_vname-vname = lw_vname-joa.
*      APPEND wa_vname TO it_vname.
*      CLEAR wa_vname.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT it_vname BY vname .
  DELETE iv_allpartners WHERE partn EQ iv_oldpart.
  SORT iv_allpartners BY bukrs vname egrup partn.

* New partner and their split logic
  LOOP AT iv_splittab ASSIGNING <fs_split>.
    IF <fs_split>-indicator EQ 'E'.
      APPEND INITIAL LINE TO ev_newsplittab ASSIGNING  <fs_new_split>.
      <fs_new_split>-bukrs = lv_bukrs.
      <fs_new_split>-vname = <fs_split>-vname.
      <fs_new_split>-etype = <fs_split>-etype.
      <fs_new_split>-vtext = <fs_split>-vtext.
      <fs_new_split>-message = TEXT-001.
      <fs_new_split>-status = 'E'.
    ELSE.
      lv_oldsplitshare = <fs_split>-eqshare / 100 *  lv_oldsplit.

      READ TABLE lt_t8j9a INTO DATA(wa_t8j9a) WITH KEY bukrs = lv_bukrs
                                                       joa = <fs_split>-vname
                                                       egrup = <fs_split>-egrup
                                                       BINARY SEARCH .
      IF sy-subrc = 0 .
        LOOP AT iv_splitseltab INTO wa_splitseltab .
          APPEND INITIAL LINE TO ev_newsplittab ASSIGNING  <fs_new_split>.
          <fs_new_split>-new_partner = wa_splitseltab-new_partner.
          <fs_new_split>-new_share = lv_oldsplitshare / 100 * wa_splitseltab-split.
          READ TABLE iv_allpartners INTO DATA(ls_partners) WITH KEY bukrs = lv_bukrs
                                                   vname = <fs_split>-vname
                                                   egrup = <fs_split>-egrup
                                                   partn = <fs_new_split>-new_partner
                                                   BINARY SEARCH.
          IF sy-subrc = 0 .
            <fs_new_split>-new_share = <fs_new_split>-new_share + ls_partners-eqshare.
            CLEAR ls_partners.
          ENDIF.
          READ TABLE iv_partnernames INTO ls_partnername WITH KEY partner = wa_splitseltab-new_partner BINARY SEARCH .
          IF sy-subrc  = 0 .
            <fs_new_split>-newpart_name = ls_partnername-name_org1.
          ENDIF.
          CLEAR ls_partnername.
          READ TABLE iv_partnernames INTO ls_partnername WITH KEY partner = <fs_split>-opartn BINARY SEARCH .
          IF sy-subrc  = 0 .
            <fs_new_split>-oldpart_name = ls_partnername-name_org1.
          ENDIF.

          <fs_new_split>-bukrs = lv_bukrs.
          <fs_new_split>-vname = <fs_split>-vname.
          <fs_new_split>-etype = <fs_split>-etype.
          <fs_new_split>-old_partner = <fs_split>-opartn.
          <fs_new_split>-old_share = <fs_split>-eqshare.
          <fs_new_split>-old_sharsplit =  lv_oldsplitshare.
          IF <fs_split>-vfrom LT iv_edate.
            IF <fs_split>-vto GT iv_edate.
              <fs_new_split>-edate = iv_edate.
            ENDIF.
          ELSE.
            <fs_new_split>-edate = <fs_split>-vfrom.
          ENDIF.
          <fs_new_split>-egrup = <fs_split>-egrup.
          <fs_new_split>-vtext = <fs_split>-vtext.
          <fs_new_split>-egtxt = <fs_split>-egtxt .
          <fs_new_split>-descp = iv_descp.
          <fs_new_split>-seleffdate = iv_edate.
          <fs_new_split>-equitygrpopsh = wa_t8j9a-opshare.
          <fs_new_split>-equitygrpnopsh = wa_t8j9a-nopshare.
          <fs_new_split>-oldsplitshare = lv_oldsplitshare.
          <fs_new_split>-vfrom = <fs_split>-vfrom.
          <fs_new_split>-vto = <fs_split>-vto.
          <fs_new_split>-oldremsplitshare = <fs_split>-eqshare / 100 * ( 100  - lv_oldsplit ).
* Begin of change CSC9463841 spalit 06/30/2020
          IF wa_splitseltab-suspense IS INITIAL AND wa_splitseltab-nosuspense IS INITIAL AND wa_splitseltab-opsuspense IS INITIAL .
            <fs_new_split>-susstat = 'N'.
          ELSEIF wa_splitseltab-suspense IS NOT INITIAL.
            <fs_new_split>-susstat = wa_splitseltab-suspense.
          ELSEIF wa_splitseltab-nosuspense IS NOT INITIAL.
            <fs_new_split>-susstat = wa_splitseltab-nosuspense.
          ELSEIF wa_splitseltab-opsuspense IS NOT INITIAL.
            <fs_new_split>-susstat = wa_splitseltab-opsuspense.
          ENDIF.
* End of change CSC9463841 spalit 06/30/2020
          CLEAR: wa_splitseltab,ls_partnername .
          IF  <fs_new_split> IS  ASSIGNED.
            UNASSIGN <fs_new_split>.
          ENDIF.

        ENDLOOP.
      ENDIF.
    ENDIF.
    CLEAR:lv_oldsplitshare,wa_t8j9a.
  ENDLOOP.

ENDFUNCTION.
