*&---------------------------------------------------------------------*
*& Include          ZJVAN_JIB_AR_STMNT_PRC_01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Author          : Divya Cheruvathari (DCHERUVATHAR)
* Creation Date   : 20-02-2019
* Transaction     : ZJVA_AR_STATEMENT
* Technical design: TSD_F01_11.2.6_AR Statement for JIB
* Description     : Program generates AR Statement in PDF format for
*                   each Customer
*----------------------------------------------------------------------*
* Modification Information (Most Recent on Top)
*----------------------------------------------------------------------*
* Date            : 20-02-2019
* Author          : Divya Cheruvathari (DCHERUVATHAR)
* Change request  : N/A
* Transport number: S4DK900328
* Description     : Initial Development
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Date            : 09/12/2019
* Author          : Ajitesh Kumar
* Change request  : DF-849
* Transport number: S4DK902780
* Description     : Owner Number added above Customer Address area.
*                   Payment terms logic changed from NT* to whatever
*                   we coming. Debit & Credit logic implemented for
*                   amount in the current charges. Unapplid cash
*                   logic changed to filter on doc type DZ & DA. Past
*                   due dys logic updated.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Date            : 12/05/2019
* Author          : Ajitesh Kumar
* Change request  : DF-1756
* Transport number: S4DK904358 S4DK904506 S4DK904548
* Description     : Description changes, invoice and pay credit logic modified.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form GET_FILE
*&---------------------------------------------------------------------*
*& F4 for File Path
*&---------------------------------------------------------------------*
FORM f4_file_path .
* Local - Data Declaration
  DATA: l_title TYPE string,
        l_path  TYPE string.

* Get Directory path in F4
  l_title = TEXT-001.
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title         = l_title
    CHANGING
      selected_folder      = l_path
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc EQ 0.
    p_path = l_path.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCREEN_VALIDATE
*&---------------------------------------------------------------------*
*& Validate Selection-Screen
*&---------------------------------------------------------------------*
FORM screen_validate .
* Company code validation
  SELECT bukrs
    FROM t001
    UP TO 1 ROWS
    INTO g_bukrs
    WHERE bukrs IN s_bukrs.
  ENDSELECT.
  IF sy-subrc NE 0.
    MESSAGE TEXT-m01 TYPE c_e.
  ENDIF.

* Customer validation
  SELECT kunnr
    FROM kna1
    UP TO 1 ROWS
    INTO g_kunnr
    WHERE kunnr IN s_kunnr.
  ENDSELECT.
  IF sy-subrc NE 0.
    MESSAGE TEXT-m02 TYPE c_e.
  ENDIF.

* GL Account validation
  SELECT saknr
    FROM skb1
    UP TO 1 ROWS
    INTO g_hkont
    WHERE saknr IN s_hkont.
  ENDSELECT.
  IF sy-subrc NE 0.
    MESSAGE TEXT-m03 TYPE c_e.
  ENDIF.

  CLEAR: g_bukrs, g_kunnr, g_hkont.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& Get Form Data
*&---------------------------------------------------------------------*
FORM get_data .
* Local - Constants Declaration
  CONSTANTS: lc_i     TYPE c VALUE 'I',
             lc_eq(2) TYPE c VALUE 'EQ'.

* Local - Data Declaration
  DATA: lr_kunnr  TYPE RANGE OF kunnr,
        lwa_kunnr LIKE LINE OF lr_kunnr.

* Local - Field-Symbols Declaration
  FIELD-SYMBOLS: <lfs_knb1> TYPE t_knb1.

* Initialization
  REFRESH: it_bsid[], it_knb1[], it_t052[], it_but000[], it_but020[], it_adrc[].

* Get Accounting document data
  SELECT bukrs kunnr umskz gjahr belnr budat bldat
         waers blart monat bschl shkzg dmbtr wrbtr
         hkont zfbdt zterm rebzg btype
    FROM bsid
    INTO TABLE it_bsid
    WHERE bukrs IN s_bukrs
    AND   kunnr IN s_kunnr
    AND   hkont IN s_hkont.
  IF sy-subrc EQ 0.
    SORT it_bsid BY bukrs kunnr rebzg.

*   Get Customer/Company code data
    SELECT kunnr bukrs zterm
      FROM knb1
      INTO TABLE it_knb1
      FOR ALL ENTRIES IN it_bsid
      WHERE kunnr EQ it_bsid-kunnr
      AND   bukrs EQ it_bsid-bukrs
      ORDER BY PRIMARY KEY.
    IF sy-subrc EQ 0.
*     Get Customer Baseline of Payment
      SELECT zterm ztag1
        FROM t052
        INTO TABLE it_t052
        FOR ALL ENTRIES IN it_bsid
        WHERE zterm EQ it_bsid-zterm.
      IF sy-subrc EQ 0.
        SORT it_t052 BY zterm.
      ENDIF.
*     Get Payment terms,
      SELECT spras zterm
        vtext
        FROM tvzbt
        INTO TABLE it_tvzbt
        FOR ALL ENTRIES IN it_bsid
        WHERE zterm EQ it_bsid-zterm
        AND spras EQ 'EN'.
      IF sy-subrc EQ 0.
        SORT it_tvzbt BY zterm.
      ENDIF.
    ENDIF.

*   Begin of change by Ajitesh

**   Collect all the CompanyCode (BUKRS) & Customer (KUNNR) into a range table
*    LOOP AT it_knb1 ASSIGNING <lfs_knb1>.
*      lwa_kunnr-sign   = lc_i.
*      lwa_kunnr-option = lc_eq.
**     Company Code - Convert to KUNNR internal format
*      lwa_kunnr-low = <lfs_knb1>-bukrs.
*      PERFORM conv_kunnr_input CHANGING lwa_kunnr-low.
*      COLLECT lwa_kunnr INTO lr_kunnr.
**     Customer
*      lwa_kunnr-low = <lfs_knb1>-kunnr.
*      COLLECT lwa_kunnr INTO lr_kunnr.
*      CLEAR: lwa_kunnr.
*    ENDLOOP.
*    IF <lfs_knb1> IS ASSIGNED.
*      UNASSIGN <lfs_knb1>.
*    ENDIF.
*
**   Exit if no partners
*    IF lr_kunnr[] IS INITIAL.
*      RETURN.
*    ENDIF.

**   Get Partner data (Partner = CompanyCode / Customer)
*    SELECT partner name_org1 addrcomm
*      FROM but000
*      INTO TABLE it_but000
*      WHERE partner IN lr_kunnr
*      ORDER BY PRIMARY KEY.
*    IF sy-subrc EQ 0.
**     Get Partner address number
*      SELECT partner addrnumber addr_valid_from addr_valid_to
*        FROM but020
*        INTO TABLE it_but020
*        FOR ALL ENTRIES IN it_but000
*        WHERE partner EQ it_but000-partner.
*      IF sy-subrc EQ 0.
*        SORT it_but020 BY addr_valid_from addr_valid_to DESCENDING. "Important
*
**       Get Partner address
*        SELECT addrnumber city1 post_code1 street region
*          FROM adrc
*          INTO TABLE it_adrc
*          FOR ALL ENTRIES IN it_but020
*          WHERE addrnumber EQ it_but020-addrnumber.
*        IF sy-subrc EQ 0.
*          SORT it_adrc BY addrnumber.
*        ENDIF.
*      ENDIF.
*    ENDIF.
    SELECT adrnr addr_type custid vendid
            INTO TABLE  it_/pra/bp_ad
            FROM /pra/bp_ad
            FOR ALL ENTRIES IN it_knb1
            WHERE addr_type EQ 'Z2'
            AND ( custid EQ it_knb1-kunnr
            OR vendid EQ it_knb1-kunnr ).
    SELECT kunnr adrnr
            INTO TABLE it_kna1
            FROM kna1
            FOR ALL ENTRIES IN it_knb1
            WHERE kunnr EQ it_knb1-kunnr.
    IF it_/pra/bp_ad[] IS NOT INITIAL.
      SELECT addrnumber name1 name2 street
                     str_suppl3 city1 region post_code1
                INTO TABLE it_adrc
                FROM adrc
                FOR ALL ENTRIES IN it_/pra/bp_ad
                WHERE addrnumber EQ it_/pra/bp_ad-adrnr.
    ENDIF.
    IF it_kna1[] IS NOT INITIAL.
      SELECT addrnumber name1 name2 street
                     str_suppl3 city1 region post_code1
                APPENDING TABLE it_adrc
                FROM adrc
                FOR ALL ENTRIES IN it_kna1
                WHERE addrnumber EQ it_kna1-adrnr.
    ENDIF.
    SORT it_adrc BY addrnumber.

*    End of change by Ajitesh
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESS_DATA
*&---------------------------------------------------------------------*
*& Process Data
*&---------------------------------------------------------------------*
FORM process_data .
* Local - Constants Declaration
  CONSTANTS: lc_nt(2)    TYPE c      VALUE 'NT',
             lc_0001     TYPE dzterm VALUE '0001',
             lc_blart_dc TYPE blart  VALUE 'DC',
             lc_blart_dr TYPE blart  VALUE 'DR',
             lc_blart_zl TYPE blart  VALUE 'ZL',
             lc_blart_dg TYPE blart  VALUE 'DG',
             lc_blart_dz TYPE blart  VALUE 'DZ',
             lc_blart_zf TYPE blart  VALUE 'ZF',
             lc_blart_zj TYPE blart  VALUE 'ZJ',
             lc_blart_ck TYPE blart  VALUE 'CK',
             lc_blart_zb TYPE blart  VALUE 'ZB',
             lc_blart_dk TYPE blart  VALUE 'DK',
             lc_blart_da TYPE blart  VALUE 'DA'.
*             lc_blart_bt TYPE blart  VALUE 'BT'.                         "S4DK905544-Rollback threshold changes



* Local - Data Declaration
  DATA: lit_item        TYPE zjva_t_cust_item_data,
        lit_partner_adr TYPE zjva_t_partner_addr,
        lwa_misc_data   TYPE zjva_s_cust_misc_data,
        l_fm_name       TYPE rs38l_fnam,
        l_inv_num(16)   TYPE c,
        l_tot_bal       TYPE wrbtr,
        l_kunnr         TYPE kunnr,
        l_ztag1         TYPE dztage,
        l_nt(2)         TYPE c
        .

* Local - Field-Symbols Declaration
  FIELD-SYMBOLS: <lfs_knb1>  TYPE t_knb1,
                 <lfs_bsid>  TYPE t_bsid,
                 <lfs_bsid2> TYPE t_bsid,
                 <lfs_t052>  TYPE t_t052,
                 <lfs_tvzbt> TYPE t_tvzbt,
                 <lfs_item>  TYPE zjva_s_cust_item_data.

* Open Form
  PERFORM open_form CHANGING l_fm_name.
* A single PDF form is generated for all the Customer/CompanyCode
  LOOP AT it_knb1 ASSIGNING <lfs_knb1>.
*   Company Code (PDC) Address Data
    l_kunnr = <lfs_knb1>-bukrs.
    PERFORM conv_kunnr_input CHANGING l_kunnr.
    PERFORM get_partner_address USING    l_kunnr
                                         abap_true  "PDC Address
                                         abap_false
                                CHANGING lit_partner_adr.

*   Customer Address Data
    PERFORM get_partner_address USING    <lfs_knb1>-kunnr
                                         abap_false
                                         abap_true  "Customer Address
                                CHANGING lit_partner_adr.

*   Customer Days from Baseline Date for Payment      "Change by Ajitesh
    READ TABLE it_t052 ASSIGNING <lfs_t052> WITH KEY zterm = <lfs_knb1>-zterm
                                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      l_ztag1 = <lfs_t052>-ztag1.
      UNASSIGN <lfs_t052>.
    ENDIF.

*   Line Items
    LOOP AT it_bsid ASSIGNING <lfs_bsid> WHERE bukrs EQ <lfs_knb1>-bukrs
                                         AND   kunnr EQ <lfs_knb1>-kunnr.

*     Calculate the past due amounts (-0 / 0-30 / 30-60 / 60-90 / 90+)
      PERFORM past_due_amt USING    <lfs_bsid>
                                    l_ztag1
                           CHANGING lwa_misc_data.

*     Collect all the data for the same invoice number in a single line
      IF <lfs_bsid>-blart EQ lc_blart_dc.
        l_inv_num = <lfs_bsid>-monat && <lfs_bsid>-gjahr && <lfs_bsid>-kunnr.
      ELSE.
        l_inv_num = <lfs_bsid>-belnr.
      ENDIF.
*      l_inv_num = <lfs_bsid>-monat && <lfs_bsid>-gjahr && <lfs_bsid>-kunnr.
      READ TABLE lit_item ASSIGNING <lfs_item> WITH KEY inv_num = l_inv_num.
      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO lit_item ASSIGNING <lfs_item>.
      ENDIF.

      IF <lfs_item> IS ASSIGNED.
        <lfs_item>-inv_num  = l_inv_num.
        <lfs_item>-inv_date = <lfs_bsid>-bldat. "budat.
*        <lfs_item>-inv_desc = TEXT-t04. "Invoice                "SOC by Ajitesh Date-05/12/2019
        IF <lfs_bsid>-blart EQ lc_blart_zb.
          <lfs_item>-inv_desc = TEXT-t07. "Netted Revenue
        ELSEIF <lfs_bsid>-blart EQ lc_blart_ck.
          <lfs_item>-inv_desc = TEXT-t08. "CC Applied
*        ELSEIF <lfs_bsid>-blart EQ lc_blart_bt.
*          <lfs_item>-inv_desc = TEXT-t09. "Threshold            "Ticket 9441706 "S4DK905544-Rollback threshold changes
        ELSE.
          <lfs_item>-inv_desc = TEXT-t04. "Invoice
        ENDIF.
        <lfs_item>-waers    = <lfs_bsid>-waers.                  "EOC by Ajitesh Date-05/12/2019

        IF <lfs_bsid>-blart EQ lc_blart_dc  "Get Invoice Amt only for Doc Type = 'DC'/'DR'/'ZL'
        OR <lfs_bsid>-blart EQ lc_blart_dr  "Additionally included ZB, ZF, ZJ, DA
        OR <lfs_bsid>-blart EQ lc_blart_zl
*        OR <lfs_bsid>-blart EQ lc_blart_zb "SOC and EOC by Ajitesh Date-05/12/2019
        OR <lfs_bsid>-blart EQ lc_blart_zf
        OR <lfs_bsid>-blart EQ lc_blart_zj
        OR <lfs_bsid>-blart EQ lc_blart_da.
*        OR <lfs_bsid>-blart EQ lc_blart_bt.                      "Ticket 9441706 "S4DK905544-Rollback threshold changes
          IF <lfs_bsid>-shkzg EQ 'S'.
            <lfs_item>-inv_amt = <lfs_item>-inv_amt + <lfs_bsid>-wrbtr.
            l_tot_bal = l_tot_bal + <lfs_bsid>-wrbtr. "Change by Ajitesh
          ELSEIF <lfs_bsid>-shkzg EQ 'H'.
            <lfs_item>-inv_amt = <lfs_item>-inv_amt - <lfs_bsid>-wrbtr.
            l_tot_bal = l_tot_bal - <lfs_bsid>-wrbtr. "Change by Ajitesh
          ENDIF.
        ENDIF.

*        IF <lfs_bsid>-rebzg IS NOT INITIAL "Get Pay Credit only for Doc Type = 'DG'/'DZ' where REBZG <> space
*        AND ( <lfs_bsid>-blart EQ lc_blart_dg
*        OR    <lfs_bsid>-blart EQ lc_blart_dz ).
        IF <lfs_bsid>-blart EQ lc_blart_dg
        OR <lfs_bsid>-blart EQ lc_blart_zb  "SOC and EOC by Ajitesh Date-05/12/2019
        OR ( <lfs_bsid>-blart EQ lc_blart_ck AND <lfs_bsid>-btype EQ 3 AND <lfs_bsid>-umskz EQ 3 AND <lfs_bsid>-bschl EQ 19 ). "SOC and EOC by Ajitesh Date-06/12/2019
          IF <lfs_bsid>-shkzg EQ 'S'.                                                                   "SOC by Ajitesh 13/12/2019
            <lfs_item>-pay_credit = <lfs_item>-pay_credit + <lfs_bsid>-wrbtr."( <lfs_bsid>-wrbtr * -1 ).
            l_tot_bal = l_tot_bal + <lfs_bsid>-wrbtr.
          ELSEIF <lfs_bsid>-shkzg EQ 'H'.
            <lfs_item>-pay_credit = <lfs_item>-pay_credit - <lfs_bsid>-wrbtr.
            l_tot_bal = l_tot_bal - <lfs_bsid>-wrbtr.
          ENDIF.
        ELSE.
          LOOP AT it_bsid ASSIGNING <lfs_bsid2> WHERE  bukrs EQ <lfs_bsid>-bukrs
                                                         AND   kunnr EQ <lfs_bsid>-kunnr
                                                         AND  rebzg EQ <lfs_bsid>-belnr.
            IF <lfs_bsid2>-shkzg EQ 'S'.
              <lfs_item>-pay_credit = <lfs_item>-pay_credit + <lfs_bsid2>-wrbtr."( <lfs_bsid>-wrbtr * -1 ). "SOC TR-S4DK904548
              l_tot_bal = l_tot_bal + <lfs_bsid2>-wrbtr. "Change by Ajitesh
            ELSEIF <lfs_bsid2>-shkzg EQ 'H'.
              <lfs_item>-pay_credit = <lfs_item>-pay_credit - <lfs_bsid2>-wrbtr.
              l_tot_bal = l_tot_bal - <lfs_bsid2>-wrbtr. "Change by Ajitesh                                 "EOC TR-S4DK904548
            ENDIF.
          ENDLOOP.                                                                                      "EOC by Ajitesh 13/12/2019
        ENDIF.
*        ENDIF.

        <lfs_item>-curr_bal = <lfs_item>-inv_amt + <lfs_item>-pay_credit.

*        ADD <lfs_item>-curr_bal TO l_tot_bal. "Add up the Current Balance for all the line items "Change by Ajitesh
        UNASSIGN <lfs_item>.
      ENDIF.
      CLEAR: l_inv_num.
    ENDLOOP.
    IF <lfs_bsid> IS ASSIGNED.
      UNASSIGN <lfs_bsid>.
    ENDIF.

*   Delete those lines with zero amount
    DELETE lit_item WHERE inv_amt    EQ 0
                    AND   pay_credit EQ 0.

*   Payment Terms
*    CASE <lfs_knb1>-zterm.
*      WHEN lc_0001. "'0001'
*        lwa_misc_data-pay_term = TEXT-t03.
*      WHEN OTHERS. "'NT10' / 'NT15' / 'NT20' / 'NT30'
*        l_nt = <lfs_knb1>-zterm.
*        IF l_nt EQ lc_nt.
*          lwa_misc_data-pay_term = TEXT-t01 && | | && <lfs_knb1>-zterm+2(2) && | | && TEXT-t02.
*        ENDIF.
*    ENDCASE.
    READ TABLE it_tvzbt ASSIGNING <lfs_tvzbt> WITH KEY zterm = <lfs_knb1>-zterm
                                                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      lwa_misc_data-pay_term = <lfs_tvzbt>-vtext.  "Change by Ajitesh
    ENDIF.

    lwa_misc_data-tot_bal_due = l_tot_bal + lwa_misc_data-unapp_cash.


    SORT lit_item ASCENDING BY inv_date inv_num ASCENDING. "Change by AJitesh

    IF lit_item[] IS INITIAL.                   "Form not generated if all amount fields has no values
      IF lwa_misc_data-unapp_cash IS INITIAL
      AND lwa_misc_data-tot_bal_due IS INITIAL
      AND lwa_misc_data-curr_charges IS INITIAL
      AND lwa_misc_data-days_30 IS INITIAL
      AND lwa_misc_data-days_60 IS INITIAL
      AND lwa_misc_data-days_90 IS INITIAL
      AND lwa_misc_data-days_90plus IS INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Generate Customer AR Form - PDF
    PERFORM generate_form USING lit_item
                                lit_partner_adr
                                lwa_misc_data
                                l_fm_name.

*   Clear all the parameters for the Next Customer/CompanyCode data
    REFRESH: lit_item[], lit_partner_adr[].
    CLEAR: lwa_misc_data, l_tot_bal, l_kunnr, l_ztag1, l_nt.
  ENDLOOP.
  IF <lfs_knb1> IS ASSIGNED.
    UNASSIGN <lfs_knb1>.
  ENDIF.

* Close the adobe form
  PERFORM close_form.

* Save the PDF file to the directory
  PERFORM save_pdf.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_PARTNER_ADDRESS
*&---------------------------------------------------------------------*
*& Populate Partner Address
*&---------------------------------------------------------------------*
*&      --> I_KUNNR
*&      --> I_PDC_ADR
*&      --> I_CUST_ADR
*&      <-- C_ADDRESS
*&---------------------------------------------------------------------*
FORM get_partner_address  USING    i_kunnr    TYPE kunnr
                                   i_pdc_adr  TYPE flag
                                   i_cust_adr TYPE flag
                          CHANGING ct_address TYPE zjva_t_partner_addr.
* Local - Field-Symbols Declaration
  FIELD-SYMBOLS: <lfs_but000>     TYPE t_but000,
                 <lfs_but020>     TYPE t_but020,
                 <lfs_adrc>       TYPE t_adrc,
                 <lfs_address>    TYPE zjva_s_partner_addr,
                 <lfs_/pra/bp_ad> TYPE t_/pra/bp_ad,
                 <lfs_kna1>       TYPE t_kna1.
  DATA: l_adrnr TYPE ad_addrnum.

* Fetch address data from BUT000 - BUT020 - ADRC as per the below logic
*  READ TABLE it_but000 ASSIGNING <lfs_but000> WITH KEY partner = i_kunnr
*                                              BINARY SEARCH.

*  IF sy-subrc EQ 0.
  APPEND INITIAL LINE TO ct_address ASSIGNING <lfs_address>.
  IF <lfs_address> IS ASSIGNED.
*    <lfs_address>-partner   = i_kunnr.
*    <lfs_address>-name_org1 = <lfs_but000>-name_org1.
*    <lfs_address>-pdc_adr   = i_pdc_adr.
*    <lfs_address>-cust_adr  = i_cust_adr.
*      READ TABLE it_but020 ASSIGNING <lfs_but020> WITH KEY partner = i_kunnr.
*      IF sy-subrc EQ 0.
    IF  i_pdc_adr EQ abap_true.
      <lfs_address>-partner   = i_kunnr.
      <lfs_address>-pdc_adr   = i_pdc_adr.
    ELSEIF i_cust_adr EQ abap_true.
      <lfs_address>-partner   = i_kunnr.
      <lfs_address>-cust_adr  = i_cust_adr.
      READ TABLE it_/pra/bp_ad ASSIGNING <lfs_/pra/bp_ad> WITH KEY custid = i_kunnr.
      IF sy-subrc EQ 0.
        l_adrnr = <lfs_/pra/bp_ad>-adrnr.
      ELSE.
        READ TABLE it_/pra/bp_ad ASSIGNING <lfs_/pra/bp_ad> WITH KEY vendid = i_kunnr.
        IF sy-subrc EQ 0.
          l_adrnr = <lfs_/pra/bp_ad>-adrnr.
        ELSE.
          READ TABLE it_kna1 ASSIGNING <lfs_kna1> WITH KEY kunnr = i_kunnr.
          IF sy-subrc = 0.
            l_adrnr = <lfs_kna1>-adrnr.
          ENDIF.
        ENDIF.
      ENDIF.
      READ TABLE it_adrc ASSIGNING <lfs_adrc> WITH KEY addrnumber = l_adrnr "<lfs_but020>-addrnumber
                                              BINARY SEARCH.
      IF sy-subrc EQ 0.
        <lfs_address>-name1 = <lfs_adrc>-name1.
        <lfs_address>-name2 = <lfs_adrc>-name2.
        <lfs_address>-city1      = <lfs_adrc>-city1.
        <lfs_address>-post_code1 = <lfs_adrc>-post_code1.
        <lfs_address>-street     = <lfs_adrc>-street.
        <lfs_address>-str_suppl3 = <lfs_adrc>-str_suppl3.
        <lfs_address>-region     = <lfs_adrc>-region.
        UNASSIGN <lfs_adrc>.
      ENDIF.
    ENDIF.
*    UNASSIGN <lfs_but020>.
*  ENDIF.
    UNASSIGN <lfs_address>.
  ENDIF.
*UNASSIGN <lfs_but000>.
*ENDIF.
  CLEAR l_adrnr.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PAST_DUE_AMT
*&---------------------------------------------------------------------*
*& Calculate Past Due Amounts
*&---------------------------------------------------------------------*
*&      --> IS_BSID
*&      --> I_ZTAG1
*&      <-- CS_MISC_DATA
*&---------------------------------------------------------------------*
FORM past_due_amt  USING    is_bsid      TYPE t_bsid
                            i_ztag1      TYPE dztage
                   CHANGING cs_misc_data TYPE zjva_s_cust_misc_data.
* Local - Data Declaration
  DATA: l_days   TYPE verzn,
        l_date   TYPE d,
        l_dlate  TYPE i,
        l_amount TYPE wrbtr.

  CONSTANTS: lc_blart_dc TYPE blart  VALUE 'DC',
             lc_blart_dr TYPE blart  VALUE 'DR',
             lc_blart_zl TYPE blart  VALUE 'ZL',
             lc_blart_dg TYPE blart  VALUE 'DG',
             lc_blart_dz TYPE blart  VALUE 'DZ',
             lc_blart_zf TYPE blart  VALUE 'ZF',
             lc_blart_zj TYPE blart  VALUE 'ZJ',
             lc_blart_ck TYPE blart  VALUE 'CK',
             lc_blart_zb TYPE blart  VALUE 'ZB',
             lc_blart_dk TYPE blart  VALUE 'DK',
             lc_blart_da TYPE blart  VALUE 'DA'.

  CLEAR l_amount.
  IF is_bsid-shkzg EQ 'S'.
    l_amount = is_bsid-wrbtr.
  ELSEIF is_bsid-shkzg EQ 'H'.
    l_amount = is_bsid-wrbtr * -1.
  ENDIF.

* Calculate Un-applied Cash
*Begin of change by Ajitesh 30/09/2109
  IF ( is_bsid-rebzg EQ space AND is_bsid-blart EQ lc_blart_dz )
    OR ( is_bsid-blart EQ lc_blart_dk AND is_bsid-btype EQ '01' AND is_bsid-umskz EQ '1' AND is_bsid-bschl EQ '09')
    OR ( is_bsid-blart EQ lc_blart_dk AND is_bsid-btype EQ '24' AND is_bsid-umskz EQ '2' AND is_bsid-bschl EQ '19')
    OR ( is_bsid-blart EQ lc_blart_ck AND is_bsid-btype EQ '02' AND is_bsid-umskz EQ '2' AND is_bsid-bschl EQ '09')
    OR ( is_bsid-blart EQ lc_blart_ck AND is_bsid-btype EQ '02' AND is_bsid-umskz EQ '1' AND is_bsid-bschl EQ '19').
*End of change by Ajitesh 30/09/2109
    ADD l_amount TO cs_misc_data-unapp_cash.
  ENDIF.

*  IF is_bsid-blart EQ lc_blart_dc "Get Amt only for Doc Type = 'DC'/'DR'/'ZL'
*  OR is_bsid-blart EQ lc_blart_dr
*  OR is_bsid-blart EQ lc_blart_zl
*  OR is_bsid-blart EQ lc_blart_zb
*  OR is_bsid-blart EQ lc_blart_zf
*  OR is_bsid-blart EQ lc_blart_zj
*  OR is_bsid-blart EQ 'DG'
*  OR is_bsid-blart EQ 'DA'
*  OR is_bsid-blart EQ 'DZ'
*  OR ( is_bsid-blart EQ lc_blart_dk AND is_bsid-btype EQ '1' AND is_bsid-umskz EQ '1' AND is_bsid-bschl EQ '09')
*  OR ( is_bsid-blart EQ lc_blart_dk AND is_bsid-btype EQ '24' AND is_bsid-umskz EQ '2' AND is_bsid-bschl EQ '19')
*  OR ( is_bsid-blart EQ lc_blart_ck AND is_bsid-btype EQ '2' AND is_bsid-umskz EQ '2' AND is_bsid-bschl EQ '9')
*  OR ( is_bsid-blart EQ lc_blart_ck AND is_bsid-btype EQ '2' AND is_bsid-umskz EQ '1' AND is_bsid-bschl EQ '19').
  l_date = is_bsid-zfbdt + i_ztag1.
  CALL FUNCTION 'ITEM_OVERDUE_DAYS'
    EXPORTING
      key_date     = l_date
      due_date     = sy-datum "is_bsid-budat
    IMPORTING
      overdue_days = l_days.
  l_dlate = l_days * -1.

  IF l_dlate LE 0.      "Current Charges
    ADD l_amount TO cs_misc_data-curr_charges.

  ELSEIF l_dlate GT 0   "Past Due 0-30 Days
    AND  l_dlate LE 30.
    ADD l_amount TO cs_misc_data-days_30.

  ELSEIF l_dlate GT 30  "Past Due 30-60 Days
    AND  l_dlate LE 60.
    ADD l_amount TO cs_misc_data-days_60.

  ELSEIF l_dlate GT 60  "Past Due 60-90 Days
    AND  l_dlate LE 90.
    ADD l_amount TO cs_misc_data-days_90.

  ELSE.                 "Past Due 90+ Days
    ADD l_amount TO cs_misc_data-days_90plus.
  ENDIF.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONV_KUNNR_INPUT
*&---------------------------------------------------------------------*
*& Convert Customer Number to Input Format
*&---------------------------------------------------------------------*
*&      <-- C_KUNNR
*&---------------------------------------------------------------------*
FORM conv_kunnr_input  CHANGING c_kunnr TYPE kunnr.
* Convert KUNNR to input format
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_kunnr
    IMPORTING
      output = c_kunnr.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GENERATE_FORM
*&---------------------------------------------------------------------*
*& Generate PDF Form
*&---------------------------------------------------------------------*
*&      --> IT_ITEM
*&      --> IT_PARTNER_ADR
*&      --> IS_MISC_DATA
*&      --> I_NAME
*&---------------------------------------------------------------------*
FORM generate_form  USING    it_item          TYPE zjva_t_cust_item_data
                             it_partner_adr   TYPE zjva_t_partner_addr
                             is_misc_data     TYPE zjva_s_cust_misc_data
                             i_fm_name        TYPE rs38l_fnam.
* Call the Adobe Form
  CALL FUNCTION i_fm_name
    EXPORTING
      it_item        = it_item
      it_partner_adr = it_partner_adr
      is_misc_data   = is_misc_data
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.
  IF sy-subrc NE 0.
    MESSAGE TEXT-m04 TYPE c_e.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form OPEN_FORM
*&---------------------------------------------------------------------*
*& Open Form
*&---------------------------------------------------------------------*
*&      <-- C_FM_NAME
*&---------------------------------------------------------------------*
FORM open_form  CHANGING c_fm_name TYPE rs38l_fnam.
* Local - Constants Declaration
  CONSTANTS: lc_fname  TYPE fpname   VALUE 'ZJVAF_JIB_ARSTMT_FORM_01',
             lc_device TYPE fpmedium VALUE 'PRINTER',
             lc_m      TYPE c        VALUE 'M'.

* Local - Data Declaration
  DATA: lwa_output_params TYPE sfpoutputparams,
        lwa_usr01         TYPE usr01.

* Get the user-default data
  CALL FUNCTION 'GET_PRINT_PARAM'
    EXPORTING
      i_bname = sy-uname
    IMPORTING
      e_usr01 = lwa_usr01.

  lwa_output_params-device   = lc_device.
  lwa_output_params-dest     = lwa_usr01-spld. "User-default printer
  lwa_output_params-nodialog = abap_true.
  lwa_output_params-preview  = abap_false.
  lwa_output_params-getpdf   = abap_true.
  lwa_output_params-reqnew   = abap_true.
  lwa_output_params-preview  = abap_false.
  lwa_output_params-assemble = abap_true.
  lwa_output_params-bumode   = lc_m.   "This is Bundle Mode
  lwa_output_params-getpdf   = lc_m.   "This should be set to M, compared to regular value X being passed

* Open the Adobe form
  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = lwa_output_params
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.
  IF sy-subrc NE 0.
    MESSAGE TEXT-m04 TYPE c_e.
  ENDIF.

  TRY.
*     Get the generated FM name for the adobe form
      CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
        EXPORTING
          i_name     = lc_fname
        IMPORTING
          e_funcname = c_fm_name.
    CATCH cx_fp_api_repository
          cx_fp_api_usage
          cx_fp_api_internal.
      MESSAGE TEXT-m07 TYPE c_e.
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLOSE_FORM
*&---------------------------------------------------------------------*
*& Close Form
*&---------------------------------------------------------------------*
FORM close_form .
* Close the Adobe form
  CALL FUNCTION 'FP_JOB_CLOSE'
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.
  IF sy-subrc NE 0.
    MESSAGE TEXT-m04 TYPE c_e.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_PDF
*&---------------------------------------------------------------------*
*& Save PDF
*&---------------------------------------------------------------------*
FORM save_pdf .
* Local - Constants Declaration
  CONSTANTS: lc_bin(10) TYPE c VALUE 'BIN',
             lc_slash   TYPE c VALUE '/',
             lc_undsc   TYPE c VALUE '_'.

* Local - Data Declaration
  DATA: lit_pdf    TYPE tfpcontent,
        lwa_pdf    TYPE fpcontent,
        lit_tab    TYPE tsfixml,

        l_len      TYPE i,
        l_filename TYPE string.

* Get the PDF data in RAWSTRING format
  CALL FUNCTION 'FP_GET_PDF_TABLE'
    IMPORTING
      e_pdf_table = lit_pdf.

  READ TABLE lit_pdf INTO lwa_pdf INDEX 1.
  IF sy-subrc EQ 0.
*   Convert RAWSTRING PDF data to BINARY format
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lwa_pdf
      IMPORTING
        output_length = l_len
      TABLES
        binary_tab    = lit_tab.

*   Filename = Directory Path (selection-screen) + '/ARStatement_<timestamp>.pdf'
    l_filename = p_path && lc_slash && TEXT-t05 && lc_undsc && sy-datum && sy-uzeit && TEXT-t06.

*   Download the '.PDF' file
    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        bin_filesize = l_len
        filename     = l_filename
        filetype     = lc_bin
      CHANGING
        data_tab     = lit_tab
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc EQ 0.
      MESSAGE TEXT-m06 TYPE c_s. "Successfully downloaded
    ELSE.
      MESSAGE TEXT-m04 TYPE c_e. "Error while downloading
    ENDIF.
  ENDIF.
ENDFORM.
