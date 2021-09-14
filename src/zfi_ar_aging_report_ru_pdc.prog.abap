*&---------------------------------------------------------------------*
*& ZFI_AR_AGING_REPORT_RU_PDC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Author          : Ajitesh Kumar
* Creation Date   : 14-Feb-2019
* Transaction     : Z_ARAGING
* Technical design: TSD_ETP_I_R01_11.2.6_AR Aging Report_V1.0
* Description     : AR Aging Report
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Date            : 09/12/2019
* Author          : Ajitesh Kumar
* Change request  : DF-769
* Description     : Recon account HKONT derived from BSID and BSAD added
*                   for both venture and non venture format.
*----------------------------------------------------------------------*
REPORT (sy-repid)  no standard page heading
                   line-count 65
                   line-size  132
                   message-id zz.


*----------------------------------------------------------------------*
* Types:
*----------------------------------------------------------------------*
TYPE-POOLS: slis.        "ALV Class

*----------------------------------------------------------------------*
* tables
*----------------------------------------------------------------------*
TABLES: knb1, bsid.

*----------------------------------------------------------------------*
* parameters & select-options
*----------------------------------------------------------------------*
*text-001: Additional Selection Criteria
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS:
   s_bukrs    FOR bsid-bukrs,
   s_kunnr    FOR bsid-kunnr,
   s_hkont    FOR bsid-hkont.

PARAMETERS:
  p_post  LIKE bsid-budat OBLIGATORY,
  p_budat LIKE bsid-budat OBLIGATORY.

PARAMETERS:
   p_vari     LIKE disvariant-variant.       "alv variant

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (30) TEXT-903.
PARAMETERS: p_ven AS CHECKBOX.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* Include files
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS:
  c_i(01)       TYPE c VALUE 'I',    "include
  c_e(01)       TYPE c VALUE 'E',    "exclude
  c_on(01)      TYPE c VALUE 'X',    "on
  c_eq(02)      TYPE c VALUE 'EQ',   "equal
  c_cp(02)      TYPE c VALUE 'CP',   "contains pattern
  c_top_of_page TYPE slis_formname   "top of page
                VALUE 'TOP_OF_PAGE'.

*----------------------------------------------------------------------*
* variables  (global)
*----------------------------------------------------------------------*
DATA:
  g_fieldcat    TYPE slis_t_fieldcat_alv,
  g_layout      TYPE slis_layout_alv,
  g_print       TYPE slis_print_alv,
  g_sort        TYPE slis_t_sortinfo_alv,
  g_group       TYPE slis_t_sp_group_alv,
  g_events      TYPE slis_t_event,
  g_num(2)      TYPE n,

* Data to be displayed
  g_repid       LIKE sy-repid,
  g_top_of_page TYPE slis_t_listheader,
*
  g_save(1)     TYPE c,
  g_exit(1)     TYPE c,
  gx_variant    LIKE disvariant,
  g_variant     LIKE disvariant.

*----------------------------------------------------------------------*
* internal tables & structures
*----------------------------------------------------------------------*
DATA: BEGIN OF t_data OCCURS 0,
        bukrs LIKE bsid-bukrs,
        kunnr LIKE bsid-kunnr,
        hkont LIKE bsid-hkont,   ""SOC and EOC by Ajitesh Date-10/09/2019
        name1 LIKE kna1-name1,
        belnr LIKE bsid-belnr,
        xblnr LIKE bsis-xblnr,
        budat LIKE bsid-budat,
        blart LIKE bsid-blart,
        shkzg LIKE bsid-shkzg,   "H=Credit/ S=Debit
        dmbtr LIKE bsid-dmbtr,
        zfbdt LIKE bsid-zfbdt,   "baseline date
        zterm LIKE knb1-zterm,
        ztag1 LIKE t052-ztag1,
        dlate TYPE i,
* Begin of change DF1255 spalit 10/11/2019
        bldat TYPE bldat,
* End of change DF1255 spalit 10/11/2019
      END OF t_data.

DATA: BEGIN OF t_sum_data OCCURS 0,
        kunnr       LIKE bsid-kunnr,
        name1       LIKE kna1-name1,
        hkont       LIKE bsid-hkont,                        "SOC and EOC by Ajitesh Date-10/09/2019
        bukrs       LIKE bsid-bukrs,
        invoice(16) TYPE c,
        post_pd(7)  TYPE c,
        doc_type(7) TYPE c,                                 "R3DK904360
        total       LIKE bsid-dmbtr,
        current     LIKE bsid-dmbtr,
        curr_days   TYPE i,
        p30         LIKE bsid-dmbtr,
        p60         LIKE bsid-dmbtr,
        p90         LIKE bsid-dmbtr,
        p120        LIKE bsid-dmbtr,
        pother      LIKE bsid-dmbtr,
        regio       LIKE csks-regio,
        ptotal      LIKE bsid-dmbtr,
        l50(1)      TYPE c,
        budat(10),
        dmbtr       LIKE bsid-dmbtr,
* Begin of change DF1255 spalit 10/11/2019
        bldat       TYPE bldat,
* End of change DF1255 spalit 10/11/2019
      END OF t_sum_data.

DATA: BEGIN OF t_vdata OCCURS 0,
        bukrs LIKE bsid-bukrs,
        kunnr LIKE bsid-kunnr,
        vname LIKE bsid-vname,
        vtext LIKE t8jvt-vtext,
        hkont LIKE bsid-hkont,   "SOC and EOC by Ajitesh Date-10/09/2019
        name1 LIKE kna1-name1,
        belnr LIKE bsid-belnr,
        xblnr LIKE bsis-xblnr,
        budat LIKE bsid-budat,
        blart LIKE bsid-blart,
        shkzg LIKE bsid-shkzg,   "H=Credit/ S=Debit
        dmbtr LIKE bsid-dmbtr,
        zfbdt LIKE bsid-zfbdt,   "baseline date
        zterm LIKE knb1-zterm,
        ztag1 LIKE t052-ztag1,
        dlate TYPE i,
* Begin of change DF1255 spalit 10/11/2019
        bldat TYPE bldat,
* End of change DF1255 spalit 10/11/2019
      END OF t_vdata.

DATA: BEGIN OF t_sum_vdata OCCURS 0,
        kunnr       LIKE bsid-kunnr,
        name1       LIKE kna1-name1,
        vname       LIKE bsid-vname,
        vtext       LIKE t8jvt-vtext,
        hkont       LIKE bsid-hkont,   "SOC and EOC by Ajitesh Date-10/09/2019
        bukrs       LIKE bsid-bukrs,
        invoice(16) TYPE c,
        post_pd(7)  TYPE c,
        doc_type(7) TYPE c,                                 "R3DK904360
        total       LIKE bsid-dmbtr,
        current     LIKE bsid-dmbtr,
        curr_days   TYPE i,
        p30         LIKE bsid-dmbtr,
        p60         LIKE bsid-dmbtr,
        p90         LIKE bsid-dmbtr,
        p120        LIKE bsid-dmbtr,
        pother      LIKE bsid-dmbtr,
        regio       LIKE csks-regio,
        ptotal      LIKE bsid-dmbtr,
        l50(1)      TYPE c,
        budat(10),
        dmbtr       LIKE bsid-dmbtr,
* Begin of change DF1255 spalit 10/11/2019
        bldat       TYPE bldat,
* End of change DF1255 spalit 10/11/2019

      END OF t_sum_vdata.

DATA:
  BEGIN OF t_bsid OCCURS 0,
    bukrs LIKE bsid-bukrs,
    kunnr LIKE bsid-kunnr,
    name1 LIKE kna1-name1,
    budat LIKE bsid-budat,
    blart LIKE bsid-blart,
    shkzg LIKE bsid-shkzg,
    dmbtr LIKE bsid-dmbtr,
  END   OF t_bsid.

DATA:
  BEGIN OF t_recent OCCURS 0,
    invoice(16) TYPE c,
     kunnr(10)  type c,
    date        LIKE sy-datum,
    days        LIKE t_data-dlate,
  END   OF t_recent.

*----------------------------------------------------------------------*
* initialization
*----------------------------------------------------------------------*
INITIALIZATION.

* Set Options: save variants userspecific or general
  g_save = 'A'.
  CLEAR g_variant.
  g_repid = sy-repid.
  g_variant-report = g_repid.

* Get default variant
  gx_variant = g_variant.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = g_save
    CHANGING
      cs_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 0.
    p_vari = gx_variant-variant.
  ENDIF.

*----------------------------------------------------------------------*
* AT Selection-Screen On Value-Request
*----------------------------------------------------------------------*
* Process on value request
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM sub_f4_for_variant.

*----------------------------------------------------------------------*
* AT Selection-Screen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM pai_of_selection_screen.

*----------------------------------------------------------------------*
* start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM sub_get_open_doc.
  PERFORM sub_sum_up_data.

*----------------------------------------------------------------------*
* end-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM sub_fieldcat_init  TABLES g_fieldcat.
  PERFORM sub_eventtab_build USING g_events[].

  PERFORM sub_comment_build  USING g_top_of_page[].
  PERFORM sub_sort_build     USING g_sort[].

  PERFORM sub_layout_build USING g_layout.
  PERFORM sub_call_alv.

*eject
*&---------------------------------------------------------------------*
*&      Form  sub_f4_for_variant
*&---------------------------------------------------------------------*
*       get list of possible variants
*----------------------------------------------------------------------*
FORM sub_f4_for_variant.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = g_variant
      i_save     = g_save
    IMPORTING
      e_exit     = g_exit
      es_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
*     text-900: No display variants exist
    MESSAGE i000(zz) WITH TEXT-900.
  ELSE.
    IF g_exit = space.
      p_vari = gx_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " sub_f4_for_variant
*eject
*&---------------------------------------------------------------------*
*&      Form  pai_of_selection_screen
*&---------------------------------------------------------------------*
*       determine if variant exists
*----------------------------------------------------------------------*
FORM pai_of_selection_screen.

  IF NOT p_vari IS INITIAL.
    MOVE g_variant TO gx_variant.
    MOVE p_vari TO gx_variant-variant.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = g_save
      CHANGING
        cs_variant = gx_variant.
    g_variant = gx_variant.

  ELSE.
    CLEAR g_variant.
    g_variant-report = g_repid.
  ENDIF.

ENDFORM.                    " pai_of_selection_screen
*eject
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       Top of page for ALV                                           *
*       dynamically called form                                       *
*---------------------------------------------------------------------*
FORM top_of_page.
*
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = g_top_of_page.

ENDFORM.

*eject
*&---------------------------------------------------------------------*
*&      Form  sub_fieldcat_init
*&---------------------------------------------------------------------*
*       build field catalog table
*----------------------------------------------------------------------*
*      -->P_FIELDCAT  field catalog
*----------------------------------------------------------------------*
FORM sub_fieldcat_init TABLES   p_fieldcat TYPE slis_t_fieldcat_alv.

  DATA: l_str(50),
        l_vo2(17).

  IF p_ven = 'X'.
    PERFORM sub_fill_fieldcat
*                                fieldname      key     key_sel
*                  ref_tabname   text_fieldname do_sum  no_out
*                  reptext_ddic  outputlen      input   sp_group
*                  ref_field     fname          tname   tech
*                  no_zero
              TABLES p_fieldcat
              USING:               space          'X'      'X'
                     space         space          space    space
                     TEXT-002      space          space    space
                     space         'NAME1'    'T_SUM_VDATA' space
                     space,

                                   space          'X'      'X'
                     space         space          space    space
                     TEXT-003      space          space    space
                     space         'KUNNR'    'T_SUM_VDATA' space
                     space,

                     "Begin of change by Ajitesh
                                   space          'X'      'X'
                     space         space          space    space
                     TEXT-004      space          space    space
                     space         'VNAME'    'T_SUM_VDATA' space
                     space,

                                   space          'X'      'X'
                     space         space          space    space
                     TEXT-005      space          space    space
                     space         'VTEXT'    'T_SUM_VDATA' space
                     space,
                     "End of change by Ajitesh

                     "SOC by Ajitesh Date-18/08/2019
                                   space          'X'      'X'
                     space         space          space    space
                     TEXT-021      space          space    space
                     space         'HKONT'    'T_SUM_VDATA' space
                     space,
                     "EOC by Ajitesh Date-18/08/2019

                                   space          'X'      'X'
                     space         space          'X'      space
                     TEXT-006      space          space   space
                     space         'TOTAL'    'T_SUM_VDATA' space
                     space,

                                   space          'X'      'X'
                     space         space          space    space
                     TEXT-007      space          space    space
                     space         'INVOICE'  'T_SUM_VDATA' space
                     space,

                                   space          'X'      'X'
                     space         space           space    space
                     TEXT-008      space          space    space
                     space         'POST_PD'      'T_SUM_VDATA' space
                     space,

                                   space          'X'      'X'
                     space         space           space    space
                     TEXT-009      space          space    space
                     space         'DOC_TYPE'      'T_SUM_VDATA' space
                     space,

                                   space          'X'      'X'
                     space         space          'X'      space
                     TEXT-010      space          space    space
                     space         'CURRENT'  'T_SUM_VDATA' space
                     space,

                                   space          'X'      'X'
                     space         space          space    space
                     TEXT-011      space          space    space
                     space         'CURR_DAYS'  'T_SUM_VDATA' space
                     space,

                                   space          space    space
                     space         space          'X'      space
                     TEXT-012      space          space    space
                     space         'P30'      'T_SUM_VDATA' space
                     space,

                                   space          space    space
                     space         space          'X'      space
                     TEXT-013      space          space    space
                     space         'P60'      'T_SUM_VDATA' space
                     space,

                                   space          space    space
                     space         space          'X'      space
                     TEXT-014      space          space    space
                     space         'P90'      'T_SUM_VDATA' space
                     space,

                                   space          space    space
                     space         space          'X'      space
                     TEXT-015      space          space    space
                     space         'P120'      'T_SUM_VDATA' space
                     space,

                                   space          space    space
                     space         space          'X'      space
                     TEXT-016      space          space    space
                     space         'POTHER'   'T_SUM_VDATA' space
                     space,

                                   space          space    space
                     space         space          space    space
                     TEXT-017      space          space    space
                     space         'PTOTAL'   'T_SUM_VDATA' space
                     space.

  ELSE.
    PERFORM sub_fill_fieldcat
*                                fieldname      key     key_sel
*                  ref_tabname   text_fieldname do_sum  no_out
*                  reptext_ddic  outputlen      input   sp_group
*                  ref_field     fname          tname   tech
*                  no_zero
              TABLES p_fieldcat
              USING:               space          'X'      'X'
                     space         space          space    space
                     TEXT-002      space          space    space
                     space         'NAME1'    'T_SUM_DATA' space
                     space,

                                   space          'X'      'X'
                     space         space          space    space
                     TEXT-003      space          space    space
                     space         'KUNNR'    'T_SUM_DATA' space
                     space,

                     "SOC by Ajitesh Date-10/09/2019
                                   space          'X'      'X'
                     space         space          space    space
                     TEXT-021      space          space    space
                     space         'HKONT'    'T_SUM_DATA' space
                     space,
                     "EOC by Ajitesh Date-10/09/2019

                                   space          'X'      'X'
                     space         space          'X'      space
                     TEXT-006      space          space   space
                     space         'TOTAL'    'T_SUM_DATA' space
                     space,

                                   space          'X'      'X'
                     space         space          space    space
                     TEXT-007      space          space    space
                     space         'INVOICE'  'T_SUM_DATA' space
                     space,

                                   space          'X'      'X'
                     space         space           space    space
                     TEXT-008      space           space    space
                     space         'POST_PD'      'T_SUM_DATA' space
                     space,

                                   space          'X'      'X'
                     space         space           space    space
                     TEXT-009      space           space    space
                     space         'DOC_TYPE'      'T_SUM_DATA' space
                     space,

                                   space          'X'      'X'
                     space         space          'X'      space
                     TEXT-010      space          space    space
                     space         'CURRENT'  'T_SUM_DATA' space
                     space,

                                   space          'X'      'X'
                     space         space          space    space
                     TEXT-011      space          space    space
                     space         'CURR_DAYS'  'T_SUM_DATA' space
                     space,

                                   space          space    space
                     space         space          'X'      space
                     TEXT-012      space          space    space
                     space         'P30'      'T_SUM_DATA' space
                     space,

                                   space          space    space
                     space         space          'X'      space
                     TEXT-013      space          space    space
                     space         'P60'      'T_SUM_DATA' space
                     space,

                                   space          space    space
                     space         space          'X'      space
                     TEXT-014      space          space    space
                     space         'P90'      'T_SUM_DATA' space
                     space,

                     space          space          space
                     space          space          'X'      space
                     TEXT-015       space space    space
                     space         'P120'      'T_SUM_DATA' space
                     space,

                                   space          space    space
                     space         space          'X'      space
                     TEXT-016      space          space    space
                     space         'POTHER'   'T_SUM_DATA' space
                     space,

                                   space          space    space
                     space         space          space    space
                     TEXT-017      space          space    space
                     space         'PTOTAL'   'T_SUM_DATA' space
                     space.
  ENDIF.

ENDFORM.                    " sub_fieldcat_init

*eject
*&---------------------------------------------------------------------*
*&      Form  sub_fill_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELDCAT     field catalog
*      -->col_pos        column position
*      -->cfieldnam      field name
*      -->key            indicates key field
*      -->key_sel        select on key
*      -->ref_tabname    reference table name
*      -->text_fieldname text field name
*      -->do_sum         total amounts
*      -->no_out         no display indicator
*      -->reptext_ddic   replace datadictionary text
*      -->outputlen      output lenght to display
*      -->input          ?
*      -->sp_group       grouping code
*      -->ref_field      reference field name
*      -->fname          internal field name
*      -->tname          internal table name
*      -->tech           technical field - no dislay
*      -->nozero         display no zeroes
*----------------------------------------------------------------------*
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

ENDFORM.                    " sub_fill_fieldcat
*eject
*&---------------------------------------------------------------------*
*&      Form  sub_eventtab_build
*&---------------------------------------------------------------------*
*       build event for ALV header
*----------------------------------------------------------------------*
*      -->P_EVENTS  events table
*----------------------------------------------------------------------*
FORM sub_eventtab_build USING    p_events TYPE slis_t_event.

  DATA: l_event TYPE slis_alv_event.
*
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = p_events.

  READ TABLE p_events WITH KEY name = slis_ev_top_of_page
                           INTO l_event.
  IF sy-subrc EQ 0.
    MOVE c_top_of_page TO l_event-form.
    APPEND l_event TO p_events.
  ENDIF.

ENDFORM.                    " sub_eventtab_build
*eject
*&---------------------------------------------------------------------*
*&      Form  sub_comment_build
*&---------------------------------------------------------------------*
*       build ALV header
*----------------------------------------------------------------------*
*      -->P_TOP_OF_PAGE
*----------------------------------------------------------------------*
FORM sub_comment_build USING p_top_of_page TYPE slis_t_listheader.
  DATA: l_line     TYPE slis_listheader,
        l_msg(255) TYPE c.

  l_line-typ  = 'H'.
  l_line-info = sy-title.
  APPEND l_line TO p_top_of_page.

  CLEAR l_line.
  l_line-typ  = 'S'.
  CONCATENATE sy-sysid '/' sy-mandt INTO l_msg.
  WRITE: TEXT-018              TO l_line-key,
         l_msg                 TO l_line-info.
  APPEND l_line TO p_top_of_page.

  WRITE sy-datum TO l_msg.
  WRITE '/'      TO l_msg+11.
  WRITE sy-uzeit TO l_msg+12.

  WRITE: 'Date/Time:'          TO l_line-key,
         l_msg                 TO l_line-info.
  APPEND l_line TO p_top_of_page.

  WRITE: TEXT-019              TO l_line-key,
         sy-uname              TO l_line-info.
  APPEND l_line TO p_top_of_page.

  l_line-typ  = 'S'.
  WRITE: TEXT-020              TO l_line-key,
         sy-repid              TO l_line-info.
  APPEND l_line TO p_top_of_page.

ENDFORM.                    " sub_comment_build
*eject
*&---------------------------------------------------------------------*
*&      Form  sub_sort_build
*&---------------------------------------------------------------------*
*       set sort parameters
*----------------------------------------------------------------------*
*      -->P_G_SORT  sort parameters
*----------------------------------------------------------------------*
FORM sub_sort_build USING p_sort TYPE slis_t_sortinfo_alv.

  DATA: l_sort TYPE slis_sortinfo_alv.

*  clear l_sort.
*  l_sort-fieldname = 'PERNR'.  "employee id
*  l_sort-spos      = 1.
*  l_sort-up        = 'X'.
*  l_sort-subtot    = 'X'.
*  append l_sort to p_sort.

ENDFORM.                    " sub_sort_build
*eject
*&---------------------------------------------------------------------*
*&      Form  sub_layout_build
*&---------------------------------------------------------------------*
*       set layout parameters of ALV
*----------------------------------------------------------------------*
*      -->P_LAYOUT  layout parameter table
*----------------------------------------------------------------------*
FORM sub_layout_build USING p_layout TYPE slis_layout_alv.

  p_layout-colwidth_optimize = c_on.
  p_layout-no_input          = c_on.

ENDFORM.                    " sub_layout_build
*eject
*&---------------------------------------------------------------------*
*&      Form  sub_call_alv
*&---------------------------------------------------------------------*
*       Display ALV
*----------------------------------------------------------------------*
FORM sub_call_alv.

  IF p_ven = 'X'.
    IF t_sum_vdata[] IS INITIAL.
*     text-902: No data found to display
      MESSAGE i000(zz) WITH TEXT-902.
      EXIT.
    ENDIF.


* Call ABAP/4 List Viewer
    IF sy-batch = space.  "Online
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program = g_repid
          is_layout          = g_layout
          it_fieldcat        = g_fieldcat[]
          it_special_groups  = g_group[]
          it_sort            = g_sort[]
          i_save             = g_save
          is_variant         = g_variant
          it_events          = g_events[]
          is_print           = g_print
        TABLES
          t_outtab           = t_sum_vdata.
    ELSE.
      CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
        EXPORTING
          i_callback_program = g_repid
          is_layout          = g_layout
          it_fieldcat        = g_fieldcat[]
          it_special_groups  = g_group[]
          it_sort            = g_sort[]
          i_save             = g_save
          is_variant         = g_variant
          it_events          = g_events[]
          is_print           = g_print
        TABLES
          t_outtab           = t_sum_vdata.
    ENDIF.

  ELSE.
    IF t_sum_data[] IS INITIAL.
*     text-902: No data found to display
      MESSAGE i000(zz) WITH TEXT-902.
      EXIT.
    ENDIF.


* Call ABAP/4 List Viewer
    IF sy-batch = space.  "Online
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program = g_repid
          is_layout          = g_layout
          it_fieldcat        = g_fieldcat[]
          it_special_groups  = g_group[]
          it_sort            = g_sort[]
          i_save             = g_save
          is_variant         = g_variant
          it_events          = g_events[]
          is_print           = g_print
        TABLES
          t_outtab           = t_sum_data.
    ELSE.
      CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
        EXPORTING
          i_callback_program = g_repid
          is_layout          = g_layout
          it_fieldcat        = g_fieldcat[]
          it_special_groups  = g_group[]
          it_sort            = g_sort[]
          i_save             = g_save
          is_variant         = g_variant
          it_events          = g_events[]
          is_print           = g_print
        TABLES
          t_outtab           = t_sum_data.
    ENDIF.
  ENDIF.

ENDFORM.                    " sub_call_alv

*&---------------------------------------------------------------------*
*&      Form  sub_get_open_doc
*&---------------------------------------------------------------------*
FORM sub_get_open_doc.

  DATA: l_date TYPE d,
        l_days LIKE rfposx-verzn.

  IF p_ven = 'X'.
*         Get all open items
*    SOC by Ajitesh Date-18/08/2019
    SELECT bsid~bukrs bsid~kunnr bsid~vname
       t8jvt~vtext bsid~hkont kna1~name1 bsid~belnr bsid~xblnr
       bsid~budat
       bsid~blart bsid~shkzg bsid~dmbtr
       bsid~zfbdt bsid~zterm t052~ztag1
* Begin of change DF1255 spalit 10/11/2019
           bsid~bldat
* End of change DF1255 spalit 10/11/2019

       INTO CORRESPONDING FIELDS OF TABLE t_vdata
       FROM bsid
       JOIN kna1
         ON ( kna1~kunnr = bsid~kunnr )
       JOIN t8jvt
         ON ( bsid~vname = t8jvt~vname )
       LEFT OUTER JOIN t052
          ON ( t052~zterm = bsid~zterm )
       WHERE bsid~bukrs IN s_bukrs
         AND bsid~budat <= p_post
         AND bsid~kunnr IN s_kunnr
         AND bsid~hkont IN s_hkont.

    SELECT bsad~bukrs bsad~kunnr bsad~vname
       t8jvt~vtext bsad~hkont kna1~name1 bsad~belnr bsad~xblnr
       bsad~budat
       bsad~blart bsad~shkzg bsad~dmbtr
       bsad~zfbdt bsad~zterm t052~ztag1
* Begin of change DF1255 spalit 10/11/2019
           bsad~bldat
* End of change DF1255 spalit 10/11/2019
       APPENDING CORRESPONDING FIELDS OF TABLE t_vdata
       FROM bsad
       JOIN kna1
         ON ( kna1~kunnr = bsad~kunnr )
       JOIN t8jvt
         ON ( bsad~vname = t8jvt~vname )
       LEFT OUTER JOIN t052
          ON ( t052~zterm = bsad~zterm )
       WHERE bsad~bukrs IN s_bukrs
         AND bsad~budat <= p_post
         AND bsad~augdt > p_budat
         AND bsad~kunnr IN s_kunnr
         AND bsad~augbl <> bsad~belnr
         AND bsad~hkont IN s_hkont.
*      EOC by Ajitesh Date-18/08/2019

    IF t_vdata[] IS INITIAL.
      STOP.
    ENDIF.

    SORT t_vdata BY bukrs kunnr budat.

*       For the all records that have JV and calculate #days late.
    LOOP AT t_vdata.

      IF t_vdata-shkzg = 'H'.
        t_vdata-dmbtr = t_vdata-dmbtr * -1.
      ENDIF.

      l_date = t_vdata-zfbdt + t_vdata-ztag1.

      CALL FUNCTION 'ITEM_OVERDUE_DAYS'
        EXPORTING
          key_date     = l_date
          due_date     = p_budat
        IMPORTING
          overdue_days = l_days.

      IF sy-subrc <> 0.
*           MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      t_vdata-dlate = l_days * -1.
      MODIFY t_vdata.
    ENDLOOP.

  ELSE.
** Get all open items
    SELECT bsid~bukrs bsid~kunnr bsid~hkont kna1~name1
           bsid~belnr bsid~xblnr bsid~budat bsid~blart
           bsid~shkzg bsid~dmbtr bsid~zfbdt bsid~zterm
           t052~ztag1
* Begin of change DF1255 spalit 10/11/2019
           bsid~bldat
* End of change DF1255 spalit 10/11/2019
           INTO CORRESPONDING FIELDS OF TABLE t_data
           FROM bsid
           JOIN kna1
             ON ( kna1~kunnr = bsid~kunnr )
           LEFT OUTER JOIN t052
              ON ( t052~zterm = bsid~zterm )
           WHERE bsid~bukrs IN s_bukrs
             AND bsid~budat <= p_post
             AND bsid~kunnr IN s_kunnr
             AND bsid~hkont IN s_hkont.

* Get all open items
    SELECT bsad~bukrs bsad~kunnr bsad~hkont kna1~name1
           bsad~belnr bsad~xblnr bsad~budat bsad~blart
           bsad~shkzg bsad~dmbtr bsad~zfbdt bsad~zterm
           t052~ztag1
* Begin of change DF1255 spalit 10/11/2019
           bsad~bldat
* End of change DF1255 spalit 10/11/2019
           APPENDING CORRESPONDING FIELDS OF TABLE t_data
           FROM bsad
           JOIN kna1
             ON ( kna1~kunnr = bsad~kunnr )
           LEFT OUTER JOIN t052
              ON ( t052~zterm = bsad~zterm )
           WHERE bsad~bukrs IN s_bukrs
             AND bsad~budat <= p_post
             AND bsad~augdt > p_budat
             AND bsad~kunnr IN s_kunnr
             AND bsad~augbl <> bsad~belnr
             AND bsad~hkont IN s_hkont.

    IF t_data[] IS INITIAL.
      STOP.
    ENDIF.

    SORT t_data BY bukrs kunnr budat.

* For the all records that have JV and calculate #days late.
    LOOP AT t_data.

      IF t_data-shkzg = 'H'.
        t_data-dmbtr = t_data-dmbtr * -1.
      ENDIF.

      l_date = t_data-zfbdt + t_data-ztag1.

      CALL FUNCTION 'ITEM_OVERDUE_DAYS'
        EXPORTING
          key_date     = l_date
          due_date     = p_budat
        IMPORTING
          overdue_days = l_days.

      IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      t_data-dlate = l_days * -1.
      MODIFY t_data.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " sub_get_open_doc

*&---------------------------------------------------------------------*
*&      Form  sub_sum_up_data
*&---------------------------------------------------------------------*
FORM sub_sum_up_data.

  DATA: l_budat   LIKE bsid-budat,
        l_invoice LIKE t_sum_data-invoice,
        l_tabix   LIKE sy-tabix.
  IF p_ven = 'X'.
    SORT t_vdata BY kunnr.

    LOOP AT t_vdata.

      l_budat = t_vdata-budat.

      CLEAR t_sum_vdata.
* Begin of change DF1255 spalit 10/11/2019
*      IF t_vdata-blart = 'DC'.

*      CONCATENATE t_vdata-budat+4(2) t_vdata-budat+0(4)
*      t_vdata-kunnr INTO l_invoice.
*
**             Check to see if DC & ZB Record already exists
*      CLEAR t_sum_vdata.
*      READ TABLE t_sum_vdata WITH KEY kunnr   = t_vdata-kunnr
*                                     bukrs   = t_vdata-bukrs
*                                     invoice = l_invoice
*                                     doc_type = 'DC & ZB'.
*
*      IF sy-subrc = 0.
*        CLEAR t_sum_vdata.
*        t_sum_vdata-doc_type = 'DC & ZB'.
*        t_sum_vdata-invoice = l_invoice.
*      ELSE.
**               Check to see if ZB Exists
*        CLEAR t_sum_vdata.
*        READ TABLE t_sum_vdata WITH KEY kunnr   = t_vdata-kunnr
*                                       bukrs   = t_vdata-bukrs
*                                       invoice = l_invoice
*                                       doc_type = 'ZB'.
*
*        IF sy-subrc = 0.
*          l_tabix = sy-tabix.
*          t_sum_vdata-doc_type = 'DC & ZB'.
*          MODIFY t_sum_vdata INDEX l_tabix.
*          CLEAR t_sum_vdata.
*          t_sum_vdata-doc_type = 'DC & ZB'.
*          t_sum_vdata-invoice = l_invoice.
**               Roll into existing DC
*        ELSE.
*          CLEAR t_sum_vdata.
*          t_sum_vdata-doc_type = t_vdata-blart.
*          t_sum_vdata-invoice = l_invoice.
*        ENDIF.
*      ENDIF.
*
*    ELSEIF t_vdata-blart = 'ZB'.
*
*      CONCATENATE t_vdata-budat+4(2) t_vdata-budat+0(4)
*      t_vdata-kunnr INTO l_invoice.
*
**             Check to see if DC & ZB Record already exists
*      CLEAR t_sum_vdata.
*      READ TABLE t_sum_vdata WITH KEY kunnr   = t_vdata-kunnr
*                                     bukrs   = t_vdata-bukrs
*                                     invoice = l_invoice
*                                     doc_type = 'DC & ZB'.
*
*      IF sy-subrc = 0.
*        CLEAR t_sum_vdata.
*        t_sum_vdata-doc_type = 'DC & ZB'.
*        t_sum_vdata-invoice = l_invoice.
*      ELSE.
**               Check to see if ZB Exists
*        CLEAR t_sum_vdata.
*        READ TABLE t_sum_vdata WITH KEY kunnr   = t_vdata-kunnr
*                                       bukrs   = t_vdata-bukrs
*                                       invoice = l_invoice
*                                       doc_type = 'DC'.
*
*        IF sy-subrc = 0.
*          l_tabix = sy-tabix.
*          t_sum_vdata-doc_type = 'DC & ZB'.
*          MODIFY t_sum_vdata INDEX l_tabix.
*          CLEAR t_sum_vdata.
*          t_sum_vdata-doc_type = 'DC & ZB'.
*          t_sum_vdata-invoice = l_invoice.
**               Roll into existing DC
*        ELSE.
*          CLEAR t_sum_vdata.
*          t_sum_vdata-doc_type = t_vdata-blart.
*          t_sum_vdata-invoice = l_invoice.
*        ENDIF.
*      ENDIF.

*      ELSEIF t_vdata-blart = 'Z5'.
      IF t_vdata-blart = 'Z5'.
* End of change DF1255 spalit 10/11/2019
        t_sum_vdata-invoice = t_vdata-xblnr.

        t_sum_vdata-doc_type = t_vdata-blart.

      ELSE.
        t_sum_vdata-invoice = t_vdata-belnr.

        t_sum_vdata-doc_type = t_vdata-blart.
      ENDIF.

      MOVE: t_vdata-name1 TO t_sum_vdata-name1,
            t_vdata-kunnr TO t_sum_vdata-kunnr,
            t_vdata-bukrs TO t_sum_vdata-bukrs.
* Begin of change DF1255 spalit 10/11/2019
*      CONCATENATE t_vdata-budat+4(2) t_vdata-budat+0(4) INTO
*      t_sum_vdata-post_pd SEPARATED BY '/'.
      CONCATENATE t_vdata-bldat+4(2) t_vdata-bldat+0(4) INTO
t_sum_vdata-post_pd SEPARATED BY '/'.

* End  of change DF1255 spalit 10/11/2019
      MOVE: t_vdata-vname TO t_sum_vdata-vname,
            t_vdata-vtext TO t_sum_vdata-vtext,
            t_vdata-hkont TO t_sum_vdata-hkont. "SOC and EOC by Ajitesh Date-10/09/2019

      IF t_vdata-dlate > 0.
        PERFORM assign_past_due USING t_vdata-dlate t_vdata-dmbtr.
      ELSE.
        MOVE: t_vdata-dmbtr TO t_sum_vdata-current.
      ENDIF.
      MOVE: t_vdata-dmbtr TO t_sum_vdata-total.
*            move: t_vdata-vname to t_sum_vdata-vname,
*                  t_vdata-vtext to t_sum_vdata-vtext.
      COLLECT t_sum_vdata.

      CLEAR t_recent.
      t_recent-invoice = t_sum_vdata-invoice.
      t_recent-kunnr   = t_sum_vdata-kunnr.
      t_recent-date    = t_vdata-budat.
      t_recent-days    = t_vdata-dlate.
      APPEND t_recent.

    ENDLOOP.

    SORT t_sum_vdata BY bukrs kunnr post_pd invoice ASCENDING.

*         Add Most Recent Days Due
    SORT t_recent BY invoice ASCENDING kunnr ASCENDING date DESCENDING.
    DELETE ADJACENT DUPLICATES FROM t_recent.

    LOOP AT t_sum_vdata.
      CLEAR t_recent.
      READ TABLE t_recent WITH KEY invoice = t_sum_vdata-invoice
                                   kunnr   = t_sum_vdata-kunnr.

      IF sy-subrc = 0.
        t_sum_vdata-curr_days = t_recent-days.
        MODIFY t_sum_vdata.
      ENDIF.
    ENDLOOP.

  ELSE.

    SORT t_data BY kunnr.

    LOOP AT t_data.

      l_budat = t_data-budat.

      CLEAR t_sum_data.
*Begin of change DF1255 spalit 10/11/2019

*      IF t_data-blart = 'DC'.
*
*        CONCATENATE t_data-budat+4(2) t_data-budat+0(4)
*        t_data-kunnr INTO l_invoice.
*
**             Check to see if DC & ZB Record already exists
*        CLEAR t_sum_data.
*        READ TABLE t_sum_data WITH KEY kunnr   = t_data-kunnr
*                                       bukrs   = t_data-bukrs
*                                       invoice = l_invoice
*                                       doc_type = 'DC & ZB'.
*
*        IF sy-subrc = 0.
*          CLEAR t_sum_data.
*          t_sum_data-doc_type = 'DC & ZB'.
*          t_sum_data-invoice = l_invoice.
*        ELSE.
**               Check to see if ZB Exists
*          CLEAR t_sum_data.
*          READ TABLE t_sum_data WITH KEY kunnr   = t_data-kunnr
*                                         bukrs   = t_data-bukrs
*                                         invoice = l_invoice
*                                         doc_type = 'ZB'.
*
*          IF sy-subrc = 0.
*            l_tabix = sy-tabix.
*            t_sum_data-doc_type = 'DC & ZB'.
*            MODIFY t_sum_data INDEX l_tabix.
*            CLEAR t_sum_data.
*            t_sum_data-doc_type = 'DC & ZB'.
*            t_sum_data-invoice = l_invoice.
**               Roll into existing DC
*          ELSE.
*            CLEAR t_sum_data.
*            t_sum_data-doc_type = t_data-blart.
*            t_sum_data-invoice = l_invoice.
*          ENDIF.
*        ENDIF.
*
*      ELSEIF t_data-blart = 'ZB'.
*
*        CONCATENATE t_data-budat+4(2) t_data-budat+0(4)
*        t_data-kunnr INTO l_invoice.
*
**             Check to see if DC & ZB Record already exists
*        CLEAR t_sum_data.
*        READ TABLE t_sum_data WITH KEY kunnr   = t_data-kunnr
*                                       bukrs   = t_data-bukrs
*                                       invoice = l_invoice
*                                       doc_type = 'DC & ZB'.
*
*        IF sy-subrc = 0.
*          CLEAR t_sum_data.
*          t_sum_data-doc_type = 'DC & ZB'.
*          t_sum_data-invoice = l_invoice.
*        ELSE.
**               Check to see if ZB Exists
*          CLEAR t_sum_data.
*          READ TABLE t_sum_data WITH KEY kunnr   = t_data-kunnr
*                                         bukrs   = t_data-bukrs
*                                         invoice = l_invoice
*                                         doc_type = 'DC'.
*
*          IF sy-subrc = 0.
*            l_tabix = sy-tabix.
*            t_sum_data-doc_type = 'DC & ZB'.
*            MODIFY t_sum_data INDEX l_tabix.
*            CLEAR t_sum_data.
*            t_sum_data-doc_type = 'DC & ZB'.
*            t_sum_data-invoice = l_invoice.
**               Roll into existing DC
*          ELSE.
*            CLEAR t_sum_data.
*            t_sum_data-doc_type = t_data-blart.
*            t_sum_data-invoice = l_invoice.
*          ENDIF.
*        ENDIF.
*
*      ELSEIF t_data-blart = 'Z5'.
      IF t_data-blart = 'Z5'.
* End of change DF1255 spalit 10/11/2019

        t_sum_data-invoice = t_data-xblnr.

        t_sum_data-doc_type = t_data-blart.

      ELSE.
        t_sum_data-invoice = t_data-belnr.

        t_sum_data-doc_type = t_data-blart.
      ENDIF.

      MOVE: t_data-name1 TO t_sum_data-name1,
            t_data-kunnr TO t_sum_data-kunnr,
            t_data-bukrs TO t_sum_data-bukrs,
            t_data-hkont TO t_sum_data-hkont. "SOC and EOC by Ajitesh Date-10/09/2019

* Begin of change DF1255 spalit 10/11/2019
*      CONCATENATE t_data-budat+4(2) t_data-budat+0(4) INTO
*      t_sum_data-post_pd SEPARATED BY '/'.
      CONCATENATE t_data-bldat+4(2) t_data-bldat+0(4) INTO
t_sum_data-post_pd SEPARATED BY '/'.

* End of change DF1255 spalit 10/11/2019

      IF t_data-dlate > 0.
        PERFORM assign_past_due USING t_data-dlate t_data-dmbtr.
      ELSE.
        MOVE: t_data-dmbtr TO t_sum_data-current.
      ENDIF.
      MOVE: t_data-dmbtr TO t_sum_data-total.
      COLLECT t_sum_data.

      CLEAR t_recent.
      t_recent-invoice = t_sum_data-invoice.
      t_recent-kunnr   = t_sum_data-kunnr.
      t_recent-date    = t_data-budat.
      t_recent-days    = t_data-dlate.
      APPEND t_recent.

    ENDLOOP.

    SORT t_sum_data BY bukrs kunnr post_pd invoice ASCENDING.

*         Add Most Recent Days Due
    SORT t_recent BY invoice ASCENDING kunnr ASCENDING date DESCENDING.
    DELETE ADJACENT DUPLICATES FROM t_recent.

    LOOP AT t_sum_data.
      CLEAR t_recent.
      READ TABLE t_recent WITH KEY invoice = t_sum_data-invoice
                                   kunnr   = t_sum_data-kunnr.

      IF sy-subrc = 0.
        t_sum_data-curr_days = t_recent-days.
        MODIFY t_sum_data.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " sub_sum_up_data

*&---------------------------------------------------------------------*
*&      Form  assign_past_due
*&---------------------------------------------------------------------*
FORM assign_past_due USING dlate TYPE i
                           dmbtr LIKE bsid-dmbtr.
  IF p_ven = 'X'.
    IF dlate EQ 0.
      EXIT.
    ELSEIF dlate GT  0 AND dlate LE 30.
      t_sum_vdata-p30 = dmbtr.
    ELSEIF dlate GT 30 AND dlate LE 60.
      t_sum_vdata-p60 = dmbtr.
    ELSEIF dlate GT 60 AND dlate LE 90.
      t_sum_vdata-p90 = dmbtr.
    ELSEIF dlate GT 90 AND dlate LE 120.
      t_sum_vdata-p120 = dmbtr.
    ELSE.
      t_sum_vdata-pother = dmbtr.
    ENDIF.

    t_sum_vdata-ptotal = dmbtr.

  ELSE.

    IF dlate EQ 0.
      EXIT.
    ELSEIF dlate GT  0 AND dlate LE 30.
      t_sum_data-p30 = dmbtr.
    ELSEIF dlate GT 30 AND dlate LE 60.
      t_sum_data-p60 = dmbtr.
    ELSEIF dlate GT 60 AND dlate LE 90.
      t_sum_data-p90 = dmbtr.
    ELSEIF dlate GT 90 AND dlate LE 120.
      t_sum_data-p120 = dmbtr.
    ELSE.
      t_sum_data-pother = dmbtr.
    ENDIF.

    t_sum_data-ptotal = dmbtr.

  ENDIF.

ENDFORM.                    " assign_past_due
