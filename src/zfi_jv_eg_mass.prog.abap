*&---------------------------------------------------------------------*
*& Report ZFI_JV_EG_MASS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Author          : Krishna Kancherla
* Creation Date   : 06-05-2019
* Transaction     : ZJV_MASS_CHANGE
* Technical design: TSD_FSD_E01_11.1.1 - JV Mass change
* Description     : JV  Mass change
*----------------------------------------------------------------------*
* Modification Information (Most Recent on Top)
*----------------------------------------------------------------------*
* Date            : 06-05-2019
* Author          : Krishna Kancherla
* Change request  : N/A
* Transport number:
* Description     : Initial Development
*----------------------------------------------------------------------*


REPORT zfi_jv_eg_mass.

* TOP Include for Data Declarations
INCLUDE zfi_jv_eg_mass_top.

* Selection-Screen Include
INCLUDE zfi_jv_eg_mass_sel.

* validation Include
INCLUDE zfi_jv_eg_mass_form1.

AT SELECTION-SCREEN OUTPUT.

  PERFORM sub_toggle_screen.





INITIALIZATION.
  PERFORM sub_populate_value.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_bukrs.
*  IF sy-subrc = 0 .
*
*  ENDIF.

*AT SELECTION-SCREEN ON value-REQUEST FOR p_ptrold.
*  TABLES:t8jo,kna1.
*  DATA: BEGIN OF g_t8jo OCCURS 1,
*          partn   LIKE t8jo-partn,
*          intcocd LIKE t8jo-intcocd,
*          name1   LIKE kna1-name1,
*        END OF g_t8jo.
*  DATA:    title  LIKE rgsel-dynp_title.

*  SELECT * FROM t8jo WHERE bukrs = p_bukrs.
*    SELECT SINGLE * FROM kna1 WHERE kunnr = t8jo-partn.
*    MOVE-CORRESPONDING t8jo TO g_t8jo.
*    MOVE kna1-name1 TO g_t8jo-name1.
*    APPEND  g_t8jo.
*  ENDSELECT.
*  DESCRIBE TABLE g_t8jo LINES sy-tabix.
*  IF sy-tabix = 0.
*    MESSAGE i141(g4).                  "no value found
*  ENDIF.
*  IF sy-tabix = 1.
*    READ TABLE g_t8jo INDEX sy-tabix.
*    t8jo-partn = g_t8jo-partn.
*  ELSE.
*    title                = 'JV partners'.
*    g_t8jo-partn         = 'Partner'.
*    g_t8jo-intcocd       = 'ICoC'.
*    g_t8jo-name1         = 'Name'.
*    CALL FUNCTION 'G_DISPLAY_SELECTION_DYNPRO'
*      EXPORTING
*        allow_print     = 'X'
*        dynp_title      = title
*        sel_title1      = g_t8jo
*        sel_title2      = space
*        show_also_1     = 'X'
*        width_of_titles = 'X'
*        start_column    = '2'
*      IMPORTING
*        sel_index       = sy-tabix
*      TABLES
*        sel_table       = g_t8jo
*      EXCEPTIONS
**       extended_display_selected = 01
*        no_lines        = 02
*        no_line_picked  = 03.
*
*    IF sy-subrc = 0.
*      READ TABLE g_t8jo INDEX sy-tabix.
*      p_ptrold  = g_t8jo-partn.
*    ENDIF.
*  ENDIF.


AT SELECTION-SCREEN.
  PERFORM sub_validate.

  PERFORM sub_toggle_screen.

START-OF-SELECTION.

  PERFORM sub_fetch_data.

  PERFORM sub_prepare.

END-OF-SELECTION.

  IF NOT it_plist[] IS INITIAL.
    PERFORM sub_fldcat.

    PERFORM sub_disp_plist.
* Begin of change DF1095 09/20/2019 spalit
  ELSE.
    MESSAGE TEXT-045 TYPE 'I'.
* End of change DF1095 09/20/2019 spalit
  ENDIF.
