*&---------------------------------------------------------------------*
*& Report ZJVAR_JV_AUDIT_REPORT_01
*&---------------------------------------------------------------------*
*_---------------------------------------------------------------------*
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
REPORT zjvar_jv_audit_report_01 NO STANDARD PAGE HEADING.

*********Include for Global data declarations*******
INCLUDE zjvan_jv_audit_report_top_01.

*********Include for Selection Screen*******
INCLUDE zjvan_jv_audit_report_sel_01.

********Include for Sub Routines*******
INCLUDE zjvan_jv_audit_report_sub_01.

AT SELECTION-SCREEN OUTPUT.
  IF p_audit = abap_true.
    LOOP AT SCREEN .
      IF screen-group1 = TEXT-085.
        screen-active = TEXT-086.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF p_equity = abap_true.
    LOOP AT SCREEN .
      IF screen-group1 = TEXT-085.
        screen-active = TEXT-087.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF p_joajv = abap_true.
    LOOP AT SCREEN .
      IF screen-group1 = TEXT-085.
        screen-active = TEXT-087.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF p_jvadoi = abap_true.
    LOOP AT SCREEN .
      IF screen-group1 = TEXT-085.
        screen-active = TEXT-087.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
*  ELSEIF p_exinn = abap_true.
*    LOOP AT SCREEN .
*      IF screen-group1 = TEXT-085.
*        screen-active = TEXT-087.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_uname-low.
  PERFORM sub_f4help_username.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_uname-high.
  PERFORM sub_f4help_username.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_joa-low.
  PERFORM sub_f4help_joa.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_joa-high.
  PERFORM sub_f4help_joa.


START-OF-SELECTION.
  PERFORM get_data.

END-OF-SELECTION.
  PERFORM sub_fieldcat_init  TABLES g_fieldcat.
  PERFORM build_alv_header.
  PERFORM sub_alv_display.
