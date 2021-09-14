*&---------------------------------------------------------------------*
*& Report ZJVAF_JIB_AR_STATEMENT_01
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
REPORT zjvaf_jib_ar_statement_01.

* TOP Include for Data Declarations
INCLUDE zjvan_jib_ar_stmnt_top_01.

* Selection-Screen Include
INCLUDE zjvan_jib_ar_stmnt_sel_01.

* Processing Include
INCLUDE zjvan_jib_ar_stmnt_prc_01.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
* F4 help for file path
  PERFORM f4_file_path.

AT SELECTION-SCREEN.
* Validate selection-screen
  PERFORM screen_validate.

START-OF-SELECTION.
* Get partner data
  PERFORM get_data.

END-OF-SELECTION.
  IF it_knb1[] IS NOT INITIAL.
*   Generate AR statements for each customer in one single PDF document
    PERFORM process_data.
  ELSE. "Display Message if no data found
    MESSAGE TEXT-m05 TYPE c_s.
  ENDIF.
