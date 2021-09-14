*&---------------------------------------------------------------------*
*& Include          ZJVAN_JV_AUDIT_REPORT_SEL_01
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
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-h01.


SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_audit RADIOBUTTON GROUP r1 USER-COMMAND xyz .
SELECTION-SCREEN COMMENT  3(50) TEXT-005 FOR FIELD p_audit.
SELECTION-SCREEN END OF LINE.


SELECT-OPTIONS: s_uname FOR cdhdr-username MODIF ID id1,
                s_dat FOR cdhdr-udate DEFAULT '20180101' TO sy-datum MODIF ID id1,
                s_bukrs FOR t001-bukrs  NO INTERVALS MODIF ID id1,
                s_joa FOR t8j9a-joa MODIF ID id1.
* Begin of change 9454163 ssen 30/04/2020
*PARAMETERS p_mhits TYPE i DEFAULT TEXT-500 MODIF ID id1.
PARAMETERS p_mhits TYPE i MODIF ID id1.
* End of change 9454163 ssen 30/04/2020

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_equity RADIOBUTTON GROUP r1.
SELECTION-SCREEN COMMENT  3(50) TEXT-006 FOR FIELD p_equity.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_joajv RADIOBUTTON GROUP r1.
SELECTION-SCREEN COMMENT  3(50) TEXT-007 FOR FIELD p_joajv.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_jvadoi RADIOBUTTON GROUP r1.
SELECTION-SCREEN COMMENT  3(50) TEXT-003 FOR FIELD p_jvadoi.
SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS p_exinn RADIOBUTTON GROUP r1.
*SELECTION-SCREEN COMMENT  3(50) TEXT-004 FOR FIELD p_exinn.
*SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b1.
