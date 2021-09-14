*&---------------------------------------------------------------------*
*& Include          ZJVAN_JIB_AR_STMNT_SEL_01
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

* Block1 - Selection Parameters
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
SELECT-OPTIONS: s_bukrs FOR g_bukrs OBLIGATORY,          "Company Code - Mandatory
                s_kunnr FOR g_kunnr,                     "Customer Number
                s_hkont FOR g_hkont DEFAULT c_hkont_def. "GL Account(s) - Default value = '1101000'
SELECTION-SCREEN END OF BLOCK b1.

* Block2 - Path Selection
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
PARAMETERS: p_path TYPE localfile OBLIGATORY.            "Download File Path - Mandatory
SELECTION-SCREEN END OF BLOCK b2.
