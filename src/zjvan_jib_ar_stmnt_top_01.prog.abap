*&---------------------------------------------------------------------*
*& Include          ZJVAN_JIB_AR_STMNT_TOP_01
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

* Constants - Declarations
CONSTANTS: c_hkont_def TYPE hkont VALUE '1101000', "Changed by Ajitesh
           c_e         TYPE c     VALUE 'E',
           c_s         TYPE c     VALUE 'S'.

* Types - Declarations
TYPES: BEGIN OF t_but000,
         partner   TYPE bu_partner,
         name_org1 TYPE bu_nameor1,
         addrcomm  TYPE bu_addrcomm,
       END OF t_but000,

       BEGIN OF t_but020,
         partner         TYPE bu_partner,
         addrnumber      TYPE ad_addrnum,
         addr_valid_from TYPE bu_addr_valid_from,
         addr_valid_to   TYPE bu_addr_valid_to,
       END OF t_but020,

       BEGIN OF t_adrc,
         addrnumber TYPE ad_addrnum,
         name1      TYPE ad_name1,
         name2      TYPE ad_name2,
         street     TYPE ad_street,
         str_suppl3 TYPE ad_strspp3,
         city1      TYPE ad_city1,
         region     TYPE regio,
         post_code1 TYPE ad_pstcd1,
*         addrnumber TYPE ad_addrnum,
*         city1      TYPE ad_city1,
*         post_code1 TYPE ad_pstcd1,
*         street     TYPE ad_street,
*         region     TYPE regio,
       END OF t_adrc,

       BEGIN OF t_knb1,
         kunnr TYPE kunnr,
         bukrs TYPE bukrs,
         zterm TYPE dzterm,
       END OF t_knb1,

       BEGIN OF t_bsid,
*         bukrs TYPE bukrs,
*         kunnr TYPE kunnr,
*         gjahr TYPE gjahr,
*         budat TYPE budat,
*         waers TYPE waers,
*         blart TYPE blart,
*         monat TYPE monat,
*         dmbtr TYPE dmbtr,
*         wrbtr TYPE wrbtr,
*         hkont TYPE hkont,
*         zfbdt TYPE dzfbdt,
*         rebzg TYPE rebzg,
*         shkzg TYPE shkzg,
*         zterm TYPE dzterm,
*         btype TYPE btype,
*         umskz TYPE umskz,
*         bschl TYPE bschl,
         bukrs TYPE bukrs,
         kunnr TYPE kunnr,
         umskz TYPE umskz,
         gjahr TYPE gjahr,
         belnr TYPE belnr_d,
         budat TYPE budat,
         bldat TYPE bldat,
         waers TYPE waers,
         blart TYPE blart,
         monat TYPE monat,
         bschl TYPE bschl,
         shkzg TYPE shkzg,
         dmbtr TYPE dmbtr,
         wrbtr TYPE wrbtr,
         hkont TYPE hkont,
         zfbdt TYPE dzfbdt,
         zterm TYPE dzterm,
         rebzg TYPE rebzg,
         btype TYPE jv_bilind,
       END OF t_bsid,
       BEGIN OF t_t052,
         zterm TYPE dzterm,
         ztag1 TYPE dztage,
       END OF t_t052,
       BEGIN OF t_tvzbt,
         spras TYPE spras,
         zterm TYPE dzterm,
         vtext TYPE dzterm_bez,
       END OF t_tvzbt,
       BEGIN OF t_/pra/bp_ad,
         adrnr     TYPE adrnr,
         addr_type TYPE oiucm_address_type,
         custid    TYPE kunnr,
         vendid    TYPE lifnr,
       END OF t_/pra/bp_ad,
       BEGIN OF t_kna1,
         kunnr TYPE kunnr,
         adrnr TYPE adrnr,
       END OF t_kna1.

* Data - Declarations
DATA: it_but000     TYPE STANDARD TABLE OF t_but000,
      it_but020     TYPE STANDARD TABLE OF t_but020,
      it_adrc       TYPE STANDARD TABLE OF t_adrc,
      it_knb1       TYPE STANDARD TABLE OF t_knb1,
      it_bsid       TYPE STANDARD TABLE OF t_bsid,
      it_t052       TYPE STANDARD TABLE OF t_t052,
      it_tvzbt      TYPE STANDARD TABLE OF t_tvzbt,
      g_bukrs       TYPE bseg-bukrs,
      g_kunnr       TYPE bseg-kunnr,
      g_hkont       TYPE bseg-hkont,
      it_/pra/bp_ad TYPE STANDARD TABLE OF t_/pra/bp_ad,
      it_kna1       TYPE STANDARD TABLE OF t_kna1.
