*&---------------------------------------------------------------------*
*& Include          ZJVAN_JV_AUDIT_REPORT_TOP_01
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
TABLES: cdhdr,
        t001,
        t8j9a.
TYPES: BEGIN OF t_jadata,
         objectclas TYPE cdhdr-objectclas,
         tcode      TYPE cdhdr-tcode,
         objectid   TYPE cdhdr-objectid,
         joa        TYPE t8j9a-joa,
         changenr   TYPE cdpos-changenr,
         tabname    TYPE cdpos-tabname,
* Begin of change 9454163 ssen 30/04/2020
*         tabkey     TYPE cdpos-tabkey,
         tabkey1    TYPE cdpos-tabkey,
         tabkey2    TYPE cdpos-tabkey,
         tabkey3    TYPE cdpos-tabkey,
         tabkey4    TYPE cdpos-tabkey,
         busname    TYPE but000-name_org1,
* End of change 9454163 ssen 30/04/2020

* Begin of change DF867 spalit 09/03/2019
*         fname      TYPE cdpos-fname,
         fname      TYPE as4text,
* End of change DF867 spalit 09/03/2019
         value_new  TYPE cdpos-value_new,
         value_old  TYPE cdpos-value_old,
         username   TYPE cdhdr-username,
         udate      TYPE cdhdr-udate,
         utime      TYPE cdhdr-utime,
         opshare    TYPE t8j9a-opshare,
         nopshare   TYPE t8j9a-nopshare,
         eqshare    TYPE t8j9c-eqshare,
         sopshare   TYPE string,
         snopshare  TYPE string,
         seqshare   TYPE string,
         ownsusp    TYPE t8j9c-ownsusp,
         ownsusper  TYPE t8j9c-ownsusper,
         ownsusyear TYPE t8j9c-ownsusyear,
         ownunsus   TYPE t8j9c-ownunsus, "changed
         ownususper TYPE t8j9c-ownususper,
         ownususyr  TYPE t8j9c-ownususyr,
         egroupsus  TYPE t8j9a-egroupsus,
         grpsusper  TYPE t8j9a-grpsusper,
         grpsusyear TYPE t8j9a-grpsusyear,
         egroupusus TYPE t8j9a-egroupusus,
         grpususper TYPE t8j9a-grpususper,
         grpususyer TYPE t8j9a-grpususyer,
       END OF t_jadata,

       BEGIN OF t_ueqgdata,
         bukrs TYPE t8j9a-bukrs,
         joa   TYPE t8j9a-joa,
         egrup TYPE t8j9a-egrup,
       END OF t_ueqgdata,

       BEGIN OF t_mjjdata,
         joa   TYPE t8jv-joa,
         vname TYPE t8jv-vname,
       END OF t_mjjdata,

       BEGIN OF t_mcndata,
         bukrs           TYPE oiu_do_do-bukrs,
         own_no          TYPE oiu_do_do-own_no,
         vname           TYPE oiu_do_do-vname,
         doi_no          TYPE oiu_do_do-doi_no,
         eff_from_dt     TYPE oiu_do_do-eff_from_dt,
         eff_to_dt       TYPE oiu_do_do-eff_to_dt,
         convnetind      TYPE char1,
         jib_offs_fl     TYPE char1,
         own_int_type_cd TYPE oiu_do_do-own_int_type_cd,
       END OF t_mcndata.

*       BEGIN OF t_exinn,
*         bukrs  TYPE t8ju-bukrs,
*         joa    TYPE t8ju-joa,
*         vname  TYPE t8jv-vname,
*         aclass TYPE t8ju-aclass,
*         vclass TYPE t8jv-vclass,
*       END OF t_exinn.



DATA:it_fjadata  TYPE TABLE OF t_jadata,
     it_ueqgdata TYPE TABLE OF t_ueqgdata,
     it_mjjdata  TYPE TABLE OF t_mjjdata,
     it_mcndata  TYPE TABLE OF t_mcndata.
*      it_exinn    TYPE TABLE OF t_exinn.

*----------------------------------------------------------------------*
* variables
*----------------------------------------------------------------------*
DATA:
  g_fieldcat TYPE slis_t_fieldcat_alv,
  g_num(2)   TYPE n.

DATA: it_listheader TYPE slis_t_listheader,
      wa_listheader TYPE slis_listheader.

TYPES: BEGIN OF ty_opshare,
         bukrs TYPE bukrs,
         joa   TYPE jv_joa,
         egrup TYPE jv_aegroup,
         partn TYPE jv_partn,
       END OF ty_opshare.

DATA: it_opshare TYPE STANDARD TABLE OF ty_opshare,
      wa_opshare TYPE ty_opshare.
