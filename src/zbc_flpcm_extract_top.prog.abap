*&----------------------------------------------------------------------------&*
*&----------------------------------------------------------------------------&*
*&                                                                            &*
*&               Report ZBC_FLPCM_EXTRACT_TOP                                  &*
*&                                                                            &*
*&----------------------------------------------------------------------------&*
*&----------------------------[   MAIN INOS   ]-------------------------------&*
*&----------------------------------------------------------------------------&*
*&                                                                            &*
*&                                                                            &*
*&  Author      : Nicolas DUPRE (NDU90045)                                    &*
*&  Release     : xx.xx.2020                                                  &*
*&                                                                            &*
*&                                                                            &*
*&----------------------------------------------------------------------------&*
*&---------------------------[   DESCRIPTION   ]------------------------------&*
*&----------------------------------------------------------------------------&*
*&                                                                            &*
*&                                                                            &*
*&----------------------------------------------------------------------------&*
*&----------------------------[   REVISIONS   ]-------------------------------&*
*&----------------------------------------------------------------------------&*
*&                                                                            &*
*&---------------#------------------------------------------------------------&*
*& Date / Author | Updates Descriptions                                       &*
*&---------------#------------------------------------------------------------&*
*&               |                                                            &*
*&---------------#------------------------------------------------------------&*
*&               |                                                            &*
*&---------------#------------------------------------------------------------&*
*&----------------------------------------------------------------------------&*



*&----------------------------------------------------------------------------&"
*&   Global Constants                                                         &"
*&----------------------------------------------------------------------------&"
CONSTANTS: gc_scope_cust               TYPE /ui2/fdm_scope        VALUE 'CUST'                                           ,
           gc_scope_conf               TYPE /ui2/fdm_scope        VALUE 'CONF'                                           ,
           gc_param_txt_label_var_name TYPE string                VALUE '_%_APP_%-TEXT'                                  ,
           gc_catalog_id_prexif_cat    TYPE string                VALUE /ui2/if_fdm_db=>gc_catalog_provider-catalog_page , " X-SAP-UI2-CATALOGPAGE
           gc_catalog_id_prexif_car    TYPE string                VALUE /ui2/if_fdm_db=>gc_catalog_provider-adcat        , " X-SAP-UI2-ADCAT
           gc_chip_id_prefix_car       TYPE string                VALUE /ui2/if_fdm_db=>gc_chip_provider-adchip          , " X-SAP-UI2-ADCHIP
           gc_chip_id_prefix_cat       TYPE string                VALUE /ui2/if_fdm_db=>gc_chip_provider-page            , " X-SAP-UI2-PAGE
           gc_prop_text_key_title      TYPE string                VALUE /ui2/if_fdm_db=>gc_text_key-title                , " display_title_text
           gc_prop_text_key_subtitle   TYPE string                VALUE /ui2/if_fdm_db=>gc_text_key-subtitle             , " display_subtitle_text
           gc_prop_text_key_info       TYPE string                VALUE /ui2/if_fdm_db=>gc_text_key-info                 , " display_info_text
           gc_prop_text_key_keywords   TYPE string                VALUE /ui2/if_fdm_db=>gc_text_key-keywords             , " display_search_keywords
           gc_catalog_typ_cat(3)       TYPE c                     VALUE 'CAT'                                            ,
           gc_catalog_typ_car(3)       TYPE c                     VALUE 'CAR'                                            ,
           gc_sign_i                   TYPE sign                  VALUE 'I'                                              ,
           gc_opt_eq                   TYPE option                VALUE 'EQ'                                             ,
           gc_bag_id_title_pty         TYPE /ui2/page_id          VALUE 'tileProperties'                                 ,
           gc_ico_tile                 TYPE /ui2/fcm_ttm_matching VALUE '@89@'                                           ," Blue square over a dark one
           gc_ico_tm                   TYPE /ui2/fcm_ttm_matching VALUE '@8A@'                                           ," White square under a yellow one
           gc_ico_tiletm               TYPE /ui2/fcm_ttm_matching VALUE '@8C@'                                           ," Double black square with intersec
           gc_ttm_type_tile            TYPE /ui2/fdm_tile_type    VALUE 'TILE'                                           ,
           gc_ttm_type_tile_tm         TYPE /ui2/fdm_tile_type    VALUE 'TILE_TARGET_MAPPING'                            ,
           gc_ttm_type_tm              TYPE /ui2/fdm_tile_type    VALUE 'TARGET_MAPPING'                                 .


*&----------------------------------------------------------------------------&"
*&   Tables References                                                        &"
*&----------------------------------------------------------------------------&"



"&----------------------------------------------------------------------------&"
"&   Global Types Definition                                                  &"
"&----------------------------------------------------------------------------&"
TYPES: BEGIN OF ty_propt                ,
         bag_parentid TYPE /ui2/page_id ,
         name         TYPE /ui2/page_id ,
         langu        TYPE langu        ,
         value(255)   TYPE c            ,
       END   OF ty_propt                .


TYPES: BEGIN OF ty_report_text  ,
         key(20)  TYPE c        , " Text Key
         spras    TYPE spras    , " Text Language
         text(50) TYPE c        , " Text Value
       END   OF ty_report_text .

TYPES: BEGIN OF ty_csv_catalog                  ,
         catalog_id  TYPE /ui2/fdm_catalog_id   , " Catalog ID
         type        TYPE /ui2/fdm_catalog_type , " Type
         scope       TYPE /ui2/fdm_scope        , " Scope
         master_lang TYPE masterlang            , " Master Language
       END   OF ty_csv_catalog                  .

TYPES: BEGIN OF ty_csv_catalog_text          ,
         catalog_id TYPE /ui2/fdm_catalog_id , " Catalog ID
         langu      TYPE langu               , " Language
         sptxt      TYPE sptxt               , " Language Designation
         title      TYPE string              , " Title in language
       END   OF ty_csv_catalog_text          .

TYPES: BEGIN OF ty_csv_tile_tm                           ,
         cdm3_ba_id      TYPE /ui2/fcm_ttm_id            , " Tile TM ID
         type            TYPE /ui2/fdm_tile_type         , " Type (TILE, TM, TILE + TM)
         tile_orig_id    TYPE /ui2/fcm_ttm_tile_orig_id  , " Tile ID when Tile Only of Tile + TM
         tm_orig_id      TYPE /ui2/fcm_ttm_tm_orig_id    , " TM ID when TM only of Tile + TM
         catalog_orig_id TYPE /ui2/fcm_ttm_tile_cat_id   , " Catalog Origin ID
         semantic_obj    TYPE /ui2/fdm_semantic_object   , " Semantic Object
         semantic_act    TYPE /ui2/fdm_semantic_action   , " Semantic Action
         fiori_id        TYPE /ui2/fcm_ttm_fiori_id      , " Fiori Application Id (App Library)
         transaction     TYPE /ui2/fcm_tm_transaction    , " Transaction SAP
       END   OF ty_csv_tile_tm                           .


TYPES: BEGIN OF ty_csv_tile_tm_text          ,
         cdm3_ba_id TYPE /ui2/fcm_ttm_id     , " Tile TM ID
         langu      TYPE langu               , " Language
         sptxt      TYPE sptxt               , " Language Designation
         title      TYPE string              , " Title in language
       END   OF ty_csv_tile_tm_text          .


TYPES: BEGIN OF ty_csv_roles                         ,
          catalog_id  TYPE /ui2/fdm_catalog_id       , " Catalog ID
*         agr_name_c  TYPE agr_name_c                , " Composite Role
          agr_name    TYPE agr_name                  , " Simple Role
          description TYPE /ui2/fcm_role_description , " Description
*         parent_agr   TYPE par_agr                   , " Father Role
       END   OF ty_csv_roles                         .



"&----------------------------------------------------------------------------&"
"&   Global Variables                                                         &"
"&----------------------------------------------------------------------------&"
" Global Objects
DATA: go_flp_cont_mgr       TYPE REF TO /ui2/if_flp_cont_mgr           ,
      go_messaging          TYPE REF TO /ui2/if_fcm_messaging          ,
      go_catalog_api        TYPE REF TO /ui2/cl_fdm_catalog_api        ,
      go_type_mapper        TYPE REF TO /ui2/cl_fcm_type_mapper        ,
      go_progress_indicator TYPE REF TO /ui2/cl_gui_progress_indicator .

" Global Variable
DATA: gv_screen_name_len        TYPE i ,
      gv_screen_name_suffix_len TYPE i ,
      gv_param_name_len         TYPE i ,
      gv_param(8)               TYPE c , " Max len is 8
      gv_text(50)               TYPE c .

" Global Range
DATA: gr_chip_id               TYPE RANGE OF /ui2/chip_id_pbc ,
      gr_prop_name             TYPE RANGE OF /ui2/page_id     .


"&----------------------------------------------------------------------------&"
"&   Global Field-symbols                                                     &"
"&----------------------------------------------------------------------------&"
FIELD-SYMBOLS: <gfs_variable> TYPE any .



"&----------------------------------------------------------------------------&"
"&   Global Tables & Work Areas                                               &"
"&----------------------------------------------------------------------------&"
DATA: gt_report_texts TYPE TABLE OF ty_report_text                                      ,
      gs_report_texts TYPE          ty_report_text                                      ,
      gt_catalog_list TYPE          /ui2/if_flp_cont_mgr=>tt_catalog_flat               ,
      gs_catalog_list TYPE          /ui2/if_flp_cont_mgr=>ts_catalog_flat               ,
      gt_tile_tm_list TYPE          /ui2/if_flp_cont_mgr=>tt_tile_tm_combination_sorted ,
      gs_tile_tm_list TYPE          /ui2/if_flp_cont_mgr=>ts_tile_tm_combination        ,
      gt_role_list    TYPE          /ui2/if_flp_cont_mgr=>tt_role                       ,
      gs_role_list    TYPE          /ui2/if_flp_cont_mgr=>ts_role                       ,
      gt_agr_define   TYPE TABLE OF agr_define                                          , " Roles & Link Father<>Child
      gs_agr_define   TYPE          agr_define                                          ,
      gt_agr_agrs     TYPE TABLE OF agr_agrs                                            , " Composites roles
      gs_agr_agrs     TYPE          agr_agrs                                            ,
      gt_page_t       TYPE TABLE OF /ui2/pb_c_paget                                     , " Text Page 'CONF'
      gs_page_t       TYPE          /ui2/pb_c_paget                                     ,
      gt_pagem_t      TYPE TABLE OF /ui2/pb_c_pagemt                                    , " Text Page 'CUST'
      gs_pagem_t      TYPE          /ui2/pb_c_pagemt                                    ,
      gt_t002t        TYPE TABLE OF t002t                                               ,
      gs_t002t        TYPE          t002t                                               ,
      gt_propt        TYPE TABLE OF ty_propt                                            ,
      gs_propt        TYPE          ty_propt                                            ,
      gt_csv_catalog  TYPE TABLE OF ty_csv_catalog                                      ,
      gs_csv_catalog  TYPE          ty_csv_catalog                                      ,
      gt_csv_catalogt TYPE TABLE OF ty_csv_catalog_text                                 ,
      gs_csv_catalogt TYPE          ty_csv_catalog_text                                 ,
      gt_csv_tiletm   TYPE TABLE OF ty_csv_tile_tm                                      ,
      gs_csv_tiletm   TYPE          ty_csv_tile_tm                                      ,
      gt_csv_tiletmt  TYPE TABLE OF ty_csv_tile_tm_text                                 ,
      gs_csv_tiletmt  TYPE          ty_csv_tile_tm_text                                 ,
      gt_csv_role     TYPE TABLE OF ty_csv_roles                                        ,
      gs_csv_role     TYPE          ty_csv_roles                                        .




"&----------------------------------------------------------------------------&"
"&   Class Definitions                                                        &"
"&----------------------------------------------------------------------------&"



"&----------------------------------------------------------------------------&"
"&   Class Implementations                                                    &"
"&----------------------------------------------------------------------------&"
