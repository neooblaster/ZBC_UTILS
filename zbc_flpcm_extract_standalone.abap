*&----------------------------------------------------------------------------&*
*&----------------------------------------------------------------------------&*
*&                                                                            &*
*&               Report ZBC_FLPCM_EXTRACT                                     &*
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
*&  The report must be less intrusive and the easiest to install as possible. &*
*&  The entire code stand in this only report and all text as well.           &*
*&  So this report contains hardcoded text.                                   &*
*&                                                                            &*
*&  The purpose of the report is to extract all FLP Catalogs with             &*
*&  their tile/target mapping and associated roles in an CSV Excel File       &*
*&                                                                            &*
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
REPORT zbc_flpcm_extract.
****************************************************
* abapmerge - ZBC_FLPCM_EXTRACT_TOP
****************************************************
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
CONSTANTS: gc_scope_cust               TYPE /ui2/fdm_scope VALUE 'CUST'                                           ,
           gc_scope_conf               TYPE /ui2/fdm_scope VALUE 'CONF'                                           ,
           gc_param_txt_label_var_name TYPE string         VALUE '_%_APP_%-TEXT'                                  ,
           gc_catalog_id_prexif_cat    TYPE string         VALUE /ui2/if_fdm_db=>gc_catalog_provider-catalog_page , " X-SAP-UI2-CATALOGPAGE
           gc_catalog_id_prexif_car    TYPE string         VALUE /ui2/if_fdm_db=>gc_catalog_provider-adcat        , " X-SAP-UI2-ADCAT
           gc_chip_id_prefix_car       TYPE string         VALUE /ui2/if_fdm_db=>gc_chip_provider-adchip          , " X-SAP-UI2-ADCHIP
           gc_chip_id_prefix_cat       TYPE string         VALUE /ui2/if_fdm_db=>gc_chip_provider-page            , " X-SAP-UI2-PAGE
           gc_prop_text_key_title      TYPE string         VALUE /ui2/if_fdm_db=>gc_text_key-title                , " display_title_text
           gc_prop_text_key_subtitle   TYPE string         VALUE /ui2/if_fdm_db=>gc_text_key-subtitle             , " display_subtitle_text
           gc_prop_text_key_info       TYPE string         VALUE /ui2/if_fdm_db=>gc_text_key-info                 , " display_info_text
           gc_prop_text_key_keywords   TYPE string         VALUE /ui2/if_fdm_db=>gc_text_key-keywords             , " display_search_keywords
           gc_catalog_typ_cat(3)       TYPE c              VALUE 'CAT'                                            ,
           gc_catalog_typ_car(3)       TYPE c              VALUE 'CAR'                                            ,
           gc_sign_i                   TYPE sign           VALUE 'I'                                              ,
           gc_opt_eq                   TYPE option         VALUE 'EQ'                                             ,
           gc_bag_id_title_pty         TYPE /ui2/page_id   VALUE 'tileProperties'                                 .

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

*TYPES: BEGIN OF ty_csv_extract                             ,
*                                                             " Composite Role
*                                                             " Father Role
*                                                             " Simple / Child Role
*        id                  TYPE /ui2/fdm_catalog_id       , " Catalogue ID
*        title               TYPE /ui2/fdm_catalog_title    , " Catalogue Title
*        type                TYPE /ui2/fcm_ttm_matching     , " Tile/TM Type (TILE/TM)
*        ttm_title           TYPE /ui2/fcm_ttm_title        , " Tile/TM Title
*        semantic_object     TYPE /ui2/fdm_semantic_object  , " Semantic Object
*        semantic_action     TYPE /ui2/fdm_semantic_action  , " Semantic Action
*        fiori_id            TYPE /ui2/fcm_ttm_fiori_id     , " SAP Fiori Library ID
*        tm_orig_id          TYPE /ui2/fcm_ttm_tm_orig_id   , " ID du mappage cible d'origine
*        tm_orig_catalog_id  TYPE /ui2/fcm_ttm_tm_cat_id    , " ID de catalogue de mappages cibles
*                                                             " Space
*                                                             " Space Desription
*       END   OF ty_csv_extract                             .

TYPES: BEGIN OF ty_csv_catalog               ,
         catalog_id TYPE /ui2/fdm_catalog_id , " Catalog ID
       END   OF ty_csv_catalog               .

TYPES: BEGIN OF ty_csv_catalog_text          ,
         catalog_id TYPE /ui2/fdm_catalog_id , " Catalog ID
         langu      TYPE langu               , " Language
         sptxt      TYPE sptxt               , " Language Designation
         title      TYPE string              , " Title in language
       END   OF ty_csv_catalog_text          .

*TYPES: BEGIN OF ty_csv_tile_tm               ,
*         tile_id    TYPE
*       END   OF ty_csv_tile_tm               .
*
*TYPES: BEGIN OF ty_csv_tile_tm_text          ,
*         tile_id    TYPE n
*         langu      TYPE langu               , " Language
*         sptxt      TYPE sptxt               , " Language Designation
*         title      TYPE string              , " Title in language
*       END   OF ty_csv_tile_tm_text          .
TYPES: BEGIN OF ty_csv_roles                 ,
         agr_name_c TYPE agr_name_c          , " Composite Role
         agr_name   TYPE agr_name            , " Simple Role
         parent_agr TYPE par_agr             , " Father Role
         catalog_id TYPE /ui2/fdm_catalog_id , " Catalog ID
       END   OF ty_csv_roles                 .

"&----------------------------------------------------------------------------&"
"&   Global Variables                                                         &"
"&----------------------------------------------------------------------------&"
" Global Objects
DATA: go_flp_cont_mgr TYPE REF TO /ui2/if_flp_cont_mgr    ,
      go_messaging    TYPE REF TO /ui2/if_fcm_messaging   ,
      go_catalog_api  TYPE REF TO /ui2/cl_fdm_catalog_api ,
      go_type_mapper  TYPE REF TO /ui2/cl_fcm_type_mapper .

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
*      gt_csv_extract  TYPE TABLE OF ty_csv_extract                                      ,
*      gs_csv_extract  TYPE          ty_csv_extract                                      ,
      gt_csv_catalog  TYPE TABLE OF ty_csv_catalog                                      ,
      gs_csv_catalog  TYPE          ty_csv_catalog                                      ,
      gt_csv_catalogt TYPE TABLE OF ty_csv_catalog_text                                 ,
      gs_csv_catalogt TYPE          ty_csv_catalog_text                                 ,
      gt_csv_role     TYPE TABLE OF ty_csv_roles                                        ,
      gs_csv_role     TYPE          ty_csv_roles                                        .
"&----------------------------------------------------------------------------&"
"&   Class Definitions                                                        &"
"&----------------------------------------------------------------------------&"

"&----------------------------------------------------------------------------&"
"&   Class Implementations                                                    &"
"&----------------------------------------------------------------------------&"

****************************************************
* abapmerge - ZBC_FLPCM_EXTRACT_SCR
****************************************************
*&----------------------------------------------------------------------------&*
*&----------------------------------------------------------------------------&*
*&                                                                            &*
*&               Report ZBC_FLPCM_EXTRACT_SCR                                 &*
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
*&----------------------------------------------------------------------------&*=
*&----------------------------------------------------------------------------&"
*&   Selection Screen                                                         &"
*&----------------------------------------------------------------------------&"
PARAMETERS: p_scope TYPE /ui2/fdm_scope DEFAULT gc_scope_cust .

*&----------------------------------------------------------------------------&"
*&   Screen Validations                                                       &"
*&----------------------------------------------------------------------------&"
AT SELECTION-SCREEN OUTPUT.

  " Set Parameters Texts lables
  LOOP AT SCREEN.
    " Handle Parameter Text Label : %_xxxxxx_%_APP_%-TEXT
    IF screen-name CS gc_param_txt_label_var_name.
      " Count Screen Name Length
      gv_screen_name_len        = strlen( screen-name ).                              " Len of %_xxxxxx_%_APP_%-TEXT
      gv_screen_name_suffix_len = strlen( gc_param_txt_label_var_name ).              " Len of _%_APP_%-TEXT
      gv_param_name_len         = gv_screen_name_len - gv_screen_name_suffix_len - 2. " Len of xxxxxx  (-2 is for begenning %_)
      gv_param                  = screen-name+2(gv_param_name_len) .

      " Get text label
      PERFORM get_text USING gv_param CHANGING gv_text.

      " Set Text Label
      IF gv_text IS NOT INITIAL.
        ASSIGN (screen-name) TO <gfs_variable>.

        IF <gfs_variable> IS ASSIGNED.
          <gfs_variable> = gs_report_texts-text.
          UNASSIGN <gfs_variable>.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_scope.
  PERFORM f4_scope_value_request USING 'P_SCOPE' CHANGING p_scope .
*  PERFORM f4_scope_value_request USING 'P_SCOPE' CHANGING p_scope.

****************************************************
* abapmerge - ZBC_FLPCM_EXTRACT_F01
****************************************************
*&----------------------------------------------------------------------------&*
*&----------------------------------------------------------------------------&*
*&                                                                            &*
*&               Report ZBC_FLPCM_EXTRACT_F01                                  &*
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
"&----------------------------------------------------------------------------&"
"&   INITIALIZE                                                               &"
"&                                                                            &"
"&   Initialize data, tables & instantiate objects.                           &"
"&----------------------------------------------------------------------------&"
"&
"&----------------------------------------------------------------------------&"
"&
"& PERFORM INITIALIZE
"&
"&----------------------------------------------------------------------------&"
FORM initialize .
  " --------------------------------------------------------------------------
  "  Initialize Ranges
  " --------------------------------------------------------------------------
  APPEND VALUE #( sign = gc_sign_i option = gc_opt_eq low = gc_prop_text_key_title    ) TO gr_prop_name .
  APPEND VALUE #( sign = gc_sign_i option = gc_opt_eq low = gc_prop_text_key_subtitle ) TO gr_prop_name .
  APPEND VALUE #( sign = gc_sign_i option = gc_opt_eq low = gc_prop_text_key_info     ) TO gr_prop_name .
  APPEND VALUE #( sign = gc_sign_i option = gc_opt_eq low = gc_prop_text_key_keywords ) TO gr_prop_name .

  " --------------------------------------------------------------------------
  "  Initialize Texts
  " --------------------------------------------------------------------------
  " • Paramters Text Label
  APPEND VALUE #( key = 'P_SCOPE' spras = 'F' text = 'Scope'  ) TO gt_report_texts.
  APPEND VALUE #( key = 'P_SCOPE' spras = 'E' text = 'Scope'  ) TO gt_report_texts.

  " • Report Texts
  APPEND VALUE #( key = 'SCOPE_CONF_DESCR' spras = 'F' text = 'Configuration'  ) TO gt_report_texts.
  APPEND VALUE #( key = 'SCOPE_CONF_DESCR' spras = 'F' text = 'Configuration'  ) TO gt_report_texts.

  APPEND VALUE #( key = 'SCOPE_CUST_DESCR' spras = 'F' text = 'Customizing'    ) TO gt_report_texts.
  APPEND VALUE #( key = 'SCOPE_CUST_DESCR' spras = 'E' text = 'Customizing'    ) TO gt_report_texts.

ENDFORM.
"&---[   INITIALIZE   ]-------------------------------------------------------&"
"&----------------------------------------------------------------------------&"
"&----------------------------------------------------------------------------&"
"&   F4_SCOPE_VALUE_REQUEST                                                   &"
"&                                                                            &"
"&   Generates available value for paramter P_SCOPE                           &"
"&----------------------------------------------------------------------------&"
"&
"&----------------------------------------------------------------------------&"
"&
"& PERFORM INITIALIZE
"&
"&----------------------------------------------------------------------------&"
FORM f4_scope_value_request USING    i_dynpf
                            CHANGING c_field .

  TYPES: BEGIN OF ty_value                          ,
           scope     TYPE /ui2/fdm_scope            ,
           descr     TYPE gdv_comp_type_description , " Len = 60
         END OF ty_value                            .

  DATA: l_value     TYPE dynfieldvalue                ,
        l_repid     TYPE sy-repid                     ,
        l_pvalkey   TYPE ddshpvkey                    , " personal value key
        ls_dynp     TYPE dynpread                     ,
        ls_return   TYPE ddshretval                   ,
        lt_dynp     TYPE STANDARD TABLE OF dynpread   ,
        lt_scope    TYPE STANDARD TABLE OF ty_value   ,
        ls_scope    TYPE                   ty_value   ,
        lt_return   TYPE STANDARD TABLE OF ddshretval ,
        lv_text(50) TYPE c                            .
  " Get current value from screen
  ls_dynp-fieldname = i_dynpf.
  APPEND ls_dynp TO lt_dynp.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_dynp
    EXCEPTIONS
      OTHERS     = 0.

  READ TABLE lt_dynp INTO ls_dynp INDEX 1.
  " Make Scope List
  PERFORM get_text USING 'SCOPE_CONF_DESCR' CHANGING lv_text.
  APPEND VALUE #( scope = gc_scope_conf descr = lv_text ) TO lt_scope .
  PERFORM get_text USING 'SCOPE_CUST_DESCR' CHANGING lv_text.
  APPEND VALUE #( scope = gc_scope_cust descr = lv_text ) TO lt_scope .

  " Call own F4-help, not showing internal status number
  l_value = ls_dynp-fieldvalue.
  l_repid = sy-repid.

  " fill key for personal value list
  l_pvalkey = sy-uname.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*      DDIC_STRUCTURE         = ' '
      retfield               = 'SCOPE'
      PVALKEY                = l_pvalkey
*     DYNPPROG               = ' '
*     DYNPNR                 = ' '
*     DYNPROFIELD            = ' '
*     STEPL                  = 0
*     WINDOW_TITLE           =
      VALUE                  = l_value
      VALUE_ORG              = 'S' "def = C
*     MULTIPLE_CHOICE        = ' '
*     DISPLAY                = ' '
      CALLBACK_PROGRAM       = l_repid
*      CALLBACK_FORM          = 'F4_SCOPE_VALUE_REQUEST_CALLBACK'
*     CALLBACK_METHOD        =
*     MARK_TAB               =
*   IMPORTING
*     USER_RESET             =
    tables
      value_tab              = lt_scope
*     FIELD_TAB              =
      RETURN_TAB             = lt_return
*     DYNPFLD_MAPPING        =
   EXCEPTIONS
     PARAMETER_ERROR        = 1
     NO_VALUES_FOUND        = 2
     OTHERS                 = 3                .

  " Get selected value and internal status number
  READ TABLE lt_return INTO ls_return WITH KEY fieldname = 'F0001'. " Generated name. Fieldname are available when DDIC_STRUCTURE is set
  IF sy-subrc = 0.
    READ TABLE lt_scope WITH KEY scope = ls_return-fieldval
                        INTO ls_scope.
    CHECK sy-subrc = 0.
    c_field = ls_scope-scope.
  ELSE.
    CLEAR: c_field.
  ENDIF.
ENDFORM.
"&---[   F4_SCOPE_VALUE_REQUEST   ]-------------------------------------------&"
"&----------------------------------------------------------------------------&"
"&----------------------------------------------------------------------------&"
"&   GET_TEXT                                                                 &"
"&                                                                            &"
"&   Retrieve text in internal table which storing text for provided key      &"
"&----------------------------------------------------------------------------&"
"&
"&----------------------------------------------------------------------------&"
"&
"& PERFORM get_text USING '<textKey>' CHANGING gv_text .
"&
"&----------------------------------------------------------------------------&"
FORM get_text USING i_text_key CHANGING c_text .

  CLEAR: gs_report_texts ,
         c_text          .

  READ TABLE gt_report_texts INTO gs_report_texts WITH KEY key   = i_text_key
                                                           spras = sy-langu   .

  IF sy-subrc EQ 0.
    c_text = gs_report_texts-text .
  ENDIF.

ENDFORM.
"&---[   GET_TEXT   ]---------------------------------------------------------&"
"&----------------------------------------------------------------------------&"
"&----------------------------------------------------------------------------&"
"&   INITIALIZE_FLP_CONT_MGR                                                  &"
"&                                                                            &"
"&   Initialize util FLP Content Manager class                                &"
"&   (source: report /UI2/FLP_CONT_MGR cf start-of-selection event.           &"
"&----------------------------------------------------------------------------&"
"&
"&----------------------------------------------------------------------------&"
"&
"& PERFORM initialize_flp_cont_mgr .
"&
"&----------------------------------------------------------------------------&"
FORM initialize_flp_cont_mgr .

  " Initialization
  go_messaging = NEW /ui2/cl_fcm_messaging( ).

  go_catalog_api = NEW /ui2/cl_fdm_catalog_api(
    iv_scope     = p_scope
    iv_use_cache = abap_true
  ).

  go_flp_cont_mgr = NEW /ui2/cl_flp_cont_mgr(
    iv_scope       = p_scope
    io_messaging   = go_messaging
    io_catalog_api = go_catalog_api
  ).

  go_type_mapper   = NEW /ui2/cl_fcm_type_mapper( ).

ENDFORM.
"&---[   INITIALIZE_FLP_CONT_MGR   ]------------------------------------------&"
"&----------------------------------------------------------------------------&"
"&----------------------------------------------------------------------------&"
"&   GET_DATA                                                                 &"
"&                                                                            &"
"&   Retrieve Catalog & Role data as it done in /ui2/flpcm_cust & conf        &"
"&                                                                            &"
"&   Catalogue =                                                              &"
"&     - 0-n tile                                                             &"
"&     - 0-n tm                                                               &"
"&     - 0-n role                                                             &"
"&                                                                            &"
"&   Tile / TM :                                                              &"
"&     - 0-n role                                                             &"
"&                                                                            &"
"&   Catalogue :                                                              &"
"&     n Tile TM                                                              &"
"&       n Role                                                               &"
"&                                                                            &"
"&                                                                            &"
"&                                                                            &"
"&----------------------------------------------------------------------------&"
FORM get_data.

  TYPES: BEGIN OF lty_catalog_id       ,
           id    TYPE /ui2/page_id_pbc ,
         END   OF lty_catalog_id       .

  DATA: lt_catalog_ids_conf TYPE TABLE OF lty_catalog_id   ,
        lt_catalog_ids_cust TYPE TABLE OF lty_catalog_id   ,
        lv_chip_id          TYPE          /ui2/chip_id_pbc .

  "  Initialize FLP Content Manager if not done
  " -------------------------------------------------------------------
  IF go_flp_cont_mgr IS NOT BOUND.
    PERFORM initialize_flp_cont_mgr .
  ENDIF.
  "  Get All Catalogs "Tab Catalogs" :
  " -------------------------------------------------------------------
  go_flp_cont_mgr->initialize_tables_for_catalogs( ).       " This statement collects all Catalog / Tile TM data
  gt_catalog_list = go_flp_cont_mgr->get_all_catalogs( ).   " Returns Catalog List
  gt_tile_tm_list = go_flp_cont_mgr->get_all_tiles_tms( ).  " Returns Tile TM List
  "  Get Catalog Texts
  "
  "   Catalogue Page (IS_CATALOG_PAGE = X), Catalog ID are prefixed with 'X-SAP-UI2-CATALOGPAGE:' in table /ui2/pb_c_page (and in text table /ui2/pb_c_paget)
  "   If the type catalogue type is CAR, the prefix is X-SAP-UI2-ADCAT and the system_alias value is append to the end
  "
  " -------------------------------------------------------------------
  lt_catalog_ids_conf = VALUE #(
    FOR <ls_conf> IN gt_catalog_list WHERE ( scope = gc_scope_conf ) ( id =
      COND #( WHEN <ls_conf>-TYPE EQ gc_catalog_typ_car
              THEN |{ gc_catalog_id_prexif_car }:{ <ls_conf>-id }:{ <ls_conf>-system_alias }| " X-SAP-UI2-ADCAT:<catalog_id>:<system_alias>
              ELSE |{ gc_catalog_id_prexif_cat }:{ <ls_conf>-id }|                            " X-SAP-UI2-CATALOGPAGE:<catalog_id>
  ) ) ).
  lt_catalog_ids_cust = VALUE #(
    FOR <ls_cust> IN gt_catalog_list WHERE ( scope = gc_scope_cust ) ( id =
      COND #( WHEN <ls_cust>-TYPE EQ gc_catalog_typ_car
              THEN |{ gc_catalog_id_prexif_car }:{ <ls_cust>-id }:{ <ls_cust>-system_alias }| " X-SAP-UI2-ADCAT:<catalog_id>:<system_alias>
              ELSE |{ gc_catalog_id_prexif_cat }:{ <ls_cust>-id }|                            " X-SAP-UI2-CATALOGPAGE:<catalog_id>
  ) ) ).
  SORT lt_catalog_ids_conf.
  SORT lt_catalog_ids_cust.
  DELETE ADJACENT DUPLICATES FROM lt_catalog_ids_conf.
  DELETE ADJACENT DUPLICATES FROM lt_catalog_ids_cust.

  " 'CONF' Scope
  IF lt_catalog_ids_conf IS NOT INITIAL.
    SELECT * FROM /ui2/pb_c_paget INTO TABLE gt_page_t FOR ALL ENTRIES IN lt_catalog_ids_conf WHERE id = lt_catalog_ids_conf-id.
  ENDIF.
  " 'CUST' Scope
  IF lt_catalog_ids_cust IS NOT INITIAL.
    SELECT * FROM /ui2/pb_c_pagemt INTO TABLE gt_pagem_t FOR ALL ENTRIES IN lt_catalog_ids_cust WHERE id = lt_catalog_ids_cust-id.
  ENDIF.

  " Get Language Designation
  SELECT * FROM t002t INTO TABLE gt_t002t.
  "  Get Tile & Target Mapiing Texts
  "
  "   Texts are stored in the table /ui2/pb_c_prop table under a specific property type (BAG_ID)
  "
  " -------------------------------------------------------------------
  LOOP AT gt_tile_tm_list INTO gs_tile_tm_list.
    CLEAR lv_chip_id .
    PERFORM tile_tm_to_chip_id USING gs_tile_tm_list CHANGING lv_chip_id .
    APPEND VALUE #(
      sign   = gc_sign_i
      option = gc_opt_eq
      low    = lv_chip_id
    ) TO gr_chip_id .
  ENDLOOP.

  SELECT bag_parentid name langu value FROM /ui2/pb_c_propt INTO TABLE gt_propt WHERE bag_parentid IN gr_chip_id
*                                                                                  AND bag_id       EQ gc_bag_id_title_pty
                                                                                  AND name         IN gr_prop_name        .
  "  Get All Roles "Tab Roles" :
  " -------------------------------------------------------------------
  go_flp_cont_mgr->initialize_tables_for_roles( value #( ) ). " This statement collects all roles
  gt_role_list = go_flp_cont_mgr->get_all_roles( ).           " Returns Role List
  "  Get All Role & Links
  " -------------------------------------------------------------------
  SELECT * FROM agr_define INTO TABLE gt_agr_define.          " OK for 10k entries
  SELECT * FROM agr_agrs   INTO TABLE gt_agr_agrs  .          " OK for  2k entries

  " A Tile TM can having no assgined role.
  " A Tile TM contains data about catalog.
  " CSV Data are at level PROFIL > ROLE > TILE TM > CATALOG
  " TILE / TM has a Father Role
  "   AGR_DEFINE table contains father<>child link
  "   AGR_AGRS   table contains composite<>child link
*  LOOP AT gt_tile_tm_list INTO gs_tile_tm_list.
*    IF sy-tabix GE 10.
*      EXIT.
*    ENDIF.
*
*    REFRESH: lt_roles .
*
*    " Get Tile rôle
*    lt_roles = go_flp_cont_mgr->get_roles_of_tile_tm( gs_tile_tm_list ).
*  ENDLOOP.

" Lire les roles d'une tuile c'est lire les rôles du catalogue en utilisant MT_ROLE_CATALOG_ASSIGNMENT qui est privé et pas de method pour la récupérer
" (alimenté par initialize_tables_for_roles)

" Lien CATALOG-TILE_TM :: TILE_TM_LIST-TILE_ORIG_CATALOG_ID <> CATALOG_LIST-ID
  " -> Si on veut limité au type de catalogue
  " -> Faut boucler au catalogue
  " ->
" Afficher les TILE TM du cat
" loop at gt_catalog_list into gs_catalog_list .
*  go_type_mapper   = NEW /ui2/cl_fcm_type_mapper( ).
*  ls_catalogue_key = go_type_mapper->catalog_key_from_flat( gt_catalog_list[ 1 ] ). " gs_catalog_list

*  lt_tile_tm_combination = go_flp_cont_mgr->get_tiles_tms_of_catalog( ls_catalogue_key ).
*  lt_roles               = go_flp_cont_mgr->get_roles_of_catalog( ls_catalogue_key ).
" end loop

*  BREAK-POINT.

ENDFORM.
"&---[   GET_DATA   ]---------------------------------------------------------&"
"&----------------------------------------------------------------------------&"

*FORM  .
*ENDFORM.
"&----------------------------------------------------------------------------&"
"&   EXPORT_DATA_TO_CSV                                                       &"
"&                                                                            &"
"&   Retrieve Catalog & Role data as it done in /ui2/flpcm_cust & conf        &"
"&                                                                            &"
"&----------------------------------------------------------------------------&"
FORM export_data_to_csv .

  DATA: lt_tile_tm_combination TYPE          /ui2/if_flp_cont_mgr=>tt_tile_tm_combination_sorted ,
        lt_roles               TYPE          /ui2/if_flp_cont_mgr=>tt_role                       ,
        ls_roles               TYPE          /ui2/if_flp_cont_mgr=>ts_role                       ,
        lt_role_of_catalog     TYPE          /ui2/if_flp_cont_mgr=>tt_role                       ,
        ls_role_of_catalog     TYPE          /ui2/if_flp_cont_mgr=>ts_role                       ,
        lt_role_of_tile_tm     TYPE          /ui2/if_flp_cont_mgr=>tt_role                       ,
        ls_role_of_tile_tm     TYPE          /ui2/if_flp_cont_mgr=>ts_role                       ,
        ls_catalogue_key       TYPE          /ui2/if_fdm=>ts_catalog_key                         ,
        lv_prefixed_catalog_id TYPE          string                                              ,
        lt_page_t              TYPE TABLE OF /ui2/pb_c_paget                                     ,
        lt_pagem_t             TYPE TABLE OF /ui2/pb_c_pagemt                                    ,
        lv_langu_2c(2)         TYPE          c                                                   ,
        lv_langu_txt           TYPE          sptxt                                               .

  FIELD-SYMBOLS: <fst_text_table> TYPE ANY TABLE ,
                 <fss_text_table> TYPE ANY       ,
                 <fss_text_langu> TYPE ANY       ,
                 <fss_text_title> TYPE ANY       .

  BREAK-POINT.
  "  Process Data
  " ----------------------------------------------------------
  LOOP AT gt_catalog_list INTO gs_catalog_list.

    ls_catalogue_key = go_type_mapper->catalog_key_from_flat( gs_catalog_list ).
    lt_roles         = go_flp_cont_mgr->get_roles_of_catalog( ls_catalogue_key ). " Returns Father Roles
    " Catalog Parsing
    APPEND VALUE #(
      catalog_id = gs_catalog_list-id
    ) TO gt_csv_catalog .
    " Text Parsing (Is there is at least 1 text in the table)
    lv_prefixed_catalog_id = COND #( WHEN gs_catalog_list-type EQ gc_catalog_typ_car
              THEN |{ gc_catalog_id_prexif_car }:{ gs_catalog_list-id }:{ gs_catalog_list-system_alias }| " X-SAP-UI2-ADCAT:<catalog_id>:<system_alias>
              ELSE |{ gc_catalog_id_prexif_cat }:{ gs_catalog_list-id }|                                   " X-SAP-UI2-CATALOGPAGE:<catalog_id>
    ).

    IF gs_catalog_list-scope EQ gc_scope_conf.
      REFRESH lt_page_t.
      LOOP AT gt_page_t INTO gs_page_t WHERE id = lv_prefixed_catalog_id.
        APPEND gs_page_t TO lt_page_t.
      ENDLOOP.
      ASSIGN lt_page_t TO <fst_text_table> .

*   " Automatically else case is CUST
    ELSE.
      REFRESH lt_pagem_t.
      LOOP AT gt_pagem_t INTO gs_pagem_t WHERE id = lv_prefixed_catalog_id.
        APPEND gs_pagem_t TO lt_pagem_t.
      ENDLOOP.
      ASSIGN lt_pagem_t TO <fst_text_table> .
    ENDIF.

    " Loop on the appropriate text table
    IF <fst_text_table> IS ASSIGNED.
      LOOP AT <fst_text_table> ASSIGNING <fss_text_table>.
        ASSIGN COMPONENT 'LANGU' OF STRUCTURE <fss_text_table> TO <fss_text_langu> .
        ASSIGN COMPONENT 'TITLE' OF STRUCTURE <fss_text_table> TO <fss_text_title> .

        IF <fss_text_langu> IS ASSIGNED AND <fss_text_title> IS ASSIGNED.
          " Get Language Designation Its name in its language
          READ TABLE gt_t002t INTO gs_t002t WITH KEY spras = <fss_text_langu>   " Translation Lang
                                                     sprsl = <fss_text_langu> . " Language Code

          " Append to CSV Table
          APPEND VALUE #(
            catalog_id = gs_catalog_list-id
            langu      = <fss_text_langu>
            sptxt      = gs_t002t-sptxt
            title      = <fss_text_title>
          ) TO gt_csv_catalogt.
        ENDIF.
      ENDLOOP.

      UNASSIGN: <fst_text_table> ,
                <fss_text_langu> ,
                <fss_text_title> .
    ENDIF.

    " Roles Parsing

    " Read 'father role' to get simple one
*    LOOP AT lt_roles INTO ls_roles.
*      "
*
*      APPEND VALUE #(
*        agr_name_c = ''                   " Composite Role
*        agr_name   = ''                   " Role
*        parent_agr = ls_roles-role_name   " Father
*        catalog_id = gs_catalog_list-id   " Catalog ID
*      ) TO gt_csv_role.
*
*    ENDLOOP.

  ENDLOOP.

  "  Make CSV File for Role
  " ----------------------------------------------------------
  "  Write CSV Files
  " ----------------------------------------------------------
  PERFORM int_tab_to_csv_file USING '/tmp/flpcm_cust_catalogs.csv'       CHANGING gt_csv_catalog .
  PERFORM int_tab_to_csv_file USING '/tmp/flpcm_cust_catalogs_texts.csv' CHANGING gt_csv_catalogt .
  PERFORM int_tab_to_csv_file USING '/tmp/flpcm_cust_role.csv'           CHANGING gt_csv_role .
ENDFORM.
"&---[   EXPORT_DATA_TO_CSV   ]-----------------------------------------------&"
"&----------------------------------------------------------------------------&"

FORM tile_tm_to_chip_id    USING is_tile_tm TYPE /ui2/if_flp_cont_mgr=>ts_tile_tm_combination
                        CHANGING cv_chip_id TYPE /ui2/chip_id_pbc                              .

  cv_chip_id = COND #(
    WHEN is_tile_tm-tm_orig_catalog_type EQ gc_catalog_typ_car
    THEN |{ gc_chip_id_prefix_car }:{ gc_catalog_id_prexif_car }:{ is_tile_tm-tm_orig_catalog_id }:{ is_tile_tm-tm_orig_catalog_sysalias }:{ is_tile_tm-tm_orig_id }|
    ELSE |{ gc_chip_id_prefix_cat }:{ gc_catalog_id_prexif_cat }:{ is_tile_tm-tm_orig_catalog_id }:{ is_tile_tm-tm_orig_id }|
  ).

ENDFORM .

FORM int_tab_to_csv_file using iv_filepath CHANGING ct_inttab TYPE STANDARD TABLE.

  DATA: lo_data         TYPE REF TO data                                 ,
        lo_structdescr  TYPE REF TO cl_abap_structdescr                  ,
        lt_structfields TYPE        cl_abap_structdescr=>component_table ,
        ls_structfield  TYPE        cl_abap_structdescr=>component       ,
        lo_elemdescr    TYPE REF TO cl_abap_elemdescr                    ,
        lo_datadescr    TYPE REF TO cl_abap_datadescr                    ,
        lo_typedescr    TYPE REF TO cl_abap_typedescr                    ,
        lv_csv_line     TYPE        string                               ,
        lv_quote_or_not TYPE        c                                    .
  FIELD-SYMBOLS: <fss_inttab>      TYPE ANY ,
                 <fss_field_value> TYPE ANY .

  "  Get Components List
  " -------------------------------------------------
  " Create a structure from table (even if not empty)
  CREATE DATA lo_data LIKE LINE OF ct_inttab .
  ASSIGN lo_data->* TO <fss_inttab> .
  lo_structdescr ?= cl_abap_structdescr=>describe_by_data( <fss_inttab> ).
  lt_structfields = lo_structdescr->get_components( ) .
  "  Open File
  " -------------------------------------------------
  OPEN DATASET iv_filepath FOR OUTPUT IN TEXT MODE ENCODING utf-8 WITH BYTE-ORDER MARK .

  "  Set Header
  " -------------------------------------------------
  LOOP AT lt_structfields INTO ls_structfield .
    " Use Quote for text fields
    lo_elemdescr ?= ls_structfield-type.

    IF lo_elemdescr->type_kind EQ cl_abap_typedescr=>typekind_char OR lo_elemdescr->type_kind EQ cl_abap_typedescr=>typekind_string.
      lv_quote_or_not = '"'.
    ELSE.
      lv_quote_or_not = ''.
    ENDIF.

    " Add semicolon even for first field to prevent management case
    lv_csv_line = |{ lv_csv_line };{ lv_quote_or_not }{ ls_structfield-name }{ lv_quote_or_not }|.
  ENDLOOP.

  " Remove first semicolon & Append line to CSV File
  SHIFT lv_csv_line BY 1 PLACES LEFT IN CHARACTER MODE.
  TRANSFER lv_csv_line TO iv_filepath.

  "  Set Data
  " -------------------------------------------------
  LOOP AT ct_inttab ASSIGNING <fss_inttab> .
    CLEAR lv_csv_line.

    " Use Quote for text fields
    LOOP AT lt_structfields INTO ls_structfield .
     " Use Quote for text fields
     lo_elemdescr ?= ls_structfield-type.

     IF lo_elemdescr->type_kind EQ cl_abap_typedescr=>typekind_char OR lo_elemdescr->type_kind EQ cl_abap_typedescr=>typekind_string.
       lv_quote_or_not = '"'.
     ELSE.
       lv_quote_or_not = ''.
     ENDIF.

     ASSIGN COMPONENT ls_structfield-name OF STRUCTURE <fss_inttab> TO <fss_field_value> .

     IF <fss_field_value> IS ASSIGNED.
       " Add semicolon even for first field to prevent management case
       lv_csv_line = |{ lv_csv_line };{ lv_quote_or_not }{ <fss_field_value> }{ lv_quote_or_not }|.
     ENDIF.

     UNASSIGN <fss_field_value>.
    ENDLOOP.
    " Remove first semicolon & Append line to CSV File
    SHIFT lv_csv_line BY 1 PLACES LEFT IN CHARACTER MODE.
    TRANSFER lv_csv_line TO iv_filepath.

  ENDLOOP.

  "  Close File
  " -------------------------------------------------
  CLOSE DATASET iv_filepath .

ENDFORM.


*&----------------------------------------------------------------------------&"
*&   Initialization                                                           &"
*&----------------------------------------------------------------------------&"
INITIALIZATION.

  " Initialize Report
  PERFORM initialize.

*&----------------------------------------------------------------------------&"
*&   Start of processing                                                      &"
*&----------------------------------------------------------------------------&"
START-OF-SELECTION.

  " Get data
  PERFORM get_data .

  " Generate CSV Files
  PERFORM export_data_to_csv .


*&----------------------------------------------------------------------------&"
*&   End of processing                                                        &"
*&----------------------------------------------------------------------------&"



*&----------------------------------------------------------------------------&"
*&   Notes                                                                    &"
*&----------------------------------------------------------------------------&"
" Methods of class /ui2/if_flp_cont_mgr :
*
*
*   ADD_ROLE_CATALOG_ASSIGNMENT
*   ADD_TILES_TMS_TO_CATALOG
*   ADD_TO_ROLE
*   CHECK_SERVICES
*   CHECK_TECH_PREREQUISITES
*   COPY_CATALOG
*   CREATE_CATALOG
*   DELETE_CATALOG
*   GET_ALL_CATALOGS                                      <<<<<<
*   GET_ALL_ROLES                                         <<<<<<
*   GET_ALL_TILES_TMS                                         x
*   GET_CATALOG                                               x
*   GET_CATALOGS_OF_ROLES                                     x
*   GET_CATALOGS_OF_TILE_TM                                   x
*   GET_GROUP
*   GET_GROUPS_OF_ROLE
*   GET_ICF_SERVICES
*   GET_LAST_SERVICE_CHECK_RESULT
*   GET_MESSAGES_OF_TILE_TM                                   x
*   GET_ODATA_SERVICES
*   GET_ROLES_OF_CATALOG                                      x
*   GET_ROLES_OF_TILE_TM                                      x
*   GET_SPACES_OF_ROLE                                        x
*   GET_SUCCESSOR_TCODES_FOR_TCODE
*   GET_TILES_TMS_OF_CATALOG                              <<<<<<
*   GET_TILES_TMS_OF_ROLE                                     x
*   INITIALIZE_TABLES_FOR_CATALOGS                        <<<<<<
*   INITIALIZE_TABLES_FOR_ROLES                           <<<<<<
*   IS_SERVICE_CHECK_ALLOWED
*   REMOVE_CATALOGS_FROM_ROLES
*   REMOVE_FROM_ROLE
*   REMOVE_ROLE_CATALOG_ASSIGNMENT
*   REMOVE_TILES_TMS_FROM_CATALOG
*   RENAME_CATALOG
*   SERVICE_CHECK_ALL
*   SERVICE_CHECK_FOR_CATALOGS
*   SERVICE_CHECK_FOR_ROLES
*   SERVICE_CHECK_FOR_TTMS
*   TRANSPORT_CATALOG
*   UPDATE_ROLE_STATUS
*
*
*

****************************************************
INTERFACE lif_abapmerge_marker.
* abapmerge 0.14.8 - 2023-01-13T10:06:52.252Z
ENDINTERFACE.
****************************************************
