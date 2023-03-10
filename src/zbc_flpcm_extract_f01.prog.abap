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
  " ??? Paramters Text Label
  APPEND VALUE #( key = 'P_SCOPE' spras = 'F' text = 'Scope'  ) TO gt_report_texts.
  APPEND VALUE #( key = 'P_SCOPE' spras = 'E' text = 'Scope'  ) TO gt_report_texts.

  " ??? Report Texts
  APPEND VALUE #( key = 'SCOPE_CONF_DESCR' spras = 'F' text = 'Configuration'  ) TO gt_report_texts.
  APPEND VALUE #( key = 'SCOPE_CONF_DESCR' spras = 'F' text = 'Configuration'  ) TO gt_report_texts.

  APPEND VALUE #( key = 'SCOPE_CUST_DESCR' spras = 'F' text = 'Customizing'    ) TO gt_report_texts.
  APPEND VALUE #( key = 'SCOPE_CUST_DESCR' spras = 'E' text = 'Customizing'    ) TO gt_report_texts.



  " --------------------------------------------------------------------------
  "  Initialize Utils
  " --------------------------------------------------------------------------
  go_progress_indicator = new /ui2/cl_gui_progress_indicator( ).

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
                                                            "   -> This list does not contains all data (regarding /ui2/flpcm_cust)
                                                            "   -> There is some missing tile/tm, so we will not use the result
                                                            "      but the call to get_all_tiles_tms remains usefull to increase
                                                            "      execution performance (reducing run time from 15min to 3min)


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
*    " Get Tile r??le
*    lt_roles = go_flp_cont_mgr->get_roles_of_tile_tm( gs_tile_tm_list ).
*  ENDLOOP.



" Lire les roles d'une tuile c'est lire les r??les du catalogue en utilisant MT_ROLE_CATALOG_ASSIGNMENT qui est priv?? et pas de method pour la r??cup??rer
" (aliment?? par initialize_tables_for_roles)

" Lien CATALOG-TILE_TM :: TILE_TM_LIST-TILE_ORIG_CATALOG_ID <> CATALOG_LIST-ID


  " -> Si on veut limit?? au type de catalogue
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

  DATA: lt_tile_tm_list        TYPE          /ui2/if_flp_cont_mgr=>tt_tile_tm_combination_sorted ,
        ls_tile_tm_list        TYPE          /ui2/if_flp_cont_mgr=>ts_tile_tm_combination        ,
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
        lv_langu_txt           TYPE          sptxt                                               ,
        lv_chip_id             TYPE          /ui2/chip_id_pbc                                    ,
        lv_sptxt               TYPE          sptxt                                               ,
        lv_ttm_type            TYPE          /ui2/fdm_tile_type                                  ,
        lv_catalog_orig_id     TYPE          /ui2/fcm_ttm_tile_cat_id                            ,
        lv_description         TYPE          /ui2/fcm_role_description                           .

  FIELD-SYMBOLS: <fst_text_table> TYPE ANY TABLE ,
                 <fss_text_table> TYPE ANY       ,
                 <fss_text_langu> TYPE ANY       ,
                 <fss_text_title> TYPE ANY       .



  "  Process Data
  " ----------------------------------------------------------
  PERFORM show_progress_indicator USING 'Collecting Data for CSV Extraction' '' '' '' .

  "
  " Logic :
  "
  "   - We have to read catalog by catalog
  "   - For one catalog, we have to retrieve all Tile/TM for it because method
  "     go_flp_cont_mgr->get_all_tiles_tms( ) do not returns the entire list.
  "     So we can not use a simple READ TABLE on the global list.
  "
  "     --> Performances are affected, but without this logic, we have missing data....
  "
  "         -> get_all_tiles_tms( ) Method + READ TABLE : 1 minutes 26 secondes of run for :
  "           -> Catalogs : 2159
  "           -> Final Tile TM : 17002
  "
  "         -> Nested Loop : 15 minutes 8 secondes of run for :
  "           -> Catalogs : 2159
  "           -> Final Tile TM : 17487 ( +485 )
  "
  "         -> Nested Loop when get_all_tiles_tms( ) is called : 2 minutes 36 secondes of run for :
  "           -> Catalogs : 2159
  "           -> Final Tile TM : 17487 ( +485 )
  "
  " First, we have to process
  LOOP AT gt_catalog_list INTO gs_catalog_list.
    REFRESH lt_tile_tm_list.

*    IF sy-tabix GE 10. EXIT. ENDIF. " @TMP_NDU

    ls_catalogue_key = go_type_mapper->catalog_key_from_flat( gs_catalog_list ).
    lt_tile_tm_list  = go_flp_cont_mgr->get_tiles_tms_of_catalog( ls_catalogue_key ).
    lt_roles         = go_flp_cont_mgr->get_roles_of_catalog( ls_catalogue_key ).      " Returns Father Roles


    " Catalog Parsing
    APPEND VALUE #(
      catalog_id  = gs_catalog_list-id
      type        = gs_catalog_list-type
      scope       = gs_catalog_list-scope
      master_lang = gs_catalog_list-master_language
    ) TO gt_csv_catalog .


    " Text Parsing (Is there is at least 1 text in the table)
    lv_prefixed_catalog_id = COND #( WHEN gs_catalog_list-type EQ gc_catalog_typ_car
              THEN |{ gc_catalog_id_prexif_car }:{ gs_catalog_list-id }:{ gs_catalog_list-system_alias }| " X-SAP-UI2-ADCAT:<catalog_id>:<system_alias>
              ELSE |{ gc_catalog_id_prexif_cat }:{ gs_catalog_list-id }|                                   " X-SAP-UI2-CATALOGPAGE:<catalog_id>
    ).

    " Text Table depend of the scope. (CUST or CONF)
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
          PERFORM langu_to_text USING <fss_text_langu> CHANGING lv_sptxt .

          " Append to CSV Table
          APPEND VALUE #(
            catalog_id = gs_catalog_list-id
            langu      = <fss_text_langu>
            sptxt      = lv_sptxt
            title      = <fss_text_title>
          ) TO gt_csv_catalogt.
        ENDIF.
      ENDLOOP.

      UNASSIGN: <fst_text_table> ,
                <fss_text_langu> ,
                <fss_text_title> .
    ENDIF.


    " Read all Tile & TM of the catalog
    LOOP AT lt_tile_tm_list INTO ls_tile_tm_list .

      " Generates the full ID
      PERFORM tile_tm_to_chip_id USING ls_tile_tm_list CHANGING lv_chip_id .

      " Get Type
      PERFORM get_tile_tm_type USING ls_tile_tm_list CHANGING lv_ttm_type .

      " Get Catalog Origin ID
      PERFORM get_tile_tm_orig_catalog_id USING ls_tile_tm_list CHANGING lv_catalog_orig_id .

      " Tile TM Parsing
      APPEND VALUE #(
        cdm3_ba_id      = ls_tile_tm_list-cdm3_ba_id
        type            = lv_ttm_type
        tile_orig_id    = ls_tile_tm_list-tile_orig_id
        tm_orig_id      = ls_tile_tm_list-tm_orig_id
        catalog_orig_id = lv_catalog_orig_id
        semantic_obj    = ls_tile_tm_list-semantic_object
        semantic_act    = ls_tile_tm_list-semantic_action
        fiori_id        = ls_tile_tm_list-fiori_id
        transaction     = ls_tile_tm_list-transaction
      ) TO gt_csv_tiletm .


      " Text Parsing (Is there is at least 1 text in the table)
      LOOP AT gt_propt INTO gs_propt WHERE bag_parentid = lv_chip_id .
        " Get Language Designation Its name in its language
        PERFORM langu_to_text USING gs_propt-langu CHANGING lv_sptxt .

        " -> que des display_text
        APPEND VALUE #(
*          cdm3_ba_id = gs_tile_tm_list-cdm3_ba_id
          cdm3_ba_id = ls_tile_tm_list-cdm3_ba_id
          langu      = gs_propt-langu
          sptxt      = lv_sptxt
          title      = gs_propt-value
        ) TO gt_csv_tiletmt .

      ENDLOOP.

    ENDLOOP.


    " Roles Processing
    LOOP AT lt_roles INTO ls_roles .

      " Get Role Description
      READ TABLE gt_role_list INTO gs_role_list WITH KEY role_name = ls_roles-role_name .

      CLEAR lv_description.
      IF sy-subrc EQ 0.
        lv_description = gs_role_list-description .
      ENDIF.

      " Role Parsing
      APPEND VALUE #(
        catalog_id  = gs_catalog_list-id
        agr_name    = ls_roles-role_name
        description = lv_description
      ) TO gt_csv_role .

    ENDLOOP.

  ENDLOOP.



  "  Make CSV File for Role
  " ----------------------------------------------------------


  "  Write CSV Files
  " ----------------------------------------------------------
*  PERFORM show_progress_indicator USING 'Extracting Catalogs' '' '' '' .
  PERFORM int_tab_to_csv_file USING '/tmp/flpcm_cust_catalogs.csv'       CHANGING gt_csv_catalog  .

*  PERFORM show_progress_indicator USING 'Extracting Catalogs Texts' '' '' '' .
  PERFORM int_tab_to_csv_file USING '/tmp/flpcm_cust_catalogs_texts.csv' CHANGING gt_csv_catalogt .

*  PERFORM show_progress_indicator USING 'Extracting Tile & Target Mappings' '' '' '' .
  PERFORM int_tab_to_csv_file USING '/tmp/flpcm_cust_tile_tm.csv'        CHANGING gt_csv_tiletm   .

*  PERFORM show_progress_indicator USING 'Extracting Tile & Target Mappings Texts' '' '' '' .
  PERFORM int_tab_to_csv_file USING '/tmp/flpcm_cust_tile_tm_texts.csv'  CHANGING gt_csv_tiletmt  .

*  PERFORM show_progress_indicator USING 'Etractings Roles' '' '' '' .
  PERFORM int_tab_to_csv_file USING '/tmp/flpcm_cust_role.csv'           CHANGING gt_csv_role     .


ENDFORM.
"&---[   EXPORT_DATA_TO_CSV   ]-----------------------------------------------&"
"&----------------------------------------------------------------------------&"


FORM langu_to_text    USING iv_langu TYPE langu
                   CHANGING cv_text  TYPE sptxt .

  " Get Language Designation Its name in its language
  READ TABLE gt_t002t INTO gs_t002t WITH KEY spras = iv_langu   " Translation Lang
                                             sprsl = iv_langu . " Language Code

  IF sy-subrc EQ 0.
    cv_text = gs_t002t-sptxt .
  ENDIF.

ENDFORM.


FORM tile_tm_to_chip_id    USING is_tile_tm TYPE /ui2/if_flp_cont_mgr=>ts_tile_tm_combination
                        CHANGING cv_chip_id TYPE /ui2/chip_id_pbc                              .

  " When entri is Tile + Target Mapping
  " In any case, title cam from Target Map (synchronize even they represents independant entry)
  " But for Tile Only, we have to use TILE_ORIG_ID instead of TM_ORIG_ID.
  cv_chip_id = COND #(
    WHEN is_tile_tm-tile_tm_matching EQ gc_ico_tile " White square under a yellow one (Tile only -> no TM ID)
    THEN
      COND #(
        WHEN is_tile_tm-tm_orig_catalog_type EQ gc_catalog_typ_car
        THEN |{ gc_chip_id_prefix_car }:{ gc_catalog_id_prexif_car }:{ is_tile_tm-tile_orig_catalog_id }:{ is_tile_tm-tm_orig_catalog_sysalias }:{ is_tile_tm-tile_orig_id }|
        ELSE |{ gc_chip_id_prefix_cat }:{ gc_catalog_id_prexif_cat }:{ is_tile_tm-tile_orig_catalog_id }:{ is_tile_tm-tile_orig_id }|
      )
    ELSE
      COND #(
        WHEN is_tile_tm-tm_orig_catalog_type EQ gc_catalog_typ_car
        THEN |{ gc_chip_id_prefix_car }:{ gc_catalog_id_prexif_car }:{ is_tile_tm-tm_orig_catalog_id }:{ is_tile_tm-tm_orig_catalog_sysalias }:{ is_tile_tm-tm_orig_id }|
        ELSE |{ gc_chip_id_prefix_cat }:{ gc_catalog_id_prexif_cat }:{ is_tile_tm-tm_orig_catalog_id }:{ is_tile_tm-tm_orig_id }|
      )
  ).

ENDFORM .


FORM get_tile_tm_type     USING is_tile_tm  TYPE /ui2/if_flp_cont_mgr=>ts_tile_tm_combination
                       CHANGING cv_ttm_type TYPE /ui2/fdm_tile_type                           .

  CASE is_tile_tm-tile_tm_matching .
    WHEN gc_ico_tile .
      cv_ttm_type = gc_ttm_type_tile .

    WHEN gc_ico_tiletm .
      cv_ttm_type = gc_ttm_type_tile_tm .

    WHEN gc_ico_tm .
      cv_ttm_type = gc_ttm_type_tm .

    WHEN OTHERS.

  ENDCASE.

ENDFORM.

FORM get_tile_tm_orig_catalog_id      USING is_tile_tm     TYPE /ui2/if_flp_cont_mgr=>ts_tile_tm_combination
                                   CHANGING cv_cat_orig_id TYPE /ui2/fcm_ttm_tile_cat_id                     .

  " When entri is Tile + Target Mapping
  " In any case, title cam from Target Map (synchronize even they represents independant entry)
  " But for Tile Only, we have to use TILE_ORIG_ID instead of TM_ORIG_ID.
  cv_cat_orig_id = COND #(
    WHEN is_tile_tm-tile_tm_matching EQ gc_ico_tile " White square under a yellow one (Tile only -> no TM ID)
    THEN
      |{ is_tile_tm-tile_orig_catalog_id }|
    ELSE
      |{ is_tile_tm-tm_orig_catalog_id }|
  ).

ENDFORM .


FORM show_progress_indicator USING iv_msgv1 TYPE msgv1
                                   iv_msgv2 TYPE msgv2
                                   iv_msgv3 TYPE msgv3
                                   iv_msgv4 TYPE msgv4 .

  go_progress_indicator->/ui2/if_gui_progress_indicator~progress_indicate_for_msg(
    is_msg                = value #(
      msgid = '0K'
      msgno = 000
      msgty = 'I'
      msgv1 = iv_msgv1
      msgv2 = iv_msgv2
      msgv3 = iv_msgv3
      msgv4 = iv_msgv4
    )
    iv_num_item_processed = 0
    iv_num_item_total     = 1
  ).

ENDFORM.



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


" Pour avoir toutes les tuiles pour 1 catalogue :
"
"   -> Ne pas utiliser get_all_tile_tms machin et les r??cup??rer individuellement MAIS la concerver pour les perfs
"
"
"
"   mt_tile_tm_combination = mo_flp_cont_mgr->get_tiles_tms_of_catalog( lo_type_mapper->catalog_key_from_flat( mt_selected_catalog[ 1 ] ) ).
"
